pub mod interpret;

use {
    std::{
        fmt,
        rc::Rc,
        collections::HashMap,
    },
    pas_syn::{
        self as syn,
        ast,
    },
    pas_typecheck as pas_ty
};

pub use {
    self::interpret::Interpreter,
};

#[derive(Debug, Clone)]
pub enum Type {
    None,
    I32,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Type::None => "none",
            Type::I32 => "i32",
        })
    }
}

fn translate_type(ty: &pas_ty::Type) -> Type {
    match ty {
        pas_typecheck::Type::None => Type::None,
        pas_typecheck::Type::Integer => Type::I32,
        pas_typecheck::Type::Function(_) => unimplemented!(),
        pas_typecheck::Type::Class(_) => unimplemented!(),
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Local(usize),
    LiteralI32(i32),
    Global(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Local(id) => write!(f, "${}", id),
            Value::LiteralI32(i) => write!(f, "{}i32", i),
            Value::Global(name) => write!(f, "`{}`", name),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    LocalAlloc(usize, Type),
    LocalDelete(usize),
    Set { out: Value, new_val: Value },
    Add { out: Value, a: Value, b: Value },
    Call { out: Option<Value>, function: Value, args: Vec<Value>, },
    Member { out: Value, of: Value, struct_name: Rc<String>, member: Rc<String> },
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const IX_WIDTH: usize = 8;
        match self {
            Instruction::LocalAlloc(id, ty) => write!(f, "{:>width$} {} of {}", "local", id, ty, width = IX_WIDTH),
            Instruction::LocalDelete(id) => write!(f, "{:>width$} {}", "drop", id, width = IX_WIDTH),
            Instruction::Set { out, new_val } => write!(f, "{:>width$} {} := {}", "set", out, new_val, width = IX_WIDTH),
            Instruction::Add { out, a, b } => write!(f, "{:>width$} {} := {} + {}", "add", out, a, b, width = IX_WIDTH),

            Instruction::Call { out, function, args } => {
                write!(f, "{:>width$} ", "call", width = IX_WIDTH)?;
                if let Some(out_val) = out {
                    write!(f, "{} := ", out_val)?;
                }
                write!(f, "{}(", function)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }

            Instruction::Member { out, of, struct_name, member } => {
                write!(f, "{:>width$} ", "member", width = IX_WIDTH)?;
                write!(f, "{} := ({} as {}).{}", out, of, struct_name, member)
            }
        }
    }
}

enum Local {
    // the builder created this local allocation and must track its lifetime to drop it
    Allocated {
        id: usize,
        name: Option<String>,
    },
    // the builder didn't create this local allocation but we know it exists e.g. we know where
    // function params are found due to the calling convention. we need to know its name but we
    // don't need to drop it
    Bound {
        id: usize,
        name: String,
    },
}

impl Local {
    fn id(&self) -> usize {
        match self {
            Local::Allocated { id, .. } => *id,
            Local::Bound { id, .. } => *id,
        }
    }

    fn name(&self) -> Option<&String> {
        match self {
            Local::Allocated { name, .. } => name.as_ref(),
            Local::Bound { name, .. } => Some(&name),
        }
    }
}

struct Scope {
    locals: Vec<Local>,
}

pub struct Builder {
    instructions: Vec<Instruction>,
    scopes: Vec<Scope>,
    next_id: usize,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            next_id: 0,
            scopes: vec![Scope { locals: Vec::new() }],
        }
    }

    pub fn finish(mut self) -> Vec<Instruction> {
        while !self.scopes.is_empty() {
            self.end_scope();
        }

        self.instructions
    }

    pub fn append(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn bind_local(&mut self, id: usize, name: String) {
        assert!(id >= self.next_id);
        assert!(!self.current_scope_mut().locals.iter()
            .any(|l| l.name() == Some(&name) || l.id() == id));

        self.current_scope_mut().locals.push(Local::Bound { id, name: name.to_string(), });

        self.next_id = usize::max(id + 1, self.next_id + 1);
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.iter_mut().rev().next()
            .expect("scope must be active")
    }

    pub fn new_local(&mut self, ty: Type, name: Option<String>) -> Value {
        let id = self.next_id;

        self.current_scope_mut().locals.push(Local::Allocated {
            id,
            name,
        });

        self.next_id += 1;

        self.instructions.push(Instruction::LocalAlloc(id, ty));
        Value::Local(id)
    }

    fn find_local(&self, name: &str) -> Option<&Local> {
        self.scopes.iter().rev()
            .filter_map(|scope| {
                scope.locals.iter()
                    .find(|local| {
                        local.name()
                            .map(|local_name| local_name == name)
                            .unwrap_or(false)
                    })
            })
            .next()
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(Scope {
            locals: Vec::new(),
        })
    }

    pub fn end_scope(&mut self) {
        let top_scope = self.scopes.pop()
            .expect("called end_scope with no active scope");

        for local in top_scope.locals.iter().rev() {
            if let Local::Allocated { id, .. } = local {
                self.instructions.push(Instruction::LocalDelete(*id));
            }
        }
    }
}

fn translate_bin_op(bin_op: &pas_ty::ast::BinOp, out_ty: &pas_ty::Type, builder: &mut Builder) -> Value {
    let out_ty = translate_type(out_ty);
    let out_val = builder.new_local(out_ty, None);

    builder.begin_scope();
    let lhs_val = translate_expr(&bin_op.lhs, builder);

    let op_instruction = match &bin_op.op {
        syn::Operator::Member => {
            Instruction::Member {
                out: out_val.clone(),
                of: lhs_val,
                struct_name: bin_op.lhs.annotation.ty.full_name()
                    .map(|ident| Rc::new(ident.to_string()))
                    .expect("member access must be of a named type"),
                member: bin_op.rhs.expr.as_ident()
                    .map(|ident| Rc::new(ident.to_string()))
                    .expect("rhs of member binop must be an ident"),
            }
        },

        syn::Operator::Plus => Instruction::Add {
            out: out_val.clone(),
            a: lhs_val,
            b: translate_expr(&bin_op.rhs, builder),
        },

        _ => unimplemented!("IR for op {}", bin_op.op),
    };

    builder.append(op_instruction);
    builder.end_scope();

    out_val
}

fn translate_call(call: &pas_ty::ast::Call, builder: &mut Builder) -> Option<Value> {
    let sig = match &call.target.annotation.ty {
        pas_ty::Type::Function(sig) => sig,
        _ => panic!("type of function target expr must be a function"),
    };

    let out_ty = sig.return_ty.as_ref()
        .map(|return_ty| translate_type(return_ty));

    let out_val = out_ty.map(|ty| builder.new_local(ty, None));

    builder.begin_scope();

    let target_val = translate_expr(&call.target, builder);

    let mut arg_vals = Vec::new();
    for arg in &call.args {
        arg_vals.push(translate_expr(arg, builder));
    }

    builder.append(Instruction::Call {
        function: target_val,
        args: arg_vals,
        out: out_val.clone(),
    });

    builder.end_scope();

    out_val
}

pub fn translate_expr(
    expr: &pas_ty::ast::ExpressionNode,
    builder: &mut Builder,
) -> Value {
    match expr.expr.as_ref() {
        ast::Expression::LiteralInt(i) => {
            Value::LiteralI32(i.as_i32()
                .expect("i32-typed int constant must be within range of i32"))
        },
        ast::Expression::BinOp(bin_op) => {
            translate_bin_op(bin_op, &expr.annotation.ty, builder)
        },
        ast::Expression::Ident(ident) => {
            match expr.annotation.value_kind {
                None => {
                    panic!("ident must have a type")
                },
                Some(pas_ty::ValueKind::Temporary) => {
                    panic!("temporaries cannot be referenced by ident")
                },

                Some(pas_ty::ValueKind::Function) => {
                    Value::Global(ident.name.clone())
                },

                Some(pas_ty::ValueKind::Immutable) => {
                    builder.find_local(&ident.name)
                        .map(|local| Value::Local(local.id()))
                        .unwrap_or_else(|| panic!("identifier not found in local scope: {}", ident))
                }
            }
        },
        ast::Expression::Call(call) => {
            translate_call(call, builder)
                .expect("call used in expression must have a return type")
        }
        _ => unimplemented!("expression IR for {}", expr),
    }
}

pub fn translate_stmt(stmt: &pas_ty::ast::Statement, builder: &mut Builder) {
    match stmt {
        ast::Statement::LetBinding(binding) => {
            let val_ty = translate_type(&binding.val_ty);
            let binding_id = builder.new_local(val_ty, Some(binding.name.to_string()));

            builder.begin_scope();
            let val_id = translate_expr(&binding.val, builder);
            builder.append(Instruction::Set { out: binding_id, new_val: val_id });

            builder.end_scope();
        }
        
        ast::Statement::Call(call) => {
            translate_call(call, builder);
        }

        ast::Statement::Exit(_) => {
            unimplemented!()
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    body: Vec<Instruction>,
    return_ty: Option<Type>,
}

pub fn translate_func(func: &pas_ty::ast::FunctionDecl) -> Function {
    let mut body_builder = Builder::new();

    let return_ty = func.return_ty.as_ref().map(translate_type);

    for (i, param) in func.params.iter().enumerate() {
        // if the function returns a value, $0 is the return pointer, and args start at $1
        let id = if return_ty.is_some() {
            i + 1
        } else {
            i
        };

        body_builder.bind_local(id, param.ident.to_string());
    }

    for stmt in &func.body.statements {
        translate_stmt(stmt, &mut body_builder);
    }

    if let Some(return_out) = &func.body.output {
        let out_val = Value::Local(0);
        let return_val = translate_expr(return_out, &mut body_builder);
        body_builder.append(Instruction::Set { out: out_val, new_val: return_val });
    }

    Function {
        body: body_builder.finish(),
        return_ty,
    }
}

#[derive(Clone, Debug)]
pub struct Struct {
    fields: Vec<(Rc<String>, Type)>,
}

fn translate_struct(struct_def: &pas_ty::ast::Class) -> Struct {
    let mut fields = Vec::new();
    for member in &struct_def.members {
        let name = Rc::new(member.ident.to_string());
        let ty = translate_type(&member.ty);
        fields.push((name, ty));
    }

    Struct { fields }
}

#[derive(Clone, Debug)]
pub struct Unit {
    structs: HashMap<Rc<String>, Struct>,
    functions: HashMap<String, Function>,
    init: Vec<Instruction>,
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (name, func) in &self.functions {
            writeln!(f, "{}:", name)?;
            for instruction in &func.body {
                writeln!(f, "{}", instruction)?;
            }
        }

        writeln!(f, "init:")?;
        for instruction in &self.init {
            writeln!(f, "{}", instruction)?;
        }
        Ok(())
    }
}

pub fn translate_unit(unit: &pas_ty::ast::Unit) -> Unit {
    let mut functions = HashMap::new();
    let mut structs = HashMap::new();

    for decl in &unit.decls {
        match decl {
            ast::UnitDecl::Function(func_decl) => {
                let func = translate_func(func_decl);
                functions.insert(func_decl.ident.to_string(), func);
            }

            ast::UnitDecl::Type(ast::TypeDecl::Class(class)) => {
                let struct_def = translate_struct(class);
                let name = Rc::new(class.ident.to_string());
                structs.insert(name, struct_def);
            }
        }
    }

    let mut init_builder = Builder::new();

    for stmt in &unit.init {
        translate_stmt(stmt, &mut init_builder);
    }

    Unit {
        init: init_builder.finish(),
        functions,
        structs,
    }
}
