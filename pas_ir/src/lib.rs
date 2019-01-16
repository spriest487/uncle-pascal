pub mod metadata;
pub mod interpret;

use {
    std::{
        fmt,
        collections::HashMap,
    },
    pas_syn::{
        self as syn,
        ast,
    },
    pas_typecheck as pas_ty,
    crate::metadata::*,
};

pub use {
    self::interpret::Interpreter,
};

#[derive(Debug, Clone)]
pub enum Value {
    Local(usize),
    LiteralBool(bool),
    LiteralI32(i32),
    LiteralF32(f32),
    Global(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Local(id) => write!(f, "${}", id),
            Value::LiteralI32(i) => write!(f, "{}i32", i),
            Value::Global(name) => write!(f, "`{}`", name),
            Value::LiteralBool(b) => write!(f, "{}", b),
            Value::LiteralF32(x) => write!(f, "{:.6}", x),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Label(usize);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    LocalAlloc(usize, Type),
    LocalDelete(usize),

    Set { out: Value, new_val: Value },
    Add { out: Value, a: Value, b: Value },

    Call { out: Option<Value>, function: Value, args: Vec<Value>, },

    GetField { out: Value, of: Value, struct_id: StructId, field_id: usize },
    SetField { of: Value, new_val: Value, struct_id: StructId, field_id: usize },

    Label(Label),
    Jump { dest: Label, },
    JumpIf { dest: Label, test: Value, },

//    MemAlloc { out: Value, element: Type, count: usize, },
//    MemFree { at: Value, },
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

            Instruction::GetField { out, of, struct_id, field_id } => {
                write!(f, "{:>width$} ", "getfld", width = IX_WIDTH)?;
                write!(f, "{} := ({} as {}).{}", out, of, Type::Struct(*struct_id), field_id)
            }

            Instruction::SetField { of, new_val, struct_id, field_id } => {
                write!(f, "{:>width$} ", "setfld", width = IX_WIDTH)?;
                write!(f, "({} as {}).{} := {}", of, Type::Struct(*struct_id), field_id, new_val)
            }

            Instruction::Label(label) => {
                write!(f, "{:>width$} {}", "label", label, width = IX_WIDTH)
            }

            Instruction::Jump { dest } => {
                write!(f, "{:>width$} {}", "jmp", dest, width = IX_WIDTH)
            }

            Instruction::JumpIf { dest, test } => {
                write!(f, "{:>width$} {} if {}", "jmpif", dest, test, width = IX_WIDTH)
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

pub struct Builder<'m> {
    metadata: &'m Metadata,

    instructions: Vec<Instruction>,
    scopes: Vec<Scope>,
    next_id: usize,
}

impl<'m> Builder<'m> {
    pub fn new(metadata: &'m Metadata) -> Self {
        Self {
            metadata,
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

    pub fn alloc_label(&mut self) -> Label {
        let id = self.next_id;
        self.next_id += 1;
        Label(id)
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
    let out_ty = builder.metadata.translate_type(out_ty);
    let out_val = builder.new_local(out_ty, None);

    builder.begin_scope();
    let lhs_val = translate_expr(&bin_op.lhs, builder);

    let op_instruction = match &bin_op.op {
        syn::Operator::Member => {
            let struct_name = bin_op.lhs.annotation.ty
                .full_name()
                .expect("member access must be of a named type");
            let member_name = bin_op.rhs.expr.as_ident().map(|i| i.to_string())
                .expect("rhs of member binop must be an ident");

            let (struct_id, struct_def) = builder.metadata.find_struct(&struct_name)
                .expect("referenced struct must exist");
            let field_id = struct_def.find_field(&member_name)
                .expect("referenced field must exist");

            Instruction::GetField { out: out_val.clone(), of: lhs_val, struct_id, field_id }
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

    let out_ty = match &sig.return_ty {
        pas_ty::Type::None => None,
        return_ty => Some(builder.metadata.translate_type(return_ty)),
    };

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

fn translate_object_ctor(ctor: &pas_ty::ast::ObjectCtor, builder: &mut Builder) -> Value {
    let (struct_id, struct_def) = builder.metadata.find_struct(&ctor.ident.to_string())
        .unwrap_or_else(|| panic!("struct {} referenced in object ctor must exist", ctor.ident));

    let out_val = builder.new_local(Type::Struct(struct_id), None);

    builder.begin_scope();

    // todo: lookup members by id, don't assume they're in order
    for member in &ctor.args.members {
        let member_val = translate_expr(&member.value, builder);
        let field_id = struct_def.find_field(&member.ident.to_string())
            .unwrap_or_else(|| panic!("field {} referenced in object ctor must exist", member.ident));

        builder.append(Instruction::SetField {
            of: out_val.clone(),
            new_val: member_val,
            struct_id,
            field_id
        });
    }

    builder.end_scope();
    out_val
}

fn translate_literal(lit: &ast::Literal, ty: &pas_ty::Type, _builder: &mut Builder) -> Value {
    match lit {
        ast::Literal::Boolean(b) => {
            assert_eq!(pas_ty::Type::Boolean, *ty);
            Value::LiteralBool(*b)
        }

        ast::Literal::Integer(i) => {
            match ty {
                pas_ty::Type::Integer => {
                    Value::LiteralI32(i.as_i32()
                        .expect("Int32-typed constant must be within range of i32"))
                },
                _ => panic!("bad type for integer literal: {}", ty),
            }
        }

        ast::Literal::Real(r) => {
            match ty {
                pas_ty::Type::Real32 => {
                    Value::LiteralF32(r.as_f32()
                        .expect("Real32-typed constant must be within range of f32"))
                },
                _ => panic!("bad type for real literal: {}", ty),
            }
        }

        ast::Literal::String(_s) => {
            unimplemented!("IR string literal")
        },
    }
}

fn translate_if_cond(if_cond: &pas_ty::ast::IfCond, builder: &mut Builder) -> Option<Value> {
    let out_val = match &if_cond.annotation.ty {
        pas_ty::Type::None => None,
        out_ty => {
            let out_ty = builder.metadata.translate_type(out_ty);
            Some(builder.new_local(out_ty, None))
        }
    };

    builder.begin_scope();

    let then_label = builder.alloc_label();
    let end_label = builder.alloc_label();
    let else_label = if_cond.else_branch.as_ref().map(|_| builder.alloc_label());

    let test_val = translate_expr(&if_cond.cond, builder);
    builder.append(Instruction::JumpIf { test: test_val, dest: then_label });

    if let Some(else_label) = else_label {
        builder.append(Instruction::Jump { dest: else_label });
    }

    builder.append(Instruction::Label(then_label));
    let then_val = translate_expr(&if_cond.then_branch, builder);
    if let Some(out_val) = out_val.clone() {
        builder.append(Instruction::Set { out: out_val, new_val: then_val });
    }
    builder.append(Instruction::Jump { dest: end_label });

    if let Some(else_branch) = &if_cond.else_branch {
        builder.append(Instruction::Label(else_label.unwrap()));

        let else_val = translate_expr(else_branch, builder);
        if let Some(out_val) = out_val.clone() {
            builder.append(Instruction::Set { out: out_val, new_val: else_val });
        }
    }

    builder.append(Instruction::Label(end_label));
    builder.end_scope();

    out_val
}

pub fn translate_expr(
    expr: &pas_ty::ast::ExpressionNode,
    builder: &mut Builder,
) -> Value {
    match expr.expr.as_ref() {
        ast::Expression::Literal(lit) => {
            translate_literal(lit, &expr.annotation.ty, builder)
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

        ast::Expression::ObjectCtor(ctor) => {
            translate_object_ctor(ctor, builder)
        }

        ast::Expression::IfCond(if_cond) => {
            translate_if_cond(if_cond, builder)
                .expect("conditional used in expression must have a type")
        }

        ast::Expression::Block(block) => {
            translate_block(block, builder)
                .expect("block used in expression must have a type")
        }

//        _ => unimplemented!("expression IR for {}", expr),
    }
}

pub fn translate_stmt(stmt: &pas_ty::ast::Statement, builder: &mut Builder) {
    match stmt {
        ast::Statement::LetBinding(binding) => {
            let val_ty = builder.metadata.translate_type(&binding.val_ty);
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

pub fn translate_func(func: &pas_ty::ast::FunctionDecl, metadata: &Metadata) -> Function {
    let mut body_builder = Builder::new(metadata);

    let return_ty = func.return_ty.as_ref()
        .map(|ty| body_builder.metadata.translate_type(ty));

    for (i, param) in func.params.iter().enumerate() {
        // if the function returns a value, $0 is the return pointer, and args start at $1
        let id = if return_ty.is_some() {
            i + 1
        } else {
            i
        };

        body_builder.bind_local(id, param.ident.to_string());
    }

    let block_output = translate_block(&func.body, &mut body_builder);

    if let Some(return_val) = block_output {
        let return_at = Value::Local(0);
        body_builder.append(Instruction::Set { out: return_at, new_val: return_val });
    }

    Function {
        body: body_builder.finish(),
        return_ty,
    }
}

fn translate_block(block: &pas_ty::ast::Block, builder: &mut Builder) -> Option<Value> {
    let out_val = block.output.as_ref().map(|out_expr| {
        let out_ty = builder.metadata.translate_type(&out_expr.annotation.ty);
        builder.new_local(out_ty, None)
    });

    builder.begin_scope();

    for stmt in &block.statements {
        translate_stmt(stmt, builder);
    }

    if let Some(out) = &block.output {
        let result_val = translate_expr(out, builder);
        builder.append(Instruction::Set { out: out_val.clone().unwrap(), new_val: result_val });
    }

    builder.end_scope();

    out_val
}

#[derive(Clone, Debug)]
pub struct Unit {
    metadata: Metadata,
    functions: HashMap<String, Function>,
    init: Vec<Instruction>,
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "* Structures")?;
        for (id, struct_def) in self.metadata.structs() {
            writeln!(f, "{{{}}} ({}):", id, struct_def.name)?;
            for (id, field) in &struct_def.fields {
                writeln!(f, "  {:8>} ({}): {}", format!(".{}", id), field.name, field.ty)?;
            }
            writeln!(f)?;
        }

        writeln!(f, "* Functions")?;
        for (name, func) in &self.functions {
            writeln!(f, "{}:", name)?;
            for instruction in &func.body {
                writeln!(f, "{}", instruction)?;
            }
            writeln!(f)?;
        }

        writeln!(f, "* Init:")?;
        for instruction in &self.init {
            writeln!(f, "{}", instruction)?;
        }
        Ok(())
    }
}

pub fn translate_unit(unit: &pas_ty::ast::Unit) -> Unit {
    let mut metadata = Metadata::new();
    let mut functions = HashMap::new();

    for ty_decl in unit.type_decls() {
        match ty_decl {
            ast::TypeDecl::Class(class) => {
                metadata.translate_struct(class);
            }
        }
    }

    for func_decl in unit.func_decls() {
        let func = translate_func(func_decl, &metadata);
        functions.insert(func_decl.ident.to_string(), func);
    }

    let mut init_builder = Builder::new(&metadata);

    for stmt in &unit.init {
        translate_stmt(stmt, &mut init_builder);
    }

    Unit {
        init: init_builder.finish(),
        functions,
        metadata,
    }
}
