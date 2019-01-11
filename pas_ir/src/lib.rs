pub mod interpret;

use {
    std::fmt,
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
    Call { out: Option<Value>, function: Value, args: Vec<Value>, }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const IX_WIDTH: usize = 8;
        match self {
            Instruction::LocalAlloc(id, ty) => write!(f, "{:>width$} {} of {}", "local", id, ty, width = IX_WIDTH),
            Instruction::LocalDelete(id) => write!(f, "{:>width$} {}", "drop", id, width = IX_WIDTH),
            Instruction::Set { out, new_val } => write!(f, "{:>width$} {} = {}", "set", out, new_val, width = IX_WIDTH),
            Instruction::Add { out, a, b } => write!(f, "{:>width$} {} = {} + {}", "add", out, a, b, width = IX_WIDTH),

            Instruction::Call { out, function, args } => {
                write!(f, "{:>width$} ", "call", width = IX_WIDTH)?;
                if let Some(out_val) = out {
                    write!(f, "{} = ", out_val)?;
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
        }
    }
}

struct Local {
    id: usize,
    name: Option<String>,
}

struct Scope {
    locals: Vec<Local>,
}

pub struct Builder {
    next_id: usize,
    instructions: Vec<Instruction>,
    scopes: Vec<Scope>,
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

    pub fn get_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    pub fn append(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn new_local(&mut self, ty: Type, name: Option<&str>) -> Value {
        let id = self.get_id();
        self.instructions.push(Instruction::LocalAlloc(id, ty));

        let current_scope = self.scopes.iter_mut().rev().next()
            .expect("scope must be active");

        current_scope.locals.push(Local {
            id,
            name: name.map(str::to_string),
        });

        Value::Local(id)
    }

    fn find_local(&self, name: &str) -> Option<&Local> {
        self.scopes.iter().rev()
            .filter_map(|scope| {
                scope.locals.iter()
                    .find(|local| {
                        local.name.as_ref()
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
        let top_scope = self.scopes.pop().unwrap();
        for local in top_scope.locals.iter().rev() {
            self.instructions.push(Instruction::LocalDelete(local.id));
        }
    }
}

fn translate_bin_op(bin_op: &pas_ty::ast::BinOp, out_ty: &pas_ty::Type, builder: &mut Builder) -> Value {
    let out_ty = translate_type(out_ty);
    let out_val = builder.new_local(out_ty, None);

    builder.begin_scope();
    let lhs_val = translate_expr(&bin_op.lhs, builder);
    let rhs_val = translate_expr(&bin_op.rhs, builder);

    let op_instruction = match bin_op.op {
        syn::Operator::Plus => Instruction::Add { out: out_val.clone(), a: lhs_val, b: rhs_val },
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
            Value::LiteralI32(i.as_i32().unwrap())
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
                        .map(|local| Value::Local(local.id))
                        .unwrap()
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
            let binding_id = builder.new_local(val_ty, Some(&binding.name.name));

            builder.begin_scope();
            let val_id = translate_expr(&binding.val, builder);
            builder.append(Instruction::Set { out: binding_id, new_val: val_id });

            builder.end_scope();
        }
        
        ast::Statement::Call(call) => {
            translate_call(call, builder);
        }
    }
}

pub fn translate(unit: &pas_ty::ast::Unit) -> Vec<Instruction> {
    let mut builder = Builder::new();

    for stmt in &unit.init {
        translate_stmt(stmt, &mut builder);
    }

    builder.finish()
}
