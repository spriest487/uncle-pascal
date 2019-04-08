use pas_ir::{self as ir, Label, LocalID};
use crate::ast::{Type, Module, StructName, FunctionName};
use std::fmt;
use ir::metadata::{self, FunctionID, StringID};
use crate::ast::ty::FieldName;
use std::io::SeekFrom::Start;

pub enum InfixOp {
    Eq,
    Assign,
    Add,
    Sub,
}

impl fmt::Display for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InfixOp::Eq => write!(f, "=="),
            InfixOp::Assign => write!(f, "="),
            InfixOp::Add => write!(f, "+"),
            InfixOp::Sub => write!(f, "-"),
        }
    }
}

pub enum Expr {
    Local(LocalID),
    Function(FunctionName),
    Deref(Box<Expr>),
    LitString(StringID),
    LitBool(bool),
    LitInt(i64),
    LitFloat(f64),
    Null,
    InfixOp { lhs: Box<Expr>, op: InfixOp, rhs: Box<Expr> },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    AddrOf(Box<Expr>),
    Element { base: Box<Expr>, index: Box<Expr> },
    Field { base: Box<Expr>, field: FieldName },
    Arrow { base: Box<Expr>, field: FieldName },
    Cast(Box<Expr>, Type),
}

impl Expr {
    pub fn translate_val(v: &ir::Value, module: &Module) -> Self {
        match v {
            ir::Value::LiteralBool(b) => Expr::LitBool(*b),
            ir::Value::LiteralNull => Expr::Null,
            ir::Value::LiteralI32(i) => Expr::LitInt(*i as i64),
            ir::Value::LiteralF32(f) => Expr::LitFloat(*f as f64),
            ir::Value::Ref(r) => Expr::translate_ref(r, module),
        }
    }

    pub fn translate_ref(r: &ir::Ref, module: &Module) -> Self {
        match r {
            ir::Ref::Local(local_id) => Expr::Local(*local_id),
            ir::Ref::Deref(inner) => Expr::translate_val(inner.as_ref(), module).deref(),
            ir::Ref::Global(ir::GlobalRef::Function(id)) => {
                let name = module.function_name(*id);
                Expr::Function(name)
            },
            ir::Ref::Global(ir::GlobalRef::StringLiteral(id)) => Expr::LitString(*id),
        }
    }

    pub fn deref(self) -> Self {
        Expr::Deref(Box::new(self))
    }

    pub fn addr_of(self) -> Self {
        Expr::AddrOf(Box::new(self))
    }

    pub fn infix_op(lhs: Self, op: InfixOp, rhs: Self) -> Self {
        Expr::InfixOp {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }

    pub fn call(func: Self, args: impl IntoIterator<Item=Self>) -> Self {
        Expr::Call {
            func: Box::new(func),
            args: args.into_iter().collect(),
        }
    }

    fn translate_infix_op(lhs: &ir::Value, op: InfixOp, rhs: &ir::Value, module: &Module) -> Self {
        let lhs_expr = Expr::translate_val(lhs, module);
        let rhs_expr = Expr::translate_val(rhs, module);

        Self::infix_op(lhs_expr, op, rhs_expr)
    }

    pub fn translate_assign(out: &ir::Ref, val: Self, module: &Module) -> Self {
        let out_ref = Expr::translate_ref(out, module);
        Self::infix_op(out_ref, InfixOp::Assign, val)
    }

    pub fn translate_element(a: &ir::Ref, index: &ir::Value, module: &Module) -> Self {
        let array_expr = Expr::translate_ref(a, module);
        let elements_expr = Expr::Field {
            base: Box::new(array_expr),
            field: FieldName::StaticArrayElements,
        };

        let index_expr = Expr::translate_val(index, module);
        let element = Expr::Element {
            base: Box::new(elements_expr),
            index: Box::new(index_expr),
        };

        element.addr_of()
    }

    pub fn translate_field(a: &ir::Ref, of_ty: &metadata::Type, field: FieldName, module: &Module) -> Self {
        let a_expr = Expr::translate_ref(a, module);

        match of_ty.rc_resource_type_id() {
            // pointer to RC containing pointer to class resource
            Some(resource_ty) => {
                let class_ty = match resource_ty {
                    metadata::ClassID::Class(struct_id) => Type::Struct(StructName::ID(struct_id)),
                    _ => panic!("bad resource type {:?} in Field instruction target", resource_ty),
                };

                let resource_ptr = Expr::Arrow { base: Box::new(a_expr), field: FieldName::RcResource };
                let class_ptr = Expr::Cast(Box::new(resource_ptr), class_ty.ptr());

                Expr::Arrow { base: Box::new(class_ptr), field }.addr_of()
            }

            // local struct
            None => {
                let field = Expr::Field {
                    base: Box::new(a_expr),
                    field,
                };

                field.addr_of()
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::LitString(_id) => write!(f, "/* NYI: string literals */ NULL"),
            Expr::LitFloat(x) => write!(f, "{}", x),
            Expr::LitInt(i) => write!(f, "{}", i),
            Expr::LitBool(b) => write!(f, "{}", b),
            Expr::Deref(inner) => write!(f, "(*({}))", inner),
            Expr::AddrOf(inner) => write!(f, "&({})", inner),
            Expr::InfixOp { lhs, op, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::Null => write!(f, "NULL"),
            Expr::Local(id) => write!(f, "L{}", id.0),
            Expr::Function(name) => write!(f, "{}", name),
            Expr::Call { func, args } => {
                write!(f, "{}(", func)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expr::Element { base, index } => {
                write!(f, "({})[{}]", base, index)
            }
            Expr::Field { base, field } => {
                write!(f, "({}).{}", base, field)
            }
            Expr::Arrow { base, field } => {
                write!(f, "({})->{}", base, field)
            }
            Expr::Cast(value, ty) => {
                write!(f, "(({}){})", ty.typename(), value)
            }
        }
    }
}

pub enum Statement {
    VariableDecl {
        ty: Type,
        id: ir::LocalID,
    },
    Expr(Expr),
    BeginBlock,
    EndBlock,
    Label(Label),
    Goto(Label),
    Comment(String),
}

impl Statement {
    pub fn translate(instruction: &ir::Instruction, module: &mut Module, stmts: &mut Vec<Self>) {
        match instruction {
            ir::Instruction::LocalAlloc(id, ty) => {
                let ty = Type::from_metadata(ty, module);
                stmts.push(Statement::VariableDecl { ty, id: *id });
            }

            ir::Instruction::LocalBegin => stmts.push(Statement::BeginBlock),
            ir::Instruction::LocalEnd => stmts.push(Statement::EndBlock),
            ir::Instruction::Label(label) => stmts.push(Statement::Label(*label)),
            ir::Instruction::Jump { dest } => stmts.push(Statement::Goto(*dest)),
            ir::Instruction::Comment(text) => {
                let safe_text = text
                    .replace("/*", "")
                    .replace("*/", "");
                stmts.push(Statement::Comment(safe_text));
            }

            ir::Instruction::AddrOf { out, a } => {
                let addr = Expr::translate_ref(a, module).addr_of();
                stmts.push(Statement::Expr(Expr::translate_assign(out, addr, module)));
            }

            ir::Instruction::Move { out, new_val } => {
                let val = Expr::translate_val(new_val, module);
                stmts.push(Statement::Expr(Expr::translate_assign(out, val, module)));
            }

            ir::Instruction::Eq { out, a, b } => {
                let cmp = Expr::translate_infix_op(a, InfixOp::Eq, b, module);
                stmts.push(Statement::Expr(Expr::translate_assign(out, cmp, module)));
            }

            ir::Instruction::Add { out, a, b } => {
                let add = Expr::translate_infix_op(a, InfixOp::Add, b, module);
                stmts.push(Statement::Expr(Expr::translate_assign(out, add, module)));
            }

            ir::Instruction::Sub { out, a, b } => {
                let sub = Expr::translate_infix_op(a, InfixOp::Sub, b, module);
                stmts.push(Statement::Expr(Expr::translate_assign(out, sub, module)));
            }

            ir::Instruction::Element { out, a, index, element: _ } => {
                let element = Expr::translate_element(a, index, module);
                stmts.push(Statement::Expr(Expr::translate_assign(out, element, module)));
            }

            ir::Instruction::Field { out, a, of_ty, field } => {
                let field = Expr::translate_field(a, of_ty, FieldName::ID(*field), module);
                stmts.push(Statement::Expr(Expr::translate_assign(out, field, module)))
            }

            ir::Instruction::Call { out, function, args } => {
                let func_expr = Expr::translate_val(function, module);
                let args = args.iter()
                    .map(|arg_val| Expr::translate_val(arg_val, module));

                let call = Expr::call(func_expr, args);

                stmts.push(Statement::Expr(match out {
                    Some(out) => {
                        Expr::translate_assign(out, call, module)
                    }

                    None => call,
                }));
            }

            _ => {
                eprintln!("missing: C backend translation of instruction `{}`", instruction);
            }
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::VariableDecl { ty, id } => {
                let name = format!("L{}", id.0);
                write!(f, "{};", ty.to_decl_string(&name))
            }

            Statement::Expr(expr) => write!(f, "{};", expr),

            Statement::BeginBlock => write!(f, "{{"),
            Statement::EndBlock => write!(f, "}}"),

            Statement::Label(label) => write!(f, "J{}:", label.0),
            Statement::Goto(label) => write!(f, "goto J{};", label.0),

            Statement::Comment(text) => write!(f, "/* {} */", text),
        }
    }
}

pub fn translate_instructions(
    instructions: &[ir::Instruction],
    module: &mut Module,
) -> Vec<Statement> {
    let mut stmts = Vec::new();
    for instruction in instructions {
        stmts.push(Statement::Comment(instruction.to_string()));
        Statement::translate(instruction, module, &mut stmts);
    }
    stmts
}
