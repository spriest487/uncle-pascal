use pas_ir::{self as ir, Label, LocalID};
use crate::ast::{
    Type,
    Module,
};
use std::fmt;
use pas_ir::metadata::{FunctionID, StringID};

pub enum InfixOp {
    Eq,
    Assign,
    Add,
}

impl fmt::Display for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InfixOp::Eq => write!(f, "=="),
            InfixOp::Assign => write!(f, "="),
            InfixOp::Add => write!(f, "+"),
        }
    }
}

pub enum Expr {
    Local(LocalID),
    Function(FunctionID),
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
}

impl Expr {
    pub fn translate_val(v: &ir::Value) -> Self {
        match v {
            ir::Value::LiteralBool(b) => Expr::LitBool(*b),
            ir::Value::LiteralNull => Expr::Null,
            ir::Value::LiteralI32(i) => Expr::LitInt(*i as i64),
            ir::Value::LiteralF32(f) => Expr::LitFloat(*f as f64),
            ir::Value::Ref(r) => Expr::translate_ref(r),
        }
    }

    pub fn translate_ref(r: &ir::Ref) -> Self {
        match r {
            ir::Ref::Local(local_id) => Expr::Local(*local_id),
            ir::Ref::Deref(inner) => Expr::translate_val(inner.as_ref()).deref(),
            ir::Ref::Global(ir::GlobalRef::Function(id)) => Expr::Function(*id),
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

    fn translate_infix_op(lhs: &ir::Value, op: InfixOp, rhs: &ir::Value) -> Self {
        let lhs_expr = Expr::translate_val(lhs);
        let rhs_expr = Expr::translate_val(rhs);

        Self::infix_op(lhs_expr, op, rhs_expr)
    }

    pub fn translate_assign(out: &ir::Ref, val: Self) -> Self {
        let out_ref = Expr::translate_ref(out);
        Self::infix_op(out_ref, InfixOp::Assign, val)
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
            Expr::Function(id) => write!(f, "Function{}", id.0),
            Expr::Call { func, args } => {
                write!(f, "{}(", func)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            },
        }
    }
}

pub enum Statement {
    VariableDecl {
        ty: Type,
        id: ir::LocalID
    },
    Expr(Expr),
    BeginBlock,
    EndBlock,
    Label(Label),
    Goto(Label),
    Comment(String),
}

impl Statement {
    pub fn translate(instruction: &ir::Instruction, module: &mut Module) -> Vec<Self> {
        match instruction {
            ir::Instruction::LocalAlloc(id, ty) => {
                let ty = Type::from_metadata(ty, module);
                vec![Statement::VariableDecl { ty, id: *id }]
            },

            ir::Instruction::LocalBegin => vec![Statement::BeginBlock],
            ir::Instruction::LocalEnd => vec![Statement::EndBlock],
            ir::Instruction::Label(label) => vec![Statement::Label(*label)],
            ir::Instruction::Jump { dest } => vec![Statement::Goto(*dest)],
            ir::Instruction::Comment(text) => {
                let safe_text = text
                    .replace("/*", "")
                    .replace("*/", "");
                vec![Statement::Comment(safe_text)]
            }

            ir::Instruction::AddrOf { out, a } => {
                let addr = Expr::translate_ref(a).addr_of();
                vec![Statement::Expr(Expr::translate_assign(out, addr))]
            }

            ir::Instruction::Move { out, new_val } => {
                let val = Expr::translate_val(new_val);
                vec![Statement::Expr(Expr::translate_assign(out, val))]
            }

            ir::Instruction::Eq { out, a, b } => {
                let cmp = Expr::translate_infix_op(a, InfixOp::Eq, b);
                vec![Statement::Expr(Expr::translate_assign(out, cmp))]
            }

            ir::Instruction::Add { out, a, b } => {
                let add = Expr::translate_infix_op(a, InfixOp::Add, b);
                vec![Statement::Expr(Expr::translate_assign(out, add))]
            }

            ir::Instruction::Call { out, function, args } => {
                let func_expr = Expr::translate_val(function);
                let args = args.iter()
                    .map(Expr::translate_val);

                let call = Expr::call(func_expr, args);

                vec![Statement::Expr(match out {
                    Some(out) => {
                        Expr::translate_assign(out, call)
                    }

                    None => call,
                })]
            }

            _ => {
                eprintln!("missing: C backend translation of instruction `{}`", instruction);
                Vec::new()
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
        let instruction_stmts = Statement::translate(instruction, module);
        stmts.extend(instruction_stmts);
    }
    stmts
}
