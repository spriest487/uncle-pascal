use pas_ir::{self as ir, Label, LocalID};
use crate::ast::{Type, Module, StructName, FunctionName};
use std::fmt;
use ir::metadata::{self, StringID, StructID};
use crate::ast::ty::FieldName;

pub enum InfixOp {
    Eq,
    Assign,
    Add,
    Sub,
    Gt,
    And,
    Or,
}

impl fmt::Display for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InfixOp::Eq => write!(f, "=="),
            InfixOp::Assign => write!(f, "="),
            InfixOp::Add => write!(f, "+"),
            InfixOp::Sub => write!(f, "-"),
            InfixOp::Gt => write!(f, ">"),
            InfixOp::And => write!(f, "&&"),
            InfixOp::Or => write!(f, "||"),
        }
    }
}

pub enum PrefixOp {
    Not,
}

impl fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PrefixOp::Not => write!(f, "!"),
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
    PrefixOp { op: PrefixOp, operand: Box<Expr>, },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    AddrOf(Box<Expr>),
    Element { base: Box<Expr>, index: Box<Expr> },
    Field { base: Box<Expr>, field: FieldName },
    Arrow { base: Box<Expr>, field: FieldName },
    Cast(Box<Expr>, Type),
    SizeOf(Type),
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
            }
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
            Expr::PrefixOp { op, operand } => write!(f, "({}({}))", op, operand),
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
            Expr::SizeOf(ty) => {
                write!(f, "sizeof({})", ty.typename())
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
    IfCond {
        cond: Expr,
        then: Box<Statement>,
    },
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

            Statement::IfCond { cond, then } => {
                writeln!(f, "if ({}) {{", cond)?;
                writeln!(f, "{}", then)?;
                writeln!(f, "}}")
            }
        }
    }
}

pub struct Builder<'a> {
    module: &'a mut Module,
    pub stmts: Vec<Statement>,
}

impl<'a> Builder<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Self {
            module,
            stmts: Vec::new(),
        }
    }

    pub fn translate_instructions(&mut self, instructions: &[ir::Instruction]) {
        for instruction in instructions {
            self.translate_instruction(instruction);
        }
    }

    fn translate_instruction(&mut self, instruction: &ir::Instruction) {
        self.stmts.push(Statement::Comment(instruction.to_string()));

        match instruction {
            ir::Instruction::LocalAlloc(id, ty) => {
                let ty = Type::from_metadata(ty, self.module);
                self.stmts.push(Statement::VariableDecl { ty, id: *id });
            }

            ir::Instruction::LocalBegin => self.stmts.push(Statement::BeginBlock),
            ir::Instruction::LocalEnd => self.stmts.push(Statement::EndBlock),
            ir::Instruction::Label(label) => self.stmts.push(Statement::Label(*label)),
            ir::Instruction::Jump { dest } => self.stmts.push(Statement::Goto(*dest)),
            ir::Instruction::JumpIf { dest, test } => {
                let cond_expr = Expr::translate_val(test, self.module);
                self.stmts.push(Statement::IfCond {
                    cond: cond_expr,
                    then: Box::new(Statement::Goto(*dest)),
                });
            }
            ir::Instruction::Comment(text) => {
                let safe_text = text
                    .replace("/*", "")
                    .replace("*/", "");
                self.stmts.push(Statement::Comment(safe_text));
            }

            ir::Instruction::AddrOf { out, a } => {
                let addr = Expr::translate_ref(a, self.module).addr_of();
                self.stmts.push(Statement::Expr(Expr::translate_assign(out, addr, self.module)));
            }

            ir::Instruction::Move { out, new_val } => {
                let val = Expr::translate_val(new_val, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(out, val, self.module)));
            }

            ir::Instruction::Eq { out, a, b } => {
                let cmp = Expr::translate_infix_op(a, InfixOp::Eq, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(out, cmp, self.module)));
            }

            ir::Instruction::Add { out, a, b } => {
                let add = Expr::translate_infix_op(a, InfixOp::Add, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(out, add, self.module)));
            }

            ir::Instruction::Sub { out, a, b } => {
                let sub = Expr::translate_infix_op(a, InfixOp::Sub, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(out, sub, self.module)));
            }

            ir::Instruction::Element { out, a, index, element: _ } => {
                let element = Expr::translate_element(a, index, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(out, element, self.module)));
            }

            ir::Instruction::Field { out, a, of_ty, field } => {
                let field = Expr::translate_field(a, of_ty, FieldName::ID(*field), self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(out, field, self.module)))
            }

            ir::Instruction::Call { out, function, args } => {
                self.translate_call(out.as_ref(), function, args);
            }

            ir::Instruction::RcNew { out, struct_id } => {
                self.translate_rc_new(out, *struct_id);
            }

            ir::Instruction::Gt { out, a, b } => {
                let gt = Expr::translate_infix_op(a, InfixOp::Gt, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(out, gt, self.module)))
            }

            ir::Instruction::And { out, a, b } => {
                let and = Expr::translate_infix_op(a, InfixOp::And, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(out, and, self.module)))
            }

            ir::Instruction::Or { out, a, b } => {
                let or = Expr::translate_infix_op(a, InfixOp::Or, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(out, or, self.module)))
            }

            ir::Instruction::Not { out, a } => {
                let a_expr = Expr::translate_val(a, self.module);
                let not = Expr::PrefixOp { op: PrefixOp::Not, operand: Box::new(a_expr) };
                self.stmts.push(Statement::Expr(Expr::translate_assign(out, not, self.module)))
            }

            ir::Instruction::Retain { at } => {
                let release = Expr::Function(FunctionName::RcRetain);
                let rc_ptr = Expr::translate_ref(at, self.module);
                self.stmts.push(Statement::Expr(Expr::call(release, vec![rc_ptr])))
            }

            ir::Instruction::Release { at } => {
                let release = Expr::Function(FunctionName::RcRelease);
                let rc_ptr = Expr::translate_ref(at, self.module);
                self.stmts.push(Statement::Expr(Expr::call(release, vec![rc_ptr])))
            }

            ir::Instruction::VirtualCall { .. } => {
                unimplemented!("virtual calls not implemented in C backend")
            }
        }
    }

    fn translate_call(
        &mut self,
        out: Option<&ir::Ref>,
        function: &ir::Value,
        args: &[ir::Value],
    ) {
        let func_expr = Expr::translate_val(function, self.module);
        let args = args.iter()
            .map(|arg_val| Expr::translate_val(arg_val, self.module));

        let call = Expr::call(func_expr, args);

        self.stmts.push(Statement::Expr(match out {
            Some(out) => {
                Expr::translate_assign(out, call, self.module)
            }

            None => call,
        }));
    }

    fn translate_rc_new(
        &mut self,
        out: &ir::Ref,
        struct_id: StructID,
    ) {
        let struct_ty = Type::Struct(StructName::ID(struct_id));
        let ty_size = Expr::SizeOf(struct_ty);

        let new_rc = Expr::Call {
            func: Box::new(Expr::Function(FunctionName::RcAlloc)),
            args: vec![ty_size],
        };

        self.stmts.push(Statement::Expr(Expr::translate_assign(out, new_rc, self.module)))
    }
}
