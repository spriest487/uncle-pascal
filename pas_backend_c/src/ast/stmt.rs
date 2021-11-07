use std::fmt;

use pas_ir::{
    self as ir,
    metadata::{self, ClassID, StringID, StructID},
    Label, LocalID,
};

use crate::ast::{ty::FieldName, FunctionName, Module, StructName, Type};

#[allow(unused)]
pub enum InfixOp {
    Eq,
    Assign,
    Add,
    Sub,
    Gt,
    And,
    Or,
    Mul,
    Div,
    Rem,
    Shl,
    Shr,
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
            InfixOp::Mul => write!(f, "*"),
            InfixOp::Div => write!(f, "/"),
            InfixOp::Rem => write!(f, "%"),
            InfixOp::Shl => write!(f, "<<"),
            InfixOp::Shr => write!(f, ">>"),
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

#[allow(unused)]
pub enum Expr {
    Local(LocalID),
    Function(FunctionName),
    Class(StructID),
    Deref(Box<Expr>),
    LitString(StringID),
    LitBool(bool),
    LitInt(i64),
    LitFloat(f64),
    Null,
    InfixOp {
        lhs: Box<Expr>,
        op: InfixOp,
        rhs: Box<Expr>,
    },
    PrefixOp {
        op: PrefixOp,
        operand: Box<Expr>,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    AddrOf(Box<Expr>),
    Element {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    Field {
        base: Box<Expr>,
        field: FieldName,
    },
    Arrow {
        base: Box<Expr>,
        field: FieldName,
    },
    Cast(Box<Expr>, Type),
    SizeOf(Type),
}

impl Expr {
    pub fn translate_val(v: &ir::Value, module: &Module) -> Self {
        match v {
            ir::Value::LiteralBool(b) => Expr::LitBool(*b),
            ir::Value::LiteralNull => Expr::Null,
            ir::Value::LiteralI32(i) => Expr::LitInt(i64::from(*i)),
            ir::Value::LiteralByte(i) => Expr::LitInt(i64::from(*i)),
            ir::Value::LiteralF32(f) => Expr::LitFloat(f64::from(*f)),
            ir::Value::Ref(r) => Expr::translate_ref(r, module),
        }
    }

    pub fn translate_ref(r: &ir::Ref, module: &Module) -> Self {
        match r {
            ir::Ref::Discard => panic!("can't translate a discard ref, it should only be used in assignments"),
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

    pub fn call(func: Self, args: impl IntoIterator<Item = Self>) -> Self {
        Expr::Call {
            func: Box::new(func),
            args: args.into_iter().collect(),
        }
    }

    pub fn cast(self, ty: Type) -> Self {
        Expr::Cast(Box::new(self), ty)
    }

    fn translate_infix_op(lhs: &ir::Value, op: InfixOp, rhs: &ir::Value, module: &Module) -> Self {
        let lhs_expr = Expr::translate_val(lhs, module);
        let rhs_expr = Expr::translate_val(rhs, module);

        Self::infix_op(lhs_expr, op, rhs_expr)
    }

    pub fn translate_assign(out: &ir::Ref, val: Self, module: &Module) -> Self {
        match out {
            ir::Ref::Discard => val,
            _ => {
                let out_ref = Expr::translate_ref(out, module);
                Self::infix_op(out_ref, InfixOp::Assign, val)
            }
        }
    }

    pub fn translate_element(arr: &ir::Ref, index: &ir::Value, module: &Module) -> Self {
        let array_expr = Expr::translate_ref(arr, module);
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

    pub fn translate_field(
        a: &ir::Ref,
        of_ty: &metadata::Type,
        field_id: metadata::FieldID,
        module: &Module,
    ) -> Self {
        let a_expr = Expr::translate_ref(a, module);

        match of_ty {
            metadata::Type::RcPointer(class_id) => {
                // pointer to RC containing pointer to class resource
                let class_ty = match class_id {
                    Some(metadata::ClassID::Class(struct_id)) => {
                        Type::Struct(StructName::Struct(*struct_id))
                    }

                    _ => panic!(
                        "bad resource type {:?} in Field instruction target",
                        class_id
                    ),
                };

                let resource_ptr = Expr::Arrow {
                    base: Box::new(a_expr),
                    field: FieldName::RcResource,
                };
                let class_ptr = resource_ptr.cast(class_ty.ptr());

                let field_ref = Expr::Arrow {
                    base: Box::new(class_ptr),
                    field: FieldName::ID(field_id),
                };
                field_ref.addr_of()
            }

            _ => {
                // local struct
                let field = Expr::Field {
                    base: Box::new(a_expr),
                    field: FieldName::ID(field_id),
                };

                field.addr_of()
            }
        }
    }

    pub fn class_ptr(struct_id: StructID) -> Self {
        Expr::Class(struct_id)
            .addr_of()
            .cast(Type::Struct(StructName::Class).ptr())
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::LitString(id) => write!(f, "&StringRc_{}", id.0),
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
            Expr::Element { base, index } => write!(f, "({})[{}]", base, index),
            Expr::Field { base, field } => write!(f, "({}).{}", base, field),
            Expr::Arrow { base, field } => write!(f, "({})->{}", base, field),
            Expr::Cast(value, ty) => write!(f, "(({}){})", ty.typename(), value),
            Expr::SizeOf(ty) => write!(f, "sizeof({})", ty.typename()),
            Expr::Class(struct_id) => write!(f, "Class_{}", struct_id),
        }
    }
}

pub enum Statement {
    VariableDecl {
        ty: Type,
        id: ir::LocalID,
        null_init: bool,
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
            Statement::VariableDecl { ty, id, null_init } => {
                let name = format!("L{}", id.0);
                write!(f, "{}", ty.to_decl_string(&name))?;

                if *null_init {
                    write!(f, " = NULL")?;
                }

                write!(f, ";")
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
        if self.module.opts.trace_ir {
            self.stmts.push(Statement::Comment(instruction.to_string()));
        }

        match instruction {
            ir::Instruction::LocalAlloc(id, ty) => {
                let null_init = ty.is_rc();
                let ty = Type::from_metadata(ty, self.module);
                self.stmts.push(Statement::VariableDecl {
                    ty,
                    id: *id,
                    null_init,
                });
            }

            ir::Instruction::LocalBegin => self.stmts.push(Statement::BeginBlock),
            ir::Instruction::LocalEnd => self.stmts.push(Statement::EndBlock),

            ir::Instruction::Label(label) => {
                self.stmts.push(Statement::Label(*label));
                // this might be at end end of a block, which C doesn't allow,
                // so insert an empty block too so there's something to label
                self.stmts.push(Statement::BeginBlock);
                self.stmts.push(Statement::EndBlock);
            }

            ir::Instruction::Jump { dest } => self.stmts.push(Statement::Goto(*dest)),
            ir::Instruction::JumpIf { dest, test } => {
                let cond_expr = Expr::translate_val(test, self.module);
                self.stmts.push(Statement::IfCond {
                    cond: cond_expr,
                    then: Box::new(Statement::Goto(*dest)),
                });
            }
            ir::Instruction::Comment(text) => {
                let safe_text = text.replace("/*", "").replace("*/", "");
                self.stmts.push(Statement::Comment(safe_text));
            }

            ir::Instruction::AddrOf { out, a } => {
                let addr = Expr::translate_ref(a, self.module).addr_of();
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    addr,
                    self.module,
                )));
            }

            ir::Instruction::Move { out, new_val } => {
                let val = Expr::translate_val(new_val, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    val,
                    self.module,
                )));
            }

            ir::Instruction::Eq { out, a, b } => {
                let cmp = Expr::translate_infix_op(a, InfixOp::Eq, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    cmp,
                    self.module,
                )));
            }

            ir::Instruction::Add { out, a, b } => {
                let add = Expr::translate_infix_op(a, InfixOp::Add, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    add,
                    self.module,
                )));
            }

            ir::Instruction::Sub { out, a, b } => {
                let sub = Expr::translate_infix_op(a, InfixOp::Sub, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    sub,
                    self.module,
                )));
            }

            ir::Instruction::Mul { out, a, b } => {
                let mul_result = Expr::translate_infix_op(a, InfixOp::Mul, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    mul_result,
                    self.module,
                )));
            }

            ir::Instruction::IDiv { out, a, b } => {
                let div_result = Expr::translate_infix_op(a, InfixOp::Div, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    div_result,
                    self.module,
                )));
            }

            ir::Instruction::Shl { out, a, b } => {
                let div_result = Expr::translate_infix_op(a, InfixOp::Shl, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    div_result,
                    self.module,
                )));
            }

            ir::Instruction::Shr { out, a, b } => {
                let div_result = Expr::translate_infix_op(a, InfixOp::Shr, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    div_result,
                    self.module,
                )));
            }

            ir::Instruction::Element { out, a, index, .. } => {
                let element = Expr::translate_element(a, index, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    element,
                    self.module,
                )));
            }

            ir::Instruction::Field {
                out,
                a,
                of_ty,
                field,
            } => {
                let field = Expr::translate_field(a, of_ty, *field, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    field,
                    self.module,
                )))
            }

            ir::Instruction::VariantTag { out, a, .. } => {
                let tag_field = Expr::Field {
                    base: Box::new(Expr::translate_ref(a, self.module)),
                    field: FieldName::VariantTag,
                };

                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    tag_field.addr_of(),
                    self.module,
                )));
            }

            ir::Instruction::VariantData { out, a, tag, .. } => {
                let data_field = Expr::Field {
                    base: Box::new(Expr::translate_ref(a, self.module)),
                    field: FieldName::VariantData,
                };

                let case_field = Expr::Field {
                    base: Box::new(data_field),
                    field: FieldName::VariantDataCase(*tag),
                };

                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    case_field.addr_of(),
                    self.module,
                )));
            }

            ir::Instruction::Call {
                out,
                function,
                args,
            } => {
                self.translate_call(out.as_ref(), function, args);
            }

            ir::Instruction::RcNew { out, struct_id } => {
                self.translate_rc_new(out, *struct_id);
            }

            ir::Instruction::Gt { out, a, b } => {
                let gt = Expr::translate_infix_op(a, InfixOp::Gt, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    gt,
                    self.module,
                )))
            }

            ir::Instruction::And { out, a, b } => {
                let and = Expr::translate_infix_op(a, InfixOp::And, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    and,
                    self.module,
                )))
            }

            ir::Instruction::Or { out, a, b } => {
                let or = Expr::translate_infix_op(a, InfixOp::Or, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    or,
                    self.module,
                )))
            }

            ir::Instruction::Not { out, a } => {
                let a_expr = Expr::translate_val(a, self.module);
                let not = Expr::PrefixOp {
                    op: PrefixOp::Not,
                    operand: Box::new(a_expr),
                };
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    not,
                    self.module,
                )))
            }

            ir::Instruction::Retain { at } => {
                let retain = Expr::Function(FunctionName::RcRetain);
                let rc_ptr = Expr::translate_ref(at, self.module);
                let call_retain = Expr::call(retain, vec![rc_ptr]);

                self.stmts.push(Statement::Expr(call_retain));
            }

            ir::Instruction::Release { at } => {
                let release = Expr::Function(FunctionName::RcRelease);
                let rc_ptr = Expr::translate_ref(at, self.module);
                let call_release = Expr::call(release, vec![rc_ptr]);

                self.stmts.push(Statement::Expr(call_release));
            }

            ir::Instruction::DynAlloc {
                out,
                element_ty,
                len,
            } => {
                let get_mem = Expr::Function(FunctionName::GetMem);

                let el_ty = Type::from_metadata(&element_ty, self.module);
                let sizeof_el = Expr::SizeOf(el_ty.clone());

                let el_count = Expr::translate_val(len, self.module);
                let total_len = Expr::infix_op(sizeof_el, InfixOp::Mul, el_count);

                let call_get_mem = Expr::call(get_mem, vec![total_len])
                    .cast(el_ty.ptr());
                let assign_result = Expr::translate_assign(out, call_get_mem, self.module);

                self.stmts.push(Statement::Expr(assign_result));
            }

            ir::Instruction::DynFree { at } => {
                let free_mem = Expr::Function(FunctionName::FreeMem);
                let at_ptr = Expr::translate_ref(at, self.module);
                let as_u8 = at_ptr.cast(Type::UChar.ptr());
                let call_free_mem = Expr::call(free_mem, vec![as_u8]);

                self.stmts.push(Statement::Expr(call_free_mem));
            }

            ir::Instruction::VirtualCall {
                out,
                iface_id,
                method,
                self_arg,
                rest_args,
            } => {
                let method_func = Expr::Function(FunctionName::Method(*iface_id, *method));

                let mut args = vec![Expr::translate_val(self_arg, self.module)];
                args.extend(
                    rest_args
                        .iter()
                        .map(|arg| Expr::translate_val(arg, self.module)),
                );

                let call = Expr::call(method_func, args);

                self.stmts.push(Statement::Expr(match out {
                    Some(out) => Expr::translate_assign(out, call, self.module),
                    None => call,
                }));
            }

            ir::Instruction::ClassIs { out, a, class_id } => {
                self.translate_is(out, a, *class_id);
            }

            ir::Instruction::Raise { val } => {
                let raise_func = Expr::Function(FunctionName::Raise);
                let val_expr = Expr::translate_ref(val, self.module);
                self.stmts.push(Statement::Expr(Expr::call(raise_func, vec![val_expr])));
            }

            ir::Instruction::SizeOf { ty, out } => {
                let ty = Type::from_metadata(&ty, self.module);
                let size_of_expr = Expr::SizeOf(ty);

                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    size_of_expr,
                    self.module,
                )));
            }
        }
    }

    fn translate_is(&mut self, out: &ir::Ref, a: &ir::Value, class_id: ClassID) {
        let actual_expr = Expr::translate_val(a, self.module);

        // get class ptr from rc
        let actual_class_ptr = Expr::Arrow {
            base: Box::new(actual_expr),
            field: FieldName::RcClass,
        };

        let is = match class_id {
            metadata::ClassID::Class(struct_id) => {
                let is_class_ptr = Expr::class_ptr(struct_id);
                Expr::infix_op(actual_class_ptr, InfixOp::Eq, is_class_ptr)
            }

            metadata::ClassID::Interface(iface_id) => {
                let is_impl_func = Expr::Function(FunctionName::IsImpl);

                Expr::call(
                    is_impl_func,
                    vec![actual_class_ptr, Expr::LitInt(iface_id.0 as i64)],
                )
            }
        };
        self.stmts.push(Statement::Expr(Expr::translate_assign(
            out,
            is,
            self.module,
        )));
    }

    fn translate_call(&mut self, out: Option<&ir::Ref>, function: &ir::Value, args: &[ir::Value]) {
        let func_expr = Expr::translate_val(function, self.module);
        let args = args
            .iter()
            .map(|arg_val| Expr::translate_val(arg_val, self.module));

        let call = Expr::call(func_expr, args);

        self.stmts.push(Statement::Expr(match out {
            Some(out) => Expr::translate_assign(out, call, self.module),

            None => call,
        }));
    }

    fn translate_rc_new(&mut self, out: &ir::Ref, struct_id: StructID) {
        let ty_class_ptr = Expr::class_ptr(struct_id);

        let new_rc = Expr::Call {
            func: Box::new(Expr::Function(FunctionName::RcAlloc)),
            args: vec![ty_class_ptr],
        };

        self.stmts.push(Statement::Expr(Expr::translate_assign(
            out,
            new_rc,
            self.module,
        )))
    }
}
