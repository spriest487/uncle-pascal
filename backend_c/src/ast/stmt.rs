use crate::ast::expr::Expr;
use crate::ast::expr::InfixOp;
use crate::ast::expr::PrefixOp;
use crate::ast::ty_def::FieldName;
use crate::ast::FunctionName;
use crate::ast::Module;
use crate::ast::Type;
use ir_lang as ir;
use std::fmt;

#[derive(Clone, Debug)]
pub enum GlobalName {
    ClassInstance(ir::TypeDefID),

    StringLiteral(ir::StringID),

    StaticClosure(ir::StaticClosureID),
}

impl fmt::Display for GlobalName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GlobalName::ClassInstance(id) => write!(f, "Class_{}", id.0),
            GlobalName::StringLiteral(id) => write!(f, "String_{}", id.0),
            GlobalName::StaticClosure(id) => write!(f, "StaticClosure_{}", id.0),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    VariableDecl {
        ty: Type,
        id: ir::LocalID,
        null_init: bool,
    },
    Expr(Expr),
    BeginBlock,
    EndBlock,
    Label(ir::Label),
    Goto(ir::Label),
    Comment(String),
    IfCond {
        cond: Expr,
        then: Box<Statement>,
    },
    Return(Expr),
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
            },

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
            },

            Statement::Return(expr) => write!(f, "return {};", expr),
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
            },

            ir::Instruction::DebugPush(ctx) => self
                .stmts
                .push(Statement::Comment(format!("context: {}", ctx))),
            ir::Instruction::DebugPop => {
                // no-op
            },

            ir::Instruction::LocalBegin => self.stmts.push(Statement::BeginBlock),
            ir::Instruction::LocalEnd => self.stmts.push(Statement::EndBlock),

            ir::Instruction::Label(label) => {
                self.stmts.push(Statement::Label(*label));
                // this might be at end end of a block, which C doesn't allow,
                // so insert an empty block too so there's something to label
                self.stmts.push(Statement::BeginBlock);
                self.stmts.push(Statement::EndBlock);
            },

            ir::Instruction::Jump { dest } => self.stmts.push(Statement::Goto(*dest)),
            ir::Instruction::JumpIf { dest, test } => {
                let cond_expr = Expr::translate_val(test, self.module);
                self.stmts.push(Statement::IfCond {
                    cond: cond_expr,
                    then: Box::new(Statement::Goto(*dest)),
                });
            },
            ir::Instruction::Comment(text) => {
                let safe_text = text.replace("/*", "").replace("*/", "");
                self.stmts.push(Statement::Comment(safe_text));
            },

            ir::Instruction::AddrOf { out, a } => {
                let addr = Expr::translate_ref(a, self.module).addr_of();
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    addr,
                    self.module,
                )));
            },

            ir::Instruction::Move { out, new_val } => {
                let val = Expr::translate_val(new_val, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    val,
                    self.module,
                )));
            },

            ir::Instruction::Eq(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Eq, b);
            },

            ir::Instruction::Add(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Add, b);
            },

            ir::Instruction::Sub(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Sub, b);
            },

            ir::Instruction::Mul(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Mul, b);
            },

            ir::Instruction::Mod(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Rem, b);
            },

            ir::Instruction::IDiv(ir::BinOpInstruction { out, a, b }) => {
                // TODO: make sure integer divisions with 2 floats returns an int
                self.write_infix_op(out, a, InfixOp::Div, b);
            },

            ir::Instruction::FDiv(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Div, b);
            },

            ir::Instruction::Shl(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Shl, b);
            },

            ir::Instruction::Shr(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::Shr, b);
            },

            ir::Instruction::BitAnd(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::BitAnd, b);
            },

            ir::Instruction::BitOr(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::BitOr, b);
            },

            ir::Instruction::BitXor(ir::BinOpInstruction { out, a, b }) => {
                self.write_infix_op(out, a, InfixOp::BitXor, b);
            },

            ir::Instruction::BitNot(ir::UnaryOpInstruction { out, a }) => {
                let val = Expr::PrefixOp {
                    op: PrefixOp::BitNot,
                    operand: Box::new(Expr::translate_val(a, self.module)),
                };
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    val,
                    self.module,
                )))
            },

            ir::Instruction::Element { out, a, index, .. } => {
                let element = Expr::translate_element(a, index, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    element,
                    self.module,
                )));
            },

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
            },

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
            },

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
            },

            ir::Instruction::Call {
                out,
                function,
                args,
            } => {
                self.translate_call(out.as_ref(), function, args);
            },

            ir::Instruction::RcNew { out, type_id: struct_id } => {
                self.translate_rc_new(out, *struct_id);
            },

            ir::Instruction::Gt(ir::BinOpInstruction { out, a, b }) => {
                let gt = Expr::translate_infix_op(a, InfixOp::Gt, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    gt,
                    self.module,
                )))
            },

            ir::Instruction::Gte(ir::BinOpInstruction { out, a, b }) => {
                let gt = Expr::translate_infix_op(a, InfixOp::Gte, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    gt,
                    self.module,
                )))
            },

            ir::Instruction::Lt(ir::BinOpInstruction { out, a, b }) => {
                let gt = Expr::translate_infix_op(a, InfixOp::Lt, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    gt,
                    self.module,
                )))
            },
            ir::Instruction::Lte(ir::BinOpInstruction { out, a, b }) => {
                let gt = Expr::translate_infix_op(a, InfixOp::Lte, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    gt,
                    self.module,
                )))
            },
            

            ir::Instruction::And(ir::BinOpInstruction { out, a, b }) => {
                let and = Expr::translate_infix_op(a, InfixOp::And, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    and,
                    self.module,
                )))
            },

            ir::Instruction::Or(ir::BinOpInstruction { out, a, b }) => {
                let or = Expr::translate_infix_op(a, InfixOp::Or, b, self.module);
                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    or,
                    self.module,
                )))
            },

            ir::Instruction::Not(ir::UnaryOpInstruction { out, a }) => {
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
            },

            ir::Instruction::Retain { at, weak } => {                
                let retain = Expr::Function(FunctionName::RcRetain);

                let rc_ptr = Expr::translate_ref(at, self.module);
                let call_retain = Expr::call(retain, vec![rc_ptr, Expr::LitBool(*weak)]);

                self.stmts.push(Statement::Expr(call_retain));
            },

            ir::Instruction::Release { at, weak } => {
                let release = Expr::Function(FunctionName::RcRelease);

                let rc_ptr = Expr::translate_ref(at, self.module);
                let call_release = Expr::call(release, vec![rc_ptr, Expr::LitBool(*weak)]);

                self.stmts.push(Statement::Expr(call_release));
            },

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
            },

            ir::Instruction::ClassIs { out, a, class_id } => {
                self.translate_is(out, a, *class_id);
            },

            ir::Instruction::Raise { val } => {
                let raise_func = Expr::Function(FunctionName::Raise);
                let val_expr = Expr::translate_ref(val, self.module);
                self.stmts
                    .push(Statement::Expr(Expr::call(raise_func, vec![val_expr])));
            },

            ir::Instruction::Cast { ty, out, a } => {
                let ty = Type::from_metadata(ty, self.module);
                let expr = Expr::translate_val(a, self.module);

                // TODO: just assume any valid pascal cast is a valid C cast for now...
                let cast_result = Expr::Cast(Box::new(expr), ty);

                self.stmts.push(Statement::Expr(Expr::translate_assign(
                    out,
                    cast_result,
                    self.module,
                )));
            },
        }
    }

    fn write_infix_op(&mut self, out: &ir::Ref, lhs: &ir::Value, op: InfixOp, rhs: &ir::Value) {
        let and = Expr::translate_infix_op(lhs, op, rhs, self.module);
        self.stmts.push(Statement::Expr(Expr::translate_assign(
            out,
            and,
            self.module,
        )))
    }

    fn translate_is(&mut self, out: &ir::Ref, a: &ir::Value, class_id: ir::VirtualTypeID) {
        let actual_expr = Expr::translate_val(a, self.module);

        // get class ptr from rc
        let rc_ptr = actual_expr.cast(Type::Rc.ptr());
        let actual_class_ptr = rc_ptr.arrow(FieldName::RcClass);

        let is = match class_id {
            ir::VirtualTypeID::Class(struct_id) => {
                let is_class_ptr = Expr::class_ptr(struct_id);
                Expr::infix_op(actual_class_ptr, InfixOp::Eq, is_class_ptr)
            },

            ir::VirtualTypeID::Any => {
                // `is Any` is true for anything!
                Expr::LitBool(true)
            },

            ir::VirtualTypeID::Closure(_func_ty_id) => {
                // TODO - can you use `is` on a function type?
                Expr::LitBool(false)
            },

            ir::VirtualTypeID::Interface(iface_id) => {
                let is_impl_func = Expr::Function(FunctionName::IsImpl);

                Expr::call(
                    is_impl_func,
                    vec![actual_class_ptr, Expr::LitInt(iface_id.0 as i128)],
                )
            },
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

    fn translate_rc_new(&mut self, out: &ir::Ref, struct_id: ir::TypeDefID) {
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
