use {
    crate::{
        prelude::*,
        translate_stmt,
    },
    pas_typecheck as pas_ty,
    pas_syn::{
        self as syn,
        ast as ast,
    },
};

pub fn translate_expr(
    expr: &pas_ty::ast::ExpressionNode,
    builder: &mut Builder,
) -> Ref {
    let out_val = match expr.expr.as_ref() {
        ast::Expression::Literal(lit) => {
            translate_literal(lit, &expr.annotation.ty, builder)
        }

        ast::Expression::BinOp(bin_op) => {
            translate_bin_op(bin_op, &expr.annotation.ty, builder)
        }

        ast::Expression::UnaryOp(unary_op) => {
            translate_unary_op(unary_op, &expr.annotation.ty, builder)
        }

        ast::Expression::Ident(ident) => {
            match expr.annotation.value_kind {
                None => {
                    panic!("ident must have a type")
                }
                Some(pas_ty::ValueKind::Temporary) => {
                    panic!("temporaries cannot be referenced by ident")
                }

                Some(pas_ty::ValueKind::Function) => {
                    let func_ref = GlobalRef::Function(ident.to_string());
                    Ref::Global(func_ref)
                }

                Some(pas_ty::ValueKind::Immutable) |
                Some(pas_ty::ValueKind::Mutable) => {
                    let local_ref = builder.find_local(&ident.name)
                        .map(|local| Value::Ref(local.local_ref()))
                        .unwrap_or_else(|| {
                            panic!("identifier not found in local scope @ {}: {}", expr.annotation.span, ident)
                        });

                    // we need to bind rc values to a local so we know when to
                    // release the ref, even if we're simply referencing a local by name
                    let temp_ty = builder.metadata.translate_type(&expr.annotation.ty).clone();
                    let temp_val = builder.local_new(temp_ty.clone(), None);

                    builder.append(Instruction::Move { out: temp_val.clone(), new_val: local_ref });
                    builder.retain(temp_val.clone(), &temp_ty);
                    temp_val
                }
            }
        }

        ast::Expression::Call(call) => {
            let return_ref = translate_call(call, builder)
                .expect("call used in expression must have a return type");
            return_ref
        }

        ast::Expression::ObjectCtor(ctor) => {
            let obj_ref = translate_object_ctor(ctor, builder);
            obj_ref
        }

        ast::Expression::IfCond(if_cond) => {
            let result_ref = translate_if_cond(if_cond, builder)
                .expect("conditional used in expression must have a type");
            result_ref
        }

        ast::Expression::Block(block) => {
            let out_ref = translate_block(block, builder)
                .expect("block used in expression must have a type");

            out_ref
        }
    };

    out_val
}

fn translate_if_cond(if_cond: &pas_ty::ast::IfCond, builder: &mut Builder) -> Option<Ref> {
    let (out_val, out_ty) = match &if_cond.annotation.ty {
        pas_ty::Type::Nothing => (None, Type::Nothing),
        out_ty => {
            let out_ty = builder.metadata.translate_type(out_ty);
            let out_val = builder.local_new(out_ty.clone(), None);
            (Some(out_val), out_ty)
        }
    };

    builder.begin_scope();

    let then_label = builder.alloc_label();
    let end_label = builder.alloc_label();
    let else_label = if_cond.else_branch.as_ref().map(|_| builder.alloc_label());

    let test_val = translate_expr(&if_cond.cond, builder);
    builder.append(Instruction::JumpIf { test: test_val.into(), dest: then_label });

    if let Some(else_label) = else_label {
        builder.append(Instruction::Jump { dest: else_label });
    }

    builder.append(Instruction::Label(then_label));
    let then_val = translate_expr(&if_cond.then_branch, builder);
    if let Some(out_val) = out_val.clone() {
        builder.append(Instruction::Move { out: out_val, new_val: then_val.into() });
    }
    builder.append(Instruction::Jump { dest: end_label });

    if let Some(else_branch) = &if_cond.else_branch {
        builder.append(Instruction::Label(else_label.unwrap()));

        let else_val = translate_expr(else_branch, builder);
        if let Some(out_val) = out_val.clone() {
            builder.append(Instruction::Move { out: out_val, new_val: else_val.into() });
        }
    }

    builder.append(Instruction::Label(end_label));

    if let Some(out_val) = &out_val {
        builder.retain(out_val.clone(), &out_ty);
    }

    builder.end_scope();

    out_val
}

pub fn translate_call(call: &pas_ty::ast::Call, builder: &mut Builder) -> Option<Ref> {
    let sig = match &call.target.annotation.ty {
        pas_ty::Type::Function(sig) => sig,
        _ => panic!("type of function target expr must be a function"),
    };

    let out_val = match &sig.return_ty {
        pas_ty::Type::Nothing => None,
        return_ty => {
            let out_ty = builder.metadata.translate_type(return_ty).clone();
            let out_val = builder.local_new(out_ty, None);
            Some(out_val)
        }
    };

    builder.begin_scope();

    let target_val = translate_expr(&call.target, builder);

    let mut arg_vals = Vec::new();
    for arg in &call.args {
        let arg_expr = translate_expr(arg, builder);
        arg_vals.push(Value::from(arg_expr));
    }

    builder.append(Instruction::Call {
        function: target_val.into(),
        args: arg_vals.clone(),
        out: out_val.clone(),
    });

    // no need to retain, the result of a function must be retained as part of its body

    builder.end_scope();

    out_val
}

fn translate_literal(lit: &ast::Literal, ty: &pas_ty::Type, builder: &mut Builder) -> Ref {
    let out_ty = builder.metadata.translate_type(ty);
    let out = builder.local_temp(out_ty);

    match lit {
        ast::Literal::Nil => {
            builder.mov(out.clone(), Value::LiteralNull);
        }

        ast::Literal::Boolean(b) => {
            builder.mov(out.clone(), Value::LiteralBool(*b));
        }

        ast::Literal::Integer(i) => {
            match ty {
                pas_ty::Type::Primitive(pas_ty::Primitive::Int32) => {
                    let val = i.as_i32()
                        .expect("Int32-typed constant must be within range of i32");
                    builder.mov(out.clone(), Value::LiteralI32(val));
                }
                _ => panic!("bad type for integer literal: {}", ty),
            }
        }

        ast::Literal::Real(r) => {
            match ty {
                pas_ty::Type::Primitive(pas_ty::Primitive::Real32) => {
                    let val = r.as_f32()
                        .expect("Real32-typed constant must be within range of f32");
                    builder.mov(out.clone(), Value::LiteralF32(val));
                }
                _ => panic!("bad type for real literal: {}", ty),
            }
        }

        ast::Literal::String(s) => {
            match ty {
                pas_ty::Type::Class(class) if class.ident.name == "String" => {
                    let lit_id = builder.metadata.find_or_insert_string(s);
                    let lit_ref = GlobalRef::StringLiteral(lit_id);

                    builder.mov(out.clone(), Value::Ref(Ref::Global(lit_ref)));
                }
                _ => panic!("bad type for string literal: {}", ty),
            }
        }
    }

    out
}

fn translate_bin_op(bin_op: &pas_ty::ast::BinOp, out_ty: &pas_ty::Type, builder: &mut Builder) -> Ref {
    let out_ty = builder.metadata.translate_type(out_ty);
    let out_val = builder.local_new(out_ty.clone(), None);

    builder.begin_scope();
    let lhs_val = translate_expr(&bin_op.lhs, builder);

    let op_instruction = match &bin_op.op {
        syn::Operator::Member => {
            // auto-deref for rc types
            let struct_ref = if bin_op.lhs.annotation.ty.is_rc() {
                lhs_val.deref()
            } else {
                lhs_val
            };

            let struct_name = bin_op.lhs.annotation.ty
                .full_name()
                .expect("member access must be of a named type");
            let member_name = bin_op.rhs.expr.as_ident().map(|i| i.to_string())
                .expect("rhs of member binop must be an ident");

            let (struct_id, struct_def) = builder.metadata.find_struct(&struct_name)
                .expect("referenced struct must exist");
            let field_id = struct_def.find_field(&member_name)
                .expect("referenced field must exist");

            Instruction::GetField { out: out_val.clone(), of: struct_ref, struct_id, field_id }
        }

        syn::Operator::NotEquals => {
            let eq_val = builder.local_temp(Type::Bool);
            let eq_instruction = Instruction::Eq {
                out: eq_val.clone(),
                a: lhs_val.into(),
                b: translate_expr(&bin_op.rhs, builder).into(),
            };
            builder.append(eq_instruction);
            Instruction::Not { out: out_val.clone(), a: Value::Ref(eq_val) }
        }

        syn::Operator::Equals => Instruction::Eq {
            out: out_val.clone(),
            a: lhs_val.into(),
            b: translate_expr(&bin_op.rhs, builder).into(),
        },

        syn::Operator::Plus => Instruction::Add {
            out: out_val.clone(),
            a: lhs_val.into(),
            b: translate_expr(&bin_op.rhs, builder).into(),
        },

        syn::Operator::Minus => Instruction::Sub {
            out: out_val.clone(),
            a: lhs_val.into(),
            b: translate_expr(&bin_op.rhs, builder).into(),
        },

        syn::Operator::And => Instruction::And {
            out: out_val.clone(),
            a: lhs_val.into(),
            b: translate_expr(&bin_op.rhs, builder).into(),
        },

        syn::Operator::Or => Instruction::Or {
            out: out_val.clone(),
            a: lhs_val.into(),
            b: translate_expr(&bin_op.rhs, builder).into(),
        },

        _ => unimplemented!("IR for op {}", bin_op.op),
    };

    builder.append(op_instruction);
    builder.retain(out_val.clone(), &out_ty);

    builder.end_scope();

    out_val
}

fn translate_unary_op(
    unary_op: &pas_ty::ast::UnaryOp,
    out_ty: &pas_ty::Type,
    builder: &mut Builder)
    -> Ref
{
    let operand_ref = translate_expr(&unary_op.operand, builder);

    match unary_op.op {
        syn::Operator::AddressOf => {
            let out_ty = builder.metadata.translate_type(out_ty);
            let out_val = builder.local_new(out_ty.clone(), None);

            builder.append(Instruction::AddrOf { out: out_val.clone(), a: operand_ref });
            builder.retain(out_val.clone(), &out_ty);

            out_val
        }

        syn::Operator::Deref => {
            operand_ref.deref()
        }

        op => unimplemented!("unary operator {}", op)
    }
}

fn translate_object_ctor(ctor: &pas_ty::ast::ObjectCtor, builder: &mut Builder) -> Ref {
    let (struct_id, struct_def) = builder.metadata.find_struct(&ctor.ident.to_string())
        .map(|(id, def)| (id, def.clone()))
        .unwrap_or_else(|| panic!("struct {} referenced in object ctor must exist", ctor.ident));

    let struct_ty = builder.metadata.translate_type(&ctor.annotation.ty);

    let (out_val, struct_ref) = match struct_ty {
        Type::Rc(struct_ty) => {
            let struct_id = match struct_ty.as_ref() {
                Type::Struct(id) => *id,
                _ => panic!("bad object ctor: class type {:?} doesn't have struct layout", struct_ty),
            };

            let class_ty = (*struct_ty).clone().ptr();
            let out_ptr = builder.local_new(class_ty, None);

            // allocate class struct at out pointer
            builder.append(Instruction::RcNew { out: out_ptr.clone(), struct_id });

            (out_ptr.clone(), out_ptr.deref())
        }

        Type::Struct(_) => {
            let out_val = builder.local_new(struct_ty, None);
            (out_val.clone(), out_val)
        }

        ty => panic!("bad object ctor: type {:?} is not a record or class", ty),
    };

    builder.begin_scope();

    // todo: lookup members by id, don't assume they're in order
    for member in &ctor.args.members {
        let member_val = translate_expr(&member.value, builder);
        let field_id = struct_def.find_field(&member.ident.to_string())
            .unwrap_or_else(|| panic!("field {} referenced in object ctor must exist", member.ident));

        let member_ty = builder.metadata.translate_type(&member.value.annotation.ty);
        builder.retain(member_val.clone(), &member_ty);

        builder.append(Instruction::SetField {
            of: struct_ref.clone(),
            new_val: Value::Ref(member_val),
            struct_id,
            field_id,
        });
    }

    builder.end_scope();
    out_val
}

pub fn translate_block(block: &pas_ty::ast::Block, builder: &mut Builder) -> Option<Ref> {
    let (out_val, out_ty) = match &block.output {
        Some(out_expr) => {
            let out_ty = builder.metadata.translate_type(&out_expr.annotation.ty);
            let out_val = builder.local_new(out_ty.clone(), None);

            (Some(out_val), out_ty)
        }
        None => (None, Type::Nothing)
    };

    builder.begin_scope();

    for stmt in &block.statements {
        translate_stmt(stmt, builder);
    }

    if let Some(out) = &block.output {
        let result_val = translate_expr(out, builder);
        builder.mov(out_val.clone().unwrap(), result_val.clone());
        builder.retain(result_val, &out_ty);
    }

    builder.end_scope();

    out_val
}