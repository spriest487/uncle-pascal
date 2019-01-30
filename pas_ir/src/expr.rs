use {
    crate::{
        prelude::*,
        translate_stmt,
    },
    pas_typecheck as pas_ty,
    pas_syn::{
        self as syn,
        ast as ast
    },
};

pub fn translate_expr(
    expr: &pas_ty::ast::ExpressionNode,
    builder: &mut Builder,
) -> Value {
    match expr.expr.as_ref() {
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
                    Value::Ref(Ref::Global(func_ref))
                }

                Some(pas_ty::ValueKind::Immutable) |
                Some(pas_ty::ValueKind::Mutable) => {
                    builder.find_local(&ident.name)
                        .map(|local| Value::Ref(Ref::Local(local.id())))
                        .unwrap_or_else(|| {
                            panic!("identifier not found in local scope @ {}: {}", expr.annotation.span, ident)
                        })
                }
            }
        }

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

fn translate_if_cond(if_cond: &pas_ty::ast::IfCond, builder: &mut Builder) -> Option<Value> {
    let out_val = match &if_cond.annotation.ty {
        pas_ty::Type::Nothing => None,
        out_ty => {
            let rc = out_ty.is_rc();
            let out_ty = builder.metadata.translate_type(out_ty);
            Some(builder.new_local(out_ty, rc, None))
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

    out_val.map(Value::Ref)
}

pub fn translate_call(call: &pas_ty::ast::Call, builder: &mut Builder) -> Option<Value> {
    let sig = match &call.target.annotation.ty {
        pas_ty::Type::Function(sig) => sig,
        _ => panic!("type of function target expr must be a function"),
    };

    let out_val = match &sig.return_ty {
        pas_ty::Type::Nothing => None,
        return_ty => {
            let out_ty = builder.metadata.translate_type(return_ty).clone();
            let out_val = builder.new_local(out_ty, return_ty.is_rc(), None);
            Some(out_val)
        },
    };

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

    out_val.map(Value::Ref)
}

fn translate_literal(lit: &ast::Literal, ty: &pas_ty::Type, builder: &mut Builder) -> Value {
    match lit {
        ast::Literal::Nil => {
            Value::LiteralNull
        }

        ast::Literal::Boolean(b) => {
            assert_eq!(pas_ty::Type::Primitive(pas_ty::Primitive::Boolean), *ty);
            Value::LiteralBool(*b)
        }

        ast::Literal::Integer(i) => {
            match ty {
                pas_ty::Type::Primitive(pas_ty::Primitive::Int32) => {
                    Value::LiteralI32(i.as_i32()
                        .expect("Int32-typed constant must be within range of i32"))
                }
                _ => panic!("bad type for integer literal: {}", ty),
            }
        }

        ast::Literal::Real(r) => {
            match ty {
                pas_ty::Type::Primitive(pas_ty::Primitive::Real32) => {
                    Value::LiteralF32(r.as_f32()
                        .expect("Real32-typed constant must be within range of f32"))
                }
                _ => panic!("bad type for real literal: {}", ty),
            }
        }

        ast::Literal::String(s) => {
            match ty {
                pas_ty::Type::Class(class) if class.ident.name == "String" => {
                    let lit_id = builder.metadata.find_or_insert_string(s);
                    let lit_ref = GlobalRef::StringLiteral(lit_id);
                    Value::Ref(Ref::Global(lit_ref))
                }
                _ => panic!("bad type for string literal: {}", ty),
            }
        }
    }
}

fn translate_bin_op(bin_op: &pas_ty::ast::BinOp, out_ty: &pas_ty::Type, builder: &mut Builder) -> Value {
    let out_rc = out_ty.is_rc();
    let out_ty = builder.metadata.translate_type(out_ty);
    let out_val = builder.new_local(out_ty, out_rc, None);

    builder.begin_scope();
    let lhs_val = translate_expr(&bin_op.lhs, builder);

    let op_instruction = match &bin_op.op {
        syn::Operator::Member => {
            // auto-deref for rc types
            let struct_ref = match lhs_val {
                Value::Ref(lhs_ref) => if bin_op.lhs.annotation.ty.is_rc() {
                    lhs_ref.deref()
                } else {
                    lhs_ref
                },

                x => panic!("member expression {} references non-ref value {:?}", bin_op, x),
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
            let eq_val = builder.new_local(Type::Bool, false, None);
            let eq_instruction = Instruction::Eq {
                out: eq_val.clone(),
                a: lhs_val,
                b: translate_expr(&bin_op.rhs, builder)
            };
            builder.append(eq_instruction);
            Instruction::Not { out: out_val.clone(), a: Value::Ref(eq_val) }
        },

        syn::Operator::Equals => Instruction::Eq {
            out: out_val.clone(),
            a: lhs_val,
            b: translate_expr(&bin_op.rhs, builder)
        },

        syn::Operator::Plus => Instruction::Add {
            out: out_val.clone(),
            a: lhs_val,
            b: translate_expr(&bin_op.rhs, builder),
        },

        syn::Operator::And => Instruction::And {
            out: out_val.clone(),
            a: lhs_val,
            b: translate_expr(&bin_op.rhs, builder),
        },

        syn::Operator::Or => Instruction::Or {
            out: out_val.clone(),
            a: lhs_val,
            b: translate_expr(&bin_op.rhs, builder),
        },

        _ => unimplemented!("IR for op {}", bin_op.op),
    };

    builder.append(op_instruction);
    builder.end_scope();

    Value::Ref(out_val)
}

fn translate_unary_op(
    unary_op: &pas_ty::ast::UnaryOp,
    out_ty: &pas_ty::Type,
    builder: &mut Builder)
    -> Value
{
    let operand_val = translate_expr(&unary_op.operand, builder);

    match unary_op.op {
        syn::Operator::AddressOf => {
            let out_rc = out_ty.is_rc();
            let out_ty = builder.metadata.translate_type(out_ty);
            let out_val = builder.new_local(out_ty, out_rc, None);

            let operand_ref = operand_val.into_ref_val()
                .expect("operand of address-of operator must be a referencable value");
            builder.append(Instruction::AddrOf { out: out_val.clone(), a: operand_ref });

            Value::Ref(out_val)
        }

        syn::Operator::Deref => {
            operand_val.deref()
        }

        op => unimplemented!("unary operator {}", op)
    }
}

fn translate_object_ctor(ctor: &pas_ty::ast::ObjectCtor, builder: &mut Builder) -> Value {
    let (struct_id, struct_def) = builder.metadata.find_struct(&ctor.ident.to_string())
        .map(|(id, def)| (id, def.clone()))
        .unwrap_or_else(|| panic!("struct {} referenced in object ctor must exist", ctor.ident));

    let struct_ty = builder.metadata.translate_type(&ctor.annotation.ty);

    let (out_val, struct_ref) = match struct_ty {
        Type::Pointer(struct_ty) => {
            if !struct_ty.is_struct() {
                panic!("bad object ctor: class type {:?} doesn't have struct layout", struct_ty);
            }

            let class_ty = (*struct_ty).clone().ptr();
            let out_ptr = builder.new_local(class_ty, true, None);

            // allocate class struct at out pointer
            builder.append(Instruction::MemAlloc { out: out_ptr.clone(), ty: *struct_ty, count: 1 });

            (out_ptr.clone(), out_ptr.deref())
        },

        Type::Struct(_) => {
            // allocate locally
            let out_val = builder.new_local(struct_ty, false, None);
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

        if member.value.annotation.ty.is_rc() {
            let member_val_ref = member_val.as_ref_val().unwrap();
            builder.append(Instruction::Retain(member_val_ref.clone()));
        }

        builder.append(Instruction::SetField {
            of: struct_ref.clone(),
            new_val: member_val,
            struct_id,
            field_id,
        });
    }

    builder.end_scope();
    Value::Ref(out_val)
}

pub fn translate_block(block: &pas_ty::ast::Block, builder: &mut Builder) -> Option<Value> {
    let out_val = block.output.as_ref().map(|out_expr| {
        let rc = out_expr.annotation.ty.is_rc();
        let out_ty = builder.metadata.translate_type(&out_expr.annotation.ty);
        builder.new_local(out_ty, rc, None)
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

    out_val.map(Value::Ref)
}