use crate::builder::Builder;
use crate::expr;
use crate::Instruction;
use crate::Ref;
use crate::Type;
use crate::Value;
use crate::metadata::VirtualTypeID;
use crate::syn;
use crate::typ;
use syn::Operator;

pub fn translate_bin_op(
    bin_op: &typ::ast::BinOp,
    out_ty: &typ::Type,
    builder: &mut Builder,
) -> Ref {
    if bin_op.lhs.annotation().is_namespace() {
        // there's nothing to actually translate on the lhs, it's just for name resolution
        return expr::translate_expr(&bin_op.rhs, builder);
    }

    let out_ty = builder.translate_type(out_ty);

    // the functions to translate IR and member operators return pointers to the value
    let (out_val, out_is_ptr) = match bin_op.op {
        Operator::Period | Operator::Index => {
            let out_val = builder.local_new(out_ty.clone().ptr(), None);
            (out_val, true)
        },

        _ => {
            let out_val = builder.local_new(out_ty.clone(), None);
            (out_val, false)
        },
    };

    builder.begin_scope();
    let lhs_val = expr::translate_expr(&bin_op.lhs, builder);

    match &bin_op.op {
        syn::Operator::Period => {
            // auto-deref for rc types
            let of_ty = builder.translate_type(&bin_op.lhs.annotation().ty());

            let struct_id = match &of_ty {
                Type::Struct(id) => *id,
                Type::RcPointer(VirtualTypeID::Class(id)) => *id,
                other => panic!(
                    "lhs ty of member binop must be a struct or class, was: {}",
                    other
                ),
            };

            let struct_def = builder
                .get_struct(struct_id)
                .expect("referenced struct must exist");

            let member_name = bin_op
                .rhs
                .as_ident()
                .map(syn::Ident::to_string)
                .expect("rhs of member binop must be an ident");

            let field = struct_def
                .find_field(&member_name)
                .expect("referenced field must exist");

            builder.append(Instruction::Field {
                out: out_val.clone(),
                a: lhs_val,
                of_ty,
                field,
            });
        },

        syn::Operator::Index => {
            let index_val = expr::expr_to_val(&bin_op.rhs, builder);
            let element_val = expr::translate_indexer(
                &out_ty,
                lhs_val,
                index_val,
                &bin_op.lhs.annotation().ty(),
                builder,
            );

            builder.mov(out_val.clone(), element_val);
        },

        syn::Operator::NotEquals => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.append(Instruction::Eq {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
            builder.append(Instruction::Not {
                out: out_val.clone(),
                a: Value::Ref(out_val.clone()),
            });
        },

        syn::Operator::Equals => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.append(Instruction::Eq {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Add => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.append(Instruction::Add {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Mul => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.append(Instruction::Mul {
                out: out_val.clone(),
                a: lhs_val.into(),
                b,
            });
        }

        syn::Operator::Mod => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.append(Instruction::Mod {
                out: out_val.clone(),
                a: lhs_val.into(),
                b,
            });
        }

        // both types of division translate to the same instructions, it just may imply some
        // conversions for the values which should already be done by this point
        syn::Operator::FDiv | syn::Operator::IDiv => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.append(Instruction::Div {
                out: out_val.clone(),
                a: lhs_val.into(),
                b,
            });
        }

        syn::Operator::Gt => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.append(Instruction::Gt {
                out: out_val.clone(),
                a: lhs_val.into(),
                b,
            });
        },

        syn::Operator::Gte => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);

            let gt = builder.local_temp(Type::Bool);
            builder.append(Instruction::Gt {
                out: gt.clone(),
                a: lhs_val.clone().into(),
                b: b.clone(),
            });

            let eq = builder.local_temp(Type::Bool);
            builder.append(Instruction::Eq {
                out: eq.clone(),
                a: lhs_val.clone().into(),
                b,
            });

            builder.append(Instruction::Or {
                out: out_val.clone(),
                a: gt.into(),
                b: eq.into(),
            });
        },

        syn::Operator::Lt => {
            let b = expr::translate_expr(&bin_op.rhs, builder);

            let gt = builder.local_temp(Type::Bool);
            builder.append(Instruction::Gt {
                out: gt.clone(),
                a: lhs_val.clone().into(),
                b: b.clone().into(),
            });

            let eq = builder.local_temp(Type::Bool);
            builder.append(Instruction::Eq {
                out: eq.clone(),
                a: lhs_val.clone().into(),
                b: b.clone().into(),
            });

            let gte = builder.local_temp(Type::Bool);
            builder.append(Instruction::Or {
                out: gte.clone(),
                a: gt.into(),
                b: eq.into(),
            });

            builder.append(Instruction::Not {
                out: out_val.clone(),
                a: gte.into(),
            });
        },

        syn::Operator::Lte => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);

            let gt = builder.local_temp(Type::Bool);
            builder.append(Instruction::Gt {
                out: gt.clone(),
                a: lhs_val.clone().into(),
                b: b.clone().into(),
            });

            builder.append(Instruction::Not {
                out: out_val.clone(),
                a: gt.into(),
            });
        },

        syn::Operator::Sub => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.append(Instruction::Sub {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Shl => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.append(Instruction::Shl {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Shr => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.append(Instruction::Shr {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::And => {
            let short_circuit = builder.alloc_label();
            let exit = builder.alloc_label();

            // if lhs is false, short circuit
            let is_short_circuit = builder.not_to_val(lhs_val.clone());
            builder.jmp_if(short_circuit, is_short_circuit);

            // eval both sides
            let rhs = expr::expr_to_val(&bin_op.rhs, builder);
            builder.and(out_val.clone(), lhs_val, rhs);
            builder.jmp(exit);
            
            // short circuit: out := false
            builder.label(short_circuit);
            builder.mov(out_val.clone(), false);
            
            builder.label(exit);
        },

        syn::Operator::Or => {
            let short_circuit = builder.alloc_label();
            let exit = builder.alloc_label();
            
            // if lhs is true, short circuit
            builder.jmp_if(short_circuit, lhs_val.clone());

            // eval both sides
            let rhs = expr::expr_to_val(&bin_op.rhs, builder);
            builder.or(out_val.clone(), lhs_val, rhs);
            builder.jmp(exit);

            // short circuit: out := true
            builder.label(short_circuit);
            builder.mov(out_val.clone(), true);
            
            builder.label(exit);
        },

        syn::Operator::BitAnd => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.append(Instruction::BitAnd {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::BitOr => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.append(Instruction::BitOr {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Caret => {
            let b = expr::expr_to_val(&bin_op.rhs, builder);
            builder.append(Instruction::BitXor {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        _ => unimplemented!("IR for op {}", bin_op.op),
    };

    if !out_is_ptr {
        builder.retain(out_val.clone(), &out_ty);
    }

    builder.end_scope();

    if out_is_ptr {
        out_val.to_deref()
    } else {
        out_val
    }
}

pub fn translate_unary_op(
    unary_op: &typ::ast::UnaryOp,
    out_ty: &typ::Type,
    builder: &mut Builder,
) -> Ref {
    let operand_ref = expr::translate_expr(&unary_op.operand, builder);

    match unary_op.op {
        syn::Operator::AddressOf => {
            let out_ty = builder.translate_type(out_ty);
            let out_val = builder.local_new(out_ty.clone(), None);

            builder.append(Instruction::AddrOf {
                out: out_val.clone(),
                a: operand_ref,
            });
            builder.retain(out_val.clone(), &out_ty);

            out_val
        },

        syn::Operator::Caret => operand_ref.to_deref(),

        syn::Operator::Sub => {
            let out_ty = builder.translate_type(out_ty);
            let out_val = builder.local_new(out_ty.clone(), None);

            let op_ty = unary_op.annotation.ty();

            let zero_val = match op_ty.as_ref() {
                typ::Type::Primitive(typ::Primitive::Int8) => Value::LiteralI8(0),
                typ::Type::Primitive(typ::Primitive::UInt8) => Value::LiteralU8(0),
                typ::Type::Primitive(typ::Primitive::Int16) => Value::LiteralI16(0),
                typ::Type::Primitive(typ::Primitive::UInt16) => Value::LiteralI16(0),
                typ::Type::Primitive(typ::Primitive::Int32) => Value::LiteralI32(0),
                typ::Type::Primitive(typ::Primitive::UInt32) => Value::LiteralU32(0),
                typ::Type::Primitive(typ::Primitive::Int64) => Value::LiteralI64(0),
                typ::Type::Primitive(typ::Primitive::UInt64) => Value::LiteralU64(0),
                typ::Type::Primitive(typ::Primitive::NativeInt) => Value::LiteralISize(0),
                typ::Type::Primitive(typ::Primitive::NativeUInt) => Value::LiteralUSize(0),
                typ::Type::Primitive(typ::Primitive::Real32) => Value::LiteralF32(0.0),
                _ => unimplemented!("unary negation of {}", op_ty),
            };

            builder.append(Instruction::Sub {
                a: zero_val,
                b: Value::Ref(operand_ref),
                out: out_val.clone(),
            });

            out_val
        },

        syn::Operator::Add => {
            // just turns its operand into a temporary value
            let out_ty = builder.translate_type(out_ty);
            let out_val = builder.local_new(out_ty.clone(), None);
            builder.mov(out_val.clone(), operand_ref);

            out_val
        },

        syn::Operator::Not => {
            let out_val = builder.local_new(Type::Bool, None);

            builder.not(out_val.clone(), operand_ref);

            out_val
        },

        op => unimplemented!("IR translation of unary operator {}", op),
    }
}
