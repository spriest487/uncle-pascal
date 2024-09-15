mod call;
mod op;
mod ctor;
mod cond;

use crate::translate_stmt;
use crate::ty::VirtualTypeID;
use crate::Builder;
use crate::GlobalRef;
use crate::Instruction;
use crate::Ref;
use crate::Type;
use crate::Value;
use crate::DYNARRAY_LEN_FIELD;
use crate::DYNARRAY_PTR_FIELD;
use crate::RETURN_REF;
use pas_common::span::*;
use pas_syn as syn;
use pas_syn::Ident;
use pas_ty::Typed;
use pas_ty::ValueKind;
use pas_typecheck as pas_ty;
use syn::ast;
pub use call::*;

pub fn expr_to_val(expr: &pas_ty::ast::Expr, builder: &mut Builder) -> Value {
    match expr {
        ast::Expr::Literal(lit, typed) => literal_to_val(lit, typed.ty().as_ref(), builder),
        _ => Value::Ref(translate_expr(expr, builder)),
    }
}

pub fn translate_expr(expr: &pas_ty::ast::Expr, builder: &mut Builder) -> Ref {
    builder.comment(&expr);
    builder.push_debug_context(expr.annotation().span().clone());

    let result_ref = match expr {
        ast::Expr::Literal(lit, annotation) => {
            translate_literal(lit, &annotation.ty(), builder)
        },

        ast::Expr::BinOp(bin_op) => {
            op::translate_bin_op(bin_op, &bin_op.annotation.ty(), builder)
        },

        ast::Expr::UnaryOp(unary_op) => {
            op::translate_unary_op(unary_op, &unary_op.annotation.ty(), builder)
        },

        ast::Expr::Ident(ident, annotation) => translate_ident(ident, annotation, builder),

        ast::Expr::Call(call) => {
            call::build_call(call, builder).expect("call used in expr must have a return value")
        },

        ast::Expr::ObjectCtor(ctor) => ctor::translate_object_ctor(ctor, builder),

        ast::Expr::CollectionCtor(ctor) => ctor::translate_collection_ctor(ctor, builder),

        ast::Expr::IfCond(if_cond) => translate_if_cond_expr(if_cond, builder)
            .expect("conditional used in expr must have a type"),

        ast::Expr::Block(block) => {
            let out_ty = match &block.output {
                Some(output_expr) => builder.translate_type(&output_expr.annotation().ty()),
                None => panic!("block used in expr must have a type"),
            };
            let out_ref = builder.local_new(out_ty, None);
            translate_block(block, out_ref.clone(), builder);

            out_ref
        },

        ast::Expr::Raise(raise) => translate_raise(raise, builder),
        ast::Expr::Exit(exit) => {
            translate_exit(exit, builder);
            Ref::Discard
        },

        ast::Expr::Case(case) => cond::translate_case_expr(case, builder),
        ast::Expr::Match(match_expr) => cond::translate_match_expr(match_expr, builder),

        ast::Expr::Cast(cast) => translate_cast_expr(cast, builder),

        ast::Expr::AnonymousFunction(def) => builder.build_closure_expr(def),
    };

    builder.pop_debug_context();

    result_ref
}

fn translate_indexer(
    val_ty: &Type,
    base_ref: Ref,
    index_val: Value,
    base_ty: &pas_ty::Type,
    builder: &mut Builder,
) -> Ref {
    match base_ty {
        pas_ty::Type::Array(array_ty) => {
            let element_ptr = builder.local_temp(val_ty.clone().ptr());

            builder.begin_scope();

            let element_ty = builder.translate_type(&array_ty.element_ty);
            let len = cast::i32(array_ty.dim).expect("array dim must be within range of i32");

            gen_bounds_check(index_val.clone(), Value::LiteralI32(len), builder);

            builder.append(Instruction::Element {
                out: element_ptr.clone(),
                a: base_ref,
                index: index_val,
                element: element_ty,
            });

            builder.end_scope();

            element_ptr
        },

        pas_ty::Type::DynArray { element } => {
            let element_ptr = builder.local_temp(val_ty.clone().ptr());

            builder.begin_scope();

            let arr_field_ptr = builder.local_temp(val_ty.clone().ptr().ptr());
            let len_field_ptr = builder.local_temp(Type::I32.ptr());

            let array_struct = builder.translate_dyn_array_struct(&element);
            let array_class = VirtualTypeID::Class(array_struct);
            let array_class_ty = Type::RcPointer(array_class);

            builder.field(
                len_field_ptr.clone(),
                base_ref.clone(),
                array_class_ty.clone(),
                DYNARRAY_LEN_FIELD,
            );
            builder.field(
                arr_field_ptr.clone(),
                base_ref.clone(),
                array_class_ty,
                DYNARRAY_PTR_FIELD,
            );

            gen_bounds_check(
                index_val.clone(),
                Value::Ref(len_field_ptr.to_deref()),
                builder,
            );

            // array_ptr := (array_field_ptr)^
            // element_ptr := array_ptr + index
            let array_ptr = arr_field_ptr.to_deref();
            builder.add(element_ptr.clone(), array_ptr, index_val);

            builder.end_scope();

            element_ptr
        },

        pas_ty::Type::Pointer(_) => {
            let result_ptr = builder.local_temp(val_ty.clone().ptr());

            builder.add(result_ptr.clone(), base_ref, index_val);

            result_ptr
        },

        unimpl => unimplemented!("IR for indexing into {}", unimpl),
    }
}

fn gen_bounds_check(index_val: impl Into<Value>, len_val: impl Into<Value>, builder: &mut Builder) {
    let index_val = index_val.into();
    let len_val = len_val.into();

    builder.comment(&format!("bounds check for index={}, len={}", index_val, len_val));

    let bounds_ok_label = builder.alloc_label();

    // if index >= 0 and index < arr.len then goto "bounds_ok"
    let gte_zero = builder.gte_to_val(index_val.clone(), Value::LiteralI32(0));
    let lt_len = builder.lt_to_val(index_val, len_val);
    let bounds_check_ok = builder.and_to_val(gte_zero, lt_len);
    builder.append(Instruction::JumpIf {
        dest: bounds_ok_label,
        test: bounds_check_ok,
    });

    // otherwise: raise
    let err_str = builder.find_or_insert_string("array index out of bounds");
    builder.append(Instruction::Raise {
        val: Ref::Global(GlobalRef::StringLiteral(err_str)),
    });

    builder.append(Instruction::Label(bounds_ok_label));
}

pub fn translate_if_cond_expr(
    if_cond: &pas_ty::ast::IfCondExpression,
    builder: &mut Builder,
) -> Option<Ref> {
    cond::translate_if_cond(if_cond, builder, |branch, out_ref, out_ty, builder| {
        let val = expr_to_val(branch, builder);

        if let Some(out_ref) = out_ref.cloned() {
            builder.append(Instruction::Move {
                out: out_ref.clone(),
                new_val: val.into(),
            });
            builder.retain(out_ref, &out_ty);
        }
    })
}

pub fn translate_if_cond_stmt(
    if_cond: &pas_ty::ast::IfCondStatement,
    builder: &mut Builder,
) -> Option<Ref> {
    cond::translate_if_cond(if_cond, builder, |branch, out_ref, _out_ty, builder| {
        assert!(
            out_ref.is_none(),
            "branch translated as stmt should not have an out location"
        );

        translate_stmt(&branch, builder);
    })
}

fn is_string_class(class: &pas_ty::Symbol) -> bool {
    class.qualified.first().name.as_str() == "System"
        && class.qualified.last().name.as_str() == "String"
}

pub fn literal_to_val(
    lit: &ast::Literal<pas_ty::Type>,
    ty: &pas_ty::Type,
    builder: &mut Builder,
) -> Value {
    match lit {
        ast::Literal::Nil => Value::LiteralNull,

        ast::Literal::Boolean(b) => Value::LiteralBool(*b),

        ast::Literal::Integer(i) => match ty {
            pas_ty::Type::Primitive(pas_ty::Primitive::Int8) => {
                let val = i
                    .as_i8()
                    .expect("Int8-typed constant must be within range of i8");
                Value::LiteralI8(val)
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::UInt8) => {
                let val = i
                    .as_u8()
                    .expect("UInt8-typed constant must be within range of u8");
                Value::LiteralU8(val)
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::Int16) => {
                let val = i
                    .as_i16()
                    .expect("Int16-typed constant must be within range of i16");
                Value::LiteralI16(val)
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::UInt16) => {
                let val = i
                    .as_u16()
                    .expect("Int16-typed constant must be within range of i16");
                Value::LiteralU16(val)
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::Int32) => {
                let val = i
                    .as_i32()
                    .expect("Int32-typed constant must be within range of i32");
                Value::LiteralI32(val)
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::UInt32) => {
                let val = i
                    .as_u32()
                    .expect("Int32-typed constant must be within range of u32");
                Value::LiteralU32(val)
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::Int64) => {
                let val = i
                    .as_i64()
                    .expect("Int64-typed constant must be within range of i64");
                Value::LiteralI64(val)
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::UInt64) => {
                let val = i
                    .as_u64()
                    .expect("Int64-typed constant must be within range of u64");
                Value::LiteralU64(val)
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::NativeInt) => {
                let val = i
                    .as_isize()
                    .expect("Int64-typed constant must be within range of isize");
                Value::LiteralISize(val)
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::NativeUInt) => {
                let val = i
                    .as_usize()
                    .expect("Int64-typed constant must be within range of usize");
                Value::LiteralUSize(val)
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::Real32) => {
                let val = i
                    .as_f32()
                    .expect("Real-typed constant must be within range of f32");
                Value::LiteralF32(val)
            },
            pas_ty::Type::Enum(..) => {
                let val = i
                    .as_isize()
                    .expect("Enum-typed constant must be within range of isize");
                Value::LiteralISize(val)
            }

            _ => panic!("bad type for integer literal: {}", ty),
        },

        ast::Literal::Real(r) => match ty {
            pas_ty::Type::Primitive(pas_ty::Primitive::Real32) => {
                let val = r
                    .as_f32()
                    .expect("Real32-typed constant must be within range of f32");
                Value::LiteralF32(val)
            },
            _ => panic!("bad type for real literal: {}", ty),
        },

        ast::Literal::String(s) => match ty {
            pas_ty::Type::Class(class) if is_string_class(class) => {
                let lit_id = builder.find_or_insert_string(s);
                let lit_ref = GlobalRef::StringLiteral(lit_id);

                Value::Ref(Ref::Global(lit_ref))
            },
            _ => panic!("bad type for string literal: {}", ty),
        },

        ast::Literal::SizeOf(ty) => {
            let ty = builder.translate_type(ty);
            Value::SizeOf(ty)
        },
    }
}

pub fn translate_literal(
    lit: &ast::Literal<pas_ty::Type>,
    ty: &pas_ty::Type,
    builder: &mut Builder,
) -> Ref {
    let out_ty = builder.translate_type(ty);
    let out = builder.local_temp(out_ty);

    let val = literal_to_val(lit, ty, builder);
    builder.mov(out.clone(), val);

    out
}

fn translate_ident(ident: &Ident, annotation: &Typed, builder: &mut Builder) -> Ref {
    match annotation {
        Typed::Function(func) => {
            let func = builder.translate_func(func.ident.clone(), func.type_args.clone());
            
            // references to functions by value are turned into references to the static
            // closure for that function
            builder.build_function_closure(&func)
        },

        Typed::TypedValue(val) => {
            let local_ref = builder
                .find_local(&ident.to_string())
                .map(|local| {
                    let value_ref = Ref::Local(local.id());
                    if local.by_ref() {
                        value_ref.to_deref()
                    } else {
                        value_ref
                    }
                })
                .unwrap_or_else(|| {
                    panic!(
                        "identifier not found in local scope @ {}: {}",
                        annotation.span(),
                        ident
                    )
                });

            match val.value_kind {
                // ident lvalues are evaluated as pointers to the original values. they don't need
                // to be refcounted separately, because if they have a name, they must exist
                // in a scope at least as wide as the current one
                ValueKind::Immutable | ValueKind::Mutable | ValueKind::Uninitialized => {
                    let ref_ty = builder.translate_type(&annotation.ty());
                    let ref_temp = builder.local_temp(ref_ty.ptr());

                    builder.append(Instruction::AddrOf {
                        out: ref_temp.clone(),
                        a: local_ref,
                    });
                    ref_temp.to_deref()
                },

                // ident rvalue - just evaluate it
                ValueKind::Temporary => local_ref,
            }
        },

        _ => panic!("wrong kind of node annotation for ident: {:?}", ident),
    }
}

pub fn translate_block(block: &pas_ty::ast::Block, out_ref: Ref, builder: &mut Builder) {
    let out_ty = match &block.output {
        Some(out_expr) => builder.translate_type(&out_expr.annotation().ty()),
        None => Type::Nothing,
    };

    builder.begin_scope();

    for stmt in &block.stmts {
        translate_stmt(stmt, builder);
    }

    if let Some(out) = &block.output {
        let result_val = translate_expr(out, builder);
        builder.mov(out_ref, result_val.clone());
        builder.retain(result_val, &out_ty);
    }

    builder.end_scope();
}

pub fn translate_exit(exit: &pas_ty::ast::Exit, builder: &mut Builder) {
    if let ast::Exit::WithValue(val, _) = exit {
        let value_ty = builder.translate_type(&val.annotation().ty());
        let value_val = translate_expr(val, builder);

        // we can assume this function has a return register, otherwise an exit stmt
        // wouldn't pass typechecking
        builder.mov(RETURN_REF, value_val);

        // we are effectively reassigning the return ref, so like a normal assignment, we need
        // retain the new value to make it outlive the scope the exit expr appears in
        builder.retain(RETURN_REF.clone(), &value_ty);
    }

    builder.exit_function();
}

pub fn translate_raise(raise: &pas_ty::ast::Raise, builder: &mut Builder) -> Ref {
    let val = translate_expr(&raise.value, builder);

    builder.append(Instruction::Raise { val: val.clone() });

    Ref::Discard
}

fn translate_cast_expr(cast: &pas_ty::ast::Cast, builder: &mut Builder) -> Ref {
    let val = translate_expr(&cast.expr, builder);
    let ty = builder.translate_type(&cast.ty);
    let out_ref = builder.local_temp(ty.clone());

    builder.cast(out_ref.clone(), val, ty);

    out_ref
}
