use crate::pattern::{translate_pattern_match, PatternMatchOutput};
use crate::{
    build_case_block, translate_stmt, Builder, GlobalRef, Instruction, InterfaceID, MethodID, Ref,
    Type, Value, CLOSURE_PTR_FIELD, DYNARRAY_LEN_FIELD, DYNARRAY_PTR_FIELD, RETURN_REF,
};
use pas_common::span::*;
use pas_syn as syn;
use pas_syn::Ident;
use pas_ty::{TypeAnnotation, ValueKind};
use pas_typecheck as pas_ty;
use std::convert::TryFrom;
use syn::ast;

use crate::ty::ClassID;

pub fn translate_expr(expr: &pas_ty::ast::Expression, builder: &mut Builder) -> Ref {
    builder.push_debug_context(expr.annotation().span().clone());

    let result_ref = match expr {
        ast::Expression::Literal(lit, annotation) => {
            translate_literal(lit, &annotation.ty(), builder)
        },

        ast::Expression::BinOp(bin_op) => {
            translate_bin_op(bin_op, &bin_op.annotation.ty(), builder)
        },

        ast::Expression::UnaryOp(unary_op) => {
            translate_unary_op(unary_op, &unary_op.annotation.ty(), builder)
        },

        ast::Expression::Ident(ident, annotation) => translate_ident(ident, annotation, builder),

        ast::Expression::Call(call) => {
            build_call(call, builder).expect("call used in expression must have a return value")
        },

        ast::Expression::ObjectCtor(ctor) => translate_object_ctor(ctor, builder),

        ast::Expression::CollectionCtor(ctor) => translate_collection_ctor(ctor, builder),

        ast::Expression::IfCond(if_cond) => translate_if_cond_expr(if_cond, builder)
            .expect("conditional used in expression must have a type"),

        ast::Expression::Block(block) => {
            let out_ty = match &block.output {
                Some(output_expr) => builder.translate_type(&output_expr.annotation().ty()),
                None => panic!("block used in expression must have a type"),
            };
            let out_ref = builder.local_new(out_ty, None);
            translate_block(block, out_ref.clone(), builder);

            out_ref
        },

        ast::Expression::Raise(raise) => translate_raise(raise, builder),
        ast::Expression::Exit(exit) => {
            translate_exit(exit, builder);
            Ref::Discard
        },

        ast::Expression::Case(case) => translate_case_expr(case, builder),
        ast::Expression::Match(match_expr) => translate_match_expr(match_expr, builder),

        ast::Expression::Cast(cast) => translate_cast_expr(cast, builder),

        ast::Expression::AnonymousFunction(def) => builder.build_closure_expr(def),
    };

    builder.pop_debug_context();

    result_ref
}

fn translate_indexer(
    val_ty: &Type,
    base_ref: Ref,
    index_ref: Ref,
    base_ty: &pas_ty::Type,
    builder: &mut Builder,
) -> Ref {
    match base_ty {
        pas_ty::Type::Array(array_ty) => {
            let element_ptr = builder.local_temp(val_ty.clone().ptr());

            builder.begin_scope();

            let element_ty = builder.translate_type(&array_ty.element_ty);
            let len = cast::i32(array_ty.dim).expect("array dim must be within range of i32");

            gen_bounds_check(index_ref.clone(), Value::LiteralI32(len), builder);

            builder.append(Instruction::Element {
                out: element_ptr.clone(),
                a: base_ref,
                index: Value::Ref(index_ref),
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
            let array_class = ClassID::Class(array_struct);
            let array_class_ty = Type::RcPointer(Some(array_class));

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
                index_ref.clone(),
                Value::Ref(len_field_ptr.to_deref()),
                builder,
            );

            // array_ptr := (array_field_ptr)^
            // element_ptr := array_ptr + index
            let array_ptr = arr_field_ptr.to_deref();
            builder.add(element_ptr.clone(), array_ptr, index_ref);

            builder.end_scope();

            element_ptr
        },

        pas_ty::Type::Pointer(_) => {
            let result_ptr = builder.local_temp(val_ty.clone().ptr());

            builder.add(result_ptr.clone(), base_ref, index_ref);

            result_ptr
        },

        unimpl => unimplemented!("IR for indexing into {}", unimpl),
    }
}

fn gen_bounds_check(index_val: impl Into<Value>, len_val: impl Into<Value>, builder: &mut Builder) {
    let index_val = index_val.into();

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
    translate_if_cond(if_cond, builder, |branch, out_ref, out_ty, builder| {
        let val = translate_expr(branch, builder);

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
    translate_if_cond(if_cond, builder, |branch, out_ref, _out_ty, builder| {
        assert!(
            out_ref.is_none(),
            "branch translated as statement should not have an out location"
        );

        translate_stmt(&branch, builder);
    })
}

fn translate_if_cond<B, BranchTranslateFn>(
    if_cond: &pas_ty::ast::IfCond<B>,
    builder: &mut Builder,
    branch_translate: BranchTranslateFn,
) -> Option<Ref>
where
    BranchTranslateFn: Fn(&B, Option<&Ref>, &Type, &mut Builder),
{
    let (out_val, out_ty) = match if_cond.annotation.ty().as_ref() {
        pas_ty::Type::Nothing => (None, Type::Nothing),
        out_ty => {
            let out_ty = builder.translate_type(out_ty);
            let out_val = builder.local_new(out_ty.clone(), None);
            (Some(out_val), out_ty)
        },
    };

    builder.scope(|builder| {
        let then_label = builder.alloc_label();
        let end_label = builder.alloc_label();
        let else_label = if_cond.else_branch.as_ref().map(|_| builder.alloc_label());

        let cond_val = translate_expr(&if_cond.cond, builder);
        let cond_ty = builder.translate_type(&if_cond.cond.annotation().ty());

        let pattern_match = match &if_cond.is_pattern {
            // match the cond val against the type pattern that follows it
            Some(is_pattern) => translate_pattern_match(is_pattern, &cond_val, &cond_ty, builder),

            // no pattern, the cond val must be a boolean and we're just testing that
            None => PatternMatchOutput {
                is_match: Value::Ref(cond_val),
                bindings: Vec::new(),
            },
        };

        builder.jmp_if(then_label, pattern_match.is_match.clone());

        if let Some(else_label) = else_label {
            builder.jmp(else_label);
        } else {
            builder.jmp(end_label);
        }

        builder.label(then_label);

        builder.scope(|builder| {
            // bind pattern locals to names and retain them
            for pattern_binding in &pattern_match.bindings {
                pattern_binding.bind_local(builder);
            }

            branch_translate(&if_cond.then_branch, out_val.as_ref(), &out_ty, builder);
        });

        builder.jmp(end_label);

        if let Some(else_branch) = &if_cond.else_branch {
            builder.label(else_label.unwrap());

            builder.begin_scope();
            branch_translate(&else_branch, out_val.as_ref(), &out_ty, builder);
            builder.end_scope();
        }

        builder.label(end_label);
    });

    out_val
}

fn translate_call_with_args(
    call_target: CallTarget,
    args: &[pas_ty::ast::Expression],
    sig: &pas_ty::FunctionSig,
    builder: &mut Builder,
) -> Option<Ref> {
    let out_val = match &sig.return_ty {
        pas_ty::Type::Nothing => None,
        return_ty => {
            let out_ty = builder.translate_type(return_ty);
            let out_val = builder.local_new(out_ty.clone(), None);
            Some(out_val)
        },
    };

    builder.begin_scope();

    let mut arg_vals = Vec::new();

    if let CallTarget::Closure { closure_ptr, .. } = &call_target {
        arg_vals.push(closure_ptr.clone());
    }

    for (arg, param) in args.iter().zip(sig.params.iter()) {
        let arg_expr = if param.is_by_ref() {
            let arg_ref = translate_expr(arg, builder);
            let arg_ty = builder.translate_type(&arg.annotation().ty());
            let arg_ptr = builder.local_temp(arg_ty.ptr());

            builder.append(Instruction::AddrOf {
                out: arg_ptr.clone(),
                a: arg_ref,
            });

            arg_ptr
        } else {
            translate_expr(arg, builder)
        };

        arg_vals.push(Value::from(arg_expr));
    }

    builder.append(match call_target {
        CallTarget::Closure { function, .. } | CallTarget::Function(function) => {
            Instruction::Call {
                function,
                args: arg_vals.clone(),
                out: out_val.clone(),
            }
        },

        CallTarget::Virtual { iface_id, method } => {
            let self_arg = arg_vals[0].clone();
            let rest_args = arg_vals[1..].to_vec();

            let methods = &builder.get_iface(iface_id).unwrap().methods;

            let method_index = methods
                .iter()
                .position(|m| m.name == method)
                .map(MethodID)
                .unwrap();

            Instruction::VirtualCall {
                out: out_val.clone(),
                iface_id,
                method: method_index,
                self_arg,
                rest_args,
            }
        },
    });

    // no need to retain, the result of a function must be retained as part of its body

    builder.end_scope();

    out_val
}

enum CallTarget {
    Function(Value),
    Closure {
        function: Value,
        closure_ptr: Value,
    },
    Virtual {
        iface_id: InterfaceID,
        method: String,
    },
}

pub fn build_call(call: &pas_ty::ast::Call, builder: &mut Builder) -> Option<Ref> {
    match call {
        ast::Call::Function(func_call) => build_func_call(func_call, builder),

        ast::Call::Method(method_call) => build_method_call(method_call, builder),

        ast::Call::VariantCtor(variant_ctor) => build_variant_ctor_call(variant_ctor, builder),
    }
}

fn build_func_call(
    func_call: &pas_ty::ast::FunctionCall,
    builder: &mut Builder,
) -> Option<Ref> {
    let type_args = func_call.type_args.clone();

    match func_call.target.annotation() {
        // calling a function directly
        pas_ty::TypeAnnotation::Function(func) => {
            let full_name = func.ns.clone().child(func.name.clone());
            let func = builder.translate_func(full_name, type_args, func_call.span());

            let func_val = Value::Ref(Ref::Global(GlobalRef::Function(func.id)));
            let func_sig = func.sig;

            let call_target = CallTarget::Function(func_val);

            translate_call_with_args(call_target, &func_call.args, &func_sig, builder)
        },

        // invoking a closure value that refers to a function
        pas_ty::TypeAnnotation::TypedValue(val) => {
            // it's impossible to invoke a closure with type args, so the typechecker should
            // ensure this never happens
            assert!(
                type_args.is_none(),
                "closure invocation cannot include type args"
            );

            // expr that evaluates to a closure pointer
            let target_expr_val = translate_expr(&func_call.target, builder);
            let func_sig = val
                .ty
                .as_func()
                .expect("target value of invocation must have function type");
            let func_ty_id = builder.translate_func_ty(&func_sig);

            // retrieve the actual function value
            let func_field_ptr = builder.local_temp(Type::Function(func_ty_id).ptr());

            builder.scope(|builder| {
                let closure_ptr_ty = Type::RcPointer(Some(ClassID::Closure(func_ty_id)));
                builder.field(
                    func_field_ptr.clone(),
                    target_expr_val.clone(),
                    closure_ptr_ty,
                    CLOSURE_PTR_FIELD,
                );
            });

            let call_target = CallTarget::Closure {
                function: func_field_ptr.to_deref().into(),
                closure_ptr: target_expr_val.clone().into(),
            };

            translate_call_with_args(call_target, &func_call.args, &func_sig, builder)
        },

        _ => panic!("type of function call expr must be a function or callable value"),
    }
}

fn build_method_call(
    method_call: &pas_ty::ast::MethodCall,
    builder: &mut Builder,
) -> Option<Ref> {
    if method_call.type_args.is_some() {
        unimplemented!("method call with type args")
    }

    let iface = match method_call.iface_type.as_iface() {
        Ok(iface) => iface.clone(),
        Err(bad_ty) => unreachable!("can't have non-interface interface types in method calls (trying to call method on {})", bad_ty),
    };

    let self_ty = builder.translate_type(&method_call.self_type);
    let method_sig = method_call.func_type.as_func().unwrap();

    let call_target = match &self_ty {
        Type::RcPointer(Some(ClassID::Interface(iface_id))) => CallTarget::Virtual {
            iface_id: *iface_id,
            method: method_call.ident.to_string(),
        },

        _ => {
            let method_self_ty = method_call.self_type.clone();
            let method_self_ty = match builder.type_args() {
                Some(builder_type_args) => method_self_ty.substitute_type_args(builder_type_args),
                None => method_self_ty,
            };

            //            println!("translating method {}::{} of {}", iface, method_call.ident, method_call.self_type);
            let method_decl =
                builder.translate_method_impl(iface, method_call.ident.clone(), method_self_ty);

            let func_val = Ref::Global(GlobalRef::Function(method_decl.id));

            CallTarget::Function(func_val.into())
        },
    };

    translate_call_with_args(call_target, &method_call.args, &method_sig, builder)
}

fn build_variant_ctor_call(
    variant_ctor: &pas_ty::ast::VariantCtorCall,
    builder: &mut Builder,
) -> Option<Ref> {
    let variant_ty = pas_ty::Type::Variant(Box::new(variant_ctor.variant.clone()));

    let variant_ty = match builder.type_args() {
        Some(builder_type_args) => variant_ty.substitute_type_args(builder_type_args),
        None => variant_ty,
    };

    let variant_name = variant_ty.as_variant().unwrap();

    let out_ty = builder.translate_type(&variant_ty);
    let out = builder.local_new(out_ty.clone(), None);

    builder.begin_scope();

    let tag_ptr = builder.local_temp(Type::I32.ptr());
    builder.append(Instruction::VariantTag {
        out: tag_ptr.clone(),
        a: out.clone(),
        of_ty: out_ty.clone(),
    });

    let (_, case_index, _) = builder.translate_variant_case(variant_name, &variant_ctor.case);

    // todo: proper index type
    builder.mov(tag_ptr.to_deref(), Value::LiteralI32(case_index as i32));

    if let Some(arg) = &variant_ctor.arg {
        let arg_ref = translate_expr(arg, builder);

        let arg_ty = builder.translate_type(&arg.annotation().ty());
        let field_ptr = builder.local_temp(arg_ty.clone().ptr());
        builder.append(Instruction::VariantData {
            out: field_ptr.clone(),
            a: out.clone(),
            tag: case_index,
            of_ty: out_ty.clone(),
        });

        builder.mov(field_ptr.clone().to_deref(), Value::Ref(arg_ref));
        builder.retain(field_ptr.to_deref(), &arg_ty);
    }

    builder.end_scope();
    Some(out)
}

fn is_string_class(class: &pas_ty::Symbol) -> bool {
    class.qualified.first().name.as_str() == "System"
        && class.qualified.last().name.as_str() == "String"
}

fn translate_literal(
    lit: &ast::Literal<pas_ty::Type>,
    ty: &pas_ty::Type,
    builder: &mut Builder,
) -> Ref {
    let out_ty = builder.translate_type(ty);
    let out = builder.local_temp(out_ty);

    match lit {
        ast::Literal::Nil => {
            builder.mov(out.clone(), Value::LiteralNull);
        },

        ast::Literal::Boolean(b) => {
            builder.mov(out.clone(), Value::LiteralBool(*b));
        },

        ast::Literal::Integer(i) => match ty {
            pas_ty::Type::Primitive(pas_ty::Primitive::Int8) => {
                let val = i
                    .as_i8()
                    .expect("Int8-typed constant must be within range of i8");
                builder.mov(out.clone(), Value::LiteralI8(val))
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::Byte) => {
                let val = i
                    .as_u8()
                    .expect("Byte-typed constant must be within range of u8");
                builder.mov(out.clone(), Value::LiteralByte(val))
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::Int16) => {
                let val = i
                    .as_i16()
                    .expect("Int16-typed constant must be within range of i16");
                builder.mov(out.clone(), Value::LiteralI16(val))
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::UInt16) => {
                let val = i
                    .as_u16()
                    .expect("Int16-typed constant must be within range of i16");
                builder.mov(out.clone(), Value::LiteralU16(val))
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::Int32) => {
                let val = i
                    .as_i32()
                    .expect("Int32-typed constant must be within range of i32");
                builder.mov(out.clone(), Value::LiteralI32(val));
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::UInt32) => {
                let val = i
                    .as_u32()
                    .expect("Int32-typed constant must be within range of u32");
                builder.mov(out.clone(), Value::LiteralU32(val));
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::Int64) => {
                let val = i
                    .as_i64()
                    .expect("Int64-typed constant must be within range of i64");
                builder.mov(out.clone(), Value::LiteralI64(val))
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::UInt64) => {
                let val = i
                    .as_u64()
                    .expect("Int64-typed constant must be within range of i64");
                builder.mov(out.clone(), Value::LiteralU64(val))
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::NativeInt) => {
                let val = i
                    .as_isize()
                    .expect("Int64-typed constant must be within range of i64");
                builder.mov(out.clone(), Value::LiteralISize(val))
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::NativeUInt) => {
                let val = i
                    .as_usize()
                    .expect("Int64-typed constant must be within range of isize");
                builder.mov(out.clone(), Value::LiteralUSize(val))
            },
            pas_ty::Type::Primitive(pas_ty::Primitive::Real32) => {
                let val = i
                    .as_f32()
                    .expect("Real-typed constant must be within range of usize");
                builder.mov(out.clone(), Value::LiteralF32(val))
            },

            _ => panic!("bad type for integer literal: {}", ty),
        },

        ast::Literal::Real(r) => match ty {
            pas_ty::Type::Primitive(pas_ty::Primitive::Real32) => {
                let val = r
                    .as_f32()
                    .expect("Real32-typed constant must be within range of f32");
                builder.mov(out.clone(), Value::LiteralF32(val));
            },
            _ => panic!("bad type for real literal: {}", ty),
        },

        ast::Literal::String(s) => match ty {
            pas_ty::Type::Class(class) if is_string_class(class) => {
                let lit_id = builder.find_or_insert_string(s);
                let lit_ref = GlobalRef::StringLiteral(lit_id);

                builder.mov(out.clone(), Value::Ref(Ref::Global(lit_ref)));
            },
            _ => panic!("bad type for string literal: {}", ty),
        },

        ast::Literal::SizeOf(ty) => {
            let ty = builder.translate_type(ty);
            builder.append(Instruction::SizeOf {
                out: out.clone(),
                ty,
            });
        },
    }

    out
}

fn translate_bin_op(
    bin_op: &pas_ty::ast::BinOp,
    out_ty: &pas_ty::Type,
    builder: &mut Builder,
) -> Ref {
    if bin_op.lhs.annotation().is_namespace() {
        // there's nothing to actually translate on the lhs, it's just for name resolution
        return translate_expr(&bin_op.rhs, builder);
    }

    let out_ty = builder.translate_type(out_ty);

    // the functions to translate IR and member operators return pointers to the value
    let (out_val, out_is_ptr) = match bin_op.op {
        syn::Operator::Member | syn::Operator::Index => {
            let out_val = builder.local_new(out_ty.clone().ptr(), None);
            (out_val, true)
        },

        _ => {
            let out_val = builder.local_new(out_ty.clone(), None);
            (out_val, false)
        },
    };

    builder.begin_scope();
    let lhs_val = translate_expr(&bin_op.lhs, builder);

    match &bin_op.op {
        syn::Operator::Member => {
            // auto-deref for rc types
            let of_ty = builder.translate_type(&bin_op.lhs.annotation().ty());

            let struct_id = match &of_ty {
                Type::Struct(id) => *id,
                Type::RcPointer(Some(ClassID::Class(id))) => *id,
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
            let index_val = translate_expr(&bin_op.rhs, builder);
            let element_val = translate_indexer(
                &out_ty,
                lhs_val,
                index_val,
                &bin_op.lhs.annotation().ty(),
                builder,
            );

            builder.mov(out_val.clone(), element_val);
        },

        syn::Operator::NotEquals => {
            let b = translate_expr(&bin_op.rhs, builder);
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
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Eq {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Add => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Add {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Multiply => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Mul {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Divide => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::IDiv {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Gt => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Gt {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Gte => {
            let b = translate_expr(&bin_op.rhs, builder);

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

            builder.append(Instruction::Or {
                out: out_val.clone(),
                a: gt.into(),
                b: eq.into(),
            });
        },

        syn::Operator::Lt => {
            let b = translate_expr(&bin_op.rhs, builder);

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
            let b = translate_expr(&bin_op.rhs, builder);

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

        syn::Operator::Subtract => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Sub {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Shl => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Shl {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Shr => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Shr {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::And => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::And {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Or => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Or {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::BitAnd => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::BitAnd {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::BitOr => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::BitOr {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        },

        syn::Operator::Caret => {
            let b = translate_expr(&bin_op.rhs, builder);
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

fn translate_unary_op(
    unary_op: &pas_ty::ast::UnaryOp,
    out_ty: &pas_ty::Type,
    builder: &mut Builder,
) -> Ref {
    let operand_ref = translate_expr(&unary_op.operand, builder);

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

        syn::Operator::Subtract => {
            let out_ty = builder.translate_type(out_ty);
            let out_val = builder.local_new(out_ty.clone(), None);

            let op_ty = unary_op.annotation.ty();

            let zero_val = match op_ty.as_ref() {
                pas_ty::Type::Primitive(pas_ty::Primitive::Int8) => Value::LiteralI8(0),
                pas_ty::Type::Primitive(pas_ty::Primitive::Byte) => Value::LiteralByte(0),
                pas_ty::Type::Primitive(pas_ty::Primitive::Int16) => Value::LiteralI16(0),
                pas_ty::Type::Primitive(pas_ty::Primitive::UInt16) => Value::LiteralI16(0),
                pas_ty::Type::Primitive(pas_ty::Primitive::Int32) => Value::LiteralI32(0),
                pas_ty::Type::Primitive(pas_ty::Primitive::UInt32) => Value::LiteralU32(0),
                pas_ty::Type::Primitive(pas_ty::Primitive::Int64) => Value::LiteralI64(0),
                pas_ty::Type::Primitive(pas_ty::Primitive::UInt64) => Value::LiteralU64(0),
                pas_ty::Type::Primitive(pas_ty::Primitive::NativeInt) => Value::LiteralISize(0),
                pas_ty::Type::Primitive(pas_ty::Primitive::NativeUInt) => Value::LiteralUSize(0),
                pas_ty::Type::Primitive(pas_ty::Primitive::Real32) => Value::LiteralF32(0.0),
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

fn translate_object_ctor(ctor: &pas_ty::ast::ObjectCtor, builder: &mut Builder) -> Ref {
    let object_ty = builder.translate_type(&ctor.annotation.ty());

    let struct_id = match &object_ty {
        Type::RcPointer(Some(ClassID::Class(struct_id))) => *struct_id,
        Type::Struct(struct_id) => *struct_id,
        _ => panic!(
            "type of object ctor expression `{}` must be a record or class",
            ctor
        ),
    };

    let struct_def = builder
        .get_struct(struct_id)
        .unwrap_or_else(|| panic!("struct {} referenced in object ctor must exist", ctor.ident))
        .clone();

    // either local struct of the correct type for value types, or a rc pointer to the struct
    // type for rc class types
    let out_val = builder.local_new(object_ty.clone(), None);

    if object_ty.is_rc() {
        // allocate class struct at out pointer
        builder.append(Instruction::RcNew {
            out: out_val.clone(),
            struct_id,
        });
    }

    builder.scope(|builder| {
        for member in &ctor.args.members {
            let member_val = translate_expr(&member.value, builder);
            let field_id = struct_def
                .find_field(&member.ident.name)
                .unwrap_or_else(|| {
                    panic!(
                        "field {} referenced in object ctor must exist",
                        member.ident
                    )
                });

            let field_def = struct_def.get_field(field_id).unwrap();

            builder.comment(&format!(
                "{}: {} ({})",
                member.ident, member.value, field_def.ty
            ));

            let field_ptr = builder.local_temp(field_def.ty.clone().ptr());
            builder.append(Instruction::Field {
                out: field_ptr.clone(),
                a: out_val.clone(),
                of_ty: object_ty.clone(),
                field: field_id,
            });

            builder.mov(field_ptr.clone().to_deref(), member_val);
            builder.retain(field_ptr.to_deref(), &field_def.ty);
        }
    });

    out_val
}

fn translate_collection_ctor(ctor: &pas_ty::ast::CollectionCtor, builder: &mut Builder) -> Ref {
    let ctor_ty = ctor.annotation.ty();
    match ctor_ty.as_ref() {
        pas_ty::Type::Array(array_ty) => {
            translate_static_array_ctor(ctor, &array_ty.element_ty, array_ty.dim, builder)
        },

        pas_ty::Type::DynArray { element } => translate_dyn_array_ctor(ctor, element, builder),

        unimpl => unimplemented!("IR for array constructor {} of type {}", ctor, unimpl),
    }
}

fn translate_static_array_ctor(
    ctor: &pas_ty::ast::CollectionCtor,
    element: &pas_ty::Type,
    dim: usize,
    builder: &mut Builder,
) -> Ref {
    let el_ty = builder.translate_type(element);

    let array_ty = el_ty.clone().array(dim);
    let arr = builder.local_temp(array_ty.clone());

    builder.begin_scope();

    let el_ptr = builder.local_temp(el_ty.clone().ptr());

    for (i, el) in ctor.elements.iter().enumerate() {
        builder.begin_scope();

        let index = i32::try_from(i).expect("invalid array index in array ctor");

        builder.append(Instruction::Element {
            out: el_ptr.clone(),
            a: arr.clone(),
            index: Value::LiteralI32(index),
            element: el_ty.clone(),
        });

        let el_init = translate_expr(el, builder);

        builder.mov(el_ptr.clone().to_deref(), el_init);
        builder.end_scope();
    }

    builder.end_scope();

    arr
}

fn translate_dyn_array_ctor(
    ctor: &pas_ty::ast::CollectionCtor,
    element: &pas_ty::Type,
    builder: &mut Builder,
) -> Ref {
    let elem_ty = builder.translate_type(element);

    // should be a class rc-ptr to the unique class for this dyn array element type
    let array_ty = builder.translate_type(&ctor.annotation.ty());
    let struct_id = match &array_ty {
        Type::RcPointer(Some(ClassID::Class(struct_id))) => *struct_id,
        _ => unreachable!("dynamic array must have an rc class type"),
    };

    let arr = builder.local_new(array_ty.clone(), None);

    // allocate the array object itself
    builder.scope(|builder| {
        builder.append(Instruction::RcNew {
            out: arr.clone(),
            struct_id,
        });

        // get pointer to the length
        let len_ref = builder.local_temp(Type::I32.ptr());
        builder.append(Instruction::Field {
            out: len_ref.clone(),
            of_ty: array_ty.clone(),
            field: DYNARRAY_LEN_FIELD,
            a: arr.clone(),
        });

        // set length
        let len = i32::try_from(ctor.elements.len()).expect("invalid dynamic array ctor length");
        builder.mov(len_ref.clone().to_deref(), Value::LiteralI32(len));

        // get pointer to storage pointer
        let arr_ptr = builder.local_temp(elem_ty.clone().ptr().ptr());
        builder.append(Instruction::Field {
            out: arr_ptr.clone(),
            of_ty: array_ty,
            field: DYNARRAY_PTR_FIELD,
            a: arr.clone(),
        });

        // allocate array storage
        if len > 0 {
            builder.append(Instruction::DynAlloc {
                out: arr_ptr.clone().to_deref(),
                count: Value::LiteralI32(len),
                element_ty: elem_ty.clone(),
            });

            let el_ptr = builder.local_temp(elem_ty.clone().ptr());

            for (i, el) in ctor.elements.iter().enumerate() {
                builder.scope(|builder| {
                    // we know this cast is OK because we check the length is in range of i32 previously
                    let index = Value::LiteralI32(i as i32);

                    // el_ptr := arr_ptr^ + i
                    builder.append(Instruction::Add {
                        a: Value::Ref(arr_ptr.clone().to_deref()),
                        b: index,
                        out: el_ptr.clone(),
                    });

                    // el_ptr^ := el
                    let el = translate_expr(el, builder);
                    builder.mov(el_ptr.clone().to_deref(), el);

                    // retain each element. we don't do this for static arrays because retaining
                    // a static array retains all its elements - for dynamic arrays, retaining
                    // the array object itself does not retain the elements
                    builder.retain(el_ptr.clone().to_deref(), &elem_ty);
                });
            }
        } else {
            builder.mov(arr_ptr.to_deref(), Value::LiteralNull);
        }
    });

    arr
}

fn translate_ident(ident: &Ident, annotation: &TypeAnnotation, builder: &mut Builder) -> Ref {
    match annotation {
        TypeAnnotation::Function(func) => {
            let func_name = func.ns.clone().child(func.name.clone());
            let func = builder.translate_func(func_name, func.type_args.clone(), &func.span);
            let func_ref = GlobalRef::Function(func.id);

            Ref::Global(func_ref)
        },

        TypeAnnotation::TypedValue(val) => {
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

    for stmt in &block.statements {
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

        // we can assume this function has a return register, otherwise an exit statement
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

fn translate_case_expr(case: &pas_ty::ast::CaseExpr, builder: &mut Builder) -> Ref {
    let out_ty = builder.translate_type(&case.annotation.ty());
    let out_ref = builder.local_temp(out_ty);

    build_case_block(case, builder, |item, builder| {
        let branch_result = translate_expr(item, builder);
        builder.mov(out_ref.clone(), branch_result);
    });

    out_ref
}

fn translate_cast_expr(cast: &pas_ty::ast::Cast, builder: &mut Builder) -> Ref {
    let val = translate_expr(&cast.expr, builder);
    let ty = builder.translate_type(&cast.ty);
    let out_ref = builder.local_temp(ty.clone());

    builder.cast(out_ref.clone(), val, ty);

    out_ref
}

fn translate_match_expr(match_expr: &pas_ty::ast::MatchExpr, builder: &mut Builder) -> Ref {
    let out_ty = builder.translate_type(&match_expr.annotation.ty());
    let out_ref = builder.local_new(out_ty.clone(), None);

    builder.scope(|builder| {
        let cond_expr = translate_expr(&match_expr.cond_expr, builder);
        let cond_ty = builder.translate_type(&match_expr.cond_expr.annotation().ty());

        let break_label = builder.alloc_label();

        let else_label = if match_expr.else_branch.is_some() {
            Some(builder.alloc_label())
        } else {
            None
        };

        let is_skip = builder.local_temp(Type::Bool);

        for branch in &match_expr.branches {
            // label to skip this branch if it isn't a match
            let skip_label = builder.alloc_label();

            builder.scope(|builder| {
                let pattern_match =
                    translate_pattern_match(&branch.pattern, &cond_expr, &cond_ty, builder);

                // jump to skip label if pattern match return false
                builder.not(is_skip.clone(), pattern_match.is_match.clone());
                builder.jmp_if(skip_label, is_skip.clone());

                builder.scope(|builder| {
                    // code to run if we didn't skip - the actual branch
                    for binding in pattern_match.bindings {
                        binding.bind_local(builder);
                    }

                    let branch_val = translate_expr(&branch.item, builder);

                    builder.mov(out_ref.clone(), branch_val);
                    builder.retain(out_ref.clone(), &out_ty);
                });

                // only one branch must run so break out of the block now
                builder.jmp(break_label);

                builder.label(skip_label);
            });
        }

        // write the else branch - will fall through to here if we didn't run any branches
        if let Some(else_branch) = &match_expr.else_branch {
            builder.scope(|builder| {
                builder.label(else_label.unwrap());

                let else_val = translate_expr(else_branch, builder);
                builder.mov(out_ref.clone(), else_val);
                builder.retain(out_ref.clone(), &out_ty);
            });

            builder.jmp(break_label);
        }

        // we MUST have executed a branch!
        let err = "unhandled pattern in match expression";
        let err_str = builder.find_or_insert_string(err);
        builder.append(Instruction::Raise {
            val: Ref::Global(GlobalRef::StringLiteral(err_str)),
        });

        builder.label(break_label);
    });

    out_ref
}
