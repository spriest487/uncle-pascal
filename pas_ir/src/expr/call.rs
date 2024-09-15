use crate::builder::Builder;
use crate::expr;
use crate::metadata::InterfaceID;
use crate::metadata::MethodID;
use crate::metadata::VirtualTypeID;
use crate::metadata::CLOSURE_PTR_FIELD;
use crate::GlobalRef;
use crate::Instruction;
use crate::Ref;
use crate::Type;
use crate::Value;
use pas_syn::ast;
use pas_syn::Ident;
use pas_typecheck::Typed;
use pas_typecheck as pas_ty;

fn translate_call_with_args(
    call_target: CallTarget,
    args: &[pas_ty::ast::Expr],
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
            let arg_ref = expr::translate_expr(arg, builder);
            let arg_ty = builder.translate_type(&arg.annotation().ty());
            let arg_ptr = builder.local_temp(arg_ty.ptr());

            builder.append(Instruction::AddrOf {
                out: arg_ptr.clone(),
                a: arg_ref,
            });

            arg_ptr
        } else {
            expr::translate_expr(arg, builder)
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
        ast::Call::FunctionNoArgs(func_call) => {
            build_func_call(&func_call.target, &[], None, builder)
        },

        ast::Call::MethodNoArgs(method_call) => match method_call.target.annotation() {
            Typed::InterfaceMethod(method) => {
                let self_arg = method_call.self_arg.clone();
                let self_ty = self_arg.annotation().ty().into_owned();
                let args = vec![self_arg];

                build_method_call(
                    &method.iface_ty,
                    &method.method_ident,
                    &method.method_sig,
                    &self_ty,
                    &args,
                    None,
                    builder,
                )
            },
            _ => panic!("target of no-args method call was not a method"),
        },

        ast::Call::Function(func_call) => build_func_call(
            &func_call.target,
            &func_call.args,
            func_call.type_args.as_ref(),
            builder,
        ),

        ast::Call::Method(method_call) => {
            let method_sig = method_call.func_type.as_func().unwrap();

            build_method_call(
                &method_call.iface_type,
                &method_call.ident,
                &method_sig,
                &method_call.self_type,
                &method_call.args,
                method_call.type_args.as_ref(),
                builder,
            )
        },

        ast::Call::VariantCtor(variant_ctor) => build_variant_ctor_call(variant_ctor, builder),
    }
}

fn build_func_call(
    target: &pas_ty::ast::Expr,
    args: &[pas_ty::ast::Expr],
    type_args: Option<&pas_ty::TypeList>,
    builder: &mut Builder,
) -> Option<Ref> {
    match target.annotation() {
        // calling a function directly
        pas_ty::Typed::Function(func) => {
            let func = builder.translate_func(func.ident.clone(), type_args.cloned());

            let func_val = Value::Ref(Ref::Global(GlobalRef::Function(func.id)));
            let func_sig = func.sig;

            let call_target = CallTarget::Function(func_val);

            translate_call_with_args(call_target, args, &func_sig, builder)
        },

        pas_ty::Typed::UfcsFunction(func) => {
            let func_instance = builder.translate_func(func.function_name.clone(), None);
            let func_val = Value::Ref(Ref::Global(GlobalRef::Function(func_instance.id)));
            let func_sig = func_instance.sig;

            let call_target = CallTarget::Function(func_val);

            let mut args_with_self_arg = Vec::with_capacity(args.len() + 1);
            args_with_self_arg.push((*func.self_arg).clone());
            args_with_self_arg.extend(args.iter().cloned());

            translate_call_with_args(call_target, &args_with_self_arg, &func_sig, builder)
        },

        // invoking a closure value that refers to a function
        pas_ty::Typed::TypedValue(val) => {
            // it's impossible to invoke a closure with type args, so the typechecker should
            // ensure this never happens
            assert!(
                type_args.is_none(),
                "closure invocation cannot include type args"
            );

            // expr that evaluates to a closure pointer
            let target_expr_val = expr::translate_expr(target, builder);
            let func_sig = val
                .ty
                .as_func()
                .expect("target value of invocation must have function type");
            let func_ty_id = builder.translate_func_ty(&func_sig);

            // retrieve the actual function value
            let func_field_ptr = builder.local_temp(Type::Function(func_ty_id).ptr());

            builder.scope(|builder| {
                let closure_ptr_ty = Type::RcPointer(VirtualTypeID::Closure(func_ty_id));
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

            translate_call_with_args(call_target, args, &func_sig, builder)
        },

        unexpected => panic!(
            "type of function call expr must be a function or callable value got: {:#?}",
            unexpected
        ),
    }
}

fn build_method_call(
    iface_ty: &pas_ty::Type,
    method_ident: &Ident,
    method_sig: &pas_ty::FunctionSig,
    self_ty: &pas_ty::Type,
    args: &[pas_ty::ast::Expr],
    ty_args: Option<&pas_ty::TypeList>,
    builder: &mut Builder,
) -> Option<Ref> {
    if ty_args.is_some() {
        unimplemented!("method call with type args")
    }

    let iface = match iface_ty.as_iface() {
        Ok(iface) => iface.clone(),
        Err(bad_ty) => unreachable!("can't have non-interface interface types in method calls (trying to call method on {})", bad_ty),
    };

    let actual_self_ty = match builder.type_args() {
        Some(builder_type_args) => self_ty.clone().substitute_type_args(builder_type_args),
        None => self_ty.clone(),
    };

    let self_ir_ty = builder.translate_type(&actual_self_ty);

    let call_target = match &self_ir_ty {
        Type::RcPointer(VirtualTypeID::Interface(iface_id)) => CallTarget::Virtual {
            iface_id: *iface_id,
            method: method_ident.to_string(),
        },

        _ => {
            //            println!("translating method {}::{} of {}", iface, method_call.ident, method_call.self_type);

            let method_decl =
                builder.translate_method_impl(iface, method_ident.clone(), self_ty.clone());

            let func_val = Ref::Global(GlobalRef::Function(method_decl.id));

            CallTarget::Function(func_val.into())
        },
    };

    translate_call_with_args(call_target, &args, &method_sig, builder)
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
        let arg_val = expr::expr_to_val(arg, builder);

        let arg_ty = builder.translate_type(&arg.annotation().ty());
        let field_ptr = builder.local_temp(arg_ty.clone().ptr());
        builder.append(Instruction::VariantData {
            out: field_ptr.clone(),
            a: out.clone(),
            tag: case_index,
            of_ty: out_ty.clone(),
        });

        builder.mov(field_ptr.clone().to_deref(), arg_val);
        builder.retain(field_ptr.to_deref(), &arg_ty);
    }

    builder.end_scope();
    Some(out)
}
