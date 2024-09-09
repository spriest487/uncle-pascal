use crate::{
    annotation::VariantCtorAnnotation,
    ast::{
        const_eval_integer, implicit_conversion, member_annotation, typecheck_expr,
        typecheck_object_ctor, Call, Expr, MethodCall,
    },
    string_type, Context, FunctionAnnotation, FunctionSig, InstanceMember,
    InterfaceMethodAnnotation, NameContainer, NameError, OverloadAnnotation, Primitive, Symbol,
    Type, TypeAnnotation, TypeMember, TypecheckError, TypecheckResult, TypedValueAnnotation,
    UfcsFunctionAnnotation, ValueKind, DISPLAYABLE_IFACE_NAME, DISPLAYABLE_TOSTRING_METHOD,
    SYSTEM_UNIT_NAME,
};
use pas_common::span::{Span, Spanned};
use pas_syn::{ast, Ident, IdentPath, IntConstant, Operator};
use std::rc::Rc;
use crate::ast::{OverloadCandidate};

pub type BinOp = ast::BinOp<TypeAnnotation>;

fn invalid_bin_op(bin_op: &ast::BinOp<Span>, lhs: &Expr, rhs: &Expr) -> TypecheckError {
    TypecheckError::InvalidBinOp {
        lhs: lhs.annotation().ty().into_owned(),
        rhs: rhs.annotation().ty().into_owned(),
        op: bin_op.op,
        span: bin_op.annotation.span().clone(),
    }
}

pub fn typecheck_bin_op(
    bin_op: &ast::BinOp<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Expr> {
    let span = bin_op.annotation.clone();

    match &bin_op.op {
        Operator::Period => typecheck_member_of(&bin_op.lhs, &bin_op.rhs, span, expect_ty, ctx),

        Operator::Index => typecheck_indexer(&bin_op.lhs, &bin_op.rhs, &span, ctx),

        Operator::And | Operator::Or => {
            let bin_op = typecheck_logical_op(bin_op, span, ctx)?;
            Ok(Expr::from(bin_op))
        },

        Operator::Equals | Operator::NotEquals => {
            let bin_op = typecheck_equality(bin_op, span, ctx)?;
            Ok(Expr::from(bin_op))
        },

        Operator::Gt | Operator::Gte | Operator::Lt | Operator::Lte => {
            let bin_op = typecheck_comparison(bin_op, span, ctx)?;
            Ok(Expr::from(bin_op))
        },

        Operator::BitAnd | Operator::BitOr | Operator::Shr | Operator::Shl | Operator::Caret => {
            let bin_op = typecheck_bitwise_op(bin_op, expect_ty, ctx)?;
            Ok(Expr::from(bin_op))
        },

        _ => {
            let mut lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
            let mut rhs = typecheck_expr(&bin_op.rhs, &lhs.annotation().ty(), ctx)?;

            // string concat sugar isn't available if the String class isn't loaded
            if let Ok(string_ty) = string_type(ctx) {
                let mut lhs_string = *lhs.annotation().ty() == string_ty;
                let mut rhs_string = *rhs.annotation().ty() == string_ty;

                if lhs_string && !rhs_string {
                    if let Some(rhs_to_string) = desugar_displayable_to_string(&rhs, &span, ctx) {
                        rhs = rhs_to_string;
                        rhs_string = true;
                    }
                } else if !lhs_string && lhs_string {
                    if let Some(lhs_to_string) = desugar_displayable_to_string(&lhs, &span, ctx) {
                        lhs = lhs_to_string;
                        lhs_string = true;
                    }
                }

                let is_string_concat = bin_op.op == Operator::Add && lhs_string && rhs_string;

                if is_string_concat {
                    return desugar_string_concat(lhs, rhs, &string_ty, ctx);
                }
            }

            let valid_math = lhs
                .annotation()
                .ty()
                .valid_math_op(bin_op.op, &rhs.annotation().ty());

            if !valid_math {
                return Err(invalid_bin_op(&bin_op, &lhs, &rhs));
            }

            // check valid ops etc, result type etc
            let result_ty = lhs.annotation().ty().into_owned();

            let annotation = match result_ty {
                Type::Nothing => TypeAnnotation::Untyped(span.clone()),
                ty => TypedValueAnnotation {
                    ty,
                    value_kind: ValueKind::Temporary,
                    span,
                    decl: None,
                }
                .into(),
            };

            Ok(Expr::from(ast::BinOp {
                lhs,
                op: bin_op.op,
                rhs,
                annotation,
            }))
        },
    }
}

fn typecheck_logical_op(
    bin_op: &ast::BinOp<Span>,
    span: Span,
    ctx: &mut Context,
) -> TypecheckResult<BinOp> {
    let bool_ty = Type::Primitive(Primitive::Boolean);

    let lhs = typecheck_expr(&bin_op.lhs, &bool_ty, ctx)?;
    lhs.annotation().expect_value(&bool_ty)?;

    let rhs = typecheck_expr(&bin_op.rhs, &bool_ty, ctx)?;
    rhs.annotation().expect_value(&bool_ty)?;

    let annotation = TypedValueAnnotation {
        ty: bool_ty,
        value_kind: ValueKind::Temporary,
        span,
        decl: None,
    }
    .into();

    Ok(BinOp {
        lhs,
        rhs,
        op: bin_op.op,
        annotation,
    })
}

fn typecheck_equality(
    bin_op: &ast::BinOp<Span>,
    span: Span,
    ctx: &mut Context,
) -> TypecheckResult<BinOp> {
    let lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
    let rhs = typecheck_expr(&bin_op.rhs, &lhs.annotation().ty(), ctx)?;

    if !lhs.annotation().ty().equatable(&rhs.annotation().ty()) {
        return Err(invalid_bin_op(bin_op, &lhs, &rhs));
    }

    let annotation = TypedValueAnnotation {
        ty: Type::Primitive(Primitive::Boolean),
        value_kind: ValueKind::Temporary,
        span,
        decl: None,
    }
    .into();

    Ok(BinOp {
        lhs,
        rhs,
        op: bin_op.op,
        annotation,
    })
}

fn typecheck_comparison(
    bin_op: &ast::BinOp<Span>,
    span: Span,
    ctx: &mut Context,
) -> TypecheckResult<BinOp> {
    let lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
    let rhs = typecheck_expr(&bin_op.rhs, &lhs.annotation().ty(), ctx)?;

    rhs.annotation().expect_value(&lhs.annotation().ty())?;

    if !lhs.annotation().ty().self_orderable() {
        return Err(invalid_bin_op(&bin_op, &lhs, &rhs));
    }

    let annotation = TypedValueAnnotation {
        ty: Type::Primitive(Primitive::Boolean),
        value_kind: ValueKind::Temporary,
        span,
        decl: None,
    }
    .into();

    Ok(BinOp {
        lhs,
        rhs,
        op: bin_op.op,
        annotation,
    })
}

fn typecheck_bitwise_op(
    bin_op: &ast::BinOp<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<BinOp> {
    // if there's no expected type for a bitwise op, expect UInt32
    let expect_ty = match expect_ty {
        Type::Nothing => &Type::Primitive(Primitive::UInt32),
        _ => expect_ty, 
    };

    let lhs = typecheck_expr(&bin_op.lhs, expect_ty, ctx)?;

    // for bitwise operations to make sense the lhs and rhs must be the exact same type so insert a
    // conversion here as necessary
    let rhs = implicit_conversion(
        typecheck_expr(&bin_op.rhs, &lhs.annotation().ty(), ctx)?,
        &lhs.annotation().ty(),
        ctx,
    )?;

    let lhs_ty = lhs.annotation().ty();
    let rhs_ty = rhs.annotation().ty();

    if !lhs_ty.valid_math_op(bin_op.op, &rhs_ty) {
        return Err(invalid_bin_op(&bin_op, &lhs, &rhs));
    }

    Ok(BinOp {
        annotation: TypedValueAnnotation {
            ty: lhs.annotation().ty().into_owned(),
            span: bin_op.span().clone(),
            value_kind: ValueKind::Temporary,
            decl: None,
        }
        .into(),
        lhs,
        rhs,
        op: bin_op.op,
    })
}

// turn value `x` that implements System.Displayable into a call to `System.Displayable.ToString(x)`
fn desugar_displayable_to_string(expr: &Expr, span: &Span, ctx: &Context) -> Option<Expr> {
    let src_ty = expr.annotation().ty();

    let system_unit_ident = Ident::new(SYSTEM_UNIT_NAME, span.clone());
    let displayable_iface_ident = Ident::new(DISPLAYABLE_IFACE_NAME, span.clone());

    let to_string_ident = Ident::new(DISPLAYABLE_TOSTRING_METHOD, span.clone());

    let displayable_path = IdentPath::from_parts(vec![system_unit_ident, displayable_iface_ident]);

    if !ctx.is_iface_impl(src_ty.as_ref(), &displayable_path) {
        return None;
    }

    let displayable_iface = match ctx.find_iface_def(&displayable_path) {
        Ok(iface_def) => iface_def,
        Err(..) => return None,
    };

    let displayable_ty = Type::Interface(Box::new(displayable_path));

    let to_string_method = match displayable_iface.get_method(&to_string_ident) {
        Some(method_decl) => method_decl,
        None => return None,
    };

    let to_string_sig = Rc::new(FunctionSig::of_decl(&to_string_method.decl));

    // make a call
    let displayable_call = Call::Method(MethodCall {
        iface_type: displayable_ty.clone(),
        ident: to_string_ident.clone(),
        args: vec![expr.clone()],
        type_args: None,
        args_span: span.clone(),
        self_type: src_ty.into_owned(),
        func_type: Type::Function(to_string_sig.clone()),
        annotation: InterfaceMethodAnnotation {
            iface_ty: displayable_ty,
            method_ident: to_string_ident,
            span: span.clone(),
            method_sig: to_string_sig,
        }
        .into(),
    });

    Some(Expr::from(displayable_call))
}

// desugar a binary + operation on two strings into a call to System.StringConcat
fn desugar_string_concat(
    lhs: Expr,
    rhs: Expr,
    string_ty: &Type,
    ctx: &Context,
) -> TypecheckResult<Expr> {
    let span = lhs.annotation().span().to(rhs.annotation().span());
    let annotation = TypedValueAnnotation {
        ty: string_ty.clone(),
        span: span.clone(),
        value_kind: ValueKind::Temporary,
        decl: None,
    }
    .into();

    // if LHS and RHS are both string literals, we can concat them ahead of time
    match (&lhs, &rhs) {
        (
            ast::Expr::Literal(ast::Literal::String(a), _),
            ast::Expr::Literal(ast::Literal::String(b), _),
        ) => Ok(ast::Expr::Literal(
            ast::Literal::String(Rc::new((**a).clone() + b.as_str())),
            annotation,
        )),

        _ => {
            let system_path = IdentPath::from(Ident::new("System", span.clone()));
            let concat_path = system_path.child(Ident::new("StringConcat", span.clone()));
            let (concat_path, concat_sig) = ctx
                .find_function(&concat_path)
                .map_err(|err| TypecheckError::from_name_err(err, span.clone()))?;

            let concat_annotation = FunctionAnnotation {
                ident: concat_path.clone(),
                span: span.clone(),
                sig: concat_sig.clone(),
                type_args: None,
            }
            .into();

            let concat_func = ast::Expr::Ident(concat_path.last().clone(), concat_annotation);

            let concat_call = ast::Call::Function(ast::FunctionCall {
                annotation: annotation.clone(),
                args: vec![lhs, rhs],
                type_args: None,
                target: concat_func,
                args_span: span.clone(),
            });

            Ok(ast::Expr::from(concat_call))
        },
    }
}

fn typecheck_member_of(
    lhs: &ast::Expr<Span>,
    rhs: &ast::Expr<Span>,
    span: Span,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Expr> {
    let lhs = typecheck_expr(lhs, &Type::Nothing, ctx)?;

    match rhs {
        // x.y
        ast::Expr::Ident(member_ident, _) => {
            let member_ident = member_ident.clone();

            let annotation = match lhs.annotation() {
                // x is the name of a variant type - we are constucting that variant
                TypeAnnotation::Type(Type::Variant(variant_name), ..) => {
                    typecheck_variant_ctor(variant_name, &member_ident, &span, ctx)?
                },

                // x is a non-variant typename - we are accessing a member of that type
                // e.g. calling an interface method by its type-qualified name
                TypeAnnotation::Type(ty, _) => typecheck_type_member(ty, &member_ident, span.clone(), ctx)?,

                // x is a value - we are accessing a member of that value
                TypeAnnotation::TypedValue(base_val) => typecheck_member_value(
                    &lhs,
                    &base_val.ty,
                    base_val.value_kind,
                    &member_ident,
                    span.clone(),
                    ctx,
                )?,

                TypeAnnotation::Namespace(path, _) => {
                    let mut full_path = path.clone();
                    full_path.push(member_ident.clone());

                    match ctx.find_path(&full_path) {
                        Some(member) => member_annotation(member, span.clone(), ctx),
                        None => {
                            let err = NameError::MemberNotFound {
                                member: member_ident,
                                base: NameContainer::for_annotated(lhs.annotation()),
                            };

                            return Err(TypecheckError::from_name_err(err, span));
                        },
                    }
                },

                _ => {
                    let err = NameError::MemberNotFound {
                        member: member_ident,
                        base: NameContainer::for_annotated(lhs.annotation()),
                    };

                    return Err(TypecheckError::from_name_err(err, span));
                },
            };

            let rhs = ast::Expr::Ident(member_ident, annotation.clone());

            let lhs_ty = lhs.annotation().ty().into_owned();

            let mut member_op = BinOp {
                lhs,
                op: Operator::Period,
                rhs,
                annotation,
            };

            // member operations that reference function values with no params automatically turn into a
            // no-args call to that function, except in contexts where we expect a matching function value
            match &member_op.annotation {
                TypeAnnotation::Function(func_annotation)
                    if func_annotation.should_call_noargs_in_expr(expect_ty) =>
                {
                    let sig = func_annotation.sig.clone();
                    let call = Call::FunctionNoArgs(sig.new_no_args_function_call(Expr::from(member_op)));

                    Ok(Expr::from(call))
                },

                TypeAnnotation::UfcsFunction(ufcs_annotation)
                    if ufcs_annotation.should_call_noargs_in_expr(expect_ty) =>
                {
                    let sig = ufcs_annotation.sig.clone();
                    let call = Call::FunctionNoArgs(sig.new_no_args_function_call(Expr::from(member_op)));

                    Ok(Expr::from(call))
                },

                TypeAnnotation::InterfaceMethod(method) if method.should_call_noargs_in_expr(expect_ty, &lhs_ty) => {
                    let sig = method.method_sig.clone();
                    let self_arg = member_op.lhs.clone();
                    let call = Call::MethodNoArgs(sig.new_no_args_method_call(Expr::from(member_op), self_arg));

                    Ok(Expr::from(call))
                }

                TypeAnnotation::Overload(overloaded) => {
                    // if any of the possible overloads can be called with no args, assume this expression
                    // is a no-args call - if arguments are applied later we'll re-resolve the overload
                    match resolve_no_args_overload(overloaded, expect_ty, &member_op.lhs) {
                        Some(OverloadCandidate::Function { decl_name, sig }) => {
                            let sig = sig.clone();
                            member_op.annotation = TypeAnnotation::from(FunctionAnnotation {
                                ident: decl_name.clone(),
                                sig: sig.clone(),
                                span: member_op.span().clone(),
                                type_args: None,
                            });
                            let call = Call::FunctionNoArgs(sig.new_no_args_function_call(Expr::from(member_op)));
                            Ok(Expr::from(call))
                        }

                        Some(OverloadCandidate::Method { sig, ident, iface_ty, .. }) => {
                            let sig = sig.clone();
                            member_op.annotation = TypeAnnotation::from(InterfaceMethodAnnotation {
                                iface_ty: iface_ty.clone(),
                                method_ident: ident.clone(),
                                method_sig: sig.clone(),
                                span: member_op.span().clone(),
                            });

                            let self_arg = member_op.lhs.clone();

                            let call = Call::MethodNoArgs(sig.new_no_args_method_call(Expr::from(member_op), self_arg));
                            Ok(Expr::from(call))
                        }

                        None => {
                            // no valid overloads, this must be resolved later by providing args
                            Ok(Expr::from(member_op))
                        }
                    }
                }

                _ => Ok(Expr::from(member_op)),
            }
        },

        // a.B(x: x)
        ast::Expr::ObjectCtor(ctor) => {
            match lhs.annotation() {
                // a must be a namespace qualifier before the constructed object name
                TypeAnnotation::Namespace(ns_path, ..) => {
                    assert_eq!(
                        1,
                        ctor.ident.as_ref().unwrap().as_slice().len(),
                        "parsed ctor should only have a single ident in its path until this point, but found {}",
                        ctor.ident.as_ref().unwrap()
                    );

                    let qualified_ident = ns_path
                        .clone()
                        .child(ctor.ident.as_ref().unwrap().last().clone());
                    let qualified_ctor = ast::ObjectCtor {
                        ident: Some(qualified_ident),
                        ..(**ctor).clone()
                    };

                    let span = lhs.annotation().span().to(qualified_ctor.annotation.span());

                    let ctor = typecheck_object_ctor(&qualified_ctor, span, expect_ty, ctx)?;
                    Ok(Expr::from(ctor))
                },

                _ => Err(TypecheckError::InvalidCtorType {
                    ty: lhs.annotation().ty().into_owned(),
                    span,
                }),
            }
        },

        _ => {
            let rhs = typecheck_expr(rhs, &Type::Nothing, ctx)?;

            Err(TypecheckError::InvalidBinOp {
                lhs: lhs.annotation().ty().into_owned(),
                rhs: rhs.annotation().ty().into_owned(),
                span,
                op: Operator::Period,
            })
        },
    }
}

fn resolve_no_args_overload<'a>(overloaded: &'a OverloadAnnotation, expect_ty: &Type, self_arg: &Expr) -> Option<&'a OverloadCandidate> {
    let mut result = None;

    for candidate in &overloaded.candidates {
        let should_call_no_args = match candidate {
            OverloadCandidate::Function { sig, .. } => {
                sig.should_call_noargs_in_expr(expect_ty, None)
            }

            OverloadCandidate::Method { sig, .. } => {
                let self_arg_ty = self_arg.annotation().ty();
                sig.should_call_noargs_in_expr(expect_ty, Some(&self_arg_ty))
            }
        };

        if should_call_no_args {
            if result.is_some() {
                // it's ambiguous
                return None;
            }

            result = Some(candidate);
        }
    }

    result
}

fn typecheck_type_member(
    ty: &Type,
    member_ident: &Ident,
    span: Span,
    ctx: &mut Context,
) -> TypecheckResult<TypeAnnotation> {
    let type_member = ctx
        .find_type_member(ty, member_ident)
        .map_err(|err| TypecheckError::from_name_err(err, span.clone()))?;

    let annotation = match type_member {
        TypeMember::Method { decl } => {
            InterfaceMethodAnnotation::new(&decl, ty.clone(), span).into()
        },
    };

    Ok(annotation)
}

pub fn typecheck_member_value(
    lhs: &Expr,
    base_ty: &Type,
    value_kind: ValueKind,
    member_ident: &Ident,
    span: Span,
    ctx: &mut Context,
) -> TypecheckResult<TypeAnnotation> {
    let member = ctx
        .find_instance_member(&lhs.annotation().ty(), &member_ident)
        .map_err(|err| TypecheckError::from_name_err(err, span.clone()))?;

    let annotation = match member {
        InstanceMember::Method { iface_ty, method } => {
            let iface_id = match &iface_ty {
                Type::Interface(iface_id) => iface_id,
                _ => unimplemented!("non-interface interface types"),
            };

            // if it's being called through an interface, it's a virtual call
            let method = if base_ty.as_iface().is_ok() {
                // calling the virtual method
                let iface_decl = ctx
                    .find_iface_def(iface_id)
                    .map_err(|err| TypecheckError::from_name_err(err, span.clone()))?;

                let method_decl = iface_decl
                    .get_method(&method)
                    .expect("method must exist, it was found by find_instance_member");

                let iface_ty = Type::Interface(iface_id.clone());

                OverloadAnnotation::method(
                    iface_ty,
                    lhs.clone(),
                    method_decl.decl.clone(),
                    span.clone(),
                )
            } else {
                let method_decl = match base_ty {
                    Type::GenericParam(type_param_ty) => {
                        match &type_param_ty.is_iface {
                            None => None,
                            Some(is_iface) => {
                                // the implementor type is generic - so unknown during typechecking
                                // get the declared method from the interface. we'll have to look up
                                // which implementation to call during codegen
                                let iface_ident = is_iface
                                    .as_iface()
                                    .expect("type constraint must be an interface");
                                let method_iface =
                                    ctx.find_iface_def(iface_ident).map_err(|err| {
                                        TypecheckError::from_name_err(err, span.clone())
                                    })?;

                                method_iface.get_method(&method).map(|m| m.decl.clone())
                            },
                        }
                    },

                    _ => ctx
                        .find_method_impl_def(iface_id, base_ty, &method)
                        .map(|def| &def.decl)
                        .cloned(),
                };

                match method_decl {
                    Some(method_decl) => {
                        OverloadAnnotation::method(iface_ty, lhs.clone(), method_decl, span.clone())
                    },

                    None => {
                        return Err(TypecheckError::from_name_err(
                            NameError::MemberNotFound {
                                base: NameContainer::Type(base_ty.clone()),
                                member: method,
                            },
                            span,
                        ))
                    },
                }
            };

            TypeAnnotation::from(method)
        },

        InstanceMember::UFCSCall { func_name, sig } => UfcsFunctionAnnotation {
            function_name: func_name,
            sig,
            span: span.clone(),
            self_arg: Box::new(lhs.clone()),
        }
        .into(),

        InstanceMember::Overloaded { candidates } => OverloadAnnotation::new(
            candidates,
            Some(Box::new(lhs.clone())),
            Vec::new(),
            span.clone(),
        )
        .into(),

        InstanceMember::Data { ty: member_ty } => {
            /* class members are always mutable because a mutable class ref is only
            a mutable *reference*. record and variant members are accessed by readonly value */
            let value_kind = match base_ty {
                Type::Class(..) => ValueKind::Mutable,
                _ => value_kind,
            };

            TypedValueAnnotation {
                ty: member_ty.clone(),
                span: span.clone(),
                value_kind,
                decl: None,
            }
            .into()
        },
    };

    Ok(annotation)
}

pub fn typecheck_variant_ctor(
    variant_name: &Symbol,
    member_ident: &Ident,
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<TypeAnnotation> {
    assert!(
        variant_name.type_args.is_none(),
        "shouldn't be possible to have explicit type args for a variant constructor expr"
    );

    // we check the named case exists in the unspecialized definition here, but
    // we don't want to try instantiating the actual variant type because we have
    // no information about its type args.
    let variant_def = ctx
        .find_variant_def(&variant_name.qualified)
        .map_err(|err| TypecheckError::from_name_err(err, span.clone()))?;

    let case_exists = variant_def
        .cases
        .iter()
        .any(|case| case.ident == *member_ident);

    if !case_exists {
        return Err(TypecheckError::from_name_err(
            NameError::MemberNotFound {
                base: NameContainer::Type(Type::Variant(Box::new(variant_name.clone()))),
                member: member_ident.clone(),
            },
            span.clone(),
        ));
    }

    let ctor_annotation = VariantCtorAnnotation {
        variant_name: variant_name.qualified.clone(),
        case: member_ident.clone(),
        span: member_ident.span().clone(),
    };

    Ok(ctor_annotation.into())
}

pub type UnaryOp = ast::UnaryOp<TypeAnnotation>;

pub fn typecheck_unary_op(
    unary_op: &ast::UnaryOp<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<UnaryOp> {
    let operand_expect_ty = match unary_op.op {
        Operator::Add | Operator::Sub | Operator::Not => expect_ty.clone(),
        Operator::AddressOf => match expect_ty {
            // value of the operator expr is the pointer to the deref type, so the operand
            // is of the deref type
            Type::Pointer(deref_ty) => (**deref_ty).clone(),
            _ => Type::Nothing,
        },

        Operator::Caret => expect_ty.clone().ptr(),
        _ => Type::Nothing,
    };

    let span = unary_op.span().clone();
    let operand = typecheck_expr(&unary_op.operand, &operand_expect_ty, ctx)?;
    let operand_ty = operand.annotation().ty();

    let annotation = match unary_op.op {
        Operator::AddressOf => {
            let ty = operand.annotation().ty();
            let value_kind = operand.annotation().value_kind();

            let kind_addressable = match operand.annotation().value_kind() {
                None
                | Some(ValueKind::Temporary | ValueKind::Immutable | ValueKind::Uninitialized) => {
                    false
                },

                Some(ValueKind::Mutable) => true,
            };

            match (kind_addressable, ty.as_ref()) {
                (false, _) | (true, Type::Nothing | Type::Nil | Type::Function(..)) => {
                    Err(TypecheckError::NotAddressable {
                        ty: ty.into_owned(),
                        value_kind,
                        span,
                    })
                },

                (
                    true,
                    Type::Class(..)
                    | Type::Interface(..)
                    | Type::DynArray { .. }
                    | Type::Array { .. }
                    | Type::MethodSelf { .. }
                    | Type::Variant(..)
                    | Type::GenericParam(..),
                ) if !ctx.allow_unsafe() => Err(TypecheckError::UnsafeAddressoOfNotAllowed {
                    ty: ty.into_owned(),
                    span,
                }),

                _ => Ok(TypedValueAnnotation {
                    ty: ty.into_owned().ptr(),
                    value_kind: ValueKind::Temporary,
                    span,
                    decl: None,
                }
                .into()),
            }?
        },

        // unary +, is this always a no-op?
        Operator::Add if operand_ty.valid_math_op(Operator::Add, &operand_ty) => {
            TypedValueAnnotation {
                ty: operand_ty.into_owned(),
                value_kind: ValueKind::Temporary,
                span,
                decl: None,
            }
            .into()
        },

        // unary negation - should this be disallowed for unsigned types?
        Operator::Sub if operand_ty.valid_math_op(Operator::Sub, &operand_ty) => {
            TypedValueAnnotation {
                ty: operand_ty.into_owned(),
                value_kind: ValueKind::Temporary,
                span,
                decl: None,
            }
            .into()
        },

        Operator::Caret => {
            let deref_ty = operand
                .annotation()
                .ty()
                .deref_ty()
                .cloned()
                .ok_or_else(|| TypecheckError::NotDerefable {
                    ty: operand.annotation().ty().into_owned(),
                    span: span.clone(),
                })?;

            let value_kind = ValueKind::Mutable;

            TypedValueAnnotation {
                ty: deref_ty,
                value_kind,
                span,
                decl: operand.annotation().decl().cloned(),
            }
            .into()
        },

        Operator::Not => {
            operand
                .annotation()
                .expect_value(&Type::Primitive(Primitive::Boolean))?;

            TypedValueAnnotation {
                ty: Type::Primitive(Primitive::Boolean),
                value_kind: ValueKind::Temporary,
                span,
                decl: operand.annotation().decl().cloned(),
            }
            .into()
        },

        Operator::BitNot => {
            let valid_ty = match operand.annotation().ty().as_ref() {
                Type::Primitive(p) => p.is_integer() && !p.is_signed(),
                _ => false,
            };

            if !valid_ty {
                return Err(TypecheckError::InvalidUnaryOp {
                    operand: operand.annotation().ty().into_owned(),
                    op: unary_op.op,
                    span,
                });
            }

            TypedValueAnnotation {
                ty: operand.annotation().ty().into_owned(),
                value_kind: ValueKind::Temporary,
                span,
                decl: None,
            }
            .into()
        },

        _ => {
            return Err(TypecheckError::InvalidUnaryOp {
                op: unary_op.op,
                operand: operand.annotation().ty().into_owned(),
                span: unary_op.annotation.clone(),
            });
        },
    };

    Ok(UnaryOp {
        operand,
        op: unary_op.op,
        annotation,
    })
}

pub fn typecheck_indexer(
    base: &ast::Expr<Span>,
    index: &ast::Expr<Span>,
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<Expr> {
    // todo: other index types
    let index_ty = Type::Primitive(Primitive::Int32);
    let index = typecheck_expr(&index, &index_ty, ctx)?;

    index.annotation().expect_value(&index_ty)?;

    let base = typecheck_expr(&base, &Type::Nothing, ctx)?;

    check_array_bound_static(&base, &index, ctx)?;

    let base_ty = base.annotation().ty();

    let (el_ty, value_kind) = match (base_ty.index_element_ty(), base.annotation().value_kind()) {
        (Some(el_ty), Some(base_value_kind)) => {
            let base_value_kind = if base.annotation().ty().is_by_ref() {
                // on heap e.g. dynamic array, always mutable
                ValueKind::Mutable
            } else {
                // inherit mutability from owning variable
                base_value_kind
            };

            (el_ty.clone(), base_value_kind)
        },

        // not indexable
        _ => {
            return Err(TypecheckError::InvalidIndexer {
                index_ty: index_ty.clone(),
                base: Box::new(base.clone()),
                span: span.clone(),
            })
        },
    };

    let annotation = TypedValueAnnotation {
        value_kind,
        ty: el_ty,
        span: span.clone(),
        decl: None,
    }
    .into();

    Ok(Expr::from(BinOp {
        lhs: base,
        rhs: index,
        op: Operator::Index,
        annotation,
    }))
}

fn check_array_bound_static(base: &Expr, index: &Expr, ctx: &mut Context) -> TypecheckResult<()> {
    fn out_of_range(dim: usize, index: IntConstant) -> bool {
        index.as_i128() < 0 || index.as_i128() >= dim as i128
    }

    match (
        base.annotation().ty().as_ref(),
        const_eval_integer(index, ctx),
    ) {
        (Type::Array(array_ty), Ok(index_const)) if out_of_range(array_ty.dim, index_const) => {
            Err(TypecheckError::IndexOutOfBounds {
                index: index_const,
                base_ty: Box::new(base.annotation().ty().into_owned()),
                span: index.span().clone(),
            })
        },

        _ => Ok(()),
    }
}
