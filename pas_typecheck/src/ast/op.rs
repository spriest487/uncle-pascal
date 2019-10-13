use crate::{annotation::VariantCtorAnnotation, ast::prelude::*};
use pas_syn::Operator;

pub type BinOp = ast::BinOp<TypeAnnotation>;

pub fn typecheck_bin_op(
    bin_op: &ast::BinOp<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Expression> {
    let span = bin_op.annotation.clone();

    match &bin_op.op {
        Operator::Member => typecheck_member_of(&bin_op.lhs, &bin_op.rhs, span, expect_ty, ctx),

        Operator::And | Operator::Or => {
            let bool_ty = Type::Primitive(Primitive::Boolean);

            let lhs = typecheck_expr(&bin_op.lhs, &bool_ty, ctx)?;
            lhs.annotation().expect_value(&bool_ty)?;

            let rhs = typecheck_expr(&bin_op.rhs, &bool_ty, ctx)?;
            rhs.annotation().expect_value(&bool_ty)?;

            let annotation = TypeAnnotation::TypedValue {
                ty: bool_ty,
                value_kind: ValueKind::Temporary,
                span,
                decl: None,
            };

            Ok(Expression::from(BinOp {
                lhs,
                rhs,
                op: bin_op.op,
                annotation: annotation.clone(),
            }))
        }

        Operator::Equals | Operator::NotEquals => {
            let lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
            let rhs = typecheck_expr(&bin_op.rhs, lhs.annotation().ty(), ctx)?;

            rhs.annotation().expect_value(lhs.annotation().ty())?;

            if !lhs.annotation().ty().self_comparable() {
                return Err(TypecheckError::InvalidBinOp {
                    lhs: lhs.annotation().ty().clone(),
                    rhs: rhs.annotation().ty().clone(),
                    op: bin_op.op,
                    span: bin_op.annotation.span().clone(),
                });
            }

            let annotation = TypeAnnotation::TypedValue {
                ty: Type::Primitive(Primitive::Boolean),
                value_kind: ValueKind::Temporary,
                span,
                decl: None,
            };

            Ok(Expression::from(BinOp {
                lhs,
                rhs,
                op: bin_op.op,
                annotation: annotation.clone(),
            }))
        }

        Operator::Gt | Operator::Gte | Operator::Lt | Operator::Lte => {
            let lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
            let rhs = typecheck_expr(&bin_op.rhs, lhs.annotation().ty(), ctx)?;

            rhs.annotation().expect_value(lhs.annotation().ty())?;

            if !lhs.annotation().ty().self_orderable() {
                return Err(TypecheckError::InvalidBinOp {
                    lhs: lhs.annotation().ty().clone(),
                    rhs: rhs.annotation().ty().clone(),
                    op: bin_op.op,
                    span: bin_op.annotation.span().clone(),
                });
            }

            let annotation = TypeAnnotation::TypedValue {
                ty: Type::Primitive(Primitive::Boolean),
                value_kind: ValueKind::Temporary,
                span,
                decl: None,
            };

            Ok(Expression::from(BinOp {
                lhs,
                rhs,
                op: bin_op.op,
                annotation: annotation.clone(),
            }))
        }

        Operator::Plus | Operator::Minus | Operator::Multiply | Operator::Divide => {
            let lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
            let rhs = typecheck_expr(&bin_op.rhs, lhs.annotation().ty(), ctx)?;

            // string concat sugar isn't available if the String class isn't loaded
            if let Ok(string_ty) = string_type(ctx) {
                let string_concat = bin_op.op == Operator::Plus
                    && *lhs.annotation().ty() == string_ty
                    && *rhs.annotation().ty() == string_ty;

                if string_concat {
                    return desugar_string_concat(lhs, rhs, &string_ty, ctx);
                }
            }

            let valid_math = lhs
                .annotation()
                .ty()
                .valid_math_op(bin_op.op, rhs.annotation().ty());

            if !valid_math {
                return Err(TypecheckError::InvalidBinOp {
                    lhs: lhs.annotation().ty().clone(),
                    rhs: rhs.annotation().ty().clone(),
                    op: bin_op.op,
                    span: bin_op.annotation.span().clone(),
                });
            }

            // check valid ops etc, result type etc
            let result_ty = lhs.annotation().ty().clone();

            let annotation = match result_ty {
                Type::Nothing => TypeAnnotation::Untyped(span.clone()),
                ty => TypeAnnotation::TypedValue {
                    ty,
                    value_kind: ValueKind::Temporary,
                    span,
                    decl: None,
                },
            };

            Ok(Expression::from(ast::BinOp {
                lhs,
                op: bin_op.op,
                rhs,
                annotation,
            }))
        }

        _ => unimplemented!(
            "typechecking for expression containing binary operator {} @ {}",
            bin_op.op,
            bin_op.span()
        ),
    }
}

fn desugar_string_concat(
    lhs: Expression,
    rhs: Expression,
    string_ty: &Type,
    ctx: &Context,
) -> TypecheckResult<Expression> {
    let span = lhs.annotation().span().to(rhs.annotation().span());
    let annotation = TypeAnnotation::TypedValue {
        ty: string_ty.clone(),
        span: span.clone(),
        value_kind: ValueKind::Temporary,
        decl: None,
    };

    // if LHS and RHS are both string literals, we can concat them ahead of time
    match (&lhs, &rhs) {
        (
            ast::Expression::Literal(ast::Literal::String(a), _),
            ast::Expression::Literal(ast::Literal::String(b), _),
        ) => Ok(ast::Expression::Literal(
            ast::Literal::String(a.clone() + b),
            annotation,
        )),

        _ => {
            let system_path = IdentPath::from(Ident::new("System", span.clone()));
            let concat_path = system_path.child(Ident::new("StringConcat", span.clone()));
            let (concat_path, concat_sig) = ctx.find_function(&concat_path)?;

            let concat_annotation = TypeAnnotation::Function {
                ns: concat_path.parent().unwrap(),
                name: concat_path.last().clone(),
                span: span.clone(),
                func_ty: Type::Function(concat_sig),
                type_args: Vec::new(),
            };

            let concat_func = ast::Expression::Ident(concat_path.last().clone(), concat_annotation);

            let concat_call = ast::Call::Function(ast::FunctionCall {
                annotation: annotation.clone(),
                args: vec![lhs, rhs],
                type_args: Vec::new(),
                target: concat_func,
                args_brackets: (span.clone(), span.clone()),
            });

            Ok(ast::Expression::from(concat_call))
        }
    }
}

fn typecheck_member_of(
    lhs: &ast::Expression<Span>,
    rhs: &ast::Expression<Span>,
    span: Span,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Expression> {
    let lhs = typecheck_expr(lhs, &Type::Nothing, ctx)?;

    match rhs {
        // x.y
        ast::Expression::Ident(member_ident, _) => {
            let member_ident = member_ident.clone();

            let annotation = match lhs.annotation() {
                TypeAnnotation::Type(Type::Variant(variant_name), ..) => {
                    assert!(
                        variant_name.type_args.is_empty(),
                        "shouldn't be possible to have explicit type args for a variant constructor expression"
                    );

                    // we check the named case exists in the unspecialized definition here, but
                    // we don't want to try instantiating the actual variant type because we have
                    // no information about its type args.
                    let variant_def = ctx.find_variant_def(&variant_name.qualified)?;

                    let case_exists = variant_def
                        .cases
                        .iter()
                        .any(|case| case.ident == member_ident);

                    if !case_exists {
                        return Err(NameError::MemberNotFound {
                            span: rhs.annotation().span().clone(),
                            base: Type::Variant(variant_name.clone()),
                            member: member_ident.clone(),
                        }.into());
                    }

                    let ctor_annotation = VariantCtorAnnotation {
                        variant_name: variant_name.qualified.clone(),
                        case: member_ident.clone(),
                        span: member_ident.span().clone(),
                    };

                    TypeAnnotation::VariantCtor(ctor_annotation)
                }

                TypeAnnotation::TypedValue {
                    value_kind: base_value_kind,
                    ty: base_ty,
                    ..
                } => {
                    let member = ctx.find_instance_member(lhs.annotation().ty(), &member_ident)?;

                    match member {
                        InstanceMember::Method { iface_ty, method } => {
                            let iface_id = match &iface_ty {
                                Type::Interface(iface_id) => iface_id,
                                _ => unimplemented!("non-interface interface types"),
                            };

                            let method_def = match ctx.find_method_def(iface_id, base_ty, &method) {
                                Some(method_def) => method_def,

                                None => return Err(NameError::MemberNotFound {
                                    span: span,
                                    base: base_ty.clone(),
                                    member: method,
                                }.into())
                            };

                            let method = MethodAnnotation::ufcs(
                                span.clone(),
                                iface_ty.clone(),
                                lhs.clone(),
                                Rc::new(FunctionSig::of_decl(&method_def.decl)),
                                method,
                            );
                            TypeAnnotation::Method(method)
                        }

                        InstanceMember::UFCSCall { func_name, sig } => {
                            TypeAnnotation::UFCSCall {
                                function: func_name,
                                func_ty: Type::Function(sig),
                                span: span.clone(),
                                self_arg: Box::new(lhs.clone()),
                            }
                        }

                        InstanceMember::Data { ty: member_ty } => {
                            /* class members are always mutable because a mutable class ref is only
                            a mutable *reference*. record members inherit their value kind from the
                            record they're part of */
                            let value_kind = match base_ty {
                                Type::Class(..) => ValueKind::Mutable,
                                _ => *base_value_kind,
                            };

                            TypeAnnotation::TypedValue {
                                ty: member_ty.clone(),
                                span: span.clone(),
                                value_kind,
                                decl: None,
                            }
                        }
                    }
                }

                TypeAnnotation::Type(ty, _) => match ctx.find_type_member(ty, &member_ident)? {
                    TypeMember::Method { decl } => {
                        let method =
                            MethodAnnotation::explicit(span.clone(), ty.clone(), decl.clone());
                        TypeAnnotation::Method(method)
                    }
                },

                TypeAnnotation::Namespace(path, _) => {
                    let mut full_path = path.clone();
                    full_path.push(member_ident.clone());

                    match ctx.resolve(&full_path) {
                        Some(member) => ns_member_ref_to_annotation(member, span, ctx),
                        None => {
                            return Err(NameError::MemberNotFound {
                                member: member_ident,
                                span,
                                base: lhs.annotation().ty().clone(),
                            }
                            .into());
                        }
                    }
                }

                _ => {
                    return Err(NameError::MemberNotFound {
                        member: member_ident,
                        span,
                        base: lhs.annotation().ty().clone(),
                    }
                    .into());
                }
            };

            let rhs = ast::Expression::Ident(member_ident, annotation.clone());

            Ok(Expression::from(BinOp {
                lhs,
                op: Operator::Member,
                rhs,
                annotation,
            }))
        }

        // a.B(x: x)
        ast::Expression::ObjectCtor(ctor) => {
            match lhs.annotation() {
                // a must be a namespace qualifier before the constructed object name
                TypeAnnotation::Namespace(ns_path, ..) => {
                    assert_eq!(
                        1,
                        ctor.ident.as_slice().len(),
                        "parsed ctor should only have a single ident in its path until this point, but found {}",
                        ctor.ident
                    );

                    let qualified_ident = ns_path.clone().child(ctor.ident.last().clone());
                    let qualified_ctor = ast::ObjectCtor {
                        ident: qualified_ident,
                        ..(**ctor).clone()
                    };

                    let span = lhs.annotation().span().to(qualified_ctor.annotation.span());

                    let ctor = typecheck_object_ctor(&qualified_ctor, span, expect_ty, ctx)?;
                    Ok(Expression::from(ctor))
                }

                _ => Err(TypecheckError::InvalidCtorType {
                    ty: lhs.annotation().ty().clone(),
                    span,
                }),
            }
        }

        _ => {
            let rhs = typecheck_expr(rhs, &Type::Nothing, ctx)?;

            Err(TypecheckError::InvalidBinOp {
                lhs: lhs.annotation().ty().clone(),
                rhs: rhs.annotation().ty().clone(),
                span,
                op: Operator::Member,
            })
        }
    }
}

pub type UnaryOp = ast::UnaryOp<TypeAnnotation>;

pub fn typecheck_unary_op(
    unary_op: &ast::UnaryOp<Span>,
    ctx: &mut Context,
) -> TypecheckResult<UnaryOp> {
    let span = unary_op.span().clone();
    let operand = typecheck_expr(&unary_op.operand, &Type::Nothing, ctx)?;

    let annotation = match unary_op.op {
        Operator::AddressOf => {
            let addr_ty = match (operand.annotation().ty(), operand.annotation().value_kind()) {
                (Type::Pointer(_), Some(ValueKind::Mutable))
                | (Type::Record(_), Some(ValueKind::Mutable))
                | (Type::Primitive(_), Some(ValueKind::Mutable)) => {
                    operand.annotation().ty().clone().ptr()
                }

                (ty, kind) => {
                    return Err(TypecheckError::NotAddressable {
                        ty: ty.clone(),
                        value_kind: kind,
                        span,
                    });
                }
            };

            TypeAnnotation::TypedValue {
                ty: addr_ty,
                value_kind: ValueKind::Temporary,
                span,
                decl: None,
            }
        }

        Operator::Deref => {
            let deref_ty = operand
                .annotation()
                .ty()
                .deref_ty()
                .cloned()
                .ok_or_else(|| TypecheckError::NotDerefable {
                    ty: operand.annotation().ty().clone(),
                    span: span.clone(),
                })?;

            let value_kind = ValueKind::Mutable;

            TypeAnnotation::TypedValue {
                ty: deref_ty,
                value_kind,
                span,
                decl: operand.annotation().decl().cloned(),
            }
        }

        _ => {
            return Err(TypecheckError::InvalidUnaryOp {
                op: unary_op.op,
                operand: operand.annotation().ty().clone(),
                span: unary_op.annotation.clone(),
            });
        }
    };

    Ok(UnaryOp {
        operand,
        op: unary_op.op,
        annotation,
    })
}

pub type Indexer = ast::Indexer<TypeAnnotation>;

pub fn typecheck_indexer(
    indexer: &ast::Indexer<Span>,
    ctx: &mut Context,
) -> TypecheckResult<Indexer> {
    // todo: other index types
    let index_ty = Type::Primitive(Primitive::Int32);
    let index = typecheck_expr(&indexer.index, &index_ty, ctx)?;
    index.annotation().expect_value(&index_ty)?;

    let base = typecheck_expr(&indexer.base, &Type::Nothing, ctx)?;

    let (el_ty, value_kind) = base
        .annotation()
        .ty()
        .collection_element_ty()
        .and_then(|el_ty| {
            let value_kind = base.annotation().value_kind()?;
            Some((el_ty.clone(), value_kind))
        })
        .ok_or_else(|| TypecheckError::InvalidIndexer {
            index_ty: index_ty.clone(),
            base: Box::new(base.clone()),
            span: indexer.span().clone(),
        })?;

    let annotation = TypeAnnotation::TypedValue {
        value_kind,
        ty: el_ty,
        span: indexer.span().clone(),
        decl: None,
    };

    Ok(Indexer {
        base,
        index,
        annotation,
    })
}
