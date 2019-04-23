use crate::ast::prelude::*;
use pas_syn::Operator;

pub type BinOp = ast::BinOp<TypeAnnotation>;

pub fn typecheck_bin_op(
    bin_op: &ast::BinOp<Span>,
    ctx: &mut Context,
) -> TypecheckResult<ExpressionNode> {
    let span = bin_op.annotation.clone();

    match &bin_op.op {
        Operator::Member => typecheck_member_of(&bin_op.lhs, &bin_op.rhs, span, ctx),

        Operator::And | Operator::Or => {
            let bool_ty = Type::Primitive(Primitive::Boolean);

            let lhs = typecheck_expr(&bin_op.lhs, &bool_ty, ctx)?;
            lhs.annotation.expect_value(&bool_ty)?;

            let rhs = typecheck_expr(&bin_op.rhs, &bool_ty, ctx)?;
            rhs.annotation.expect_value(&bool_ty)?;

            let annotation = TypeAnnotation::TypedValue {
                ty: bool_ty,
                value_kind: ValueKind::Temporary,
                span,
            };

            Ok(ExpressionNode::new(
                BinOp {
                    lhs,
                    rhs,
                    op: bin_op.op,
                    annotation: annotation.clone(),
                },
                annotation,
            ))
        }

        Operator::Equals | Operator::NotEquals => {
            let lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
            let rhs = typecheck_expr(&bin_op.rhs, lhs.annotation.value_ty(), ctx)?;

            rhs.annotation.expect_value(lhs.annotation.value_ty())?;

            if !lhs.annotation.value_ty().self_comparable() {
                return Err(TypecheckError::InvalidBinOp {
                    lhs: lhs.annotation.value_ty().clone(),
                    rhs: rhs.annotation.value_ty().clone(),
                    op: bin_op.op,
                    span: bin_op.annotation.span().clone(),
                });
            }

            let annotation = TypeAnnotation::TypedValue {
                ty: Type::Primitive(Primitive::Boolean),
                value_kind: ValueKind::Temporary,
                span,
            };

            Ok(ExpressionNode::new(
                BinOp {
                    lhs,
                    rhs,
                    op: bin_op.op,
                    annotation: annotation.clone(),
                },
                annotation,
            ))
        }

        Operator::Gt | Operator::Gte | Operator::Lt | Operator::Lte => {
            let lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
            let rhs = typecheck_expr(&bin_op.rhs, lhs.annotation.value_ty(), ctx)?;

            rhs.annotation.expect_value(lhs.annotation.value_ty())?;

            if !lhs.annotation.value_ty().self_orderable() {
                return Err(TypecheckError::InvalidBinOp {
                    lhs: lhs.annotation.value_ty().clone(),
                    rhs: rhs.annotation.value_ty().clone(),
                    op: bin_op.op,
                    span: bin_op.annotation.span().clone(),
                });
            }

            let annotation = TypeAnnotation::TypedValue {
                ty: Type::Primitive(Primitive::Boolean),
                value_kind: ValueKind::Temporary,
                span,
            };

            Ok(ExpressionNode::new(
                BinOp {
                    lhs,
                    rhs,
                    op: bin_op.op,
                    annotation: annotation.clone(),
                },
                annotation,
            ))
        }

        Operator::Plus | Operator::Minus | Operator::Multiply | Operator::Divide => {
            let lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
            let rhs = typecheck_expr(&bin_op.rhs, lhs.annotation.value_ty(), ctx)?;

            let string_ty = ctx.string_type()?;
            let string_concat = bin_op.op == Operator::Plus
                && *lhs.annotation.value_ty() == string_ty
                && *rhs.annotation.value_ty() == string_ty;

            if string_concat {
                return desugar_string_concat(lhs, rhs, &string_ty, ctx);
            }

            let valid_math = lhs
                .annotation
                .value_ty()
                .valid_math_op(bin_op.op, rhs.annotation.value_ty());

            if !valid_math {
                return Err(TypecheckError::InvalidBinOp {
                    lhs: lhs.annotation.value_ty().clone(),
                    rhs: rhs.annotation.value_ty().clone(),
                    op: bin_op.op,
                    span: bin_op.annotation.span().clone(),
                });
            }

            // check valid ops etc, result type etc
            let result_ty = lhs.annotation.value_ty().clone();

            let annotation = match result_ty {
                Type::Nothing => TypeAnnotation::Untyped(span.clone()),
                ty => TypeAnnotation::TypedValue {
                    ty,
                    value_kind: ValueKind::Temporary,
                    span,
                },
            };

            Ok(ExpressionNode::new(
                ast::BinOp {
                    lhs,
                    op: bin_op.op,
                    rhs,
                    annotation: annotation.clone(),
                },
                annotation,
            ))
        }

        _ => unimplemented!(
            "typechecking for expression containing binary operator {}",
            bin_op.op
        ),
    }
}

fn desugar_string_concat(
    lhs: ExpressionNode,
    rhs: ExpressionNode,
    string_ty: &Type,
    ctx: &Context,
) -> TypecheckResult<ExpressionNode> {
    let span = lhs.annotation.span().to(rhs.annotation.span());
    let annotation = TypeAnnotation::TypedValue {
        ty: string_ty.clone(),
        span: span.clone(),
        value_kind: ValueKind::Temporary,
    };

    // if LHS and RHS are both string literals, we can concat them ahead of time
    let result = match (lhs.expr.as_ref(), rhs.expr.as_ref()) {
        (
            ast::Expression::Literal(ast::Literal::String(a)),
            ast::Expression::Literal(ast::Literal::String(b)),
        ) => {
            ast::Expression::Literal(ast::Literal::String(a.clone() + b))
        }

        _ => {
            let system_path = IdentPath::from(Ident::new("System", span.clone()));
            let concat_path = system_path.child(Ident::new("StringConcat", span.clone()));
            let (concat_path, concat_sig) = ctx.find_function(&concat_path)?;

            let concat_func = ast::Expression::Ident(concat_path.last().clone());

            let concat_call = ast::Call::Function(ast::FunctionCall {
                annotation: annotation.clone(),
                args: vec![lhs, rhs],
                target: ast::ExpressionNode::new(concat_func, TypeAnnotation::Function {
                    ns: concat_path.parent().unwrap(),
                    name: concat_path.last().clone(),
                    span: span.clone(),
                    ty: Type::Function(concat_sig)
                }),
            });

            ast::Expression::Call(concat_call)
        }
    };

    Ok(ExpressionNode::new(result, annotation))
}

fn typecheck_member_of(
    lhs: &ast::ExpressionNode<Span>,
    rhs: &ast::ExpressionNode<Span>,
    span: Span,
    ctx: &mut Context,
) -> TypecheckResult<ExpressionNode> {
    let lhs = typecheck_expr(lhs, &Type::Nothing, ctx)?;

    match rhs.expr.as_ref() {
        // x.y
        ast::Expression::Ident(member_ident) => {
            let member_ident = member_ident.clone();

            let annotation = match &lhs.annotation {
                TypeAnnotation::TypedValue {
                    value_kind: base_value_kind,
                    ty: base_ty,
                    ..
                } => {
                    let member =
                        ctx.find_instance_member(lhs.annotation.value_ty(), &member_ident)?;

                    match member {
                        InstanceMember::Method { iface_ty, decl } => {
                            let method = MethodAnnotation::ufcs(
                                span.clone(),
                                iface_ty.clone(),
                                lhs.clone(),
                                decl.clone(),
                            );
                            TypeAnnotation::Method(method)
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
                        Some(member) => ns_member_ref_to_annotation(member, span),
                        None => {
                            return Err(NameError::MemberNotFound {
                                member: member_ident,
                                span,
                                base: lhs.annotation.value_ty().clone(),
                            }
                            .into());
                        }
                    }
                }

                _ => {
                    return Err(NameError::MemberNotFound {
                        member: member_ident,
                        span,
                        base: lhs.annotation.value_ty().clone(),
                    }
                    .into());
                }
            };

            let rhs = ast::Expression::Ident(member_ident);

            Ok(ExpressionNode::new(
                BinOp {
                    lhs,
                    op: Operator::Member,
                    rhs: ExpressionNode::new(rhs, annotation.clone()),
                    annotation: annotation.clone(),
                },
                annotation,
            ))
        }

        // a.B(x: x)
        ast::Expression::ObjectCtor(ctor) => {
            match &lhs.annotation {
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
                        ..ctor.clone()
                    };

                    let ctor = typecheck_object_ctor(&qualified_ctor, ctx)?;
                    let annotation = ctor.annotation.clone();

                    Ok(ExpressionNode::new(ctor, annotation))
                }

                _ => Err(TypecheckError::InvalidCtorType {
                    ty: lhs.annotation.value_ty().clone(),
                    span,
                }),
            }
        }

        _ => {
            let rhs = typecheck_expr(rhs, &Type::Nothing, ctx)?;

            Err(TypecheckError::InvalidBinOp {
                lhs: lhs.annotation.value_ty().clone(),
                rhs: rhs.annotation.value_ty().clone(),
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
            let addr_ty = match (
                operand.annotation.value_ty(),
                operand.annotation.value_kind(),
            ) {
                (Type::Pointer(_), Some(ValueKind::Mutable))
                | (Type::Record(_), Some(ValueKind::Mutable))
                | (Type::Primitive(_), Some(ValueKind::Mutable)) => {
                    operand.annotation.value_ty().clone().ptr()
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
            }
        }

        Operator::Deref => {
            let deref_ty = operand
                .annotation
                .value_ty()
                .deref_ty()
                .cloned()
                .ok_or_else(|| TypecheckError::NotDerefable {
                    ty: operand.annotation.value_ty().clone(),
                    span: span.clone(),
                })?;

            let value_kind = ValueKind::Mutable;

            TypeAnnotation::TypedValue {
                ty: deref_ty,
                value_kind,
                span,
            }
        }

        _ => {
            return Err(TypecheckError::InvalidUnaryOp {
                op: unary_op.op,
                operand: operand.annotation.value_ty().clone(),
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
