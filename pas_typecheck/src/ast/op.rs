use {
    crate::ast::prelude::*,
    pas_syn::Operator,
};

pub type BinOp = ast::BinOp<TypeAnnotation>;

pub fn typecheck_bin_op(
    bin_op: &ast::BinOp<Span>,
    ctx: &mut Context)
    -> TypecheckResult<BinOp>
{
    let span = bin_op.annotation.clone();

    match &bin_op.op {
        Operator::Member => {
            typecheck_member_of(&bin_op.lhs, &bin_op.rhs, span, ctx)
        }

        Operator::And | Operator::Or => {
            let bool_ty = Type::Primitive(Primitive::Boolean);

            let lhs = typecheck_expr(&bin_op.lhs, &bool_ty, ctx)?;
            lhs.annotation.expect_value(&bool_ty)?;

            let rhs = typecheck_expr(&bin_op.rhs, &bool_ty, ctx)?;
            rhs.annotation.expect_value(&bool_ty)?;

            let annotation = TypeAnnotation::TypedValue {
                ty: bool_ty,
                value_kind: ValueKind::Temporary,
                span
            };

            Ok(BinOp {
                lhs,
                rhs,
                op: bin_op.op,
                annotation,
            })
        }

        Operator::Equals | Operator::NotEquals => {
            let lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
            let rhs = typecheck_expr(&bin_op.rhs, lhs.annotation.ty(), ctx)?;

            rhs.annotation.expect_value(lhs.annotation.ty())?;

            if !lhs.annotation.ty().self_comparable() {
                return Err(TypecheckError::InvalidBinOp {
                    lhs: lhs.annotation.ty().clone(),
                    rhs: rhs.annotation.ty().clone(),
                    op: bin_op.op,
                    span: bin_op.annotation.span().clone(),
                });
            }

            let result_ty = Type::Primitive(Primitive::Boolean);
            let annotation = TypeAnnotation::TypedValue {
                ty: result_ty,
                value_kind: ValueKind::Temporary,
                span
            };

            Ok(BinOp {
                lhs,
                rhs,
                op: bin_op.op,
                annotation,
            })
        }

        Operator::Plus |
        Operator::Minus |
        Operator::Multiply |
        Operator::Divide => {
            let lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
            let rhs = typecheck_expr(&bin_op.rhs, lhs.annotation.ty(), ctx)?;

            if !lhs.annotation.ty().valid_math_op(bin_op.op, rhs.annotation.ty()) {
                return Err(TypecheckError::InvalidBinOp {
                    lhs: lhs.annotation.ty().clone(),
                    rhs: rhs.annotation.ty().clone(),
                    op: bin_op.op,
                    span: bin_op.annotation.span().clone(),
                });
            }

            // check valid ops etc, result type etc
            let result_ty = lhs.annotation.ty().clone();

            let annotation = match result_ty {
                Type::Nothing => TypeAnnotation::Untyped(span),
                ty => TypeAnnotation::TypedValue {
                    ty,
                    value_kind: ValueKind::Temporary,
                    span
                },
            };

            Ok(ast::BinOp {
                lhs,
                op: bin_op.op,
                rhs,
                annotation,
            })
        },

        _ => unimplemented!("typechecking for expression containing binary operator {}", bin_op.op)
    }
}

fn typecheck_member_of(
    lhs: &ast::ExpressionNode<Span>,
    rhs: &ast::ExpressionNode<Span>,
    span: Span,
    ctx: &mut Context
) -> TypecheckResult<BinOp> {
    let lhs = typecheck_expr(lhs, &Type::Nothing, ctx)?;

    // rhs of an ident op must be an identifier (parser checks this)
    let member_ident = rhs.expr.as_ident()
        .cloned()
        .expect("bin-op with member operator should always have an ident on the rhs");

    let annotation = match &lhs.annotation {
        TypeAnnotation::TypedValue { value_kind, .. } => {
            let member = ctx.find_instance_member(lhs.annotation.ty(), &member_ident)?;

            match member {
                InstanceMember::Method { iface_ty, decl } => {
                    let method = MethodAnnotation::ufcs(
                        span.clone(),
                        iface_ty.clone(),
                        lhs.clone(),
                        decl.clone()
                    );
                    TypeAnnotation::Method(method)
                }

                InstanceMember::Data { ty } => TypeAnnotation::TypedValue {
                    ty: ty.clone(),
                    span: span.clone(),
                    value_kind: *value_kind,
                }
            }
        }

        TypeAnnotation::Type(ty, _) => {
            match ctx.find_type_member(ty, &member_ident)? {
                TypeMember::Method { decl } => {
                    let method = MethodAnnotation::explicit(
                        span.clone(),
                        ty.clone(),
                        decl.clone()
                    );
                    TypeAnnotation::Method(method)
                }
            }
        }

        _ => {
            return Err(TypecheckError::MemberNotFound {
                member: member_ident,
                span,
                base: lhs.annotation.ty().clone(),
            });
        }
    };

    let rhs = ast::Expression::Ident(member_ident);

    Ok(BinOp {
        lhs,
        op: Operator::Member,
        rhs: ExpressionNode::new(rhs, annotation.clone()),
        annotation,
    })
}

pub type UnaryOp = ast::UnaryOp<TypeAnnotation>;

pub fn typecheck_unary_op(
    unary_op: &ast::UnaryOp<Span>,
    ctx: &mut Context)
    -> TypecheckResult<UnaryOp>
{
    let span = unary_op.span().clone();
    let operand = typecheck_expr(&unary_op.operand, &Type::Nothing, ctx)?;

    let annotation = match unary_op.op {
        Operator::AddressOf => {
            let addr_ty = match (operand.annotation.ty(), operand.annotation.value_kind()) {
                (Type::Pointer(_), Some(ValueKind::Mutable)) |
                (Type::Record(_), Some(ValueKind::Mutable)) |
                (Type::Primitive(_), Some(ValueKind::Mutable)) => operand.annotation.ty().clone().ptr(),

                (ty, kind) => return Err(TypecheckError::NotAddressable {
                    ty: ty.clone(),
                    value_kind: kind,
                    span,
                }),
            };

            TypeAnnotation::TypedValue {
                ty: addr_ty,
                value_kind: ValueKind::Temporary,
                span,
            }
        }

        Operator::Deref => {
            let deref_ty = operand.annotation.ty().deref_ty()
                .cloned()
                .ok_or_else(|| TypecheckError::NotDerefable {
                    ty: operand.annotation.ty().clone(),
                    span: span.clone(),
                })?;

            let value_kind = ValueKind::Mutable;

            TypeAnnotation::TypedValue {
                ty: deref_ty,
                value_kind,
                span
            }
        }

        _ => {
            return Err(TypecheckError::InvalidUnaryOp {
                op: unary_op.op,
                operand: operand.annotation.ty().clone(),
                span: unary_op.annotation.clone(),
            })
        }
    };

    Ok(UnaryOp {
        operand,
        op: unary_op.op,
        annotation,
    })
}