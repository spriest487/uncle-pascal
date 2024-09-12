use crate::{Context, Primitive, Type, Typed, typecheck_type, TypecheckError, TypecheckResult, TypedValue, ValueKind};
use pas_common::span::{Span, Spanned};
use pas_syn::ast;
use crate::ast::{Expr, typecheck_expr};

pub type Cast = ast::Cast<Typed>;

enum Conversion {
    Blittable,
    UnsafeBlittable,
    Illegal,
}

pub fn implicit_conversion(
    expr: Expr,
    to: &Type,
    ctx: &Context,
) -> TypecheckResult<Expr> {
    assert_ne!(Type::Nothing, *to, "bad usage of implicit_conversion: can't convert to nothing");

    let expr_ty = expr.annotation().ty();
    if *expr_ty == *to {
        return Ok(expr);
    }

    let span = expr.span().clone();

    check_implicit_conversion(&expr_ty, to, &span, ctx)?;

    Ok(Expr::from(create_cast(expr, to.clone(), span)))
}

pub fn check_implicit_conversion(
    from: &Type,
    to: &Type,
    span: &Span,
    ctx: &Context,
) -> TypecheckResult<()> {
    if *to == *from {
        return Ok(());
    }

    let conversion = match to {
        Type::Primitive(primitive_ty) => match primitive_ty {
            Primitive::Pointer if *from == Type::Nil => Conversion::Blittable,

            Primitive::Pointer if from.is_rc_reference() => Conversion::UnsafeBlittable,
            Primitive::Pointer if from.is_pointer() => Conversion::UnsafeBlittable,

            _ => Conversion::Illegal,
        },

        Type::Pointer(..) | Type::Class(..) | Type::Interface(..) | Type::DynArray { .. }
            if *from == Type::Primitive(Primitive::Pointer) =>
        {
            Conversion::UnsafeBlittable
        }

        Type::Pointer(_) if *to == *from || *from == Type::Nil => Conversion::Blittable,

        Type::Interface(iface) => match from {
            Type::Class(..) if ctx.is_iface_impl(from, &iface) => Conversion::Blittable,
            Type::Interface(from_iface) if iface == from_iface => Conversion::Blittable,
            _ => Conversion::Illegal,
        },

        Type::Any => match from {
            Type::DynArray { .. } | Type::Class(..) | Type::Interface(..) => Conversion::Blittable,
            _ => Conversion::Illegal,
        },

        _ => Conversion::Illegal,
    };

    match conversion {
        Conversion::Blittable => Ok(()),
        Conversion::UnsafeBlittable if ctx.allow_unsafe() => Ok(()),

        Conversion::UnsafeBlittable => Err(TypecheckError::UnsafeConversionNotAllowed {
            from: from.clone(),
            to: to.clone(),
            span: span.clone(),
        }),

        Conversion::Illegal => Err(TypecheckError::TypeMismatch {
            expected: to.clone(),
            actual: from.clone(),
            span: span.clone(),
        }),
    }
}

pub fn check_explicit_cast(
    from: &Type,
    to: &Type,
    span: &Span,
    ctx: &Context,
) -> TypecheckResult<()> {
    if check_implicit_conversion(from, to, span, ctx).is_ok() {
        return Ok(());
    }

    // todo: unsafe rules
    match (from, to) {
        | (a, b) if a == b => Ok(()),

        | (Type::Primitive(..), Type::Primitive(..))
        | (Type::Pointer(..) | Type::Nil, Type::Pointer(..) | Type::Nil)
        | (Type::Pointer(..) | Type::Nil, Type::Primitive(..))
        | (Type::Primitive(..), Type::Pointer(..) | Type::Nil)
            => Ok(()),

        | (Type::Enum(..), Type::Primitive(p)) if p.is_integer() => Ok(()),

        // upcast ref type to Any
        | (Type::Class(..) | Type::Interface(..), Type::Any) => Ok(()),

        // upcast class ref to interface it implements
        | (Type::Class(..), Type::Interface(iface)) if ctx.is_iface_impl(from, iface) => Ok(()),

        | _ => Err(TypecheckError::InvalidCast {
                from: from.clone(),
                to: to.clone(),
                span: span.clone(),
            })
    }
}

pub fn typecheck_cast_expr(cast: &ast::Cast<Span>, ctx: &mut Context) -> TypecheckResult<Cast> {
    let cast_ty = typecheck_type(&cast.ty, ctx)?;
    let expr = typecheck_expr(&cast.expr, &cast_ty, ctx)?;

    check_explicit_cast(&expr.annotation().ty(), &cast_ty, &cast.annotation, ctx)?;

    Ok(create_cast(expr, cast_ty, cast.span().clone()))
}

fn create_cast(expr: Expr, cast_ty: Type, span: Span) -> Cast {
    let annotation = TypedValue {
        ty: cast_ty.clone(),
        span,
        value_kind: ValueKind::Temporary,
        decl: None,
    };

    Cast {
        annotation: annotation.into(),
        expr,
        ty: cast_ty,
    }
}
