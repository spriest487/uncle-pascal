use crate::typ::ast::Expr;
use crate::typ::string_type;
use crate::typ::typecheck_type;
use crate::typ::Context;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::ValueKind;
use common::span::Span;
use crate::ast;
use crate::IntConstant;

pub type Literal = ast::Literal<Type>;

pub fn typecheck_literal(
    lit: &ast::Literal<ast::TypeName>,
    expect_ty: &Type,
    span: &Span,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    match lit {
        ast::Literal::String(s) => {
            let binding = ValueKind::Immutable;
            let annotation = TypedValue {
                ty: string_type(ctx)?,
                value_kind: binding,
                span: span.clone(),
                decl: None,
            }
            .into();

            Ok(Expr::Literal(Literal::String(s.clone()), annotation))
        },

        ast::Literal::Boolean(b) => {
            let annotation = TypedValue {
                ty: Type::Primitive(Primitive::Boolean),
                value_kind: ValueKind::Immutable,
                span: span.clone(),
                decl: None,
            }
            .into();

            Ok(Expr::Literal(Literal::Boolean(*b), annotation))
        },

        ast::Literal::Integer(i) => typecheck_literal_int(i, expect_ty, span.clone()),

        ast::Literal::Real(x) => {
            let ty = if x.as_f32().is_some() {
                Type::from(Primitive::Real32)
            } else {
                unimplemented!("real literal outside range of f32")
            };

            let annotation = TypedValue {
                ty,
                value_kind: ValueKind::Immutable,
                span: span.clone(),
                decl: None,
            }
            .into();

            Ok(ast::Expr::Literal(
                ast::Literal::Real(x.clone()),
                annotation,
            ))
        },

        ast::Literal::Nil => {
            let ty = match expect_ty {
                ptr @ Type::Pointer(..) => ptr.clone(),
                _ => Type::Nil,
            };

            let annotation = TypedValue {
                ty,
                value_kind: ValueKind::Temporary,
                span: span.clone(),
                decl: None,
            };

            Ok(ast::Expr::Literal(Literal::Nil, annotation.into()))
        },

        ast::Literal::SizeOf(size_of_ty) => {
            let ty = typecheck_type(&size_of_ty, ctx)?;
            let annotation = TypedValue {
                ty: Type::Primitive(Primitive::Int32),
                span: span.clone(),
                decl: None,
                value_kind: ValueKind::Temporary,
            };

            Ok(Expr::Literal(
                Literal::SizeOf(Box::new(ty)),
                annotation.into(),
            ))
        },
    }
}

fn typecheck_literal_int(i: &IntConstant, expect_ty: &Type, span: Span) -> TypeResult<Expr> {
    let ty = match expect_ty {
        Type::Primitive(Primitive::UInt8) => {
            try_map_primitive_int(i, Primitive::UInt8, IntConstant::as_u8)
        },
        Type::Primitive(Primitive::Int8) => {
            try_map_primitive_int(i, Primitive::Int8, IntConstant::as_i8)
        },
        Type::Primitive(Primitive::Int16) => {
            try_map_primitive_int(i, Primitive::Int16, IntConstant::as_i16)
        },
        Type::Primitive(Primitive::UInt16) => {
            try_map_primitive_int(i, Primitive::UInt16, IntConstant::as_u16)
        },
        Type::Primitive(Primitive::Int32) => {
            try_map_primitive_int(i, Primitive::Int32, IntConstant::as_i32)
        },
        Type::Primitive(Primitive::UInt32) => {
            try_map_primitive_int(i, Primitive::UInt32, IntConstant::as_u32)
        },
        Type::Primitive(Primitive::Int64) => {
            try_map_primitive_int(i, Primitive::Int64, IntConstant::as_i64)
        },
        Type::Primitive(Primitive::UInt64) => {
            try_map_primitive_int(i, Primitive::UInt64, IntConstant::as_u64)
        },
        Type::Primitive(Primitive::NativeInt) => {
            try_map_primitive_int(i, Primitive::NativeInt, IntConstant::as_isize)
        },
        Type::Primitive(Primitive::NativeUInt) => {
            try_map_primitive_int(i, Primitive::NativeUInt, IntConstant::as_usize)
        },

        Type::Primitive(Primitive::Real32) => {
            try_map_primitive_int(i, Primitive::Real32, IntConstant::as_f32)
        },

        _ => match i.as_i32() {
            Some(_) => Type::from(Primitive::Int32),
            None => unimplemented!("integer literals outside range of i32"),
        },
    };

    let annotation = TypedValue {
        ty,
        value_kind: ValueKind::Immutable,
        span,
        decl: None,
    }
    .into();

    Ok(ast::Expr::Literal(ast::Literal::Integer(*i), annotation))
}

fn try_map_primitive_int<F, T>(i: &IntConstant, primitive_ty: Primitive, f: F) -> Type
where
    F: Fn(&IntConstant) -> Option<T>,
{
    match f(&i) {
        Some(..) => Type::Primitive(primitive_ty),
        None => Type::Primitive(Primitive::Int32),
    }
}
