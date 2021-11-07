use crate::ast::prelude::*;

pub type SizeOf = ast::SizeOf<TypeAnnotation>;

pub fn typecheck_size_of(size_of: &ast::SizeOf<Span>, ctx: &Context) -> TypecheckResult<SizeOf> {
    let ty = typecheck_type(&size_of.ty, ctx)?;

    let annotation = TypeAnnotation::TypedValue {
        ty: Type::Primitive(Primitive::Int32),
        span: size_of.span().clone(),
        decl: None,
        value_kind: ValueKind::Temporary,
    };

    Ok(SizeOf{
        ty,
        annotation,
    })
}