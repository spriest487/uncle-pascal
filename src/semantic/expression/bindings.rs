use std::{
    rc::Rc,
    iter,
};

use syntax;
use semantic::{
    Scope,
    SemanticResult,
    SemanticContext,
    SemanticError,
    Expression,
    BindingKind,
};
use types::Type;

pub fn annotate_let(name: &str,
                    value: &syntax::Expression,
                    context: SemanticContext)
                    -> SemanticResult<(Expression, Rc<Scope>)> {
    let (typed_value, mut binding_scope) = Expression::annotate(value, context.scope.clone())?;
    let bound_type = typed_value.expr_type()?
        .ok_or_else(|| {
            SemanticError::type_not_assignable(None, context.clone())
        })?;

    binding_scope = Rc::new(binding_scope.as_ref()
        .clone()
        .with_binding(
            name,
            bound_type,
            BindingKind::Immutable,
        ));

    Ok((Expression::let_binding(name, typed_value, context), binding_scope))
}

pub fn annotate_with(parsed_value: &syntax::Expression,
                     body: &syntax::Expression,
                     context: SemanticContext)
                     -> SemanticResult<(Expression, Rc<Scope>)> {
    let (value, scope) = Expression::annotate(parsed_value, context.scope.clone())?;
    let value_type: Option<Type> = value.expr_type()?;

    /* find the class or record decl of the type referred to by `value` */
    let (_record_id, record) = value_type.as_ref()
        .and_then(|ty| {
            let class_id = ty.unwrap_class()?;
            scope.get_class(class_id)
        })
        .or_else(|| {
            let record_id = value_type.as_ref()?.unwrap_record()?;
            scope.get_record(record_id)
        })
        .ok_or_else(|| {
            SemanticError::invalid_with_type(value_type, context.clone())
        })?;

    /* to keep the implementation simple, with-bindings get removed at this point -
     we transform the "value" expression in the input ast into a block expression
     with let-bindings for each member of the record */

    // turn all members of the record into let-bindings
    let bindings: Vec<_> = record.members.iter()
        .map(|record_member| {
            let member_of = parsed_value.clone();
            let member = syntax::Expression::member(member_of, &record_member.name);
            let context = syntax::ParsedContext::from(context.token.clone());

            syntax::Expression::let_binding(&record_member.name, member, context)
        })
        .collect();

    let body_block = syntax::Expression::block(syntax::Block {
        context: body.context.clone(),
        statements: bindings.into_iter()
            .chain(iter::once(body.clone()))
            .collect(),
    });

    // don't need to change the scope - block + let bindings will take care of it
    Expression::annotate(&body_block, scope.clone())
}

pub fn let_type(value: &Expression, context: &SemanticContext) -> SemanticResult<Option<Type>> {
    super::expect_initialized(value)?;

    let value_type = value.expr_type()?
        .ok_or_else(|| SemanticError::type_not_assignable(None, context.clone()))?;

    if !value_type.valid_lhs_type() {
        return Err(SemanticError::type_not_assignable(None, context.clone()));
    }

    Ok(None)
}

pub fn with_type(value: &Expression, body: &Expression) -> SemanticResult<Option<Type>> {
    let val_type = value.expr_type()?;
    val_type.as_ref()
        .and_then(|ty| {
            ty.unwrap_class()
                .or_else(|| ty.unwrap_record())
        })
        .ok_or_else(|| {
            SemanticError::invalid_with_type(val_type.clone(), value.context.clone())
        })?;

    body.expr_type()?;
    Ok(None)
}