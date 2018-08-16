use std::rc::Rc;
use syntax;
use semantic::{
    Scope,
    SemanticResult,
    SemanticContext,
    SemanticError,
    Expression,
};
use types::Type;
use operators;
use super::{
    ops,
    expect_initialized,
};

pub fn annotate_element(of: &syntax::Expression,
                        index_expr: &syntax::Expression,
                        context: SemanticContext)
                        -> SemanticResult<(Expression, Rc<Scope>)> {
    // index expr is evaluated first
    let index_type = Some(Type::Int64);
    let (index_expr, scope) = Expression::annotate(
        index_expr, index_type.as_ref(),
        context.scope.clone(),
    )?;
    expect_initialized(&index_expr)?;

    let (of, scope) = Expression::annotate(of, None, scope)?;

    expect_initialized(&of)?;

    Ok((Expression::array_element(of, index_expr), scope))
}

pub fn element_type(of: &Expression,
                    index_expr: &Expression,
                    context: &SemanticContext)
                    -> SemanticResult<Option<Type>> {
    let check_index_type_is_usize = ops::expect_valid(
        operators::Assignment,
        Some(&Type::NativeUInt),
        index_expr,
        context);

    if check_index_type_is_usize.is_err() {
        return Err(SemanticError::invalid_array_index(index_expr.expr_type()?,
                                                      context.clone()));
    }

    match of.expr_type()? {
        Some(Type::Pointer(ptr_to)) => {
            Ok(Some(*ptr_to))
        }

        Some(Type::DynamicArray(dyn_array_type)) => {
            Ok(Some(*dyn_array_type.element))
        }

        Some(Type::Array(array_type)) => {
            match array_type.next_rank() {
                Some(next_array) => Ok(Some(Type::Array(next_array))),
                None => Ok(Some(*array_type.element))
            }
        }

        //pointers can also be dereferenced via indexing
        invalid @ _ => return Err(SemanticError::invalid_array_type(
            invalid,
            context.clone(),
        ))
    }
}