use std::rc::Rc;

use operators;
use semantic::{
    SemanticResult,
    SemanticContext,
    SemanticError,
    Expression,
    Scope,
};
use node::{
    ExpressionValue,
    ConstExpression,
};
use types::{
    Type,
    ParameterizedName,
};
use syntax;
use super::expect_initialized;

pub fn unary_type(op: operators::Operator,
                  rhs: &Expression,
                  context: &SemanticContext)
                  -> SemanticResult<Option<Type>> {
    let rhs_type = rhs.expr_type()?;

    let invalid_op_err = ||
        Err(SemanticError::invalid_operator(op,
                                            vec![rhs_type.clone()],
                                            context.clone()));
    match op {
        operators::Deref => match &rhs_type {
            Some(Type::Pointer(pointed_to)) =>
                Ok(Some(pointed_to.as_ref().clone())),
            _ =>
                invalid_op_err(),
        },
        operators::AddressOf => match &rhs_type {
            Some(t) if t.valid_lhs_type() =>
                Ok(Some(t.clone().pointer())),
            _ => invalid_op_err()
        }

        operators::Plus |
        operators::Minus => match &rhs_type {
            Some(Type::Int64) |
            Some(Type::Byte) =>
                Ok(rhs_type.clone()),
            _ =>
                invalid_op_err(),
        }

        operators::Not => match &rhs_type {
            Some(Type::Boolean) => Ok(Some(Type::Boolean)),
            _ => invalid_op_err(),
        }

        operators::In |
        operators::RangeInclusive |
        operators::Multiply |
        operators::Divide |
        operators::And |
        operators::Or |
        operators::Assignment |
        operators::Equals |
        operators::Gt |
        operators::Gte |
        operators::Lt |
        operators::Lte |
        operators::NotEquals =>
            invalid_op_err(),
    }
}

pub fn annotate_binary(lhs: &syntax::Expression,
                       op: operators::Operator,
                       rhs: &syntax::Expression,
                       context: SemanticContext)
                       -> SemanticResult<(Expression, Rc<Scope>)> {
    let expected_operand_type = match op {
        /*
            rhs is evaluated first, but we attempt to evaluate the lhs on its own first
            to figure out the type inference for the rhs when the operation is an assignment.
            on any kind of error we just give up and don't infer a type
        */
        | operators::Assignment => {
            Expression::annotate(lhs, None, context.scope.clone())
                .map(|(lhs, _)| lhs.expr_type())
                .unwrap_or(Ok(None))
                .ok()
                .unwrap_or(None)
        }

        | _ => None,
    };

    let (rhs, lhs_scope) = Expression::annotate(rhs, expected_operand_type.as_ref(), context.scope.clone())?;
    let (lhs, scope_after) = Expression::annotate(lhs, expected_operand_type.as_ref(), lhs_scope)?;

    expect_initialized(&rhs)?;
    if op != operators::Assignment {
        expect_initialized(&lhs)?;
    }

    let scope_out = match (op, &lhs.value) {
        (operators::Assignment, ExpressionValue::Identifier(name)) =>
            Rc::new(scope_after.as_ref().clone().initialize_symbol(name)),

        _ =>
            scope_after,
    };

    Ok((Expression::binary_op(lhs, op, rhs, context), scope_out))
}

pub fn binary_type(lhs: &Expression,
                   op: operators::Operator,
                   rhs: &Expression,
                   context: &SemanticContext)
                   -> SemanticResult<Option<Type>> {
    let lhs_type = lhs.expr_type()?;

    match op {
        operators::In |
        operators::And |
        operators::Or |
        operators::NotEquals |
        operators::Gt |
        operators::Gte |
        operators::Lt |
        operators::Lte |
        operators::Equals => {
            expect_valid(op, lhs_type.as_ref(), &rhs, context)?;
            Ok(Some(Type::Boolean))
        }

        operators::RangeInclusive |
        operators::Multiply |
        operators::Divide |
        operators::Plus |
        operators::Minus => {
            expect_valid(op, lhs_type.as_ref(), &rhs, context)?;
            Ok(lhs_type)
        }

        operators::Assignment => {
            if !is_assignable(lhs) {
                Err(SemanticError::value_not_assignable(lhs.clone()))
            } else {
                expect_valid(op, lhs_type.as_ref(), &rhs, context)?;
                Ok(None)
            }
        }

        operators::Not |
        operators::AddressOf |
        operators::Deref => {
            let operand_types = vec![lhs_type, rhs.expr_type()?];
            Err(SemanticError::invalid_operator(op, operand_types, context.clone()))
        }
    }
}

pub fn is_assignable(expr: &Expression) -> bool {
    match &expr.value {
        | ExpressionValue::Identifier(name) => {
            /* we have to differentiate actual named functions
            (always immutable) from function-valued bindings */
            if expr.scope().get_function(&name).is_some() {
                return false;
            }

            match expr.scope().get_symbol(name) {
                Some(binding) => binding.mutable(expr.scope().unit_namespace()),
                None => false,
            }
        }

        | ExpressionValue::PrefixOperator { op, .. } => match op {
            operators::Deref => true,
            _ => false,
        },

        | ExpressionValue::ArrayElement { of, .. }
        | ExpressionValue::Member { of, .. }
        => is_assignable(of),

        | ExpressionValue::ObjectConstructor(_)
        | ExpressionValue::TypeCast { .. }
        | ExpressionValue::Raise(_)
        | ExpressionValue::With { .. }
        | ExpressionValue::CollectionConstructor(_)
        | ExpressionValue::FunctionCall { .. }
        | ExpressionValue::Constant(_)
        | ExpressionValue::BinaryOperator { .. }
        | ExpressionValue::Block(_)
        | ExpressionValue::ForLoop { .. }
        | ExpressionValue::If { .. }
        | ExpressionValue::While { .. }
        | ExpressionValue::LetBinding { .. }
        => false
    }
}

pub fn expect_valid(operator: operators::Operator,
                    target: Option<&Type>,
                    actual_expr: &Expression,
                    context: &SemanticContext) -> SemanticResult<()> {    
    /* special case for assigning or comparing constant 0 to any numeric type, 
    or assigning/comparing integers in the range 0...255 to bytes */
    match operator {
        operators::Equals | operators::NotEquals | operators::Assignment =>
            if let (
                Some(lhs_type),
                ExpressionValue::Constant(ConstExpression::Integer(int))
            ) = (target, &actual_expr.value) {
                if let Some(u8_val) = int.as_u8() {
                    let u8_to_byte = *lhs_type == Type::Byte;
                    let zero_to_num = u8_val == 0 && lhs_type.is_numeric();

                    if u8_to_byte || zero_to_num {
                        return Ok(());
                    }
                }
            }

        | _ => {}
    }

    let actual = actual_expr.expr_type()?;

    let is_comparison = match operator { 
        | operators::Equals
        | operators::NotEquals
        => true,

        _ => false
    };

    let string_class = Type::Class(ParameterizedName::new_simple("System.String"));

    match (target, actual) {
        | (_, None)
        | (None, _) 
        => Err(SemanticError::unexpected_type(target.cloned(), None, context.clone())),

        | (Some(a),  Some(ref b)) 
        if is_comparison =>
            if a.comparable_to(b) {
                Ok(())
            } else {
                Err(SemanticError::types_not_comparable(Some(a.clone()),
                                                        Some(b.clone()),
                                                        context.clone()))
            },

        | (Some(a), Some(b)) => {
            let valid = match operator {
                operators::Assignment => match (a, &b) {
                    | (a, _) if !a.valid_lhs_type() => {
                        return Err(SemanticError::type_not_assignable(Some(a.clone()), context.clone()))
                    }

                    // nil can be assigned to any pointer, and nothing else
                    | (Type::RawPointer, Type::Nil)
                    | (Type::Pointer(_), Type::Nil)
                    => true,

                    // class instances can be assigned to interface vars they implement
                    | (Type::AnyImplementation(interface_id), Type::Class(ref class_id)) => {
                        let scope = actual_expr.scope();
                        scope.type_implements(&Type::Class(class_id.clone()), interface_id)
                    }

                    | (ref a, ref b) if b.promotes_to(a) => true,

                    | _ => false,
                }

                operators::Plus => {
                    /* special case for string concat sugar */
                    if *a == string_class && b == string_class {
                        true
                    } else {
                        a.can_offset_by(&b)
                    }
                }
                operators::Minus =>
                    a.can_offset_by(&b),

                operators::Multiply |
                operators::Divide =>
                    a.is_numeric() && b.promotes_to(a),

                operators::Gt |
                operators::Gte |
                operators::Lt |
                operators::Lte =>
                    a.has_ord_comparisons(&b),

                operators::And |
                operators::Or =>
                    *a == Type::Boolean && b == Type::Boolean,

                _ => false,
            };

            if valid {
                Ok(())
            } else {
                let err_types = vec![Some(a.clone()), Some(b.clone())];
                Err(SemanticError::invalid_operator(operator, err_types, context.clone()))
            }
        }
    }
}
