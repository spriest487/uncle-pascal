mod function;
mod loops;
mod conditional;
mod arrays;
mod ops;
mod bindings;

#[cfg(test)]
pub(crate) mod test;

use std::{
    rc::Rc,
};
use syntax;
use semantic::*;
use node::{
    self,
    Identifier,
    ExpressionValue,
    ConstantExpression,
};
use operators;
use types::{
    Type,
    FunctionSignature,
};
use consts::IntConstant;

pub type Expression = node::Expression<SemanticContext>;

impl Expression {
    pub fn annotate(expr: &syntax::Expression,
                    scope: Rc<Scope>)
                    -> SemanticResult<(Self, Rc<Scope>)> {
        let expr_context = SemanticContext {
            token: expr.context.token().clone(),
            scope: scope.clone(),
        };

        match &expr.value {
            ExpressionValue::Identifier(name) => {
                let identifier = annotate_identifier(name, expr_context)?;
                Ok((identifier, scope))
            }

            ExpressionValue::Block(block) => {
                let (block, scope) = Block::annotate(block, scope)?;
                Ok((Expression::block(block), scope))
            }

            ExpressionValue::LetBinding { name, value } => {
                bindings::annotate_let(name, value, expr_context)
            }

            ExpressionValue::Constant(ConstantExpression::String(s)) => {
                Ok((Expression::literal_string(s, expr_context), scope))
            }

            ExpressionValue::Constant(ConstantExpression::Integer(i)) => {
                Ok((Expression::literal_int(*i, expr_context), scope))
            }

            ExpressionValue::Constant(ConstantExpression::Nil) => {
                Ok((Expression::literal_nil(expr_context), scope))
            }

            ExpressionValue::Constant(ConstantExpression::Boolean(b)) => {
                Ok((Expression::literal_bool(*b, expr_context), scope))
            }

            ExpressionValue::Constant(ConstantExpression::Float(f)) => {
                Ok((Expression::literal_float(*f, expr_context), scope))
            }

            ExpressionValue::Constant(ConstantExpression::Enum(e)) => {
                Ok((Expression::literal_enumeration(e.clone(), expr_context), scope))
            }

            ExpressionValue::Constant(ConstantExpression::Set(set_const)) => {
                Ok((Expression::literal_set(set_const.clone(), expr_context), scope))
            }

            ExpressionValue::If { condition, then_branch, else_branch } =>
                conditional::annotate(
                    condition.as_ref(),
                    then_branch.as_ref(),
                    else_branch.as_ref().map(|else_expr| else_expr.as_ref()),
                    expr_context,
                ),

            ExpressionValue::While { condition, body } =>
                loops::annotate_while(condition, body, expr_context),

            ExpressionValue::ForLoop { from, to, body } =>
                loops::annotate_for(from, to, body, expr_context),

            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                ops::annotate_binary(lhs, *op, rhs, expr_context)
            }

            ExpressionValue::PrefixOperator { op, rhs } => {
                let (rhs, scope) = Expression::annotate(rhs, scope)?;
                let op_expr = Expression::prefix_op(op.clone(), rhs, expr_context);
                Ok((op_expr, scope))
            }

            ExpressionValue::FunctionCall { target, args } =>
                function::annotate_call(target, args, expr_context),

            ExpressionValue::TypeCast { target_type, from_value } =>
                annotate_type_cast(target_type, from_value, expr_context),

            ExpressionValue::Member { of, name } => {
                let (typed_of, scope) = Expression::annotate(of, scope)?;
                Ok((Expression::member(typed_of, name), scope))
            }

            ExpressionValue::ArrayElement { of, index_expr } => {
                arrays::annotate_element(of, index_expr, expr_context)
            }

            ExpressionValue::SetConstructor(_members) => {
                unimplemented!("set constructor semantic analysis")
            }

            ExpressionValue::With { value, body } => {
                bindings::annotate_with(value, body, expr_context)
            }

            ExpressionValue::Raise(error) => {
                // the raise expr is isolated
                let (error, _) = Expression::annotate(error.as_ref(), scope.clone())?;
                Ok((Expression::raise(error, expr_context), scope))
            }
        }
    }

    pub fn expr_type(&self) -> Result<Option<Type>, SemanticError> {
        match &self.value {
            ExpressionValue::BinaryOperator { lhs, op, rhs } =>
                ops::binary_type(lhs, *op, rhs, &self.context),

            ExpressionValue::PrefixOperator { op, rhs } =>
                ops::unary_type(*op, rhs, &self.context),

            ExpressionValue::Constant(const_val) =>
                Ok(Some(const_val.value_type())),

            ExpressionValue::LetBinding { value, .. } =>
                bindings::let_type(value, &self.context),

            ExpressionValue::FunctionCall { target, args } =>
                function::call_type(target, args, &self.context),

            ExpressionValue::TypeCast { target_type, from_value } =>
                type_cast_type(target_type, from_value, &self.context),

            ExpressionValue::Identifier(id) =>
                identifier_type(id, &self.context),

            ExpressionValue::Block(block) => {
                for statement in block.statements.iter() {
                    statement.expr_type()?;
                }

                Ok(None)
            }

            ExpressionValue::With { value, body } => {
                bindings::with_type(value, body)
            }

            ExpressionValue::If { condition, then_branch, else_branch } =>
                conditional::expr_type(condition.as_ref(),
                                       then_branch.as_ref(),
                                       else_branch.as_ref()
                                           .map(|else_expr| else_expr.as_ref()),
                                       &self.context)
                    .map(|_| None),

            ExpressionValue::ForLoop { from, to, body } =>
                loops::annotate_if(from.as_ref(), to.as_ref(), body.as_ref(), &self.context)
                    .map(|_| None),

            ExpressionValue::While { condition, body } =>
                loops::while_type(condition, body)
                    .map(|_| None),

            ExpressionValue::Member { of, name } => {
                member_type(of, name)
            }

            ExpressionValue::ArrayElement { of, index_expr } => {
                arrays::element_type(of, index_expr, &self.context)
            }

            ExpressionValue::SetConstructor(_members) => {
                unimplemented!("set constructor typechecking")
            }

            ExpressionValue::Raise(error) => {
                error.expr_type()?;
                Ok(None)
            }
        }
    }

    pub fn type_check(&self) -> Result<(), SemanticError> {
        self.expr_type()?;
        Ok(())
    }

    pub fn class_type(&self) -> SemanticResult<Option<&RecordDecl>> {
        match self.expr_type()? {
            Some(Type::Class(name)) => {
                let (_class_id, class_decl) = self.context.scope.get_class(&name)
                    .expect("record must exist in scope of expression it's used in");

                Ok(Some(class_decl))
            }
            _ => Ok(None),
        }
    }

    pub fn function_type(&self) -> SemanticResult<Option<FunctionSignature>> {
        match self.expr_type()? {
            Some(Type::Function(sig)) => Ok(Some(sig.as_ref().clone())),
            _ => Ok(None),
        }
    }

    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }

    pub fn into_const_expr(self) -> SemanticResult<Self> {
        let const_value = self.to_const_value()?;

        Ok(Expression {
            context: self.context,
            value: ExpressionValue::Constant(const_value),
        })
    }

    pub fn to_const_value(&self) -> SemanticResult<ConstantExpression> {
        match &self.value {
            ExpressionValue::Constant(val) => {
                Ok(val.clone())
            }

            ExpressionValue::PrefixOperator { op, rhs } => {
                let rhs_val = rhs.to_const_value()?;
                match (*op, rhs_val) {
                    (operators::Minus, ConstantExpression::Integer(rhs_int)) => {
                        Ok(ConstantExpression::Integer(IntConstant::from(0) - rhs_int))
                    }

                    (_, rhs_val) => {
                        let operands = vec![Some(rhs_val.value_type())];
                        Err(SemanticError::invalid_operator(*op, operands, self.context.clone()))
                    }
                }
            }

            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                let lhs_val: ConstantExpression = lhs.to_const_value()?;
                let rhs_val: ConstantExpression = rhs.to_const_value()?;

                match (lhs_val, *op, rhs_val) {
                    /* const string concatenation */
                    (
                        ConstantExpression::String(lhs_str),
                        operators::Plus,
                        ConstantExpression::String(rhs_str)
                    ) => {
                        Ok(ConstantExpression::String(lhs_str + &rhs_str))
                    }

                    /* const int + */
                    (
                        ConstantExpression::Integer(lhs_int),
                        operators::Plus,
                        ConstantExpression::Integer(rhs_int)
                    ) => {
                        Ok(ConstantExpression::Integer(lhs_int + rhs_int))
                    }

                    /* const int - */
                    (
                        ConstantExpression::Integer(lhs_int),
                        operators::Minus,
                        ConstantExpression::Integer(rhs_int)
                    ) => {
                        Ok(ConstantExpression::Integer(lhs_int - rhs_int))
                    }

                    (lhs_val, _, rhs_val) => {
                        let operands = vec![
                            Some(lhs_val.value_type()),
                            Some(rhs_val.value_type()),
                        ];
                        Err(SemanticError::invalid_operator(*op, operands, self.context.clone()))
                    }
                }
            }

            ExpressionValue::Identifier(name) => {
                self.scope().get_const(name)
                    .map(|(_const_id, const_val)| const_val.clone())
                    .ok_or_else(|| {
                        SemanticError::invalid_const_value(self.clone())
                    })
            }

            _ => {
                Err(SemanticError::invalid_const_value(self.clone()))
            }
        }
    }
}

fn annotate_identifier(name: &Identifier,
                       context: SemanticContext)
                       -> SemanticResult<Expression> {
    context.scope.get_symbol(name)
        .and_then(|symbol| match symbol {
            /* transform identifiers that reference record members into
            record member expressions */
            ScopedSymbol::RecordMember { record_id, name, .. } => {
                let base = Expression::identifier(record_id, context.clone());

                Some(Expression::member(base, &name))
            }

            ScopedSymbol::Local { name, .. } => {
                Some(Expression::identifier(name.clone(), context.clone()))
            }
        })
        .or_else(|| {
            let (_, const_val) = context.scope.get_const(name)?;

            Some(Expression::const_value(const_val.clone(), context.clone()))
        })
        .ok_or_else(|| {
            SemanticError::unknown_symbol(name.clone(), context)
        })
}

fn identifier_type(name: &Identifier,
                   context: &SemanticContext)
                   -> SemanticResult<Option<Type>> {
    context.scope.get_symbol(name)
        .map(|sym| Some(sym.decl_type().clone()))
        .ok_or_else(|| {
            SemanticError::unknown_symbol(name.clone(), context.clone())
        })
}

fn match_indirection(expr: &Expression,
                     expr_type: &Type,
                     target_type: &Type) -> Expression {
    let target_level = target_type.indirection_level();

    let mut current_level = expr_type.indirection_level();
    let mut result = expr.clone();

    while current_level < target_level {
        result = Expression::prefix_op(operators::AddressOf, result, expr.context.clone());
        current_level += 1;
    }

    while current_level > target_level {
        result = Expression::prefix_op(operators::Deref, result, expr.context.clone());
        current_level -= 1;
    }

    result
}

fn annotate_type_cast(target_type: &node::TypeName,
                      from_value: &syntax::Expression,
                      context: SemanticContext)
                      -> SemanticResult<(Expression, Rc<Scope>)> {
    let target_type = target_type.resolve(context.scope.clone())?;
    let (from_value, scope) = Expression::annotate(from_value, context.scope.clone())?;

    let context = SemanticContext {
        token: context.token().clone(),
        scope: scope.clone(),
    };

    let type_cast = Expression::type_cast(target_type, from_value, context);
    Ok((type_cast, scope))
}

fn type_cast_type(target_type: &Type,
                  from_value: &Expression,
                  context: &SemanticContext) -> SemanticResult<Option<Type>> {
    let from_expr_type = from_value.expr_type()?
        .ok_or_else(|| {
            // expr has no type, can't cast it
            SemanticError::invalid_typecast(target_type.clone(), None, context.clone())
        })?;

    match (from_expr_type, target_type) {
        (Type::UInt32, Type::Int32) => Ok(Some(Type::Int32)),
        (Type::Int32, Type::UInt32) => Ok(Some(Type::UInt32)),

        (Type::UInt64, Type::Int64) => Ok(Some(Type::Int64)),
        (Type::Int64, Type::UInt64) => Ok(Some(Type::UInt64)),

        (ref from, to) if from.promotes_to(to) => Ok(Some(to.clone())),

        // unsupported type of cast
        (from, to) => {
            Err(SemanticError::invalid_typecast(to.clone(), Some(from), context.clone()))
        }
    }
}

fn member_type(of: &Expression, name: &str) -> SemanticResult<Option<Type>> {
    let base_type = of.expr_type()?
        .map(|dt| dt.remove_indirection().clone());

    /* treat records and classes the same for this purpose, except that
     class members are always private i.e. inaccessible outside the unit that
     the class is declared in */
    let (base_decl, private_members) = match &base_type {
        Some(Type::Record(name)) => (of.scope().get_record(name), false),
        Some(Type::Class(name)) => (of.scope().get_class(name), true),
        _ => (None, false),
    };

    match base_decl {
        Some((record_id, record)) => {
            if private_members {
                let from_ns = of.scope().unit_namespace();
                if from_ns != record_id.parent().as_ref() {
                    return Err(SemanticError::private_member_access_forbidden(
                        record_id.clone(),
                        of.scope().unit_namespace().cloned(),
                        name,
                        of.context.clone(),
                    ));
                }
            }

            match record.get_member(name) {
                Some(member) =>
                    Ok(Some(member.decl_type.clone())),
                None => {
                    let name_id = Identifier::from(name);
                    Err(SemanticError::unknown_symbol(name_id, of.context.clone()))
                }
            }
        }

        _ => {
            Err(SemanticError::member_of_non_record(base_type.clone(),
                                                    name.to_string(),
                                                    of.context.clone()))
        }
    }
}

fn expect_initialized(expr: &Expression) -> SemanticResult<()> {
    node::try_visit_expressions(expr, &mut |each_expr| {
        match &each_expr.value {
            ExpressionValue::Identifier(name) => {
                match expr.scope().get_symbol(name) {
                    | Some(ScopedSymbol::RecordMember { binding_kind, .. })
                    | Some(ScopedSymbol::Local { binding_kind, .. }) => {
                        match binding_kind {
                            BindingKind::Uninitialized => {
                                let context = expr.context.clone();
                                Err(SemanticError::uninitialized_symbol(name.clone(), context))
                            }

                            _ =>
                                Ok(())
                        }
                    }

                    _ => Err(SemanticError::unknown_symbol(name.clone(), expr.context.clone()))
                }
            }

            _ => Ok(())
        }
    })
}