mod function;
mod loops;
mod conditional;
mod arrays;
mod ops;
mod bindings;
mod constructors;

#[cfg(test)]
pub(crate) mod test;

pub use self::ops::expect_valid as expect_valid_op;

use std::{
    rc::Rc,
};
use syntax;
use semantic::*;
use node::{
    self,
    Context,
    Identifier,
    ExpressionValue,
    ConstExpression,
    FunctionCall,
};
use operators;
use types::Type;
use consts::IntConstant;

pub type Expression = node::Expression<SemanticContext>;

impl Expression {
    pub fn annotate(expr: &syntax::Expression,
                    expected_type: Option<&Type>,
                    scope: Rc<Scope>)
                    -> SemanticResult<(Self, Rc<Scope>)> {
        let expr_context = SemanticContext {
            token: expr.context.token().clone(),
            scope: scope.clone(),
            type_hint: expected_type.cloned(),
        };

        match &expr.value {
            ExpressionValue::Identifier(name) => {
                let identifier = annotate_identifier(name, expr_context)?;
                Ok((identifier, scope))
            }

            ExpressionValue::Block(block) => {
                let (block, scope) = Block::annotate(block, &scope)?;
                Ok((Expression::block(block), scope))
            }

            ExpressionValue::LetBinding(binding) => {
                bindings::annotate_let(binding, expr_context)
            }

            ExpressionValue::Constant(node::ConstExpression::String(s)) => {
                Ok((Expression::literal_string(s.as_str(), expr_context), scope))
            }

            ExpressionValue::Constant(node::ConstExpression::Integer(i)) => {
                Ok((Expression::literal_int(*i, expr_context), scope))
            }

            ExpressionValue::Constant(node::ConstExpression::Nil) => {
                Ok((Expression::literal_nil(expr_context), scope))
            }

            ExpressionValue::Constant(node::ConstExpression::Boolean(b)) => {
                Ok((Expression::literal_bool(*b, expr_context), scope))
            }

            ExpressionValue::Constant(node::ConstExpression::Float(f)) => {
                Ok((Expression::literal_float(*f, expr_context), scope))
            }

            ExpressionValue::Constant(node::ConstExpression::Enum(e)) => {
                Ok((Expression::literal_enumeration(e.clone(), expr_context), scope))
            }

            ExpressionValue::Constant(node::ConstExpression::Set(set_const)) => {
                Ok((Expression::literal_set(set_const.clone(), expr_context), scope))
            }

            ExpressionValue::If { condition, then_branch, else_branch } =>
                conditional::annotate(
                    condition.as_ref(),
                    then_branch.as_ref(),
                    else_branch.as_ref().map(|else_expr| else_expr.as_ref()),
                    &expr_context,
                ),

            ExpressionValue::While { condition, body } =>
                loops::annotate_while(condition, body, expr_context),

            ExpressionValue::ForLoop { from, to, body } =>
                loops::annotate_for(from, to, body, &expr_context),

            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                ops::annotate_binary(lhs, *op, rhs, expr_context)
            }

            ExpressionValue::PrefixOperator { op, rhs } => {
                let expected_type = match op {
                    operators::Deref => expected_type.cloned().map(|ty| ty.pointer()),
                    _ => None,
                };

                let (rhs, scope) = Expression::annotate(rhs, expected_type.as_ref(), scope)?;
                let op_expr = Expression::prefix_op(*op, rhs, expr_context);
                Ok((op_expr, scope))
            }

            ExpressionValue::FunctionCall(call) => {
                function::annotate_call(call, &expr_context)
            }

            ExpressionValue::TypeCast { target_type, from_value } =>
                annotate_type_cast(target_type, from_value, &expr_context),

            ExpressionValue::Member { of, name } => {
                let (typed_of, scope) = Expression::annotate(of, expected_type, scope)?;
                expect_initialized(&typed_of)?;

                Ok((Expression::member(typed_of, name), scope))
            }

            ExpressionValue::ArrayElement { of, index_expr } => {
                arrays::annotate_element(of, index_expr, &expr_context)
            }

            ExpressionValue::SetConstructor(_members) => {
                unimplemented!("set constructor typechecking");
            }

            ExpressionValue::ObjectConstructor(obj) => {
                let (obj, scope) = constructors::annotate_object(obj, expected_type, &expr_context)?;
                Ok((Expression::object_constructor(obj, expr_context), scope))
            }

            ExpressionValue::With { value, body } => {
                bindings::annotate_with(value, body, &expr_context)
            }

            ExpressionValue::Raise(error) => {
                // the raise expr is isolated
                let (error, _) = Expression::annotate(error.as_ref(), None, scope.clone())?;
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
                Ok(Some(const_val.value_type(self.context.type_hint()))),

            ExpressionValue::LetBinding(_) =>
                Ok(None),

            ExpressionValue::FunctionCall(call) =>
                function::call_type(call, &self.context),

            ExpressionValue::TypeCast { target_type, from_value } =>
                type_cast_type(target_type, from_value, &self.context),

            ExpressionValue::Identifier(id) =>
                identifier_type(id, &self.context),

            ExpressionValue::Block(block) => {
                for statement in &block.statements {
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
                loops::for_type(from.as_ref(), to.as_ref(), body.as_ref(), &self.context)
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

            ExpressionValue::ObjectConstructor(obj) => {
                for member in &obj.members {
                    member.value.expr_type()?;
                }
                Ok(obj.object_type.clone())
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
            Some(Type::Class(type_name)) => {
                let (_class_id, class_decl) = self.context.scope.get_class(&type_name.name)
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

    pub fn into_const_expr(self, as_type: Option<&Type>) -> SemanticResult<Self> {
        let const_value = self.to_const_value()?;
        let const_expr = Expression {
            context: self.context.clone(),
            value: ExpressionValue::Constant(const_value),
        };

        expect_valid_op(
            operators::Assignment,
            as_type,
            &const_expr,
            &self.context,
        )?;

        Ok(const_expr)
    }

    pub fn to_const_value(&self) -> SemanticResult<ConstExpression> {
        match &self.value {
            ExpressionValue::Constant(val) =>
                Ok(val.clone()),

            ExpressionValue::PrefixOperator { op, rhs } => {
                let rhs_val = rhs.to_const_value()?;
                match (*op, rhs_val) {
                    (operators::Minus, node::ConstExpression::Integer(rhs_int)) => {
                        let negated = node::ConstExpression::Integer(IntConstant::from(0) - rhs_int);

                        Expression::const_value(negated, self.context.clone())
                            .to_const_value()
                    }

                    (_, rhs_val) => {
                        let operands = vec![Some(rhs_val.value_type(self.context.type_hint()))];
                        Err(SemanticError::invalid_operator(*op, operands, self.context.clone()))
                    }
                }
            }

            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                use operators::{Plus, Minus};

                let lhs_val = lhs.to_const_value()?;
                let rhs_val = rhs.to_const_value()?;

                match (lhs_val, *op, rhs_val) {
                    /* const string concatenation */
                    (ConstExpression::String(lhs_str), Plus, ConstExpression::String(rhs_str)) =>
                        Ok(ConstExpression::String(lhs_str + &rhs_str)),

                    /* const int +/- */
                    (ConstExpression::Integer(lhs_int), Plus, ConstExpression::Integer(rhs_int)) =>
                        Ok(ConstExpression::Integer(lhs_int + rhs_int)),
                    (ConstExpression::Integer(lhs_int), Minus, ConstExpression::Integer(rhs_int)) =>
                        Ok(ConstExpression::Integer(lhs_int - rhs_int)),

                    /* float +/- */
                    (ConstExpression::Float(lhs_flt), Plus, ConstExpression::Float(rhs_flt)) =>
                        Ok(ConstExpression::Float(lhs_flt + rhs_flt)),
                    (ConstExpression::Float(lhs_flt), Minus, ConstExpression::Float(rhs_flt)) =>
                        Ok(ConstExpression::Float(lhs_flt - rhs_flt)),

                    /* anything else is an error */
                    (lhs_val, _, rhs_val) => {
                        let operands = vec![
                            Some(lhs_val.value_type(lhs.context.type_hint())),
                            Some(rhs_val.value_type(rhs.context.type_hint())),
                        ];

                        Err(SemanticError::invalid_operator(
                            *op,
                            operands,
                            self.context.clone(),
                        ))
                    }
                }
            }

            ExpressionValue::Identifier(name) => {
                self.scope().get_const(name)
                    .map(|(_const_id, const_val, _const_type)| const_val.clone())
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
    if let Some(symbol) = context.scope.get_symbol(name) {
        match symbol {
            /* transform identifiers that reference record members into
            record member expressions */
            ScopedSymbol::RecordMember { record_id, name, .. } => {
                /* do the transform using syntax ast nodes so we don't have
                to duplicate any semantic checks the regular typechecking func
                for ::Member does */
                let base = syntax::Expression::identifier(record_id, context.token().clone());
                let member = syntax::Expression::member(base, &name);

                /* it should be safe to ignore the result scope here? a simple
                identifier can't initialize anything or introduce a new name */
                let (member, _) = Expression::annotate(&member, None, context.scope.clone())?;

                return Ok(member);
            }

            ScopedSymbol::Local { name, .. } => {
                return Ok(Expression::identifier(name.clone(), context.clone()));
            }
        }
    }

    if let Some((_, const_val, _)) = context.scope.get_const(name) {
        return Ok(Expression::const_value(const_val.clone(), context.clone()));
    }

    Err(SemanticError::unknown_symbol(name.clone(), context))
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
                      context: &SemanticContext)
                      -> SemanticResult<(Expression, Rc<Scope>)> {
    let target_type = target_type.resolve(context.scope.clone())?;
    let (from_value, scope) = Expression::annotate(
        from_value,
        Some(&target_type),
        context.scope.clone(),
    )?;
    expect_initialized(&from_value)?;

    let context = SemanticContext {
        token: context.token().clone(),
        scope: scope.clone(),

        // cast expressions ignore contextual type hints for obvious reasons
        type_hint: None,
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
        | (ref from, to)
        if from.promotes_to(to) || (from.is_numeric() && to.is_numeric())
        => Ok(Some(to.clone())),

        // unsupported type of cast
        | (from, to) => {
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
        Some(Type::Record(name)) => (of.scope().get_record_specialized(name), false),
        Some(Type::Class(name)) => (of.scope().get_class_specialized(name), true),
        _ => (None, false),
    };

    match base_decl {
        Some((record_id, record)) => {
            if private_members {
                let from_ns = of.scope().unit_namespace();
                if from_ns != record_id.name.parent().as_ref() {
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
    /*
        if the expression isn't an identifier, the only way it can fail this check
        is if it references an uninitialized identifier somewhere inside itself, in
        which case it's up to the typechecking code for that node type to call this
        function and check that
    */
    if let ExpressionValue::Identifier(name) = &expr.value {
        match expr.scope().get_symbol(name) {
            | Some(ScopedSymbol::RecordMember { binding_kind: BindingKind::Uninitialized, .. })
            | Some(ScopedSymbol::Local { binding_kind: BindingKind::Uninitialized, .. })
            => {
                let context = expr.context.clone();
                Err(SemanticError::uninitialized_symbol(name.clone(), context))
            }

            | Some(_) => Ok(()),

            | None => Err(SemanticError::unknown_symbol(name.clone(), expr.context.clone())),
        }
    } else {
        Ok(())
    }
}

impl FunctionCall<SemanticContext> {
    pub fn signature(&self) -> FunctionSignature {
        match self {
            FunctionCall::Function { target, .. } => {
                target.function_type()
                    .expect("called function must have a valid type")
                    .expect("target must be a function")
            }
            FunctionCall::Method { interface_id, func_name, for_type, args } => {
                /* method calls always have a self-arg in position 0 */
                let scope = args[0].context.scope();

                // todo: getting the sig for an interface call is duplicated in call_type
                match for_type {
                    Type::AnyImplementation(_) => {
                        let (_, interface) = scope.get_interface(&interface_id).unwrap();
                        interface.decl.methods.get(func_name).cloned().unwrap()
                    }

                    _ => {
                        let (_, func) = scope.get_interface_impl(&for_type, &interface_id, &func_name)
                            .expect("interface call target must exist");

                        func.signature()
                    }
                }
            }
        }
    }
}