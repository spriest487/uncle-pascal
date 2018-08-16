use std::fmt;

use operators;
use node::*;
use consts::{
    IntConstant,
    FloatConstant,
    EnumConstant,
    SetConstant,
};
use types::Type;

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionValue<TSymbol, TContext> {
    PrefixOperator {
        op: operators::Operator,
        rhs: Box<Expression<TSymbol, TContext>>,
    },
    BinaryOperator {
        lhs: Box<Expression<TSymbol, TContext>>,
        op: operators::Operator,
        rhs: Box<Expression<TSymbol, TContext>>,
    },
    FunctionCall {
        target: Box<Expression<TSymbol, TContext>>,
        args: Vec<Expression<TSymbol, TContext>>,
    },
    Constant(ConstantExpression),
    Identifier(TSymbol),
    LetBinding {
        name: String,
        value: Box<Expression<TSymbol, TContext>>,
    },
    Member {
        of: Box<Expression<TSymbol, TContext>>,
        name: String,
    },
    If {
        condition: Box<Expression<TSymbol, TContext>>,
        then_branch: Box<Expression<TSymbol, TContext>>,
        else_branch: Option<Box<Expression<TSymbol, TContext>>>,
    },
    Block(Block<TSymbol, TContext>),
    ForLoop {
        from: Box<Expression<TSymbol, TContext>>,
        to: Box<Expression<TSymbol, TContext>>,
        body: Box<Expression<TSymbol, TContext>>,
    },
}


#[derive(Clone, Debug, PartialEq)]
pub struct Expression<TSymbol, TContext> {
    pub value: ExpressionValue<TSymbol, TContext>,
    pub context: TContext,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstantExpression {
    Integer(IntConstant),
    Float(FloatConstant),
    String(String),
    Boolean(bool),
    Enum(EnumConstant),
    Set(SetConstant),
    Nil,
}

impl ConstantExpression {
    pub fn value_type(&self) -> Type {
        match self {
            ConstantExpression::String(_) => {
                Type::Class(Identifier::from("System.String"))
            }

            ConstantExpression::Integer(int_const) =>
                match int_const {
                    IntConstant::Char(_) => Type::Byte,
                    IntConstant::I32(_) => Type::Int32,
                    IntConstant::U32(_) => Type::UInt32,
                    IntConstant::I64(_) => Type::Int64,
                    IntConstant::U64(_) => Type::UInt64,
                },

            ConstantExpression::Enum(enum_const) =>
                Type::Enumeration(enum_const.enumeration.clone()),

            ConstantExpression::Float(float_const) =>
                match float_const {
                    FloatConstant::F64(_) => Type::Float64,
                }

            ConstantExpression::Boolean(_) =>
                Type::Boolean,

            ConstantExpression::Set(set_const) =>
                Type::Set(set_const.set.clone()),

            ConstantExpression::Nil =>
                Type::Nil,
        }
    }
}

impl<TSymbol, TContext> fmt::Display for Expression<TSymbol, TContext>
    where TSymbol: fmt::Debug,
          TContext: Context + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "expression: {:?} ({})", self.value, self.context.token()) //TODO better display
    }
}

#[allow(dead_code)]
impl<TSymbol, TContext> Expression<TSymbol, TContext>
    where TSymbol: fmt::Debug,
          TContext: Context + Clone + fmt::Debug
{
    pub fn prefix_op(op: operators::Operator, rhs: Self, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::PrefixOperator {
                rhs: Box::from(rhs),
                op,
            },
            context: context.into(),
        }
    }

    pub fn binary_op(lhs: Self,
                     op: operators::Operator,
                     rhs: Self,
                     context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::BinaryOperator {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
            context: context.into(),
        }
    }

    pub fn function_call(target: Self, args: impl IntoIterator<Item=Self>) -> Self {
        let context = target.context.clone();

        Expression {
            value: ExpressionValue::FunctionCall {
                target: Box::new(target),
                args: args.into_iter().collect(),
            },
            context: context.into(),
        }
    }

    pub fn let_binding(name: &str, value: Self, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::LetBinding {
                value: Box::new(value),
                name: name.to_owned(),
            },
            context: context.into(),
        }
    }

    pub fn literal_int(i: IntConstant, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstantExpression::Integer(i)),
            context: context.into(),
        }
    }

    pub fn literal_float(f: FloatConstant, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstantExpression::Float(f)),
            context: context.into(),
        }
    }

    pub fn literal_enumeration(e: EnumConstant, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstantExpression::Enum(e)),
            context: context.into(),
        }
    }

    pub fn literal_set(set_const: SetConstant, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstantExpression::Set(set_const)),
            context: context.into(),
        }
    }

    pub fn literal_string(s: &str, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstantExpression::String(s.to_owned())),
            context: context.into(),
        }
    }

    pub fn identifier(id: TSymbol, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Identifier(id),
            context: context.into(),
        }
    }

    pub fn const_value(const_val: ConstantExpression, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(const_val),
            context: context.into(),
        }
    }

    pub fn member(of: Self, name: &str) -> Self {
        let context = of.context.clone();

        Expression {
            value: ExpressionValue::Member {
                of: Box::from(of),
                name: name.to_owned(),
            },
            context: context.into(),
        }
    }

    pub fn member_deep<TNames>(of: Self, names: TNames) -> Self
        where TNames: IntoIterator<Item=String>
    {
        let mut member = of;
        for name in names {
            member = Expression::member(member, &name);
        }

        member
    }

    pub fn is_any_member(&self) -> bool {
        match &self.value {
            &ExpressionValue::Member { .. } => true,
            _ => false,
        }
    }

    pub fn unwrap_member(self) -> (Self, String) {
        match self.value {
            ExpressionValue::Member { of, name } => (*of, name),
            _ => panic!("called unwrap_member on {}", self),
        }
    }

    pub fn if_then_else(condition: Self,
                        then_branch: Self,
                        else_branch: Option<Self>,
                        context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: else_branch.map(Box::new),
            },
            context: context.into(),
        }
    }

    pub fn for_loop(from: Self, to: Self, body: Self, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::ForLoop {
                from: Box::new(from),
                to: Box::new(to),
                body: Box::new(body),
            },
            context: context.into(),
        }
    }

    pub fn block(block: Block<TSymbol, TContext>) -> Self {
        Expression {
            context: block.context.clone(),
            value: ExpressionValue::Block(block),
        }
    }

    pub fn literal_nil(context: impl Into<TContext>) -> Self {
        Expression {
            context: context.into(),
            value: ExpressionValue::Constant(ConstantExpression::Nil),
        }
    }

    pub fn literal_bool(val: bool, context: impl Into<TContext>) -> Self {
        Expression {
            context: context.into(),
            value: ExpressionValue::Constant(ConstantExpression::Boolean(val)),
        }
    }

    pub fn unwrap_function_call(self) -> (Self, Vec<Self>) {
        match self.value {
            ExpressionValue::FunctionCall { target, args } => (*target, args),
            _ => panic!("called unwrap_function_call on something other than a function call expr"),
        }
    }

    pub fn is_function_call(&self) -> bool {
        match &self.value {
            &ExpressionValue::FunctionCall { .. } => true,
            _ => false,
        }
    }

    pub fn is_if(&self) -> bool {
        match &self.value {
            &ExpressionValue::If { .. } => true,
            _ => false,
        }
    }

    pub fn is_let_binding(&self) -> bool {
        match &self.value {
            &ExpressionValue::LetBinding { .. } => true,
            _ => false
        }
    }

    pub fn unwrap_let_binding(self) -> (String, Self) {
        match self.value {
            ExpressionValue::LetBinding { name, value } => (name, *value),
            _ => panic!("called unwrap_let_binding on {}", self)
        }
    }

    pub fn unwrap_literal_string(self) -> String {
        match self.value {
            ExpressionValue::Constant(ConstantExpression::String(s)) => s,
            _ => panic!("called unwrap_literal_string on something other than a string literal expr")
        }
    }

    pub fn is_any_literal_string(&self) -> bool {
        match &self.value {
            ExpressionValue::Constant(ConstantExpression::String(_)) => true,
            _ => false,
        }
    }

    pub fn is_literal_integer(&self, val: impl Into<IntConstant>) -> bool {
        match &self.value {
            ExpressionValue::Constant(ConstantExpression::Integer(i)) => *i == val.into(),
            _ => false,
        }
    }

    pub fn is_any_literal_integer(&self) -> bool {
        match &self.value {
            &ExpressionValue::Constant(ConstantExpression::Integer(_)) => true,
            _ => false
        }
    }

    pub fn is_binary_op(&self, op: operators::Operator) -> bool {
        match &self.value {
            &ExpressionValue::BinaryOperator { op: expr_op, .. } => {
                expr_op == op
            }
            _ => false
        }
    }

    pub fn is_prefix_op(&self, op: operators::Operator) -> bool {
        match &self.value {
            &ExpressionValue::PrefixOperator { op: expr_op, .. } => {
                expr_op == op
            }
            _ => false
        }
    }

    pub fn unwrap_binary_op(self) -> (Self, operators::Operator, Self) {
        match self.value {
            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                (*lhs, op, *rhs)
            }
            _ => panic!("called unwrap_binary_op on {}", self)
        }
    }

    pub fn unwrap_prefix_op(self) -> (operators::Operator, Self) {
        match self.value {
            ExpressionValue::PrefixOperator { op, rhs } => {
                (op, *rhs)
            }
            _ => panic!("called unwrap_prefix_op on {}", self)
        }
    }

    pub fn is_block(&self) -> bool {
        match &self.value {
            &ExpressionValue::Block(_) => true,
            _ => false
        }
    }
}

#[allow(dead_code)]
impl<TSymbol, TContext> Expression<TSymbol, TContext>
    where TSymbol: PartialEq
{
    pub fn is_any_identifier(&self) -> bool {
        match &self.value {
            ExpressionValue::Identifier(_) => true,
            _ => false,
        }
    }

    pub fn is_identifier(&self, id: &TSymbol) -> bool {
        match &self.value {
            &ExpressionValue::Identifier(ref expr_id) => expr_id == id,
            _ => false,
        }
    }
}

pub fn transform_expressions<TSymbol, TContext>(
    root_expr: Expression<TSymbol, TContext>,
    replace: &mut FnMut(Expression<TSymbol, TContext>) -> Expression<TSymbol, TContext>)
    -> Expression<TSymbol, TContext>
    where TSymbol: fmt::Debug,
          TContext: Context + Clone + fmt::Debug
{
    match root_expr.value {
        ExpressionValue::BinaryOperator { lhs, op, rhs } => {
            let lhs = transform_expressions(*lhs, replace);
            let rhs = transform_expressions(*rhs, replace);

            replace(Expression::binary_op(lhs, op, rhs, root_expr.context))
        }

        ExpressionValue::Block(block) => {
            let statements = block.statements.into_iter()
                .map(|stmt| transform_expressions(stmt, replace))
                .collect();

            replace(Expression::block(Block {
                context: block.context,
                statements,
            }))
        }

        ExpressionValue::ForLoop { from, to, body } => {
            let from = transform_expressions(*from, replace);
            let to = transform_expressions(*to, replace);
            let body = transform_expressions(*body, replace);

            replace(Expression::for_loop(from, to, body, root_expr.context))
        }

        ExpressionValue::If { condition, then_branch, else_branch } => {
            let cond = transform_expressions(*condition, replace);
            let if_branch = transform_expressions(*then_branch, replace);
            let else_branch = else_branch.map(|else_expr| {
                transform_expressions(*else_expr, replace)
            });

            replace(Expression::if_then_else(cond, if_branch, else_branch, root_expr.context))
        }

        ExpressionValue::PrefixOperator { op, rhs } => {
            let rhs = transform_expressions(*rhs, replace);

            replace(Expression::prefix_op(op, rhs, root_expr.context))
        }

        ExpressionValue::LetBinding { name, value } => {
            let value = transform_expressions(*value, replace);
            replace(Expression::let_binding(&name, value, root_expr.context))
        }

        ExpressionValue::Member { of, name } => {
            let of = transform_expressions(*of, replace);
            replace(Expression::member(of, &name))
        }

        ExpressionValue::Identifier(name) => {
            replace(Expression::identifier(name, root_expr.context))
        }

        ExpressionValue::Constant(const_expr) => {
            replace(Expression::const_value(const_expr, root_expr.context))
        }

        ExpressionValue::FunctionCall { target, args } => {
            let target = transform_expressions(*target, replace);
            let args: Vec<_> = args.into_iter().map(|arg| transform_expressions(arg, replace))
                .collect();

            replace(Expression::function_call(target, args))
        }
    }
}