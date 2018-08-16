use std::fmt;

use operators;
use source;
use node::*;

#[derive(Clone, Debug)]
pub enum ExpressionValue<TSymbol> {
    PrefixOperator {
        op: operators::Operator,
        rhs: Box<Expression<TSymbol>>,
    },
    BinaryOperator {
        lhs: Box<Expression<TSymbol>>,
        op: operators::Operator,
        rhs: Box<Expression<TSymbol>>,
    },
    FunctionCall {
        target: Box<Expression<TSymbol>>,
        args: Vec<Expression<TSymbol>>,
    },
    LiteralInteger(i64),
    LiteralString(String),
    LiteralNil,
    Identifier(TSymbol),
    LetBinding {
        name: String,
        value: Box<Expression<TSymbol>>,
    },
    Member {
        of: Box<Expression<TSymbol>>,
        name: String,
    },
    If {
        condition: Box<Expression<TSymbol>>,
        then_branch: Box<Expression<TSymbol>>,
        else_branch: Option<Box<Expression<TSymbol>>>,
    },
    Block(Block<TSymbol>),
    ForLoop {
        from: Box<Expression<TSymbol>>,
        to: Box<Expression<TSymbol>>,
        body: Box<Expression<TSymbol>>,
    },
}


#[derive(Clone, Debug)]
pub struct Expression<TSymbol> {
    pub value: ExpressionValue<TSymbol>,
    pub context: source::Token,
}

impl<TSymbol> fmt::Display for Expression<TSymbol>
    where TSymbol: fmt::Debug
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "expression: {:?} ({})", self.value, self.context) //TODO better display
    }
}

#[allow(dead_code)]
impl<TSymbol> Expression<TSymbol>
    where TSymbol: fmt::Debug
{
    pub fn prefix_op(op: operators::Operator, rhs: Self, context: source::Token) -> Self {
        Expression {
            value: ExpressionValue::PrefixOperator {
                rhs: Box::from(rhs),
                op,
            },
            context,
        }
    }

    pub fn binary_op(lhs: Self,
                     op: operators::Operator,
                     rhs: Self,
                     context: source::Token) -> Self {
        Expression {
            value: ExpressionValue::BinaryOperator {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
            context,
        }
    }

    pub fn function_call(target: Self, args: impl IntoIterator<Item=Self>) -> Self {
        let context = target.context.clone();

        Expression {
            value: ExpressionValue::FunctionCall {
                target: Box::new(target),
                args: args.into_iter().collect(),
            },
            context,
        }
    }

    pub fn let_binding(let_kw: source::Token, name: &str, value: Self) -> Self {
        Expression {
            value: ExpressionValue::LetBinding {
                value: Box::new(value),
                name: name.to_owned(),
            },
            context: let_kw,
        }
    }

    pub fn literal_int(i: i64, context: source::Token) -> Self {
        Expression {
            value: ExpressionValue::LiteralInteger(i),
            context,
        }
    }

    pub fn literal_string(s: &str, context: source::Token) -> Self {
        Expression {
            value: ExpressionValue::LiteralString(s.to_owned()),
            context,
        }
    }

    pub fn identifier(id: TSymbol, context: source::Token) -> Self {
        Expression {
            value: ExpressionValue::Identifier(id),
            context,
        }
    }

    pub fn member(of: Self, name: &str) -> Self {
        let context = of.context.clone();

        Expression {
            value: ExpressionValue::Member {
                of: Box::from(of),
                name: name.to_owned(),
            },
            context,
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
                        context: source::Token) -> Self {
        Expression {
            value: ExpressionValue::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: else_branch.map(Box::new),
            },
            context,
        }
    }

    pub fn for_loop(from: Self, to: Self, body: Self, context: source::Token) -> Self {
        Expression {
            value: ExpressionValue::ForLoop {
                from: Box::new(from),
                to: Box::new(to),
                body: Box::new(body),
            },
            context,
        }
    }

    pub fn block(block: Block<TSymbol>) -> Self {
        Expression {
            context: block.context.clone(),
            value: ExpressionValue::Block(block),
        }
    }

    pub fn literal_nil(context: &source::Token) -> Self {
        Expression {
            context: context.clone(),
            value: ExpressionValue::LiteralNil,
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
            ExpressionValue::LiteralString(s) => s,
            _ => panic!("called unwrap_literal_string on something other than a string literal expr")
        }
    }

    pub fn is_any_literal_string(&self) -> bool {
        match &self.value {
            &ExpressionValue::LiteralString(_) => true,
            _ => false,
        }
    }

    pub fn is_literal_integer(&self, val: i64) -> bool {
        match &self.value {
            &ExpressionValue::LiteralInteger(i) => i == val,
            _ => false,
        }
    }

    pub fn is_any_literal_integer(&self) -> bool {
        match &self.value {
            &ExpressionValue::LiteralInteger(_) => true,
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
impl<TSymbol> Expression<TSymbol>
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

pub fn transform_expressions<TSymbol>(root_expr: Expression<TSymbol>,
                         replace: &mut FnMut(Expression<TSymbol>) -> Expression<TSymbol>) -> Expression<TSymbol>
    where TSymbol: fmt::Debug
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
            replace(Expression::let_binding(root_expr.context, &name, value))
        }

        ExpressionValue::Member { of, name } => {
            let of = transform_expressions(*of, replace);
            replace(Expression::member(of, &name))
        }

        ExpressionValue::Identifier(name) => {
            replace(Expression::identifier(name, root_expr.context))
        }

        ExpressionValue::LiteralInteger(i) => {
            replace(Expression::literal_int(i, root_expr.context))
        }

        ExpressionValue::LiteralNil => {
            replace(root_expr)
        }

        ExpressionValue::LiteralString(s) => {
            replace(Expression::literal_string(&s, root_expr.context))
        }

        ExpressionValue::FunctionCall { target, args } => {
            let target = transform_expressions(*target, replace);
            let args: Vec<_> = args.into_iter().map(|arg| transform_expressions(arg, replace))
                .collect();

            replace(Expression::function_call(target, args))
        }
    }
}