use pas_syn::Operator;
use crate::ast::{BinOp, Expression, Literal};
use crate::Context;

pub trait ConstEval {
    fn const_eval(&self, ctx: &Context) -> Option<Literal>;
}

impl ConstEval for Literal {
    fn const_eval(&self, _ctx: &Context) -> Option<Literal> {
        Some(self.clone())
    }
}

impl ConstEval for Expression {
    fn const_eval(&self, ctx: &Context) -> Option<Literal> {
        match self {
            Expression::Literal(lit, ..) => Some(lit.clone()),
            Expression::BinOp(bin_op) => bin_op.const_eval(ctx),
            _ => None,
        }
    }
}

impl ConstEval for BinOp {
    fn const_eval(&self, ctx: &Context) -> Option<Literal> {
        let lhs = self.lhs.const_eval(ctx)?;
        let rhs = self.rhs.const_eval(ctx)?;

        match self.op {
            Operator::Assignment => None,
            Operator::Equals => const_eq(lhs, rhs),
            Operator::NotEquals => const_eq(lhs, rhs).and_then(const_negate),
            Operator::Plus => const_add(lhs, rhs),
            Operator::Minus => const_sub(lhs, rhs),
            Operator::Multiply => const_mul(lhs, rhs),
            Operator::IntegerDivide => const_div(lhs, rhs),
            Operator::And => const_and(lhs, rhs),
            Operator::Or => const_or(lhs, rhs),
            Operator::Gt => const_gt(lhs, rhs),
            Operator::Gte => const_lt(lhs, rhs).and_then(const_negate),
            Operator::Lt => const_lt(lhs, rhs),
            Operator::Lte => const_gt(lhs, rhs).and_then(const_negate),
            _ => None,
        }
    }
}

fn const_negate(lit: Literal) -> Option<Literal> {
    match lit {
        Literal::Boolean(b) => Some(Literal::Boolean(!b)),
        _ => None,
    }
}

fn const_eq(a: Literal, b: Literal) -> Option<Literal> {
    match (a, b) {
        (Literal::String(a), Literal::String(b)) => Some(Literal::Boolean(a == b)),
        (Literal::Real(a), Literal::Real(b)) => Some(Literal::Boolean(a == b)),
        (Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Boolean(a == b)),
        (Literal::Nil, Literal::Nil) => Some(Literal::Boolean(true)),
        (Literal::Boolean(a), Literal::Boolean(b)) => Some(Literal::Boolean(a == b)),
        _ => None,
    }
}

fn const_add(a: Literal, b: Literal) -> Option<Literal> {
    match (a, b) {
        (Literal::String(a), Literal::String(b)) => {
            Some(Literal::String(a + &b))
        },
        (Literal::Real(a), Literal::Real(b)) => Some(Literal::Real(a + b)),
        (Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Integer(a + b)),
        _ => None,
    }
}

fn const_sub(a: Literal, b: Literal) -> Option<Literal> {
    match (a, b) {
        (Literal::Real(a), Literal::Real(b)) => Some(Literal::Real(a - b)),
        (Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Integer(a - b)),
        _ => None,
    }
}

fn const_mul(a: Literal, b: Literal) -> Option<Literal> {
    match (a, b) {
        (Literal::Real(a), Literal::Real(b)) => Some(Literal::Real(a * b)),
        (Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Integer(a * b)),
        _ => None,
    }
}

fn const_div(a: Literal, b: Literal) -> Option<Literal> {
    match (a, b) {
        (Literal::Real(a), Literal::Real(b)) => Some(Literal::Real(a / b)),
        (Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Integer(a / b)),
        _ => None,
    }
}

fn const_and(a: Literal, b: Literal) -> Option<Literal> {
    match (a, b) {
        (Literal::Boolean(a), Literal::Boolean(b)) => Some(Literal::Boolean(a && b)),
        _ => None,
    }
}

fn const_or(a: Literal, b: Literal) -> Option<Literal> {
    match (a, b) {
        (Literal::Boolean(a), Literal::Boolean(b)) => Some(Literal::Boolean(a || b)),
        _ => None,
    }
}

fn const_gt(a: Literal, b: Literal) -> Option<Literal> {
    match (a, b) {
        (Literal::Real(a), Literal::Real(b)) => Some(Literal::Boolean(a > b)),
        (Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Boolean(a > b)),
        _ => None,
    }
}

fn const_lt(a: Literal, b: Literal) -> Option<Literal> {
    match (a, b) {
        (Literal::Real(a), Literal::Real(b)) => Some(Literal::Boolean(a < b)),
        (Literal::Integer(a), Literal::Integer(b)) => Some(Literal::Boolean(a < b)),
        _ => None,
    }
}
