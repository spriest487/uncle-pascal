use std::ops::{BitAnd, BitOr, BitXor, Shl, Shr};
use std::rc::Rc;
use crate::ast::{BinOp, Expression, IfCond, Literal, UnaryOp};
use crate::Context;
use pas_syn::{IntConstant, Operator};

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
            Expression::UnaryOp(op) => op.const_eval(ctx),
            Expression::IfCond(cond) => cond.const_eval(ctx),
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
            Operator::Add => const_add(lhs, rhs),
            Operator::Subtract => const_sub(lhs, rhs),
            Operator::Multiply => const_mul(lhs, rhs),
            Operator::Divide => const_div(lhs, rhs),
            Operator::And => const_and(lhs, rhs),
            Operator::Or => const_or(lhs, rhs),
            Operator::Gt => const_gt(lhs, rhs),
            Operator::Gte => const_lt(lhs, rhs).and_then(const_negate),
            Operator::Lt => const_lt(lhs, rhs),
            Operator::Lte => const_gt(lhs, rhs).and_then(const_negate),
            Operator::Caret => const_bitwise(lhs, rhs, u64::bitxor),
            Operator::BitAnd => const_bitwise(lhs, rhs, u64::bitand),
            Operator::BitOr => const_bitwise(lhs, rhs, u64::bitor),
            Operator::Shl => const_bitwise(lhs, rhs, u64::shl),
            Operator::Shr => const_bitwise(lhs, rhs, u64::shr),
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
            let s = (*a).clone() + b.as_str();
            Some(Literal::String(Rc::new(s)))
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

fn const_bitwise<Op>(a: Literal, b: Literal, op: Op) -> Option<Literal>
where
    Op: Fn(u64, u64) -> u64
{
    match (a, b) {
        (Literal::Integer(a), Literal::Integer(b)) => {
            let val = op(a.as_u64()?, b.as_u64()?);
            Some(Literal::Integer(IntConstant::from(val)))
        }

        _ => None
    }
}

impl ConstEval for UnaryOp {
    fn const_eval(&self, ctx: &Context) -> Option<Literal> {
        match self.op {
            Operator::BitNot => {
                match self.operand.const_eval(ctx)? {
                    Literal::Integer(i) => {
                        let operand_val = i.as_u64()?;
                        Some(Literal::Integer(IntConstant::from(!operand_val)))
                    }

                    _ => None
                }
            }

            _ => None,
        }
    }
}

impl ConstEval for IfCond<Expression> {
    fn const_eval(&self, ctx: &Context) -> Option<Literal> {
        let else_branch = self.else_branch.as_ref()?;

        let cond_val = match self.cond.const_eval(ctx)? {
            Literal::Boolean(cond_val) => cond_val,
            _ => return None,
        };

        let then_val = self.then_branch.const_eval(ctx)?;
        let else_val = else_branch.const_eval(ctx)?;

        Some(if cond_val {
            then_val
        } else {
            else_val
        })
    }
}
