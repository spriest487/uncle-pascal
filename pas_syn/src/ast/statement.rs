mod assign;
mod exit;
mod local_binding;

use std::fmt;
use pas_common::span::{Span, Spanned};
use pas_common::TracedError;
pub use self::{
    assign::{Assignment, CompoundAssignment},
    exit::Exit,
    local_binding::LocalBinding,
};
use crate::{ast::{
    case::{CaseBlock, CaseStmt},
    Block, Call, Expr, ForLoop, IfCond, Raise, WhileLoop, MatchStmt,
}, DelimiterPair, Ident, Keyword, Operator, Separator};
use crate::ast::{Annotation};
use crate::parse::{InvalidStatement, LookAheadTokenStream, Matcher, Parse, ParseError, ParseResult, ParseSeq, TokenStream};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Stmt<A: Annotation> {
    Ident(Ident, A),
    LocalBinding(Box<LocalBinding<A>>),
    Call(Box<Call<A>>),
    Exit(Box<Exit<A>>),
    Block(Box<Block<A>>),
    ForLoop(Box<ForLoop<A>>),
    WhileLoop(Box<WhileLoop<A>>),
    Assignment(Box<Assignment<A>>),
    CompoundAssignment(Box<CompoundAssignment<A>>),
    If(Box<IfCond<A, Stmt<A>>>),
    Break(A),
    Continue(A),
    Raise(Box<Raise<A>>),
    Case(Box<CaseStmt<A>>),
    Match(Box<MatchStmt<A>>),
}

impl<A: Annotation> Stmt<A> {
    pub fn annotation(&self) -> &A {
        match self {
            Stmt::Ident(_ident, annotation) => annotation,
            Stmt::LocalBinding(binding) => &binding.annotation,
            Stmt::Call(call) => &call.annotation(),
            Stmt::Exit(exit) => match exit.as_ref() {
                Exit::WithValue(_expr, annotation) => annotation,
                Exit::WithoutValue(a) => a,
            },
            Stmt::Block(block) => &block.annotation,
            Stmt::ForLoop(for_loop) => &for_loop.annotation,
            Stmt::WhileLoop(while_loop) => &while_loop.annotation,
            Stmt::Assignment(assignment) => &assignment.annotation,
            Stmt::CompoundAssignment(assignment) => &assignment.annotation,
            Stmt::If(if_stmt) => &if_stmt.annotation,
            Stmt::Break(a) => a,
            Stmt::Continue(a) => a,
            Stmt::Raise(raise) => &raise.annotation,
            Stmt::Case(case) => &case.annotation,
            Stmt::Match(match_stmt) => &match_stmt.annotation,
        }
    }

    pub fn as_block(&self) -> Option<&Block<A>> {
        match self {
            Stmt::Block(block) => Some(block),
            _ => None,
        }
    }
}

impl Stmt<Span> {
    pub fn to_expr(&self) -> Option<Expr<Span>> {
        match self {
            Stmt::Ident(ident, span) => Some(Expr::Ident(ident.clone(), span.clone())),

            Stmt::Call(call) => Some(Expr::Call(call.clone())),

            Stmt::Block(block) => {
                let block_expr = block.to_expr()?;
                Some(Expr::from(block_expr))
            },

            Stmt::If(if_cond) => {
                let if_cond_expr = if_cond.to_expr()?;
                Some(Expr::from(if_cond_expr))
            },

            Stmt::Raise(raise) => Some(Expr::Raise(raise.clone())),
            Stmt::Exit(exit) => Some(Expr::Exit(exit.clone())),

            // case statements that have an else branch might also be valid as expressions if every
            // branch is also valid as an expr
            Stmt::Case(case) => {
                let case = case.to_expr()?;
                Some(Expr::from(case))
            },

            Stmt::Match(match_stmt) => {
                let match_expr = match_stmt.to_expr()?;
                Some(Expr::from(match_expr))
            }

            _ => None,
        }
    }

    pub fn try_from_expr(expr: Expr<Span>) -> Result<Self, Expr<Span>> {
        match expr.clone() {
            Expr::Block(mut block) => {
                block.output = match block.output {
                    Some(output_expr) => {
                        let last_stmt = match Self::try_from_expr(output_expr) {
                            Err(_) => return Err(expr),
                            Ok(stmt) => stmt,
                        };

                        block.stmts.push(last_stmt);
                        None
                    },
                    None => None,
                };

                Ok(Stmt::Block(block))
            },
            Expr::Call(call) => Ok(Stmt::Call(call)),

            Expr::BinOp(bin_op) => match bin_op.op {
                Operator::Assignment => {
                    let assignment = Assignment {
                        lhs: bin_op.lhs,
                        rhs: bin_op.rhs,
                        annotation: bin_op.annotation,
                    };
                    Ok(assignment.into())
                },

                Operator::CompoundAssignment(assign_op) => {
                    let assignment = CompoundAssignment {
                        lhs: bin_op.lhs,
                        rhs: bin_op.rhs,
                        annotation: bin_op.annotation,
                        op: assign_op,
                    };
                    Ok(assignment.into())
                },

                _ => {
                    let invalid_bin_op = Expr::BinOp(bin_op);
                    Err(invalid_bin_op)
                },
            },

            Expr::IfCond(if_cond) => {
                let then_branch = Self::try_from_expr(if_cond.then_branch)?;
                let else_branch = match if_cond.else_branch {
                    Some(else_expr) => Some(Self::try_from_expr(else_expr)?),
                    None => None,
                };

                Ok(Stmt::If(Box::new(IfCond {
                    cond: if_cond.cond,
                    is_pattern: if_cond.is_pattern,
                    then_branch,
                    else_branch,
                    annotation: if_cond.annotation,
                })))
            },

            // a single ident that appears in a statement context can only be a call to
            // a function with zero args or type args
            Expr::Ident(ident, span) => {
                Ok(Stmt::Ident(ident, span))
            }

            // raise and exit are always valid in either context
            Expr::Raise(raise) => Ok(Stmt::Raise(raise)),
            Expr::Exit(exit) => Ok(Stmt::Exit(exit)),

            invalid => Err(invalid),
        }
    }
}

pub fn stmt_start_matcher() -> Matcher {
    Matcher::Keyword(Keyword::Var)
        .or(Keyword::For)
        .or(Keyword::While)
        .or(Keyword::Break)
        .or(Keyword::Continue)
        .or(Keyword::Exit)
        .or(Matcher::ExprOperandStart)
}

impl<A: Annotation> Spanned for Stmt<A> {
    fn span(&self) -> &Span {
        self.annotation().span()
    }
}

impl Parse for Stmt<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Stmt<Span>> {
        let stmt_start = stmt_start_matcher();

        match tokens.look_ahead().match_one(stmt_start.clone()) {
            Some(tt) if tt.is_keyword(Keyword::Var) => {
                let binding = LocalBinding::parse(tokens)?;
                Ok(Stmt::LocalBinding(Box::new(binding)))
            },

            Some(tt) if tt.is_keyword(Keyword::For) => {
                let for_loop = ForLoop::parse(tokens)?;
                Ok(Stmt::ForLoop(Box::new(for_loop)))
            },

            Some(tt) if tt.is_keyword(Keyword::While) => {
                let while_loop = WhileLoop::parse(tokens)?;
                Ok(Stmt::WhileLoop(Box::new(while_loop)))
            },

            Some(tt) if tt.is_keyword(Keyword::Break) => {
                tokens.advance(1);
                Ok(Stmt::Break(tt.into_span()))
            },

            Some(tt) if tt.is_keyword(Keyword::Continue) => {
                tokens.advance(1);
                Ok(Stmt::Continue(tt.into_span()))
            },

            Some(tt) if tt.is_keyword(Keyword::Exit) => {
                let exit = Exit::parse(tokens)?;

                Ok(Stmt::Exit(Box::new(exit)))
            },

            Some(tt) if tt.is_delimited(DelimiterPair::BeginEnd) => {
                let block = Block::parse(tokens)?;

                Ok(Stmt::Block(Box::new(block)))
            },

            Some(tt) if tt.is_delimited(DelimiterPair::CaseEnd) => {
                let case = CaseBlock::parse(tokens)?;

                Ok(Stmt::Case(Box::new(case)))
            },

            Some(tt) if tt.is_delimited(DelimiterPair::MatchEnd) => {
                let case = MatchStmt::parse(tokens)?;

                Ok(Stmt::Match(Box::new(case)))
            },

            Some(tt) if tt.is_keyword(Keyword::If) => {
                let if_cond = IfCond::parse(tokens)?;

                Ok(Stmt::If(Box::new(if_cond)))
            }

            Some(_tt) => {
                // it doesn't start with a stmt keyword, it must be an expr
                let expr = Expr::parse(tokens)?;

                let stmt = Self::try_from_expr(expr.clone()).map_err(|invalid_expr| {
                    let invalid_stmt = InvalidStatement::from(invalid_expr);
                    let err = ParseError::InvalidStatement(invalid_stmt);
                    TracedError::trace(err)
                })?;
                Ok(stmt)
            },

            None => Err(TracedError::trace(match tokens.look_ahead().next() {
                Some(unexpected) => {
                    ParseError::UnexpectedToken(Box::new(unexpected), Some(stmt_start))
                },
                None => ParseError::UnexpectedEOF(stmt_start, tokens.context().clone()),
            })),
        }
    }
}

impl ParseSeq for Stmt<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        Stmt::parse(tokens)
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_one(stmt_start_matcher()).is_some()
    }
}

impl<A: Annotation> fmt::Display for Stmt<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Ident(ident, ..) => write!(f, "{}", ident),
            Stmt::LocalBinding(binding) => write!(f, "{}", binding),
            Stmt::Call(call) => write!(f, "{}", call),
            Stmt::Exit(exit) => write!(f, "{}", exit),
            Stmt::Block(block) => write!(f, "{}", block),
            Stmt::ForLoop(for_loop) => write!(f, "{}", for_loop),
            Stmt::WhileLoop(while_loop) => write!(f, "{}", while_loop),
            Stmt::Assignment(assignment) => write!(f, "{}", assignment),
            Stmt::CompoundAssignment(assignment) => write!(f, "{}", assignment),
            Stmt::If(if_stmt) => write!(f, "{}", if_stmt),
            Stmt::Break(..) => write!(f, "{}", Keyword::Break),
            Stmt::Continue(..) => write!(f, "{}", Keyword::Continue),
            Stmt::Raise(raise) => write!(f, "{}", raise),
            Stmt::Case(case) => write!(f, "{}", case),
            Stmt::Match(match_stmt) => write!(f, "{}", match_stmt),
        }
    }
}

impl<A: Annotation> From<LocalBinding<A>> for Stmt<A> {
    fn from(local_binding: LocalBinding<A>) -> Self {
        Stmt::LocalBinding(Box::new(local_binding))
    }
}

impl<A: Annotation> From<Assignment<A>> for Stmt<A> {
    fn from(assignment: Assignment<A>) -> Self {
        Stmt::Assignment(Box::new(assignment))
    }
}

impl<A: Annotation> From<CompoundAssignment<A>> for Stmt<A> {
    fn from(assignment: CompoundAssignment<A>) -> Self {
        Stmt::CompoundAssignment(Box::new(assignment))
    }
}
