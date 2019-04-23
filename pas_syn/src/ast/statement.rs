use crate::{
    ast::{
        expression::match_operand_start,
        Block,
        Call,
        Expression,
        ForLoop,
        Typed,
    },
    parse::prelude::*,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LocalBinding<A: Annotation> {
    pub name: Ident,
    pub val_ty: A::Type,
    pub val: Expression<A>,
    pub mutable: bool,
    pub annotation: A,
}

impl LocalBinding<Span> {
    pub fn parse(tokens: &mut TokenStream, allow_mutable: bool) -> ParseResult<Self> {
        let kw_matcher = if allow_mutable {
            Keyword::Let.or(Keyword::Var)
        } else {
            Keyword::Let.into()
        };

        let let_kw = tokens.match_one(kw_matcher)?;
        let mutable = match let_kw {
            TokenTree::Keyword {
                kw: Keyword::Var, ..
            } => true,
            _ => false,
        };

        let name_token = tokens.match_one(Matcher::AnyIdent)?;

        let val_ty = match tokens.look_ahead().match_one(Separator::Colon) {
            Some(_) => {
                tokens.advance(1);
                TypeName::parse(tokens)?
            },
            None => TypeName::Unknown(name_token.span().clone()),
        };

        tokens.match_one(Operator::Assignment)?;
        let val = Expression::parse(tokens)?;
        let span = let_kw.span().to(val.annotation());

        Ok(LocalBinding {
            name: name_token.as_ident().cloned().unwrap(),
            val_ty,
            mutable,
            val: val,
            annotation: span,
        })
    }
}

impl<A: Annotation> fmt::Display for LocalBinding<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "let {}", self.name)?;
        if self.val_ty.is_known() {
            write!(f, ": {}", self.val_ty)?;
        }
        write!(f, " := {}", self.val)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Exit<A: Annotation> {
    WithValue(Expression<A>),
    WithoutValue(A),
}

impl<A: Annotation> fmt::Display for Exit<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Exit::WithoutValue(_) => write!(f, "exit"),
            Exit::WithValue(expr) => write!(f, "exit {}", expr),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Assignment<A: Annotation> {
    pub lhs: Expression<A>,
    pub rhs: Expression<A>,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for Assignment<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} := {}", self.lhs, self.rhs)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IfStatement<A: Annotation> {
    pub cond: Expression<A>,
    pub then_branch: Box<Statement<A>>,
    pub else_branch: Option<Box<Statement<A>>>,
    pub annotation: A,
}

impl<A: Annotation> Spanned for IfStatement<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A: Annotation> fmt::Display for IfStatement<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} then {}", self.cond, self.then_branch)?;
        if let Some(else_branch) = &self.else_branch {
            write!(f, " else {}", else_branch)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Statement<A: Annotation> {
    LocalBinding(LocalBinding<A>),
    Call(Call<A>),
    Exit(Exit<A>),
    Block(Block<A>),
    ForLoop(ForLoop<A>),
    Assignment(Assignment<A>),
    If(IfStatement<A>),
}

impl<A: Annotation> Statement<A> {
    pub fn annotation(&self) -> &A {
        match self {
            Statement::LocalBinding(binding) => &binding.annotation,
            Statement::Call(call) => &call.annotation(),
            Statement::Exit(exit) => match exit {
                Exit::WithValue(expr) => expr.annotation(),
                Exit::WithoutValue(a) => a,
            },
            Statement::Block(block) => &block.annotation,
            Statement::ForLoop(for_loop) => &for_loop.annotation,
            Statement::Assignment(assignment) => &assignment.annotation,
            Statement::If(if_stmt) => &if_stmt.annotation,
        }
    }

    pub fn try_into_expr(self) -> Option<Expression<A>> {
        match self {
            Statement::Call(call) => {
                Some(Expression::from(call))
            },

            Statement::Block(block) => {
                if block.output.is_some() {
                    Some(Expression::from(block))
                } else {
                    None
                }
            },

            _ => None,
        }
    }

    pub fn try_from_expr(expr: Expression<A>) -> Result<Self, Expression<A>> {
        match expr.clone() {
            Expression::Block(mut block) => {
                block.output = match block.output {
                    Some(output_expr) => {
                        let last_stmt = match Self::try_from_expr(output_expr) {
                            Err(_) => return Err(expr),
                            Ok(stmt) => stmt,
                        };

                        block.statements.push(last_stmt);
                        None
                    },
                    None => None,
                };

                Ok(Statement::Block(*block))
            },
            Expression::Call(call) => Ok(Statement::Call(*call)),

            Expression::BinOp(bin_op) => {
                if bin_op.op == Operator::Assignment {
                    let assignment = Assignment {
                        lhs: bin_op.lhs,
                        rhs: bin_op.rhs,
                        annotation: bin_op.annotation,
                    };
                    Ok(Statement::Assignment(assignment))
                } else {
                    let invalid_bin_op = Expression::BinOp(bin_op);
                    Err(invalid_bin_op)
                }
            },

            Expression::IfCond(if_cond) => {
                let then_branch = match Self::try_from_expr(if_cond.then_branch) {
                    Ok(then_stmt) => then_stmt,
                    Err(_) => return Err(expr),
                };

                let else_branch = match if_cond.else_branch {
                    Some(else_expr) => match Self::try_from_expr(else_expr) {
                        Ok(else_stmt) => Some(else_stmt),
                        Err(_) => return Err(expr),
                    },
                    None => None,
                };

                Ok(Statement::If(IfStatement {
                    cond: if_cond.cond,
                    then_branch: Box::new(then_branch),
                    else_branch: else_branch.map(Box::new),
                    annotation: if_cond.annotation,
                }))
            },

            invalid => {
                Err(invalid)
            },
        }
    }
}

pub fn statement_start_matcher() -> Matcher {
    Matcher::Keyword(Keyword::Let)
        .or(Keyword::Var)
        .or(Keyword::For)
        .or(Keyword::Exit)
        .or(match_operand_start())
}

impl Statement<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Statement<Span>> {
        let stmt_start = statement_start_matcher();

        match tokens.look_ahead().match_one(stmt_start.clone()) {
            Some(TokenTree::Keyword {
                kw: Keyword::Let, ..
            })
            | Some(TokenTree::Keyword {
                kw: Keyword::Var, ..
            }) => {
                let binding = LocalBinding::parse(tokens, true)?;
                Ok(Statement::LocalBinding(binding))
            },

            Some(TokenTree::Keyword {
                kw: Keyword::For, ..
            }) => {
                let for_loop = ForLoop::parse(tokens)?;
                Ok(Statement::ForLoop(for_loop))
            },

            Some(_) => {
                // it doesn't start with a statement keyword, it must be an expression
                let expr = Expression::parse(tokens)?;

                let stmt = Self::try_from_expr(expr.clone()).map_err(|invalid_expr| {
                    TracedError::trace(ParseError::InvalidStatement(invalid_expr))
                })?;
                Ok(stmt)
            },

            None => Err(TracedError::trace(ParseError::UnexpectedEOF(
                stmt_start,
                tokens.context().clone(),
            ))),
        }
    }
}

impl<A: Annotation> fmt::Display for Statement<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::LocalBinding(binding) => write!(f, "{}", binding),
            Statement::Call(call) => write!(f, "{}", call),
            Statement::Exit(exit) => write!(f, "{}", exit),
            Statement::Block(block) => write!(f, "{}", block),
            Statement::ForLoop(for_loop) => write!(f, "{}", for_loop),
            Statement::Assignment(assignment) => write!(f, "{}", assignment),
            Statement::If(if_stmt) => write!(f, "{}", if_stmt),
        }
    }
}
