use crate::{
    ast::{
        expression::match_operand_start, Block, Call, Expression, ForLoop, IfCond, Typed, WhileLoop,
    },
    parse::prelude::*,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LocalBinding<A: Annotation> {
    pub name: Ident,
    pub val_ty: A::Type,
    pub val: Option<Expression<A>>,
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

        let val_ty = match tokens.match_one_maybe(Separator::Colon) {
            Some(_) => TypeName::parse(tokens)?,
            None => TypeName::Unknown(name_token.span().clone()),
        };

        let (val, span) = match tokens.match_one_maybe(Operator::Assignment) {
            Some(_) => {
                let val = Expression::parse(tokens)?;
                let span = let_kw.span().to(val.annotation());
                (Some(val), span)
            }
            None => (None, let_kw.span().to(val_ty.span())),
        };

        Ok(LocalBinding {
            name: name_token.as_ident().cloned().unwrap(),
            val_ty,
            mutable,
            val,
            annotation: span,
        })
    }
}

impl<A: Annotation> fmt::Display for LocalBinding<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let kw = if self.mutable { "var" } else { "let" };

        write!(f, "{} {}", kw, self.name)?;
        if self.val_ty.is_known() {
            write!(f, ": {}", self.val_ty)?;
        }
        if let Some(val) = &self.val {
            write!(f, " := {}", val)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Exit<A: Annotation> {
    WithValue(Expression<A>, A),
    WithoutValue(A),
}

impl<A: Annotation> fmt::Display for Exit<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Exit::WithoutValue(_) => write!(f, "exit"),
            Exit::WithValue(value, _) => write!(f, "exit {}", value),
        }
    }
}

impl<A: Annotation> Spanned for Exit<A> {
    fn span(&self) -> &Span {
        match self {
            Exit::WithValue(_, a) => a.span(),
            Exit::WithoutValue(a) => a.span()
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
pub enum Statement<A: Annotation> {
    LocalBinding(LocalBinding<A>),
    Call(Call<A>),
    Exit(Exit<A>),
    Block(Block<A>),
    ForLoop(ForLoop<A>),
    WhileLoop(WhileLoop<A>),
    Assignment(Assignment<A>),
    If(Box<IfCond<A>>),
    Break(A),
    Continue(A),
}

impl<A: Annotation> Statement<A> {
    pub fn annotation(&self) -> &A {
        match self {
            Statement::LocalBinding(binding) => &binding.annotation,
            Statement::Call(call) => &call.annotation(),
            Statement::Exit(exit) => match exit {
                Exit::WithValue(_expr, annotation) => annotation,
                Exit::WithoutValue(a) => a,
            },
            Statement::Block(block) => &block.annotation,
            Statement::ForLoop(for_loop) => &for_loop.annotation,
            Statement::WhileLoop(while_loop) => &while_loop.annotation,
            Statement::Assignment(assignment) => &assignment.annotation,
            Statement::If(if_stmt) => &if_stmt.annotation,
            Statement::Break(a) => a,
            Statement::Continue(a) => a,
        }
    }

    pub fn try_into_expr(self) -> Result<Expression<A>, Self> {
        match self {
            Statement::Call(call) => Ok(Expression::from(call)),

            Statement::Block(block) => {
                if block.output.is_some() {
                    Ok(Expression::from(block))
                } else {
                    Err(Statement::Block(block))
                }
            }

            Statement::If(if_cond) => {
                // if-expressions must always have an else branch
                if if_cond.else_branch.is_some() {
                    Ok(Expression::IfCond(if_cond))
                } else {
                    Err(Statement::If(if_cond))
                }
            }

            not_expr => Err(not_expr),
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
                    }
                    None => None,
                };

                Ok(Statement::Block(*block))
            }
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
            }

            Expression::IfCond(if_cond) => {
                let then_branch = match Self::try_from_expr(if_cond.then_branch) {
                    Ok(then_stmt) => Expression::from(Block::single_stmt(then_stmt)),
                    Err(_) => return Err(expr),
                };

                let else_branch = match if_cond.else_branch {
                    Some(else_expr) => match Self::try_from_expr(else_expr) {
                        Ok(else_stmt) => Some(Expression::from(Block::single_stmt(else_stmt))),
                        Err(_) => return Err(expr),
                    },
                    None => None,
                };

                Ok(Statement::If(Box::new(IfCond {
                    cond: if_cond.cond,
                    is_pattern: if_cond.is_pattern,
                    then_branch,
                    else_branch,
                    annotation: if_cond.annotation,
                })))
            }

            invalid => Err(invalid),
        }
    }

    pub fn as_block(&self) -> Option<&Block<A>> {
        match self {
            Statement::Block(block) => Some(block),
            _ => None,
        }
    }
}

pub fn statement_start_matcher() -> Matcher {
    Matcher::Keyword(Keyword::Let)
        .or(Keyword::Var)
        .or(Keyword::For)
        .or(Keyword::While)
        .or(Keyword::Break)
        .or(Keyword::Continue)
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
            }

            Some(TokenTree::Keyword {
                kw: Keyword::For, ..
            }) => {
                let for_loop = ForLoop::parse(tokens)?;
                Ok(Statement::ForLoop(for_loop))
            }

            Some(TokenTree::Keyword {
                kw: Keyword::While, ..
            }) => {
                let while_loop = WhileLoop::parse(tokens)?;
                Ok(Statement::WhileLoop(while_loop))
            }

            Some(TokenTree::Keyword {
                kw: Keyword::Break,
                span,
            }) => {
                tokens.advance(1);
                Ok(Statement::Break(span))
            }

            Some(TokenTree::Keyword {
                kw: Keyword::Continue,
                span,
            }) => {
                tokens.advance(1);
                Ok(Statement::Continue(span))
            }

            Some(TokenTree::Keyword {
                     kw: Keyword::Exit,
                     span,
                 }) => {
                tokens.advance(1);

                let stmt_terminator = Keyword::End.or(Separator::Semicolon);

                match tokens.look_ahead().match_one(stmt_terminator) {
                    Some(..) => {
                        Ok(Statement::Exit(Exit::WithoutValue(span.clone())))
                    }

                    _ => {
                        let value_expr = Expression::parse(tokens)?;
                        let span = span.to(value_expr.annotation().span());
                        Ok(Statement::Exit(Exit::WithValue(value_expr, span)))
                    }
                }
            }


            Some(..) => {
                // it doesn't start with a statement keyword, it must be an expression
                let expr = Expression::parse(tokens)?;

                let stmt = Self::try_from_expr(expr.clone()).map_err(|invalid_expr| {
                    let invalid_stmt = InvalidStatement::from(invalid_expr);
                    let err = ParseError::InvalidStatement(invalid_stmt);
                    TracedError::trace(err)
                })?;
                Ok(stmt)
            }

            None => Err(TracedError::trace(match tokens.look_ahead().next() {
                Some(unexpected) => {
                    ParseError::UnexpectedToken(Box::new(unexpected), Some(stmt_start))
                }
                None => ParseError::UnexpectedEOF(stmt_start, tokens.context().clone()),
            })),
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
            Statement::WhileLoop(while_loop) => write!(f, "{}", while_loop),
            Statement::Assignment(assignment) => write!(f, "{}", assignment),
            Statement::If(if_stmt) => write!(f, "{}", if_stmt),
            Statement::Break(..) => write!(f, "{}", Keyword::Break),
            Statement::Continue(..) => write!(f, "{}", Keyword::Continue),
        }
    }
}
