use {
    crate::{
        parse::prelude::*,
        ast::{
            ExpressionNode,
            Expression,
            expression::match_operand_start,
            ForLoop,
            Call,
            Block,
        },
    },
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LocalBinding<A: Annotation> {
    pub name: Ident,
    pub val_ty: Option<A::Type>,
    pub val: ExpressionNode<A>,
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
            TokenTree::Keyword { kw: Keyword::Var, .. } => true,
            _ => false,
        };

        let name_token = tokens.match_one(Matcher::AnyIdent)?;

        let val_ty = match tokens.look_ahead().match_one(Separator::Colon) {
            Some(_) => {
                tokens.advance(1);
                let ty = TypeName::parse(tokens)?;
                Some(ty)
            }
            None => None,
        };

        tokens.match_one(Operator::Assignment)?;
        let val = ExpressionNode::parse(tokens)?;
        let span = let_kw.span().to(&val.annotation);

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
        if let Some(val_ty) = &self.val_ty {
            write!(f, ": {}", val_ty)?;
        }
        write!(f, " := {}", self.val)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Exit<A: Annotation> {
    WithValue(ExpressionNode<A>),
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
    pub lhs: ExpressionNode<A>,
    pub rhs: ExpressionNode<A>,
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
    Assignment(Assignment<A>),
}

impl<A: Annotation> Statement<A> {
    pub fn annotation(&self) -> &A {
        match self {
            Statement::LocalBinding(binding) => &binding.annotation,
            Statement::Call(call) => &call.annotation,
            Statement::Exit(exit) => match exit {
                Exit::WithValue(expr) => &expr.annotation,
                Exit::WithoutValue(a) => a,
            },
            Statement::Block(block) => &block.annotation,
            Statement::ForLoop(for_loop) => &for_loop.annotation,
            Statement::Assignment(assignment) => &assignment.annotation,
        }
    }

    pub fn try_into_expr(self) -> Option<ExpressionNode<A>> {
        match self {
            Statement::Call(call) => {
                let annotation = call.annotation.clone();
                Some(ExpressionNode::new(call, annotation))
            }

            Statement::Block(block) => if block.output.is_some() {
                let annotation = block.annotation.clone();
                Some(ExpressionNode::new(block, annotation))
            } else {
                None
            }

            _ => None,
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
            Some(TokenTree::Keyword { kw: Keyword::Let, .. }) |
            Some(TokenTree::Keyword { kw: Keyword::Var, .. }) => {
                let binding = LocalBinding::parse(tokens, true)?;
                Ok(Statement::LocalBinding(binding))
            }

            Some(TokenTree::Keyword { kw: Keyword::For, .. }) => {
                let for_loop = ForLoop::parse(tokens)?;
                Ok(Statement::ForLoop(for_loop))
            }

            Some(_) => {
                // it doesn't start with a statement keyword, it must be an expression
                let expr = ExpressionNode::parse(tokens)?;
                expr_to_stmt(expr)
            }

            None => {
                Err(TracedError::trace(ParseError::UnexpectedEOF(stmt_start, tokens.context().clone())))
            }
        }
    }
}

fn expr_to_stmt(expr: ExpressionNode<Span>) -> ParseResult<Statement<Span>> {
    match *expr.expr {
        Expression::Call(call) => Ok(Statement::Call(call)),
        Expression::Block(mut block) => {
            block.output = match block.output {
                Some(output_expr) => {
                    let last_stmt = expr_to_stmt(output_expr)?;
                    block.statements.push(last_stmt);
                    None
                },
                None => None,
            };

            Ok(Statement::Block(block))
        },

        Expression::BinOp(bin_op) => {
            if bin_op.op == Operator::Assignment {
                let assignment = Assignment {
                    lhs: bin_op.lhs,
                    rhs: bin_op.rhs,
                    annotation: bin_op.annotation,
                };
                Ok(Statement::Assignment(assignment))
            } else {
                let invalid_node = ExpressionNode::new(bin_op, expr.annotation);
                Err(TracedError::trace(ParseError::InvalidStatement(invalid_node)))
            }
        }

        invalid => {
            let invalid_node = ExpressionNode::new(invalid, expr.annotation);
            Err(TracedError::trace(ParseError::InvalidStatement(invalid_node)))
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
        }
    }
}

