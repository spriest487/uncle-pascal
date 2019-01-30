use {
    std::fmt,
    pas_common::{
        span::*,
    },
    crate::{
        ident::*,
        token_tree::*,
        parse::*,
        ast::*,
    },
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LetBinding<A: Annotation> {
    pub name: Ident,
    pub val_ty: A::Type,
    pub val: ExpressionNode<A>,
    pub annotation: A,
}

impl LetBinding<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let let_kw = tokens.match_one(Keyword::Let)?;
        let name_token = tokens.match_one(Matcher::AnyIdent)?;

        tokens.match_one(Separator::Colon)?;
        let ty = TypeName::parse(tokens)?;

        tokens.match_one(Operator::Assignment)?;
        let val = ExpressionNode::parse(tokens)?;
        let span = let_kw.span().to(&val.annotation);

        Ok(LetBinding {
            name: name_token.as_ident().cloned().unwrap(),
            val_ty: ty,
            val: val,
            annotation: span,
        })
    }
}

impl<A: Annotation> fmt::Display for LetBinding<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "let {}: {} := {}", self.name, self.val_ty, self.val)
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
pub enum Statement<A: Annotation> {
    LetBinding(LetBinding<A>),
    Call(Call<A>),
    Exit(Exit<A>),
    Block(Block<A>),
}

impl<A: Annotation> Statement<A> {
    pub fn annotation(&self) -> &A {
        match self {
            Statement::LetBinding(binding) => &binding.annotation,
            Statement::Call(call) => &call.annotation,
            Statement::Exit(exit) => match exit {
                Exit::WithValue(expr) => &expr.annotation,
                Exit::WithoutValue(a) => a,
            },
            Statement::Block(block) => &block.annotation,
        }
    }

    pub fn try_into_expr(self) -> Option<ExpressionNode<A>> {
        match self {
            Statement::Call(call) => {
                let annotation = call.annotation.clone();
                let call_expr = Expression::Call(call);
                Some(ExpressionNode::new(call_expr, annotation))
            }

            Statement::Block(block) => if block.output.is_some() {
                let annotation = block.annotation.clone();
                let block_expr = Expression::Block(block);
                Some(ExpressionNode::new(block_expr, annotation))
            } else {
                None
            }

            _ => None,
        }
    }
}

pub fn statement_start_matcher() -> Matcher {
    Matcher::Keyword(Keyword::Let)
        .or(Keyword::Exit)
        .or(expression::match_operand_start())
}

impl Statement<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Statement<Span>> {
        let stmt_start = statement_start_matcher();

        match tokens.look_ahead().match_one(stmt_start.clone()) {
            Some(TokenTree::Keyword { kw: Keyword::Let, .. }) => {
                let binding = LetBinding::parse(tokens)?;

                Ok(Statement::LetBinding(binding))
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

        invalid => {
            let invalid_node = ExpressionNode::new(invalid, expr.annotation);
            Err(TracedError::trace(ParseError::InvalidStatement(invalid_node)))
        }
    }
}

impl<A: Annotation> fmt::Display for Statement<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::LetBinding(binding) => write!(f, "{}", binding),
            Statement::Call(call) => write!(f, "{}", call),
            Statement::Exit(exit) => write!(f, "{}", exit),
            Statement::Block(block) => write!(f, "{}", block),
        }
    }
}

