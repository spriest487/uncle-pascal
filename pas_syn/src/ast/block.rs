use crate::{ast::{Annotation, Expression, Statement}, Keyword, parse::*, token_tree::*};
use pas_common::span::*;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block<A: Annotation> {
    pub statements: Vec<Statement<A>>,
    pub annotation: A,
    pub output: Option<Expression<A>>,

    pub unsafe_kw: Option<Span>,

    pub begin: Span,
    pub end: Span,
}

impl<A: Annotation> Block<A> {
    pub fn single_stmt(stmt: Statement<A>) -> Self {
        Self {
            annotation: stmt.annotation().clone(),
            begin: stmt.annotation().span().clone(),
            end: stmt.annotation().span().clone(),
            statements: vec![stmt],
            unsafe_kw: None,
            output: None,
        }
    }
}

impl Block<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let unsafe_kw = tokens.match_one_maybe(Keyword::Unsafe)
            .map(|unsafe_tt| unsafe_tt.into_span());

        let body_tt = tokens.match_one(DelimiterPair::BeginEnd)?;

        let span = body_tt.span().clone();

        let (mut body_tokens, begin, end) = match body_tt {
            TokenTree::Delimited {
                inner, open, close, ..
            } => (TokenStream::new(inner, open.clone()), open, close),
            _ => unreachable!(),
        };

        let mut output_expr: Option<Expression<_>> = None;

        let statements = body_tokens.match_separated(Separator::Semicolon, |_, tokens| {
            match Statement::parse(tokens) {
                Ok(stmt) => Ok(Generate::Yield(stmt)),

                Err(traced_err) => match &traced_err.err {
                    // if the final statement is invalid as a statement but still a valid
                    // expression, assume it's the block output. some expressions (eg calls) are
                    // always valid as statements regardless of type, so in some cases the block
                    // output can't be determined until typechecking
                    ParseError::InvalidStatement(InvalidStatement(expr)) => {
                        output_expr = Some((**expr).clone());
                        Ok(Generate::Break)
                    }

                    _ => Err(traced_err),
                },
            }
        })?;

        body_tokens.finish()?;

        let block = Self {
            statements,
            annotation: span,

            // we don't know until typechecking
            output: output_expr,
            begin,
            end,

            unsafe_kw,
        };

        Ok(block)
    }
}

impl<A: Annotation> fmt::Display for Block<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "begin")?;
        for (i, stmt) in self.statements.iter().enumerate() {
            write!(f, "{}", stmt)?;

            // only add a ; on the last statement if there's also an output expr
            if i != self.statements.len() - 1 || self.output.is_some() {
                writeln!(f, ";")?;
            }
        }
        if let Some(output) = &self.output {
            write!(f, "{}", output)?;
        }
        writeln!(f)?;
        write!(f, "end")
    }
}

impl<A: Annotation> Spanned for Block<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}
