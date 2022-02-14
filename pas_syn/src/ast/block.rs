use crate::{ast::{Annotation, Expression, Statement}, Keyword, parse::*, token_tree::*};
use pas_common::span::*;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block<A: Annotation> {
    pub statements: Vec<Statement<A>>,
    pub annotation: A,

    // the final expression of the block which determines its value.
    // we can identify this during parsing if the last "statement" in the block is an
    // expression which can't be parsed as a valid standalone statement. otherwise, this gets
    // populated during typechecking.
    // e.g. a function call at the end a block may be the the block output depending on the return
    // type of the function, but we don't know that until typechecking. a value on its own, however,
    // would HAVE to be the block output to be valid!
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

        let (inner, begin, end) = match body_tt {
            TokenTree::Delimited {
                inner, open, close, ..
            } => (inner, open, close),
            _ => unreachable!(),
        };

        let mut body_tokens = TokenStream::new(inner.clone(), begin.clone());

        let mut output_expr: Option<Expression<_>> = None;

        let statements = body_tokens.match_separated(Separator::Semicolon, |_, tokens| {
            let stmt_start_pos = match tokens.look_ahead().next() {
                Some(tt) => tt.span().start,
                None => return Ok(Generate::Break),
            };

            match Statement::parse(tokens) {
                Ok(stmt) => Ok(Generate::Yield(stmt)),

                Err(traced_err) => match &traced_err.err {
                    // if the final statement is invalid as a statement but still a valid
                    // expression, assume it's the block output. some expressions (eg calls) are
                    // always valid as statements regardless of type, so in some cases the block
                    // output can't be determined until typechecking
                    ParseError::InvalidStatement(InvalidStatement(..)) => {
                        // NASTY HACK ZONE
                        // we need to re-parse the tokens used for this statement, so make a new
                        // stream out of the block's inner tokens and fast-forward it to where
                        // we started parsing this statement...
                        let mut output_expr_tokens = TokenStream::new(inner.clone(), begin.clone());
                        while let Some(tt) = output_expr_tokens.look_ahead().next() {
                            let tt_start = tt.span().start;
                            if tt_start < stmt_start_pos {
                                output_expr_tokens.advance(1);
                            } else {
                                break;
                            }
                        }

                        output_expr = Some(Expression::parse(&mut output_expr_tokens)?);
                        output_expr_tokens.match_one_maybe(Separator::Semicolon);

                        // then check we used all the tokens here, while fast-forwarding the original
                        // stream as if we read it properly (safe because the output expression
                        // must be the last token before the end of the block)
                        output_expr_tokens.finish()?;

                        while tokens.look_ahead().next().is_some() {
                            tokens.advance(1);
                        }

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

    // convert block-as-statement into an block-as-expression
    // todo: should these be two different types?
    pub fn to_expr(&self) -> Option<Self> {
        if self.output.is_some() {
            // block that already had an output expr
            return Some(self.clone());
        }

        let final_stmt_expr = self.statements.last().and_then(|final_stmt| {
            final_stmt.clone().to_expr()
        });

        if let Some(output_expr) = final_stmt_expr {
            // block where we can reinterpret the final statement as an output expr
            let mut statements = self.statements.clone();
            statements.pop();

            return Some(Block {
                statements,
                output: Some(output_expr),
                annotation: self.annotation.clone(),
                unsafe_kw: self.unsafe_kw.clone(),
                begin: self.begin.clone(),
                end: self.end.clone(),
            });
        }

        // block that doesn't work as an expr
        None
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
