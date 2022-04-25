use crate::{
    ast::{Annotation, Expression, Statement},
    parse::*,
    token_tree::*,
    Keyword,
};
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
        let unsafe_kw = tokens
            .match_one_maybe(Keyword::Unsafe)
            .map(|unsafe_tt| unsafe_tt.into_span());

        let body_tt = tokens.match_one(DelimiterPair::BeginEnd)?;

        let span = body_tt.span().clone();

        let body_tt_group = match body_tt {
            TokenTree::Delimited(group) => group,
            _ => unreachable!(),
        };

        let begin = body_tt_group.open.clone();
        let end = body_tt_group.close.clone();

        let mut stmt_tokens = body_tt_group.clone().to_inner_tokens();

        let (statements, output_expr) = parse_block_stmts(&mut stmt_tokens)?;

        stmt_tokens.finish()?;

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

        let final_stmt_expr = self
            .statements
            .last()
            .and_then(|final_stmt| final_stmt.clone().to_expr());

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

fn parse_block_stmts(
    tokens: &mut TokenStream,
) -> ParseResult<(Vec<Statement<Span>>, Option<Expression<Span>>)> {
    let mut statements = Vec::new();
    let mut output_expr: Option<Expression<_>> = None;

    loop {
        if output_expr.is_some() || !Statement::has_more(&statements, &mut tokens.look_ahead()) {
            break;
        }

        if !statements.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        // record the start position of this statement, because if it fails to parse as a statement we can
        // take a second attempt at parsing the same tokens as an output expression
        let stmt_start_pos = tokens.position();

        match Statement::parse(tokens) {
            Ok(stmt) => {
                statements.push(stmt);
            },

            Err(traced_err) => match &traced_err.err {
                // if the final statement is invalid as a statement but still a valid
                // expression, assume it's the block output. some expressions (eg calls) are
                // always valid as statements regardless of type, so in some cases the block
                // output can't be determined until typechecking
                ParseError::InvalidStatement(InvalidStatement(_invalid)) => {
                    tokens.seek(stmt_start_pos);
                    output_expr = Some(Expression::parse(tokens)?);
                },

                // failed for other reasons, this is an actual error
                _ => return Err(traced_err),
            },
        }
    }

    if !statements.is_empty() || output_expr.is_some() {
        tokens.match_one_maybe(Separator::Semicolon);
    }

    Ok((statements, output_expr))
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
