use crate::{
    ast::{
        Expression,
        Statement,
    },
    parse::prelude::*,
};

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct IfCond<A: Annotation> {
    pub cond: Expression<A>,
    pub then_branch: Expression<A>,
    pub else_branch: Option<Expression<A>>,
    pub annotation: A,
}

impl IfCond<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let if_token = tokens.match_one(Keyword::If)?;
        let cond = Expression::parse(tokens)?;
        tokens.match_one(Keyword::Then)?;
        let then_branch = Expression::parse(tokens)?;

        eprintln!("IF EXPR if {} then {}", cond, then_branch);

        let (else_branch, span) = match tokens.look_ahead().match_one(Keyword::Else) {
            Some(_else_token) => {
                tokens.advance(1);
                let else_branch = Expression::parse(tokens)?;
                let span = if_token.span().to(else_branch.annotation());

                (Some(else_branch), span)
            },

            None => (None, if_token.span().to(then_branch.annotation())),
        };

        Ok(IfCond {
            cond,
            then_branch,
            else_branch,
            annotation: span,
        })
    }
}

impl<A: Annotation> fmt::Display for IfCond<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} then {}", self.cond, self.then_branch)?;
        if let Some(else_branch) = &self.else_branch {
            write!(f, " else {}", else_branch)
        } else {
            Ok(())
        }
    }
}

impl<A: Annotation> Spanned for IfCond<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
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

impl IfStatement<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let if_token = tokens.match_one(Keyword::If)?;
        let cond = Expression::parse(tokens)?;
        tokens.match_one(Keyword::Then)?;
        let then_branch = Statement::parse(tokens)?;

        eprintln!("IF STMT if {} then {} (next: {:?})", cond, then_branch, tokens.look_ahead().next());

        let (else_branch, span) = match tokens.look_ahead().match_one(Keyword::Else) {
            Some(_else_token) => {
                tokens.advance(1);
                let else_branch = Statement::parse(tokens)?;
                let span = if_token.span().to(else_branch.annotation());

                (Some(else_branch), span)
            },

            None => (None, if_token.span().to(then_branch.annotation())),
        };

        Ok(IfStatement {
            cond,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
            annotation: span,
        })
    }
}