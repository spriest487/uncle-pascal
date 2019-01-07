use {
    crate::{
        Span,
        Ident,
        Keyword,
        Separator,
        Operator,
        TokenTree,
        TokenStream,
        Matcher,
        ast::*,
    }
};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LetBinding {
    pub span: Span,
    pub name: Ident,
    pub ty: Type,
    pub val: Expression,
}

impl LetBinding {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let let_kw = tokens.match_one(Keyword::Let)?;
        let name = tokens.match_one(Matcher::AnyIdent)?;

        tokens.match_one(Separator::Colon)?;
        let ty = Type::parse(tokens)?;

        tokens.match_one(Operator::Assignment)?;
        let val = Expression::parse(tokens)?;

        Ok(LetBinding {
            name: name.as_ident().cloned().unwrap(),
            span: let_kw.span().to(val.span()),
            ty,
            val,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Statement {
    LetBinding(LetBinding),
}

impl Statement {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let stmt_start = Matcher::AnyKeyword;

        match tokens.look_ahead().match_one(stmt_start.clone()) {
            Some(TokenTree::Keyword { kw: Keyword::Let, .. }) => {
                let let_binding = LetBinding::parse(tokens)?;
                Ok(Statement::LetBinding(let_binding))
            }

            Some(unexpected) => Err(ParseError::UnexpectedToken(unexpected, None).into()),

            None => Err(ParseError::UnexpectedEOF(stmt_start, tokens.context().clone()).into()),
        }
    }
}
