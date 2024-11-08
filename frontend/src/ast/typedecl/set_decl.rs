use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::TypeDeclName;
use crate::parse::ParseResult;
use crate::Separator;
use crate::Keyword;
use crate::DelimiterPair;
use crate::TokenStream;
use common::span::Span;
use common::span::Spanned;
use derivative::Derivative;
use std::fmt;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct SetDecl<A: Annotation = Span> {
    pub name: A::Name,
    pub items: Vec<Expr<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl SetDecl<Span> {
    pub fn parse(name: TypeDeclName, tokens: &mut TokenStream) -> ParseResult<Self> {
        let set_kw = tokens.match_one(Keyword::Set)?;
        tokens.match_one(Keyword::Of)?;

        let items_group = tokens
            .match_one(DelimiterPair::SquareBracket)?
            .into_delimited_group()
            .unwrap();
        
        let span = set_kw.into_span().to(items_group.close.span());

        let mut items = Vec::new();
        let mut items_tokens= items_group.to_inner_tokens();

        // empty set?
        while items_tokens.look_ahead().next().is_some() {
            let item = Expr::parse(&mut items_tokens)?;
            items.push(item);

            if items_tokens.match_one_maybe(Separator::Comma).is_none() {
                break;
            }
        }

        items_tokens.finish()?;
        
        Ok(SetDecl {
            name,
            items,
            span
        })
    }
}

impl<A: Annotation> Spanned for SetDecl<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for SetDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "set of [")?;
        for i in 0..self.items.len() {
            if i > 0 {
                write!(f, ", ")?;
            }

            write!(f, "{}", self.items[i])?;
        }
        write!(f, "]")
    }
}
