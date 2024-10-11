use crate::ast::type_name::TypeName;
use crate::ast::Annotation;
use crate::ast::BindingDeclKind;
use crate::ast::Expr;
use crate::ast::Ident;
use crate::parse::LookAheadTokenStream;
use crate::parse::MatchOneOf;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::Keyword;
use crate::Operator;
use crate::Separator;
use derivative::*;
use common::span::Span;
use common::span::Spanned;
use common::TracedError;
use std::fmt;

/// var or const binding (depending on the keyword)
#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct UnitBinding<A: Annotation> {
    pub kind: BindingDeclKind,

    pub items: Vec<UnitBindingItem<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl<A: Annotation> Spanned for UnitBinding<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl Parse for UnitBinding<Span> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw_token = tokens.match_one(Keyword::Const.or(Keyword::Var))?;

        let kind = match kw_token.as_keyword() {
            Some(Keyword::Const) => BindingDeclKind::Const,
            Some(Keyword::Var) => BindingDeclKind::Var,
            _ => unreachable!(),
        };

        let items = UnitBindingItem::parse_seq(tokens)?;
        let last_item = items.last().ok_or_else(|| {
            TracedError::trace(ParseError::EmptyConstDecl {
                span: kw_token.clone().into_span(),
            })
        })?;

        let span = kw_token.span().to(last_item.span());

        Ok(UnitBinding {
            kind,
            span,
            items,
        })
    }
}

impl<A: Annotation> fmt::Display for UnitBinding<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "const")?;
        for (i, item) in self.items.iter().enumerate() {
            if i > 0 {
                writeln!(f, ";")?;
            }
            write!(f, "\t{}", item)?;
        }

        Ok(())
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct UnitBindingItem<A: Annotation> {
    pub ident: Ident,
    pub ty: Option<A::Type>,

    pub val: Option<Box<Expr<A>>>,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl<A: Annotation> Spanned for UnitBindingItem<A> {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A: Annotation> fmt::Display for UnitBindingItem<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if let Some(ty) = &self.ty {
            write!(f, ": {}", ty)?;
        }

        if let Some(val) = &self.val {
            write!(f, " = {}", val)?;
        }

        Ok(())
    }
}

impl ParseSeq for UnitBindingItem<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let ident = Ident::parse(tokens)?;
        let mut span = ident.span.clone();

        let ty = match tokens.match_one_maybe(Separator::Colon) {
            Some(..) => {
                let ty_name = TypeName::parse(tokens)?;
                span = span.to(&ty_name);
                Some(ty_name)
            }
            None => None,
        };

        let val = if tokens.match_one_maybe(Operator::Equals).is_some() {
            let val = Expr::parse(tokens)?;
            span = span.to(&val);
            Some(Box::new(val))
        } else {
            None
        };

        Ok(UnitBindingItem {
            ty,
            span,
            val,
            ident,
        })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}
