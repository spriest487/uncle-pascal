use crate::{
    ast::ExpressionNode,
    parse::prelude::*,
};

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct ObjectCtorMember<A: Annotation> {
    pub ident: Ident,
    pub value: ExpressionNode<A>,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct ObjectCtorArgs<A: Annotation> {
    pub open: Span,
    pub members: Vec<ObjectCtorMember<A>>,
    pub close: Span,
}

impl ObjectCtorArgs<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let args_group = tokens.match_one(DelimiterPair::Bracket)?;
        let (open, inner, close) = match args_group {
            TokenTree::Delimited {
                open, inner, close, ..
            } => (open, inner, close),
            _ => unreachable!(),
        };

        let mut members_tokens = TokenStream::new(inner, open.clone());
        let members = members_tokens.match_separated(Separator::Semicolon, |_, tokens| {
            let ident = tokens.match_one(Matcher::AnyIdent)?.into_ident().unwrap();
            tokens.match_one(Separator::Colon)?;
            let value = ExpressionNode::parse(tokens)?;

            Ok(Generate::Yield(ObjectCtorMember { ident, value }))
        })?;
        members_tokens.finish()?;

        Ok(ObjectCtorArgs {
            open,
            members,
            close,
        })
    }
}

impl<A: Annotation> ObjectCtorArgs<A> {
    pub fn iter(&self) -> impl Iterator<Item = &ObjectCtorMember<A>> {
        self.members.iter()
    }
}

impl<A: Annotation> fmt::Display for ObjectCtorArgs<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        for (i, member) in self.members.iter().enumerate() {
            if i > 0 {
                write!(f, "; ")?;
            }
            write!(f, "{}: {}", member.ident, member.value)?;
        }
        write!(f, ")")
    }
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct ObjectCtor<A: Annotation> {
    pub ident: IdentPath,
    pub args: ObjectCtorArgs<A>,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for ObjectCtor<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.ident, self.args)
    }
}
