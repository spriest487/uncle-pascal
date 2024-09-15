use crate::{
    ast::{
        Annotation, GlobalBinding, FunctionDecl, FunctionDef, TypeDecl, UseDecl, unit::parse_unit_decl,
    },
    Keyword,
    parse::{
        LookAheadTokenStream, Matcher, MatchOneOf, ParseResult,
        TokenStream,
    }, Separator,
};
use pas_common::{
    span::{Span, Spanned},
};
use std::fmt;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub enum Visibility {
    Interface,
    Implementation,
}

#[derive(Clone, Debug)]
pub enum UnitDecl<A: Annotation> {
    FunctionDecl { decl: FunctionDecl<A> },
    FunctionDef { def: FunctionDef<A> },
    Type { decl: TypeDecl<A> },
    Uses { decl: UseDecl },
    GlobalBinding { decl: GlobalBinding<A> },
}

impl<A: Annotation> Spanned for UnitDecl<A> {
    fn span(&self) -> &Span {
        match self {
            UnitDecl::FunctionDecl {
                decl: func_decl, ..
            } => func_decl.span(),
            UnitDecl::FunctionDef { def: func_def, .. } => func_def.span(),
            UnitDecl::Type {
                decl: type_decl, ..
            } => type_decl.span(),
            UnitDecl::Uses { decl: use_decl } => use_decl.span(),
            UnitDecl::GlobalBinding { decl, .. } => decl.span(),
        }
    }
}

impl<A: Annotation> fmt::Display for UnitDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnitDecl::FunctionDecl {
                decl: func_decl, ..
            } => write!(f, "{}", func_decl),
            UnitDecl::FunctionDef { def: func_def, .. } => write!(f, "{}", func_def),
            UnitDecl::Type { decl: ty_decl, .. } => write!(f, "{}", ty_decl),
            UnitDecl::Uses { decl: uses } => write!(f, "{}", uses),
            UnitDecl::GlobalBinding { decl, .. } => write!(f, "{}", decl),
        }
    }
}

impl UnitDecl<Span> {
    pub fn start_matcher() -> Matcher {
        Keyword::Function
            .or(Keyword::Procedure)
            .or(Keyword::Uses)
            .or(Keyword::Type)
            .or(Keyword::Const)
            .or(Keyword::Var)
    }

    pub fn parse_seq(part_kw: Keyword, tokens: &mut TokenStream) -> ParseResult<Vec<Self>> {
        let mut items = Vec::new();

        loop {
            if !Self::has_more(&items, &mut tokens.look_ahead()) {
                break;
            }

            if !items.is_empty() {
                tokens.match_one(Separator::Semicolon)?;
            }

            let item = parse_unit_decl(tokens, part_kw)?;
            items.push(item);
        }

        Ok(items)
    }

    pub fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }
        tokens.match_one(UnitDecl::start_matcher()).is_some()
    }
}