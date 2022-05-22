use std::fmt;
use pas_common::{
    span::{Span, Spanned},
    TracedError
};
use crate::{ast::{
    unit::{parse_unit_decl},
    Annotation,
    ConstDecl,
    Expr,
    FunctionDecl,
    FunctionDef,
    TypeDecl
}, IdentPath, Keyword, Separator, parse::{
    Matcher,
    ParseError,
    ParseResult,
    TokenStream,
    LookAheadTokenStream,
    ParseSeq,
}, parse::MatchOneOf, Ident};
use derivative::*;
use crate::parse::Parse;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub enum Visibility {
    Interface,
    Implementation,
}

#[derive(Clone, Debug)]
pub enum UnitDecl<A: Annotation> {
    FunctionDecl {
        decl: FunctionDecl<A>,
    },
    FunctionDef {
        def: FunctionDef<A>,
    },
    Type {
        decl: TypeDecl<A>,
    },
    Uses {
        decl: UseDecl,
    },
    Const {
        decl: ConstDecl<A>,
    },
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
            UnitDecl::Const { decl, .. } => decl.span(),
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
            UnitDecl::Const { decl, .. } => write!(f, "{}", decl),
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

#[derive(Clone, Debug)]
pub struct UseDecl {
    pub units: Vec<UseDeclItem>,
    pub span: Span,
}

impl Spanned for UseDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for UseDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "uses ")?;
        for (i, item) in self.units.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }

            write!(f, "{}", item)?;
        }
        Ok(())
    }
}

impl UseDecl {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw = tokens.match_one(Keyword::Uses)?;

        let items = UseDeclItem::parse_seq(tokens)?;

        let span = match items.last() {
            None => {
                return Err(TracedError::trace(match tokens.look_ahead().next() {
                    None => ParseError::UnexpectedEOF(Matcher::AnyIdent, kw.span().clone()),
                    Some(x) => ParseError::UnexpectedToken(Box::new(x), Some(Matcher::AnyIdent)),
                }));
            },

            Some(last_item) => {
                kw.span().to(last_item.span())
            }
        };

        Ok(Self { units: items, span })
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Debug, Hash)]
pub struct UseDeclItem {
    pub ident: IdentPath,
    
    pub path: Option<String>,

    #[derivative(PartialEq = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl Spanned for UseDeclItem {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for UseDeclItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;

        if let Some(path) = &self.path {
            write!(f, " in '{}'", path)?;
        }

        Ok(())
    }
}

impl ParseSeq for UseDeclItem {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Comma)?;
        }

        let unit = IdentPath::parse(tokens)?;

        let (path, span) = match tokens.match_one_maybe(Keyword::In) {
            Some(..) => {
                let path_tt = tokens.match_one(Matcher::AnyLiteralString)?;
                let path_string = path_tt.as_literal_string().unwrap().to_string();

                (Some(path_string), unit.span().to(path_tt.span()))
            }

            None => {
                let path_string = None;
                (path_string, unit.span().clone())
            },
        };

        Ok(UseDeclItem {
            ident: unit,
            path,
            span,
        })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Comma).is_none() {
            return false;
        }

        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum DeclMod<A: Annotation> {
    External { src: A::ConstStringExpr, span: Span },
    Inline(Span),
    Forward(Span),
    Overload(Span),
}

impl<A: Annotation> DeclMod<A> {
    pub const EXTERNAL_WORD: &'static str = "external";
    pub const FORWARD_WORD: &'static str = "forward";
    pub const INLINE_WORD: &'static str = "inline";
    pub const OVERLOAD_WORD: &'static str = "overload";

    pub const RESERVED_WORDS: [&'static str; 4] = [Self::EXTERNAL_WORD, Self::FORWARD_WORD, Self::INLINE_WORD, Self::OVERLOAD_WORD];

    pub fn keyword(&self) -> &str {
        match self {
            DeclMod::External { .. } => Self::EXTERNAL_WORD,
            DeclMod::Forward(..) => Self::FORWARD_WORD,
            DeclMod::Inline(..) => Self::INLINE_WORD,
            DeclMod::Overload(..) => Self::OVERLOAD_WORD,
        }
    }
}

impl ParseSeq for DeclMod<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        tokens.match_one(Separator::Semicolon)?;

        let word_token = Ident::parse(tokens)?;

        let new_mod = match word_token.name.as_str() {
            Self::EXTERNAL_WORD => {
                let src = Expr::parse(tokens)?;
                DeclMod::External {
                    span: word_token.span().to(src.span()),
                    src: Box::new(src),
                }
            },

            Self::INLINE_WORD => DeclMod::Inline(word_token.span().clone()),
            Self::FORWARD_WORD => DeclMod::Forward(word_token.span().clone()),
            Self::OVERLOAD_WORD => DeclMod::Overload(word_token.span().clone()),

            _ => unreachable!("bad modified contextual keyword"),
        };

        let existing = prev.iter().find(|m| m.keyword() == new_mod.keyword());
        if let Some(existing) = existing {
            return Err(TracedError::trace(ParseError::DuplicateModifier {
                new: new_mod,
                existing: existing.clone(),
            }));
        }

        Ok(new_mod)
    }

    fn has_more(_prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        let match_any_reserved = Matcher::OneOf(
            Self::RESERVED_WORDS.iter()
                .map(|word| Matcher::Ident(word.to_string()))
                .collect()
        );

        tokens.match_one(match_any_reserved).is_some()
    }
}


impl<A: Annotation> fmt::Display for DeclMod<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DeclMod::External { src, .. } => write!(f, "{} '{}'", Self::EXTERNAL_WORD, src),
            DeclMod::Inline(_) => write!(f, "{}", Self::INLINE_WORD),
            DeclMod::Forward(_) => write!(f, "{}", Self::FORWARD_WORD),
            DeclMod::Overload(_) => write!(f, "{}", Self::OVERLOAD_WORD),
        }
    }
}

impl<A: Annotation> Spanned for DeclMod<A> {
    fn span(&self) -> &Span {
        match self {
            DeclMod::External { span, .. } => span,
            DeclMod::Inline(span) => span,
            DeclMod::Forward(span) => span,
            DeclMod::Overload(span) => span,
        }
    }
}