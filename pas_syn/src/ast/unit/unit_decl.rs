use std::fmt;
use pas_common::{
    span::{Span, Spanned},
    TracedError
};
use crate::{
    ast::{Annotation, ConstDecl, Expression, FunctionDecl, FunctionDef, TypeDecl},
    IdentPath,
    Keyword,
    Separator,
    parse::{Generate, Matcher, MatchSequenceOf, ParseError, ParseResult, TokenStream}
};

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub enum Visibility {
    Interface,
    Implementation,
}

#[derive(Clone, Debug)]
pub enum UnitDecl<A: Annotation> {
    FunctionDecl {
        decl: FunctionDecl<A>,
        visibility: Visibility,
    },
    FunctionDef {
        decl: FunctionDef<A>,
        visibility: Visibility,
    },
    Type {
        decl: TypeDecl<A>,
        visibility: Visibility,
    },
    Uses {
        decl: UseDecl,
    },
    Const {
        decl: ConstDecl<A>,
        visibility: Visibility,
    },
}

impl<A: Annotation> Spanned for UnitDecl<A> {
    fn span(&self) -> &Span {
        match self {
            UnitDecl::FunctionDecl {
                decl: func_decl, ..
            } => func_decl.span(),
            UnitDecl::FunctionDef { decl: func_def, .. } => func_def.span(),
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
            UnitDecl::FunctionDef { decl: func_def, .. } => write!(f, "{}", func_def),
            UnitDecl::Type { decl: ty_decl, .. } => write!(f, "{}", ty_decl),
            UnitDecl::Uses { decl: uses } => write!(f, "{}", uses),
            UnitDecl::Const { decl, .. } => write!(f, "{}", decl),
        }
    }
}

#[derive(Clone, Debug)]
pub struct UseDecl {
    pub units: Vec<IdentPath>,
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
        for (i, unit) in self.units.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }
            write!(f, "{}", unit)?;
        }
        Ok(())
    }
}

impl UseDecl {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw = tokens.match_one(Keyword::Uses)?;
        let units = tokens.match_separated(Separator::Comma, |_, tokens| {
            let unit_path = IdentPath::parse(tokens)?;
            Ok(Generate::Yield(unit_path))
        })?;

        if units.is_empty() {
            return Err(TracedError::trace(match tokens.look_ahead().next() {
                None => ParseError::UnexpectedEOF(Matcher::AnyIdent, kw.span().clone()),
                Some(x) => ParseError::UnexpectedToken(Box::new(x), Some(Matcher::AnyIdent)),
            }));
        }

        let span = kw.span().to(units.last().unwrap().span());

        Ok(Self { units, span })
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum DeclMod<A: Annotation> {
    External { src: A::ConstStringExpr, span: Span },
}

impl<A: Annotation> DeclMod<A> {
    pub const EXTERNAL_WORD: &'static str = "external";

    pub fn keyword(&self) -> &str {
        match self {
            DeclMod::External { .. } => Self::EXTERNAL_WORD,
        }
    }
}

impl DeclMod<Span> {
    fn parse_external(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw = tokens.match_one(Self::EXTERNAL_WORD)?.into_ident().unwrap();
        let src = Expression::parse(tokens)?;

        Ok(DeclMod::External {
            span: kw.span().to(src.span()),
            src: Box::new(src),
        })
    }

    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Vec<Self>> {
        let mut mods: Vec<Self> = Vec::new();

        let mod_seq = Separator::Semicolon.and_then(Self::EXTERNAL_WORD);

        while let Some(mut seq_tokens) = tokens.look_ahead().match_sequence(mod_seq.clone()) {
            tokens.advance(1);

            let word_token = seq_tokens.remove(1).into_ident().unwrap();

            let new_mod = match word_token.name.as_str() {
                Self::EXTERNAL_WORD => Self::parse_external(tokens)?,

                _ => unreachable!("bad modified contextual keyword"),
            };

            let existing = mods.iter().find(|m| m.keyword() == new_mod.keyword());
            if let Some(existing) = existing {
                return Err(TracedError::trace(ParseError::DuplicateModifier {
                    new: new_mod,
                    existing: existing.clone(),
                }));
            }

            mods.push(new_mod);
        }

        Ok(mods)
    }
}

impl<A: Annotation> fmt::Display for DeclMod<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DeclMod::External { src, .. } => write!(f, "external '{}'", src),
        }
    }
}

impl<A: Annotation> Spanned for DeclMod<A> {
    fn span(&self) -> &Span {
        match self {
            DeclMod::External { span, .. } => span,
        }
    }
}