use crate::ast::{FunctionParamMod, TypeName};
use crate::parse::{LookAheadTokenStream, Parse};
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::Keyword;
use crate::Separator;
use crate::TokenStream;
use common::span::Span;
use common::span::Spanned;
use derivative::Derivative;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, Hash, PartialEq)]
pub struct FunctionTypeName {
    pub params: Vec<FunctionTypeNameParam>,
    pub return_ty: Option<Box<TypeName>>,

    pub indirection: usize,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub span: Span,
}

impl Spanned for FunctionTypeName {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for FunctionTypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function(")?;
        for (i, param_ty) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, "; ")?;
            }
            write!(f, "{}", param_ty)?;
        }
        write!(f, ")")?;

        if let Some(return_ty) = &self.return_ty {
            write!(f, ": {}", return_ty)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Eq)]
pub struct FunctionTypeNameParam {
    pub ty: TypeName,
    pub modifier: Option<FunctionParamMod>,
}

impl PartialEq for FunctionTypeNameParam {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty && self.modifier == other.modifier
    }
}

impl ParseSeq for FunctionTypeNameParam {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Semicolon)?;
        }

        let modifier = match tokens.match_one_maybe(Keyword::Var | Keyword::Out) {
            Some(tt) if tt.is_keyword(Keyword::Var) => Some(FunctionParamMod::Var),
            Some(tt) if tt.is_keyword(Keyword::Out) => Some(FunctionParamMod::Out),
            Some(..) => unreachable!(),
            None => None,
        };

        let ty = TypeName::parse(tokens)?;

        Ok(FunctionTypeNameParam { ty, modifier })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        tokens
            .match_one(Keyword::Var | Keyword::Out | TypeName::start_matcher())
            .is_some()
    }
}

impl Hash for FunctionTypeNameParam {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ty.hash(state);
        self.modifier.hash(state);
    }
}

impl fmt::Display for FunctionTypeNameParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(modifier) = &self.modifier {
            write!(f, "{} ", modifier)?;
        }
        write!(f, "{}", self.ty)
    }
}
