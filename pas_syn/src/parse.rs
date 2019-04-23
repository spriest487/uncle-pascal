mod matcher;
mod token_stream;

pub mod prelude {
    pub use crate::{
        ast::{
            Annotation,
            TypeName,
        },
        ident::*,
        keyword::*,
        operators::*,
        parse::*,
        token_tree::{
            DelimiterPair,
            Separator,
            TokenTree,
        },
    };
    pub use pas_common::{
        span::*,
        TracedError,
    };
    pub use std::fmt;
}

use crate::{
    ast::*,
    token_tree::*,
};
use pas_common::{span::*, DiagnosticOutput, DiagnosticLabel, TracedError, DiagnosticMessage};
use std::fmt;

pub use self::{
    matcher::*,
    token_stream::*,
};

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(TokenTree, Option<Matcher>),
    UnexpectedEOF(Matcher, Span),
    EmptyOperand { operator: Span, before: bool },
    InvalidStatement(Expression<Span>),
    InvalidMember(BinOp<Span>, Span),
    DuplicateModifier { new: DeclMod, existing: DeclMod },
}

pub type ParseResult<T> = Result<T, TracedError<ParseError>>;

impl Spanned for ParseError {
    fn span(&self) -> &Span {
        match self {
            ParseError::UnexpectedToken(tt, _) => tt.span(),
            ParseError::UnexpectedEOF(_, tt) => tt.span(),
            ParseError::EmptyOperand { operator, .. } => operator.span(),
            ParseError::InvalidStatement(expr) => expr.annotation().span(),
            ParseError::InvalidMember(_, span) => span,
            ParseError::DuplicateModifier { new, .. } => new.span(),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(..) => write!(f, "Unexpected token"),
            ParseError::UnexpectedEOF(..) => write!(f, "Unexpected end of file"),
            ParseError::EmptyOperand { .. } => write!(f, "Empty operand"),
            ParseError::InvalidStatement(..) => write!(f, "Invalid statement"),
            ParseError::InvalidMember(..) => write!(f, "Invalid member expression"),
            ParseError::DuplicateModifier { .. } => write!(f, "Duplicate modifier"),
        }
    }
}

impl DiagnosticOutput for ParseError {
    fn label(&self) -> Option<DiagnosticLabel> {
        let text = match self {
            ParseError::UnexpectedToken(tt, Some(expected)) => {
                format!("expected {}, found {}", expected, tt)
            },

            ParseError::UnexpectedToken(tt, None) => format!("unexpected {}", tt),

            ParseError::UnexpectedEOF(expected, tt) => format!(
                "expected {} after {} but reached end of sequence",
                expected, tt
            ),

            ParseError::EmptyOperand { operator, before } => {
                let pos_name = if *before { "before" } else { "after" };
                format!("expected operand {} {}", pos_name, operator)
            },

            ParseError::InvalidStatement(expr) => {
                format!("the expression `{}` is not valid as a statement", expr)
            },

            ParseError::InvalidMember(bin_op, _) => format!(
                "the expression `{}` does not denote a member of `{}`",
                bin_op.rhs, bin_op.lhs
            ),

            ParseError::DuplicateModifier { new, .. } => format!(
                "the modifier `{}` is already present on this declaration",
                new.keyword(),
            )
        };

        Some(DiagnosticLabel {
            text: Some(text),
            span: self.span().clone(),
        })
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            ParseError::DuplicateModifier { existing, .. } => vec![DiagnosticMessage {
                title: "Duplicate modifier occurrence".to_string(),
                label: Some(DiagnosticLabel {
                    span: existing.span().clone(),
                    text: Some(format!("`{}` appears here", existing.keyword())),
                })
            }],

            _ => Vec::new(),
        }
    }
}
