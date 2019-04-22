mod matcher;
mod token_stream;

pub mod prelude {
    pub use crate::{
        ast::{
            Annotation,
            TypeName,
            TypeNamePattern,
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
pub struct InvalidStatement<A: Annotation>(pub Expression<A>);

impl<A: Annotation> fmt::Display for InvalidStatement<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "the expression `{}` is not valid as a statement", self.0)
    }
}

impl<A: Annotation> InvalidStatement<A> {
    pub fn title(&self) -> String {
        "Invalid statement".to_string()
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(TokenTree, Option<Matcher>),
    UnexpectedEOF(Matcher, Span),
    EmptyOperand { operator: Span, before: bool },
    UnexpectedOperator { operator: Span },
    InvalidStatement(InvalidStatement<Span>),
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
            ParseError::UnexpectedOperator { operator } => operator.span(),
            ParseError::InvalidStatement(invalid) => invalid.0.annotation().span(),
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
            ParseError::UnexpectedOperator { .. } => write!(f, "Unexpected operator"),
            ParseError::InvalidStatement(invalid) => write!(f, "{}", invalid.title()),
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

            ParseError::UnexpectedEOF(expected, _tt) => format!(
                "expected {} but reached end of sequence",
                expected
            ),

            ParseError::EmptyOperand { operator, before } => {
                let pos_name = if *before { "before" } else { "after" };
                format!("expected operand {} {}", pos_name, operator)
            },

            ParseError::UnexpectedOperator { .. } => {
                "expected operand, found operator".to_string()
            }

            ParseError::InvalidStatement(invalid_stmt) => {
                invalid_stmt.to_string()
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
