mod matcher;
mod token_stream;

pub use self::matcher::*;
pub use self::token_stream::*;
use crate::ast::TypeName;
use crate::ast::*;
use crate::token_tree::*;
use crate::ast::Ident;
use pas_common::span::*;
use pas_common::DiagnosticLabel;
use pas_common::DiagnosticMessage;
use pas_common::DiagnosticOutput;
use pas_common::TracedError;
use std::fmt;

#[derive(Debug)]
pub struct InvalidStatement<A: Annotation>(pub Box<Expr<A>>);

impl<A: Annotation> fmt::Display for InvalidStatement<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "the expr `{}` is not valid as a stmt", self.0)
    }
}

impl<A: Annotation> InvalidStatement<A> {
    pub fn title(&self) -> String {
        "Invalid stmt".to_string()
    }
}

impl<A: Annotation> From<Expr<A>> for InvalidStatement<A> {
    fn from(expr: Expr<A>) -> Self {
        InvalidStatement(Box::new(expr))
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Box<TokenTree>, Option<Matcher>),
    UnexpectedEOF(Matcher, Span),
    EmptyOperand { operator: Span, before: bool },
    UnexpectedOperator { operator: Span },
    InvalidStatement(InvalidStatement<Span>),
    DuplicateModifier { new: DeclMod<Span>, existing: DeclMod<Span> },
    CtorWithTypeArgs { span: Span },
    EmptyTypeParamList(TypeList<Ident>),
    EmptyTypeArgList(TypeList<TypeName>),
    EmptyWhereClause(WhereClause<TypeName>),
    TypeConstraintAlreadySpecified(TypeConstraint<TypeName>),
    NoMatchingParamForTypeConstraint(TypeConstraint<TypeName>),
    UnterminatedStatement { span: Span },
    InvalidFunctionImplType(TypeName),
    InvalidAssignmentExpr { span: Span },
    EmptyConstDecl { span: Span },
    EmptyTypeDecl { span: Span },
    InvalidForLoopInit(Stmt<Span>),
}

pub type ParseResult<T> = Result<T, TracedError<ParseError>>;

impl Spanned for ParseError {
    fn span(&self) -> &Span {
        match self {
            ParseError::UnexpectedToken(tt, _) => match tt.as_ref() {
                TokenTree::Delimited(group) => group.open.span(),
                _ => tt.span(),
            },
            ParseError::UnexpectedEOF(_, tt) => tt.span(),
            ParseError::EmptyOperand { operator, .. } => operator.span(),
            ParseError::UnexpectedOperator { operator } => operator.span(),
            ParseError::InvalidStatement(invalid) => invalid.0.annotation().span(),
            ParseError::DuplicateModifier { new, .. } => new.span(),
            ParseError::CtorWithTypeArgs { span } => span,
            ParseError::TypeConstraintAlreadySpecified(c) => c.span(),
            ParseError::NoMatchingParamForTypeConstraint(c) => c.span(),
            ParseError::EmptyTypeParamList(tl) => tl.span(),
            ParseError::EmptyTypeArgList(tl) => tl.span(),
            ParseError::EmptyWhereClause(c) => c.span(),
            ParseError::UnterminatedStatement { span } => span,
            ParseError::InvalidFunctionImplType(tn) => tn.span(),
            ParseError::InvalidAssignmentExpr { span } => span,
            ParseError::EmptyConstDecl { span, .. } => span,
            ParseError::EmptyTypeDecl { span, .. } => span,
            ParseError::InvalidForLoopInit(stmt) => stmt.span(),
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(..) => write!(f, "Unexpected token"),
            ParseError::UnexpectedEOF(..) => write!(f, "Unexpected end of sequence"),
            ParseError::EmptyOperand { .. } => write!(f, "Empty operand"),
            ParseError::UnexpectedOperator { .. } => write!(f, "Unexpected operator"),
            ParseError::InvalidStatement(invalid) => write!(f, "{}", invalid.title()),
            ParseError::DuplicateModifier { .. } => write!(f, "Duplicate modifier"),
            ParseError::CtorWithTypeArgs { .. } => write!(f, "Constructor with type args"),
            ParseError::TypeConstraintAlreadySpecified(..) => write!(f, "Type constraint already specified"),
            ParseError::NoMatchingParamForTypeConstraint(..) => write!(f, "No matching parameter for type constraint"),
            ParseError::EmptyTypeParamList { .. } => write!(f, "Empty type parameter list"),
            ParseError::EmptyTypeArgList { .. } => write!(f, "Empty type argument list"),
            ParseError::EmptyWhereClause(..) => write!(f, "Empty `where` clause"),
            ParseError::UnterminatedStatement { .. } => write!(f, "Unterminated stmt"),
            ParseError::InvalidFunctionImplType(..) => write!(f, "Invalid interface type for method"),
            ParseError::InvalidAssignmentExpr { .. } => write!(f, "Illegal assignment"),
            ParseError::EmptyConstDecl { .. } => write!(f, "Empty const declaration"),
            ParseError::EmptyTypeDecl { .. } => write!(f, "Empty type declaration"),
            ParseError::InvalidForLoopInit( .. ) => write!(f, "Invalid for-loop initialization stmt"),
        }
    }
}

impl DiagnosticOutput for ParseError {
    fn label(&self) -> Option<DiagnosticLabel> {
        let text = match self {
            ParseError::UnexpectedToken(tt, Some(expected)) => {
                if expected.is_multiline_display() {
                    format!("expected {}\nfound {}", expected, tt)
                } else {
                    format!("expected {}, found {}", expected, tt)
                }
            }

            ParseError::UnexpectedToken(tt, None) => format!("unexpected {}", tt),

            ParseError::UnexpectedEOF(expected, _tt) => {
                format!("expected {} but reached end of sequence", expected)
            }

            ParseError::EmptyOperand { operator, before } => {
                let pos_name = if *before { "before" } else { "after" };
                format!("expected operand {} {}", pos_name, operator)
            }

            ParseError::UnexpectedOperator { .. } => "expected operand, found operator".to_string(),

            ParseError::InvalidStatement(invalid_stmt) => invalid_stmt.to_string(),

            ParseError::DuplicateModifier { new, .. } => format!(
                "the modifier `{}` is already present on this declaration",
                new.keyword(),
            ),

            ParseError::CtorWithTypeArgs { .. } => {
                "Object constructor expr cannot explicitly specify type args".to_string()
            }

            ParseError::TypeConstraintAlreadySpecified(c) => {
                format!("parameter `{}` has more than one type constraint", c.param_ident)
            }

            ParseError::EmptyTypeParamList { .. } => {
                "type parameter list must contain one or more identifiers".to_string()
            }

            ParseError::EmptyTypeArgList { .. } => {
                "type argument list must contain one or more type names".to_string()
            }

            ParseError::EmptyWhereClause(..) => {
                "`where` clause must contain one or more type constraints in the form `Type is InterfaceName`".to_string()
            }

            ParseError::NoMatchingParamForTypeConstraint(c) => {
                format!("type constraint was specified for parameter `{}` which does not exist", c.param_ident)
            }

            ParseError::UnterminatedStatement { .. } => {
                format!("stmt here is unterminated")
            }

            ParseError::InvalidFunctionImplType(ty) => {
                format!("type {} cannot have interface implementation functions declared for it", ty)
            }

            ParseError::InvalidAssignmentExpr { .. } => {
                format!("Assignment operator can only be used in an assignment stmt")
            }

            ParseError::EmptyTypeDecl { .. } => {
                format!("type declaration must contain one or more types")
            }

            ParseError::EmptyConstDecl { .. } => {
                format!("const declaration must contain one or more constants")
            }

            ParseError::InvalidForLoopInit(stmt) => {
                format!("stmt `{}` cannot be used to initialize the counter variable of a for-loop", stmt)
            }
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
                }),
                notes: Vec::new(),
            }],

            _ => Vec::new(),
        }
    }
}

pub trait Parse: Sized {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self>;
}

pub trait Match {
    fn is_match(tokens: &mut LookAheadTokenStream) -> bool;
}

pub trait TryParse : Sized {
    fn try_parse(tokens: &mut TokenStream) -> ParseResult<Option<Self>>;
}

impl<T> TryParse for T where T : Parse + Match {
    fn try_parse(tokens: &mut TokenStream) -> ParseResult<Option<Self>> {
        if Self::is_match(&mut tokens.look_ahead()) {
            Self::parse(tokens).map(Some)
        } else {
            Ok(None)
        }
    }
}
