mod matcher;
mod token_stream;

pub use self::matcher::*;
pub use self::token_stream::*;
use crate::ast::TypeName;
use crate::ast::*;
use crate::token_tree::*;
use common::span::*;
use common::DiagnosticLabel;
use common::DiagnosticMessage;
use common::DiagnosticOutput;
use common::TracedError;
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
    InvalidUnitFilename(Span),
    
    UnexpectedToken(Box<TokenTree>, Option<Matcher>),
    UnexpectedEOF(Matcher, Span),
    EmptyOperand { operator: Span, before: bool },
    UnexpectedOperator { operator: Span },

    IsStatement(Box<Stmt<Span>>),
    InvalidStatement(InvalidStatement<Span>),
    UnterminatedStatement { span: Span },
    InvalidForLoopInit(Stmt<Span>),
    
    DuplicateModifier { new: DeclMod<Span>, existing: DeclMod<Span> },
    
    CtorWithTypeArgs { span: Span },
    InvalidAssignmentExpr { span: Span },
    
    InvalidSetRangeExpr { span: Span },
    
    EmptyTypeParamList(TypeIdentList),
    EmptyTypeArgList(TypeArgList),
    InvalidTypeParamName(Span),
    
    EmptyWhereClause(WhereClause<TypeName>),
    TypeConstraintAlreadySpecified(TypeConstraint<TypeName>),
    NoMatchingParamForTypeConstraint(TypeConstraint<TypeName>),
    
    InvalidFunctionImplType(TypeName),
    EmptyConstOrVarDecl { span: Span },
    MultiVarDeclHasInitExpr { span: Span },
    
    EmptyTypeDecl { span: Span },
}

pub type ParseResult<T> = Result<T, TracedError<ParseError>>;

impl Spanned for ParseError {
    fn span(&self) -> &Span {
        match self {
            ParseError::InvalidUnitFilename(span) => span,
            ParseError::UnexpectedToken(tt, _) => match tt.as_ref() {
                TokenTree::Delimited(group) => group.open.span(),
                _ => tt.span(),
            },
            ParseError::UnexpectedEOF(_, tt) => tt.span(),
            ParseError::EmptyOperand { operator, .. } => operator.span(),
            ParseError::UnexpectedOperator { operator } => operator.span(),
            ParseError::IsStatement(stmt) => stmt.span(),
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
            ParseError::EmptyConstOrVarDecl { span, .. } => span,
            ParseError::MultiVarDeclHasInitExpr { span, .. } => span,
            ParseError::EmptyTypeDecl { span, .. } => span,
            ParseError::InvalidForLoopInit(stmt) => stmt.span(),
            ParseError::InvalidTypeParamName(span) => span,
            ParseError::InvalidSetRangeExpr { span } => span,
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::InvalidUnitFilename(..) => write!(f, "Invalid unit filename"),

            ParseError::UnexpectedToken(..) => write!(f, "Unexpected token"),
            ParseError::UnexpectedEOF(..) => write!(f, "Unexpected end of sequence"),
            ParseError::EmptyOperand { .. } => write!(f, "Empty operand"),
            ParseError::UnexpectedOperator { .. } => write!(f, "Unexpected operator"),

            ParseError::IsStatement(..) => write!(f, "Statement is not a valid expression"),
            ParseError::InvalidStatement(invalid) => write!(f, "{}", invalid.title()),
            ParseError::UnterminatedStatement { .. } => write!(f, "Unterminated stmt"),
            ParseError::InvalidForLoopInit( .. ) => write!(f, "Invalid for-loop initialization stmt"),
            
            ParseError::DuplicateModifier { .. } => write!(f, "Duplicate modifier"),
            ParseError::CtorWithTypeArgs { .. } => write!(f, "Constructor with type args"),

            ParseError::InvalidSetRangeExpr { .. } => write!(f, "Invalid range expression for set declaration"),
            
            ParseError::TypeConstraintAlreadySpecified(..) => write!(f, "Type constraint already specified"),
            ParseError::EmptyWhereClause(..) => write!(f, "Empty `where` clause"),

            ParseError::NoMatchingParamForTypeConstraint(..) => write!(f, "No matching parameter for type constraint"),
            ParseError::EmptyTypeParamList { .. } => write!(f, "Empty type parameter list"),
            ParseError::EmptyTypeArgList { .. } => write!(f, "Empty type argument list"),
            ParseError::InvalidTypeParamName( .. ) => write!(f, "Invalid type parameter name"),

            ParseError::InvalidFunctionImplType(..) => write!(f, "Invalid interface type for method"),
            ParseError::InvalidAssignmentExpr { .. } => write!(f, "Illegal assignment"),
            ParseError::EmptyConstOrVarDecl { .. } => write!(f, "Empty const or variable declaration"),
            ParseError::MultiVarDeclHasInitExpr { .. } => write!(f, "Multiple const or variable declaration has initialization expression"),
            ParseError::EmptyTypeDecl { .. } => write!(f, "Empty type declaration"),
        }
    }
}

impl DiagnosticOutput for ParseError {
    fn label(&self) -> Option<DiagnosticLabel> {
        let text = match self {
            ParseError::InvalidUnitFilename(..) => None,
            
            ParseError::UnexpectedToken(tt, Some(expected)) => {
                Some(format!("found {}, expected {}", tt, expected))
            }

            ParseError::UnexpectedToken(tt, None) => Some(format!("unexpected {}", tt)),

            ParseError::UnexpectedEOF(expected, _tt) => {
                Some(format!("expected {} but reached end of sequence", expected))
            }

            ParseError::EmptyOperand { operator, before } => {
                let pos_name = if *before { "before" } else { "after" };
                Some(format!("expected operand {} {}", pos_name, operator))
            }

            ParseError::UnexpectedOperator { .. } => Some("expected operand, found operator".to_string()),
            
            ParseError::IsStatement(..) => None,

            ParseError::InvalidStatement(invalid_stmt) => Some(invalid_stmt.to_string()),

            ParseError::DuplicateModifier { new, .. } => Some(format!(
                "the modifier `{}` is already present on this declaration",
                new.keyword(),
            )),
            
            ParseError::InvalidSetRangeExpr { .. } => None,

            ParseError::CtorWithTypeArgs { .. } => {
                Some("Object constructor expr cannot explicitly specify type args".to_string())
            }

            ParseError::TypeConstraintAlreadySpecified(c) => {
                Some(format!("parameter `{}` has more than one type constraint", c.name))
            }

            ParseError::EmptyTypeParamList { .. } => {
                Some("type parameter list must contain one or more identifiers".to_string())
            }

            ParseError::EmptyTypeArgList { .. } => {
                Some("type argument list must contain one or more type names".to_string())
            }

            ParseError::EmptyWhereClause(..) => {
                Some("`where` clause must contain one or more type constraints in the form `Type is InterfaceName`".to_string())
            }

            ParseError::NoMatchingParamForTypeConstraint(c) => {
                Some(format!("type constraint was specified for parameter `{}` which does not exist", c.name))
            }
            
            ParseError::InvalidTypeParamName(..) => None,

            ParseError::UnterminatedStatement { .. } => {
                Some("stmt here is unterminated".to_string())
            }

            ParseError::InvalidFunctionImplType(ty) => {
                Some(format!("type {} cannot have interface implementation functions declared for it", ty))
            }

            ParseError::InvalidAssignmentExpr { .. } => {
                Some("Assignment operator can only be used in an assignment stmt".to_string())
            }

            ParseError::EmptyTypeDecl { .. } => {
                Some("type declaration must contain one or more types".to_string())
            }

            ParseError::EmptyConstOrVarDecl { .. } => {
                Some("declaration must contain one or more constants".to_string())
            }
            
            ParseError::MultiVarDeclHasInitExpr { .. } => {
                Some("declaration with an initialization expression may only declare a single name".to_string())
            }

            ParseError::InvalidForLoopInit(stmt) => {
                Some(format!("stmt `{}` cannot be used to initialize the counter variable of a for-loop", stmt))
            }
        };

        Some(DiagnosticLabel {
            text,
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
