use crate::{
    ast::{Block, Expression, Statement},
    parse::prelude::*,
};

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum CondPattern<A: Annotation> {
    Positive {
        is_ty: A::Type,
        binding: Option<Ident>,

        span: Span,
    },
    Negative {
        is_not_ty: A::Type,
        span: Span,
    },
}

impl<A: Annotation> fmt::Display for CondPattern<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CondPattern::Negative { is_not_ty, .. } => write!(f, "is not {}", is_not_ty),
            CondPattern::Positive { is_ty, binding, .. } => {
                write!(f, "is {}", is_ty)?;
                if let Some(binding) = binding {
                    write!(f, " {}", binding)?;
                }
                Ok(())
            }
        }
    }
}

impl<A: Annotation> Spanned for CondPattern<A> {
    fn span(&self) -> &Span {
        match self {
            CondPattern::Negative { span, .. } => span,
            CondPattern::Positive { span, .. } => span,
        }
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct IfCond<A: Annotation> {
    pub cond: Expression<A>,

    pub is_pattern: Option<CondPattern<A>>,

    pub then_branch: Expression<A>,
    pub else_branch: Option<Expression<A>>,
    pub annotation: A,
}

fn parse_branch_expr(tokens: &mut TokenStream) -> ParseResult<Expression<Span>> {
    match Statement::parse(tokens) {
        Ok(stmt) => Ok(Expression::from(Block::single_stmt(stmt))),

        Err(TracedError {
            err: ParseError::InvalidStatement(InvalidStatement(expr)),
            ..
        }) => Ok(expr),

        Err(err) => Err(err),
    }
}

impl IfCond<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let if_token = tokens.match_one(Keyword::If)?;
        let cond = Expression::parse(tokens)?;

        let is_pattern = match tokens.look_ahead().match_one(Keyword::Is) {
            Some(is_kw) => {
                tokens.advance(1);

                let pattern = match tokens.look_ahead().match_one(Operator::Not) {
                    Some(_not_kw) => {
                        tokens.advance(1);
                        let is_not_ty = TypeName::parse(tokens)?;
                        let span = is_kw.span().to(is_not_ty.span());

                        CondPattern::Negative { is_not_ty, span }
                    }

                    None => {
                        let is_ty = TypeName::parse(tokens)?;

                        let binding_ahead = tokens.look_ahead().match_one(Matcher::AnyIdent);
                        let (binding, span) = match binding_ahead {
                            Some(ident) => {
                                tokens.advance(1);

                                let span = is_kw.span().to(ident.span());
                                (Some(ident.into_ident().unwrap()), span)
                            }

                            None => (None, is_kw.span().to(is_ty.span())),
                        };

                        CondPattern::Positive {
                            is_ty,
                            span,
                            binding,
                        }
                    }
                };

                Some(pattern)
            }

            None => None,
        };

        tokens.match_one(Keyword::Then)?;
        let then_branch = parse_branch_expr(tokens)?;

        let (else_branch, span) = match tokens.look_ahead().match_one(Keyword::Else) {
            Some(_else_token) => {
                tokens.advance(1);
                let else_branch = parse_branch_expr(tokens)?;
                let span = if_token.span().to(else_branch.annotation());

                (Some(else_branch), span)
            }

            None => (None, if_token.span().to(then_branch.annotation())),
        };

        Ok(IfCond {
            cond,
            is_pattern,
            then_branch,
            else_branch,
            annotation: span,
        })
    }
}

impl<A: Annotation> fmt::Display for IfCond<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} ", self.cond)?;

        if let Some(is_pattern) = &self.is_pattern {
            write!(f, "{}", is_pattern)?;
        }

        write!(f, " then {}", self.then_branch)?;

        if let Some(else_branch) = &self.else_branch {
            write!(f, " else {}", else_branch)
        } else {
            Ok(())
        }
    }
}

impl<A: Annotation> Spanned for IfCond<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expression::test::parse_expr;

    fn parse_if_cond(src: &str) -> IfCond<Span> {
        match parse_expr(src) {
            Expression::IfCond(if_cond) => *if_cond,
            expr => panic!("expected expression to be an if condition, got: {:?}", expr),
        }
    }

    #[test]
    fn parses_without_is_pattern() {
        let cond = parse_if_cond("if x then y");
        assert!(cond.is_pattern.is_none());
    }

    #[test]
    fn parses_with_is_pattern() {
        let cond = parse_if_cond("if x is String then y");
        assert!(cond.is_pattern.is_some());

        match cond.is_pattern.as_ref().unwrap() {
            CondPattern::Positive { is_ty, binding, .. } => {
                assert_eq!("String", is_ty.to_string());
                assert!(binding.is_none());
            }

            _ => panic!("expected positive binding"),
        }
    }

    #[test]
    fn parses_with_is_not_pattern() {
        let cond = parse_if_cond("if x is not String then y");
        assert!(cond.is_pattern.is_some());

        match cond.is_pattern.as_ref().unwrap() {
            CondPattern::Negative { is_not_ty, .. } => {
                assert_eq!("String", is_not_ty.to_string());
            }

            _ => panic!("expected negative binding"),
        }
    }

    #[test]
    fn parses_with_is_pattern_and_binding() {
        let cond = parse_if_cond("if x is String s then y");
        assert!(cond.is_pattern.is_some());

        match cond.is_pattern.as_ref().unwrap() {
            CondPattern::Positive { is_ty, binding, .. } => {
                assert_eq!("String", is_ty.to_string());
                assert_eq!(
                    Some("s".to_string()),
                    binding.as_ref().map(|b| b.to_string())
                );
            }

            _ => panic!("expected positive binding"),
        }
    }
}
