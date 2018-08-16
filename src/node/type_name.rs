use std::{
    rc::Rc,
    fmt,
};

use syntax::{
    TokenStream,
    ParseResult,
    Matcher,
    MatchOneOf,
    Parse,
    ParseError,
    ParsedContext,
    FunctionDecl,
    IndexRange,
    Expression,
};
use node::{
    Identifier,
    ToSource,
    FunctionModifier,
    FunctionSignature,
    ExpressionValue,
};
use tokens::{
    self,
    AsToken,
};
use keywords;
use operators;
use source;
use types::{
    Type,
    ArrayType,
};
use semantic::{
    self,
    Scope,
    SemanticResult,
    SemanticError,
    SemanticContext,
};

#[derive(Clone, Debug)]
pub enum TypeName {
    Scalar {
        context: ParsedContext,
        name: Identifier,
        indirection: usize,

        array_dimensions: Vec<IndexRange>,
    },
    Function {
        context: ParsedContext,
        return_type: Option<Box<TypeName>>,
        arg_types: Vec<TypeName>,
        modifiers: Vec<FunctionModifier>,
    },
    UntypedRef {
        context: ParsedContext,
    },
}

impl PartialEq for TypeName {
    fn eq(&self, other: &Self) -> bool {
        match self {
            TypeName::Scalar { name, indirection, array_dimensions, .. } => {
                match other {
                    TypeName::Scalar {
                        name: other_name,
                        indirection: other_indirection,
                        array_dimensions: other_dims,
                        ..
                    } => {
                        other_name == name
                            && other_indirection == indirection
                            && other_dims == array_dimensions
                    }

                    _ => false,
                }
            }

            TypeName::Function { return_type, arg_types, modifiers, .. } => {
                match other {
                    TypeName::Function {
                        return_type: other_return,
                        arg_types: other_args,
                        modifiers: other_mods,
                        ..
                    } => {
                        other_return == return_type
                            && other_args == arg_types
                            && other_mods == modifiers
                    }

                    _ => false,
                }
            }

            TypeName::UntypedRef { .. } => {
                match other {
                    TypeName::UntypedRef { .. } => true,
                    _ => false
                }
            }
        }
    }
}

impl ToSource for TypeName {
    fn to_source(&self) -> String {
        let mut result = String::new();
        match self {
            TypeName::UntypedRef { .. } => {
                result.push_str("<untyped ref>");
            }

            TypeName::Scalar { name, indirection, array_dimensions, .. } => {
                if array_dimensions.len() > 0 {
                    result.push_str("array [");
                    result.push_str(&array_dimensions.iter()
                        .map(|dim| format!("{}..{}", dim.from, dim.to))
                        .collect::<Vec<_>>()
                        .join(", "));
                    result.push_str("] of ");
                }

                for _ in 0..*indirection {
                    result.push_str("^");
                }

                result.push_str(&name.to_string())
            }

            TypeName::Function { return_type, arg_types, modifiers, .. } => {
                if return_type.is_some() {
                    result.push_str("function");
                } else {
                    result.push_str("procedure");
                }

                result.push_str(&format!("({})", &arg_types.iter()
                    .map(|arg_type| arg_type.to_source())
                    .collect::<Vec<_>>()
                    .join(", ")));

                if let Some(ty) = return_type {
                    result.push_str(": ");
                    result.push_str(&ty.to_source());
                }

                for modifier in modifiers {
                    result.push_str("; ");
                    result.push_str(&modifier.to_source());
                }
            }
        }

        result
    }
}

fn parse_as_data_type(tokens: &mut TokenStream, context: source::Token) -> ParseResult<TypeName> {
    let array_kw = tokens.look_ahead().match_one(keywords::Array);

    let array_dimensions = match array_kw {
        Some(_) => {
            tokens.advance(1);

            tokens.match_one(tokens::SquareBracketLeft)?;
            let mut dims = Vec::new();
            loop {
                let dim_expr: Expression = tokens.parse()?;
                let (from, to) = match dim_expr.value {
                    ExpressionValue::BinaryOperator { lhs, op: operators::RangeInclusive, rhs } => {
                        (lhs, rhs)
                    }
                    _ => {
                        /* todo: add specific ParseError for this
                           an expression here parsed successfully but it wasn't anything
                           in the form x..y, so we already know it's invalid before typechecking.
                           we store the from and to separately in IndexRange because we know we
                           can check this here
                        */
                        let context = dim_expr.context.token.clone();
                        let expected = operators::RangeInclusive;
                        return Err(ParseError::UnexpectedToken(context, Some(expected.into())));
                    }
                };

                dims.push(IndexRange {
                    from: *from,
                    to: *to,
                });

                // if there's a comma, there's another dimension following
                match tokens.look_ahead().match_one(tokens::Comma) {
                    Some(_) => tokens.advance(1),
                    None => break,
                }
            }

            tokens.match_one(tokens::SquareBracketRight)?;
            tokens.match_one(keywords::Of)?;

            dims
        }

        None => Vec::new(),
    };

    let mut indirection = 0;

    loop {
        let pointer_sigil = tokens.look_ahead().match_one(operators::Deref);
        if pointer_sigil.is_some() {
            indirection += 1;
            tokens.advance(1);
        } else {
            let name = Identifier::parse(tokens)?;

            break Ok(TypeName::Scalar {
                context: ParsedContext::from(context),
                name,
                indirection,

                array_dimensions,
            });
        }
    }
}

fn parse_as_function_type(tokens: &mut TokenStream, context: source::Token) -> ParseResult<TypeName> {
    let kind_kw = tokens.match_one(keywords::Function.or(keywords::Procedure))?;

    let arg_list = FunctionDecl::parse_argument_list(tokens)?;
    let arg_types = arg_list.into_iter()
        .map(|decl| decl.decl_type)
        .collect();

    let return_type = if kind_kw.is_keyword(keywords::Procedure) {
        None
    } else {
        tokens.match_one(tokens::Colon)?;
        let return_type: TypeName = tokens.parse()?;
        Some(Box::new(return_type))
    };

    let modifiers = FunctionDecl::parse_modifiers(tokens)?;

    Ok(TypeName::Function {
        context: ParsedContext::from(context),
        return_type,
        arg_types,
        modifiers,
    })
}

impl Parse for TypeName {
    fn parse(tokens: &mut TokenStream) -> ParseResult<TypeName> {
        let expected = keywords::Array
            .or(keywords::Function)
            .or(keywords::Procedure)
            .or(operators::Deref)
            .or(Matcher::AnyIdentifier);

        let match_name_first_token = tokens.look_ahead().match_one(expected.clone());

        match match_name_first_token {
            Some(ref t) if t.is_keyword(keywords::Function) || t.is_keyword(keywords::Procedure) => {
                parse_as_function_type(tokens, t.clone())
            }

            Some(t) => {
                parse_as_data_type(tokens, t)
            }

            None => {
                Err(ParseError::UnexpectedEOF(expected, tokens.context().clone()))
            }
        }
    }
}

impl TypeName {
    pub fn with_name(name: impl Into<Identifier>, context: impl Into<ParsedContext>) -> Self {
        TypeName::Scalar {
            context: context.into(),
            name: name.into(),
            indirection: 0,

            array_dimensions: Vec::new(),
        }
    }

    pub fn pointer(self) -> Self {
        match self {
            TypeName::Scalar { name, indirection, array_dimensions, context } =>
                TypeName::Scalar {
                    context,
                    name,
                    array_dimensions,

                    indirection: indirection + 1,
                },

            TypeName::UntypedRef { .. } =>
                unimplemented!("pointer to untyped ref"),

            TypeName::Function { .. } =>
                unimplemented!("pointer to function")
        }
    }

    pub fn resolve(&self, scope: Rc<Scope>) -> SemanticResult<Type> {
        match self {
            TypeName::Scalar { context, name, indirection, array_dimensions } => {
                let mut result = scope.get_type_alias(name)
                    .ok_or_else(|| {
                        let err_context = SemanticContext::new(context.token.clone(),
                                                               scope.clone());
                        SemanticError::unknown_type(name.clone(), err_context)
                    })?;

                for _ in 0..*indirection {
                    result = result.pointer();
                }

                if array_dimensions.len() > 0 {
                    result = Type::Array(ArrayType {
                        element: Box::new(result),
                        first_dim: semantic::IndexRange::annotate(&array_dimensions[0],
                                                                  scope.clone())?,
                        rest_dims: array_dimensions[1..].iter()
                            .map(|index_range| {
                                semantic::IndexRange::annotate(index_range, scope.clone())
                            })
                            .collect::<SemanticResult<_>>()?,
                    })
                }

                Ok(result)
            }

            TypeName::Function { return_type, arg_types, modifiers, .. } => {
                let sig = FunctionSignature {
                    arg_types: arg_types.iter()
                        .map(|arg_type| arg_type.resolve(scope.clone()))
                        .collect::<Result<_, _>>()?,
                    return_type: match return_type.as_ref() {
                        None => None,
                        Some(return_typename) => Some(return_typename.resolve(scope)?),
                    },
                    modifiers: modifiers.clone(),
                };

                Ok(Type::Function(Box::new(sig)))
            }

            TypeName::UntypedRef { .. } => {
                Ok(Type::UntypedRef)
            }
        }
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_str(&self.to_source())
    }
}