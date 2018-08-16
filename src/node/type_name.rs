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
    DynamicArrayType,
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
    },
    Array {
        context: ParsedContext,
        element: Box<TypeName>,
        dimensions: Vec<IndexRange>,
    },
    DynamicArray {
        context: ParsedContext,
        element: Box<TypeName>,
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
            TypeName::Scalar { name, indirection, .. } => {
                match other {
                    TypeName::Scalar { name: other_name, indirection: other_indirection, .. } => {
                        other_name == name && other_indirection == indirection
                    }

                    _ => false,
                }
            }

            TypeName::DynamicArray { element, .. } => {
                match other {
                    TypeName::DynamicArray { element: other_element, .. } => {
                        element == other_element
                    }
                    _ => false,
                }
            }

            TypeName::Array { element, dimensions, .. } => {
                match other {
                    TypeName::Array { element: other_element, dimensions: other_dims, .. } => {
                        element == other_element && dimensions == other_dims
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

            TypeName::Scalar { name, indirection, .. } => {
                for _ in 0..*indirection {
                    result.push_str("^");
                }

                result.push_str(&name.to_string())
            }

            TypeName::Array { element, dimensions, .. } => {
                result.push_str("array [");
                result.push_str(&dimensions.iter()
                    .map(|dim| format!("{}..{}", dim.from, dim.to))
                    .collect::<Vec<_>>()
                    .join(", "));
                result.push_str("] of ");
                result.push_str(&element.to_source());
            }

            TypeName::DynamicArray { element, .. } => {
                result = format!("array of {}", element.to_source());
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

fn parse_as_array(tokens: &mut TokenStream) -> ParseResult<TypeName> {
    let context = ParsedContext::from(tokens.match_one(keywords::Array)?);

    match tokens.look_ahead().match_one(tokens::SquareBracketLeft) {
        Some(_) => {
            // fixed size array
            tokens.match_one(tokens::SquareBracketLeft)?;
            let dimensions = tokens.match_repeating(|i, tokens: &mut TokenStream| {
                if i > 0 {
                    match tokens.look_ahead().match_one(tokens::Comma) {
                        Some(_) => tokens.advance(1),
                        None => return Ok(None),
                    }
                }

                let dim_expr = Expression::parse(tokens)?;
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

                Ok(Some(IndexRange {
                    from: *from,
                    to: *to,
                }))
            })?;
            tokens.match_one(tokens::SquareBracketRight)?;
            tokens.match_one(keywords::Of)?;

            let element = Box::new(parse_as_data_type(tokens)?);

            Ok(TypeName::Array {
                dimensions,
                element,
                context,
            })
        }

        None => {
            // dynamic array
            tokens.match_one(keywords::Of)?;
            let element = Box::new(parse_as_data_type(tokens)?);

            Ok(TypeName::DynamicArray {
                element,
                context,
            })
        }
    }
}

fn parse_as_data_type(tokens: &mut TokenStream) -> ParseResult<TypeName> {
    let mut indirection = 0;

    let context = tokens.look_ahead().next();

    loop {
        let pointer_sigil = tokens.look_ahead().match_one(operators::Deref);
        if pointer_sigil.is_some() {
            indirection += 1;
            tokens.advance(1);
        } else {
            let name = Identifier::parse(tokens)?;

            break Ok(TypeName::Scalar {
                context: ParsedContext::from(context.unwrap()),
                name,
                indirection,
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

            Some(ref t) if t.is_keyword(keywords::Array) => {
                parse_as_array(tokens)
            }

            Some(_) => {
                parse_as_data_type(tokens)
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
        }
    }

    pub fn pointer(self) -> Self {
        match self {
            TypeName::Scalar { name, indirection, context } =>
                TypeName::Scalar {
                    context,
                    name,
                    indirection: indirection + 1,
                },

            TypeName::DynamicArray { ..} |
            TypeName::Array { .. } =>
                unimplemented!("pointer to array"),

            TypeName::UntypedRef { .. } =>
                unimplemented!("pointer to untyped ref"),

            TypeName::Function { .. } =>
                unimplemented!("pointer to function")
        }
    }

    pub fn resolve(&self, scope: Rc<Scope>) -> SemanticResult<Type> {
        match self {
            TypeName::Scalar { context, name, indirection } => {
                let result = scope.get_type_alias(name)
                    .ok_or_else(|| {
                        let err_context = SemanticContext::new(context.token.clone(),
                                                               scope.clone());
                        SemanticError::unknown_type(name.clone(), err_context)
                    })?;

                let indirected = (0..*indirection).into_iter()
                    .fold(result, |result, _| {
                        result.pointer()
                    });

                Ok(indirected)
            }

            TypeName::Array { element, dimensions, .. } => {
                let element = Box::new(element.resolve(scope.clone())?);

                let first_dim = semantic::IndexRange::annotate(
                    &dimensions[0], scope.clone())?;

                let rest_dims = dimensions[1..].iter()
                    .map(|index_range| {
                        semantic::IndexRange::annotate(index_range, scope.clone())
                    })
                    .collect::<SemanticResult<_>>()?;

                Ok(Type::Array(ArrayType {
                    element,
                    first_dim,
                    rest_dims,
                }))
            }

            TypeName::DynamicArray { element, .. } => {
                let element = Box::new(element.resolve(scope.clone())?);

                Ok(Type::DynamicArray(DynamicArrayType {
                    element,
                }))
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