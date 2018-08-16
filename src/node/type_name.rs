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
    FunctionModifier,
    FunctionSignature,
    FunctionArgSignature,
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
pub struct ScalarTypeName {
    pub name: Identifier,
    pub context: ParsedContext,
    pub indirection: usize,
}

#[derive(Clone, Debug)]
pub enum TypeName {
    Scalar(ScalarTypeName),
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
        args: Vec<FunctionArgSignature<TypeName>>,
        modifiers: Vec<FunctionModifier>,
    },
    UntypedRef {
        context: ParsedContext,
    },
}

impl PartialEq for TypeName {
    fn eq(&self, other: &Self) -> bool {
        match self {
            TypeName::Scalar(ScalarTypeName { name, indirection, .. }) => {
                match other {
                    TypeName::Scalar(ScalarTypeName { name: other_name, indirection: other_indirection, .. }) => {
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

            TypeName::Function { return_type, args, modifiers, .. } => {
                match other {
                    TypeName::Function {
                        return_type: other_return,
                        args: other_args,
                        modifiers: other_mods,
                        ..
                    } => {
                        other_return == return_type
                            && other_args == args
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

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeName::UntypedRef { .. } => {
                write!(f, "var")
            }

            TypeName::Scalar(ScalarTypeName { name, indirection, .. }) => {
                for _ in 0..*indirection {
                    write!(f, "^")?;
                }

                write!(f, "{}", name)
            }

            TypeName::Array { element, dimensions, .. } => {
                write!(f, "array [")?;
                for (i, dim) in dimensions.iter().enumerate() {
                    write!(f, "{}..{}", dim.from, dim.to)?;
                    if i < dimensions.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "] of {}", element)
            }

            TypeName::DynamicArray { element, .. } => {
                write!(f, "array of {}", element)
            }

            TypeName::Function { return_type, args, modifiers, .. } => {
                f.write_str("function")?;

                if args.len() > 0 {
                    f.write_str("(")?;
                    for (i, arg) in args.iter().enumerate() {
                        write!(f, "{}", arg)?;
                        if i < args.len() - 1 {
                            f.write_str("; ")?;
                        }
                    }
                    f.write_str(")")?;
                }

                if let Some(ty) = return_type {
                    f.write_str(": ")?;
                    write!(f, "{}", ty)?;
                }

                for modifier in modifiers {
                    write!(f, "; {}", modifier)?;
                }

                Ok(())
            }
        }
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

            break Ok(TypeName::Scalar(ScalarTypeName {
                context: ParsedContext::from(context.unwrap()),
                name,
                indirection,
            }));
        }
    }
}

fn parse_as_function_type(tokens: &mut TokenStream, context: source::Token) -> ParseResult<TypeName> {
    let kind_kw = tokens.match_one(keywords::Function.or(keywords::Procedure))?;

    let arg_list = FunctionDecl::parse_argument_list(tokens)?;
    let args = arg_list.into_iter()
        .map(|decl| FunctionArgSignature {
            modifier: decl.modifier,
            decl_type: decl.decl_type,
        })
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
        args,
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
        TypeName::Scalar(ScalarTypeName {
            context: context.into(),
            name: name.into(),
            indirection: 0,
        })
    }

    pub fn as_scalar(&self) -> Option<&ScalarTypeName> {
        match self {
            | TypeName::Scalar(scalar) => Some(scalar),
            | _ => None,
        }
    }

    pub fn context(&self) -> &ParsedContext {
        match self {
            | TypeName::UntypedRef { context } => context,
            | TypeName::Scalar(scalar) => &scalar.context,
            | TypeName::Function { context, .. } => context,
            | TypeName::DynamicArray { context, .. } => context,
            | TypeName::Array { context, .. } => context,
        }
    }

    pub fn pointer(self) -> Self {
        match self {
            TypeName::Scalar(ScalarTypeName { name, indirection, context }) =>
                TypeName::Scalar(ScalarTypeName {
                    context,
                    name,
                    indirection: indirection + 1,
                }),

            TypeName::DynamicArray { .. } |
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
            TypeName::Scalar(ScalarTypeName { context, name, indirection }) => {
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

            TypeName::Function { return_type, args, modifiers, .. } => {
                let sig = FunctionSignature {
                    args: args.iter()
                        .map(|arg| {
                            let decl_type = arg.decl_type.resolve(scope.clone())?;
                            Ok(FunctionArgSignature {
                                decl_type,
                                modifier: arg.modifier.clone(),
                            })
                        })
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
