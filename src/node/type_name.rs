use std::fmt;

use syntax::{
    TokenStream,
    ParseResult,
    Matcher,
    MatchOneOf,
    Parse,
    ParseError,
    FunctionDecl,
};
use node::{
    Identifier,
    ToSource,
    FunctionModifier,
};
use tokens::{
    self,
    AsToken,
};
use keywords;
use operators;
use source;
use consts::IntConstant;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct IndexRange {
    pub from: i32,
    pub to: i32,
}

impl IndexRange {
    pub fn elements(&self) -> u32 {
        assert!(self.to >= self.from, "array upper bound must be >= lower bound");
        self.to.wrapping_sub(self.from) as u32
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeName {
    Scalar {
        name: Identifier,
        indirection: usize,

        array_dimensions: Vec<IndexRange>,
    },
    Function {
        return_type: Option<Box<TypeName>>,
        arg_types: Vec<TypeName>,
        modifiers: Vec<FunctionModifier>,
    },
}

impl ToSource for TypeName {
    fn to_source(&self) -> String {
        let mut result = String::new();
        match self {
            TypeName::Scalar { name, indirection, array_dimensions } => {
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

            TypeName::Function { return_type, arg_types, modifiers } => {
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

fn parse_as_data_type(tokens: &mut TokenStream) -> ParseResult<TypeName> {
    let array_kw = tokens.look_ahead().match_one(keywords::Array);
    let array_dimensions = match array_kw {
        Some(_) => {
            tokens.advance(1);

            let dims_groups = tokens.match_groups(tokens::SquareBracketLeft,
                                                  tokens::SquareBracketRight,
                                                  tokens::Comma)?;

            let dims = dims_groups.groups.into_iter()
                .map(|dim_group| {
                    let mut dim_tokens = TokenStream::new(dim_group.tokens, &dim_group.context);
                    let from = dim_tokens.match_one(Matcher::AnyLiteralInteger)
                        .and_then(int_token_to_array_dim)?;
                    dim_tokens.match_one(operators::RangeInclusive)?;
                    let to = dim_tokens.match_one(Matcher::AnyLiteralInteger)
                        .and_then(int_token_to_array_dim)?;

                    Ok(IndexRange {
                        from,
                        to,
                    })
                })
                .collect::<ParseResult<_>>()?;

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
                name,
                indirection,

                array_dimensions,
            });
        }
    }
}

fn parse_as_function_type(tokens: &mut TokenStream) -> ParseResult<TypeName> {
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
        return_type,
        arg_types,
        modifiers,
    })
}

impl Parse for TypeName {
    fn parse(tokens: &mut TokenStream) -> ParseResult<TypeName> {
        let match_name_first_token = keywords::Array
            .or(keywords::Function)
            .or(keywords::Procedure)
            .or(operators::Deref)
            .or(Matcher::AnyIdentifier);

        match tokens.look_ahead().match_one(match_name_first_token) {
            Some(ref t) if t.is_keyword(keywords::Function) || t.is_keyword(keywords::Procedure) => {
                parse_as_function_type(tokens)
            }

            _ => {
                parse_as_data_type(tokens)
            }
        }
    }
}

/* check int tokens used as array dimensions are int32s */
fn int_token_to_array_dim(token: source::Token) -> ParseResult<i32> {
    let val = match token.as_token() {
        tokens::LiteralInteger(IntConstant::I32(val)) => Some(*val),
        _ => None,
    };

    val.ok_or_else(|| ParseError::ArrayDimensionOutOfBounds(token))
}

impl TypeName {
    pub fn with_name(name: impl Into<Identifier>) -> Self {
        TypeName::Scalar {
            name: name.into(),
            indirection: 0,

            array_dimensions: Vec::new(),
        }
    }

    pub fn pointer(self) -> Self {
        match self {
            TypeName::Scalar { name, indirection, array_dimensions } =>
                TypeName::Scalar {
                    name,
                    array_dimensions,

                    indirection: indirection + 1,
                },

            TypeName::Function { .. } =>
                unimplemented!("pointer to function")
        }
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_str(&self.to_source())
    }
}