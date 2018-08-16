use std::fmt;

use syntax::{TokenStream, ParseResult, Matcher, Parse};
use node::{Identifier, ToSource};
use tokens::{self, AsToken};
use keywords;
use operators;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct IndexRange {
    pub from: isize,
    pub to: isize,
}

impl IndexRange {
    pub fn len(&self) -> usize {
        //todo: can this overflow
        (self.to - self.from) as usize
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeName {
    pub name: Identifier,
    pub indirection: usize,

    pub array_dimensions: Vec<IndexRange>,
}

impl ToSource for TypeName {
    fn to_source(&self) -> String {
        self.to_string()
    }
}

impl Parse for TypeName {
    fn parse(tokens: &mut TokenStream) -> ParseResult<TypeName> {
        let array_kw = tokens.match_peek(keywords::Array)?;
        let array_dimensions = match array_kw {
            Some(_) => {
                tokens.advance(1);

                let dims_groups = tokens.match_groups(tokens::SquareBracketLeft,
                                                      tokens::SquareBracketRight,
                                                      tokens::Comma)?;

                let dims = dims_groups.groups.into_iter()
                    .map(|dim_group| {
                        let mut dim_tokens = TokenStream::new(dim_group.tokens, &dim_group.context);
                        let dim_from = dim_tokens.match_one(Matcher::AnyLiteralInteger)?;
                        dim_tokens.match_one(tokens::Period)?;
                        dim_tokens.match_one(tokens::Period)?;
                        let dim_to = dim_tokens.match_one(Matcher::AnyLiteralInteger)?;

                        Ok(IndexRange {
                            from: dim_from.unwrap_literal_integer() as isize,
                            to: dim_to.unwrap_literal_integer() as isize,
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
            let pointer_sigil = tokens.match_peek(operators::Deref)?;
            if pointer_sigil.is_some() {
                indirection += 1;
                tokens.advance(1);
            } else {
                let name = Identifier::parse(tokens)?;

                break Ok(TypeName {
                    name,
                    indirection,

                    array_dimensions,
                });
            }
        }
    }
}

impl TypeName {
    pub fn with_name(name: impl Into<Identifier>) -> Self {
        TypeName {
            name: name.into(),
            indirection: 0,

            array_dimensions: Vec::new(),
        }
    }

    pub fn pointer(self) -> Self {
        TypeName {
            name: self.name,
            indirection: self.indirection + 1,

            array_dimensions: self.array_dimensions,
        }
    }
}

impl fmt::Display for TypeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for _ in 0..self.indirection {
            f.write_str("^")?;
        }

        self.name.fmt(f)
    }
}