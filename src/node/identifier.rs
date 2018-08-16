use std::{
    fmt,
    iter::{self, FromIterator},
    vec,
};

use tokens::{self, AsToken};
use node::{self, Symbol};
use syntax::*;

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct Identifier {
    pub namespace: Vec<String>,
    pub name: String,
}

impl Symbol for Identifier {
    type Type = Self;
}

impl<'a> From<&'a str> for Identifier {
    fn from(from: &'a str) -> Self {
        let mut parts: Vec<String> = from
            .split('.')
            .map(|part: &str| part.to_owned())
            .collect();

        let name = parts.pop().unwrap_or(String::new());

        Identifier {
            namespace: parts,
            name,
        }
    }
}

impl<'a> From<&'a String> for Identifier {
    fn from(from: &'a String) -> Self {
        Identifier::from(from.as_str())
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.namespace.len() > 0 {
            write!(f, "{}.", self.namespace.join("."))?;
        }
        write!(f, "{}", self.name)
    }
}

impl IntoIterator for Identifier {
    type Item = String;
    type IntoIter = iter::Chain<vec::IntoIter<String>, iter::Once<String>>;

    fn into_iter(self) -> Self::IntoIter {
        self.namespace.into_iter()
            .chain(iter::once(self.name))
            .into_iter()
    }
}

impl FromIterator<String> for Identifier {
    fn from_iter<T: IntoIterator<Item=String>>(iter: T) -> Self {
        let mut parts = iter.into_iter();
        let mut last_part = String::new();
        let mut ns = Vec::new();

        let identifier = loop {
            match parts.next() {
                Some(next_part) => {
                    if last_part.len() > 0 {
                        ns.push(last_part);
                    }
                    last_part = next_part;
                }

                None => break Identifier {
                    name: last_part.clone(),
                    namespace: ns.clone(),
                }
            }
        };

        identifier
    }
}

impl Identifier {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<node::Identifier> {
        let mut parts = Vec::new();

        loop {
            match tokens.peek() {
                Some(ref id) if id.is_any_identifier() => {
                    parts.push(id.unwrap_identifier().to_owned());
                    tokens.next();
                }

                Some(unexpected) => return Err(ParseError::UnexpectedToken(
                    unexpected,
                    Some(Matcher::AnyIdentifier),
                )),

                //a name part always follows either a . or the beginning of the stream
                //and it should never be missing
                None => return Err(ParseError::UnexpectedEOF(
                    Matcher::AnyIdentifier,
                    tokens.context().clone(),
                ))
            }

            match tokens.peek() {
                //there's a dot, we expect another part, so keep going
                Some(ref period) if *period.as_token() == tokens::Period => {
                    tokens.next();
                }

                //anything else and stop parsing
                _ => break,
            }
        }

        assert!(parts.len() > 0, "must have at least one part in identifier after parsing");

        let name = parts.pop().unwrap();
        Ok(Identifier {
            name,
            namespace: parts,
        })
    }

    pub fn parts(&self) -> impl Iterator<Item=&String> {
        self.namespace.iter()
            .chain(iter::once(&self.name))
    }

    pub fn child(&self, child_name: &str) -> Identifier {
        let mut child_ns = self.namespace.clone();
        child_ns.push(self.name.clone());

        Identifier {
            name: child_name.to_owned(),
            namespace: child_ns,
        }
    }

    pub fn parent(&self) -> Option<Identifier> {
        if self.namespace.len() > 0 {
            let parent_namespace = self.namespace[0..self.namespace.len() - 1]
                .to_vec();

            let parent_name = self.namespace.last().unwrap().clone();

            Some(Identifier {
                namespace: parent_namespace,
                name: parent_name,
            })
        } else {
            None
        }
    }

    pub fn append(&self, other: &Identifier) -> Self {
        let mut result_ns = self.namespace.clone();
        result_ns.push(self.name.clone());
        result_ns.extend(other.namespace.clone());

        Identifier {
            name: other.name.clone(),
            namespace: result_ns,
        }
    }

    pub fn head(&self) -> String {
        self.namespace.first()
            .cloned()
            .unwrap_or_else(|| self.name.clone())
    }

    pub fn tail(&self) -> Option<Identifier> {
        if self.namespace.len() > 0 {
            Some(Identifier {
                name: self.name.clone(),
                namespace: self.namespace.iter().cloned().skip(1).collect(),
            })
        } else {
            None
        }
    }

    pub fn remove_namespace(&self, ns: &Identifier) -> Option<Identifier> {
        let mut ns_parts = ns.parts();
        let mut my_parts = self.parts();

        let mut result = Vec::new();

        loop {
            match (ns_parts.next(), my_parts.next()) {
                (Some(ns_part), Some(my_part)) => {
                    // check it matches but don't add it to the result
                    if ns_part != my_part {
                        break None;
                    }
                }

                // ran out of identifier parts to match to the namespace
                (Some(_), None) => break None,

                // ok, finished with the namespace and we have a name after that
                (None, Some(part_after_ns)) => {
                    result.push(part_after_ns.clone());
                }

                // reached end of this identifier, and the ns was complete
                (None, None) => {
                    let expected_len = (self.namespace.len() + 1)
                        - (ns.namespace.len() + 1);

                    break if result.len() != expected_len {
                        None
                    } else {
                        Some(Identifier::from_iter(result.into_iter()))
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tokenizer::*;
    use operators;

    fn try_parse_name(source: &str) -> ParseOutputResult<Identifier> {
        let tokens = tokenize("test", source).unwrap();
        let context = tokens[0].clone();

        Identifier::parse(tokens, &context)
    }

    fn parse_name(source: &str) -> Identifier {
        try_parse_name(source)
            .map(|out| out.value)
            .unwrap()
    }

    #[test]
    fn parses_simple_name() {
        let id = parse_name("String");
        assert_eq!("String", id.to_string());
    }

    #[test]
    fn parses_multipart_name() {
        let id = parse_name("System.Util.Something.Factory");

        assert_eq!("System.Util.Something.Factory", id.to_string());
    }

    #[test]
    fn fails_on_incomplete_name_with_extra_tokens() {
        let id = try_parse_name("System.Util. +");

        match id {
            Err(ParseError::UnexpectedToken(t, _)) =>
                assert_eq!(tokens::Operator(operators::Plus), *t.as_token()),
            Err(unexpected) => panic!("error should be UnexpectedToken when parsing incomplete name with extra tokens, was: {}", unexpected),
            Ok(unexpected) => panic!("incomplete name should not parse OK (was: {})", unexpected.value)
        }
    }

    #[test]
    fn fails_on_incomplete_name_with_eof() {
        let id = try_parse_name("System.Util.");

        match id {
            Err(ParseError::UnexpectedEOF(_, _)) => (),
            Err(unexpected) => panic!("error should be UnexpectedEOF when parsing incomplete name with unexpected EOF, was: {}", unexpected),
            Ok(unexpected) => panic!("incomplete name should not parse OK (was: {})", unexpected.value)
        }
    }

    #[test]
    fn next_tokens_is_correct_after_consuming_id() {
        let id = try_parse_name("System.String;")
            .unwrap();

        let after = id.next_tokens.collect::<Vec<_>>();
        assert_eq!(1, after.len());
        assert_eq!(tokens::Semicolon, *after[0].as_token());
    }
}