use std::fmt;

use tokens::{self, AsToken};
use node::{self, Symbol};
use syntax::*;
use source;

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct Identifier {
    pub namespace: Vec<String>,
    pub name: String,
}

impl Symbol for Identifier {
    type Type = Self;
}

impl<'a, TIntoStr> From<TIntoStr> for Identifier
    where TIntoStr: Into<&'a str>
{
    fn from(from: TIntoStr) -> Self {
        let mut parts: Vec<String> = from.into()
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
    type IntoIter = ::std::iter::Chain<::std::vec::IntoIter<Self::Item>, ::std::vec::IntoIter<Self::Item>>;

    fn into_iter(self) -> Self::IntoIter {
        self.namespace.into_iter()
            .chain(vec![self.name])
            .into_iter()
    }
}

impl Identifier {
    pub fn parse<TIter>(iter: TIter,
                        context: &source::Token)
                        -> ParseResult<node::Identifier>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let mut last_token = context.clone();
        let mut next_tokens = WrapIter::new(iter.into_iter());
        let mut parts = Vec::new();

        loop {
            let mut next_peekable = next_tokens.peekable();
            match next_peekable.peek().cloned() {
                Some(ref id) if id.is_any_identifier() => {
                    parts.push(id.unwrap_identifier().to_owned());

                    next_peekable.next();
                    last_token = id.clone();
                }

                Some(unexpected) => return Err(ParseError::UnexpectedToken(
                    unexpected,
                    Some(Matcher::AnyIdentifier),
                )),

                //a name part always follows either a . or the beginning of the stream
                //and it should never be missing
                None => return Err(ParseError::UnexpectedEOF(
                    Matcher::AnyIdentifier,
                    last_token,
                ))
            }

            let done = match next_peekable.peek().cloned() {
                //there's a dot, we expect another part, so keep going
                Some(ref period) if *period.as_token() == tokens::Period => {
                    last_token = period.clone();
                    next_peekable.next();
                    false
                },

                //anything else and stop parsing
                _ => true,
            };

            next_tokens = WrapIter::new(next_peekable);

            if done {
                break;
            }
        }

        assert!(parts.len() > 0, "must have at least one part in identifier after parsing");

        let name = parts.pop().unwrap();
        let id = Identifier {
            name,
            namespace: parts
        };

        Ok(ParseOutput::new(id, last_token, next_tokens))
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
}

#[cfg(test)]
mod test {
    use super::*;
    use tokenizer::*;
    use operators;

    fn try_parse_name(source: &str) -> ParseResult<Identifier> {
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