use linked_hash_map::LinkedHashMap;

use syntax::*;
use node::{self, Identifier, RecordKind, TypeName};
use keywords;
use tokens::{self, AsToken};
use operators;

pub type TypeDecl = node::TypeDecl<ParsedContext>;
pub type RecordDecl = node::RecordDecl<ParsedContext>;
pub type RecordVariantPart = node::RecordVariantPart<ParsedContext>;
pub type RecordVariantCase = node::RecordVariantCase<ParsedContext>;
pub type RecordMember = node::RecordMember<ParsedContext>;
pub type EnumerationDecl = node::EnumerationDecl<ParsedContext>;
pub type SetDecl = node::SetDecl<ParsedContext>;
pub type InterfaceDecl = node::InterfaceDecl<ParsedContext>;

fn any_valid_type_decl_first() -> Matcher {
    keywords::Record
        .or(keywords::Class)
        .or(keywords::Interface)
        .or(keywords::Function)
        .or(keywords::Procedure)
        .or(keywords::Array)
        .or(operators::Deref)
        .or(Matcher::AnyIdentifier)
        .or(tokens::BracketLeft)
        .or(keywords::Set)
}

impl Parse for Vec<TypeDecl> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        tokens.match_one(keywords::Type)?;

        tokens.match_repeating(|i, tokens| {
            if i > 0 {
                /* decls after the first must be separated by a newline or ; */
                tokens.match_or_endl(tokens::Semicolon)?;
            }

            let next_identifier = tokens.look_ahead().match_one(Matcher::AnyIdentifier);
            if next_identifier.is_none() {
                return Ok(None);
            }

            let match_name = tokens.match_sequence(Matcher::AnyIdentifier
                .and_then(operators::Equals))?;

            let decl_name = match_name[0].unwrap_identifier();

            let peek_kind = tokens.look_ahead().match_one(any_valid_type_decl_first());

            let type_decl = match peek_kind {
                Some(ref t) if t.is_keyword(keywords::Class) || t.is_keyword(keywords::Record) => {
                    let record_decl = RecordDecl::parse_with_name(decl_name, tokens)?;
                    node::TypeDecl::Record(record_decl)
                }

                Some(ref t) if t.is_keyword(keywords::Set) => {
                    let set_decl = SetDecl::parse_with_name(decl_name, tokens)?;
                    node::TypeDecl::Set(set_decl)
                }

                Some(ref t) if t.is_token(&tokens::BracketLeft) => {
                    let enum_decl = EnumerationDecl::parse_with_name(decl_name, tokens)?;
                    node::TypeDecl::Enumeration(enum_decl)
                }

                Some(ref t) if t.is_token(&tokens::Keyword(keywords::Interface)) => {
                    let interface_decl = InterfaceDecl::parse_with_name(decl_name, tokens)?;
                    node::TypeDecl::Interface(interface_decl)
                }

                _ => {
                    let alias_context = tokens.context().clone();
                    let aliased_type = tokens.parse()?;

                    node::TypeDecl::Alias {
                        alias: decl_name.to_string(),
                        of: aliased_type,
                        context: alias_context.into(),
                    }
                }
            };

            Ok(Some(type_decl))
        })
    }
}

impl EnumerationDecl {
    fn parse_with_name(decl_name: impl ToString,
                       tokens: &mut TokenStream)
                       -> ParseResult<Self> {
        let first_token = tokens.context().clone();
        let names = Self::parse_names(tokens)?;

        Ok(EnumerationDecl {
            context: ParsedContext::from(first_token),
            names,
            name: decl_name.to_string(),
        })
    }

    fn parse_names(tokens: &mut TokenStream) -> ParseResult<Vec<String>> {
        tokens.match_one(tokens::BracketLeft)?;

        let names = tokens.match_repeating(|i, name_tokens| {
            if i > 0 {
                match name_tokens.look_ahead().match_one(tokens::Comma) {
                    Some(_comma) => name_tokens.advance(1),
                    None => return Ok(None),
                }
            }

            let next_name = name_tokens.match_one(Matcher::AnyIdentifier)?
                .unwrap_identifier()
                .to_string();

            Ok(Some(next_name))
        })?;

        tokens.match_one(tokens::BracketRight)?;

        Ok(names)
    }
}

impl SetDecl {
    fn parse_with_name(name: impl ToString,
                       tokens: &mut TokenStream)
                       -> ParseResult<Self> {
        let kws = tokens.match_sequence(Matcher::Keyword(keywords::Set)
            .and_then(keywords::Of))?;

        let context = ParsedContext::from(kws[0].clone());

        let expected_first = tokens::BracketLeft.or(Matcher::AnyIdentifier);
        let enumeration_first = tokens.look_ahead()
            .match_one(expected_first.clone());

        match enumeration_first {
            Some(ref t) if t.is_any_identifier() => {
                let enum_name = Identifier::parse(tokens)?;
                let enumeration = node::SetEnumeration::Named(enum_name);
                Ok(node::SetDecl {
                    name: name.to_string(),
                    context,
                    enumeration,
                })
            }

            Some(ref t) if t.is_token(&tokens::BracketLeft) => {
                let names = EnumerationDecl::parse_names(tokens)?;
                let enumeration = node::SetEnumeration::Inline(names);

                Ok(node::SetDecl {
                    name: name.to_string(),
                    context,
                    enumeration,
                })
            }

            Some(unexpected) => {
                Err(ParseError::UnexpectedToken(unexpected, Some(expected_first)))
            }

            None => {
                Err(ParseError::UnexpectedEOF(expected_first, kws[1].clone()))
            }
        }
    }
}

impl RecordDecl {
    fn parse_variant_part(tokens: &mut TokenStream,
                          terminator: impl Into<Matcher>)
                          -> ParseResult<RecordVariantPart> {
        let terminator = terminator.into();

        let context = tokens.match_one(keywords::Case)?;

        let tag_name = tokens.match_one(Matcher::AnyIdentifier)?;

        tokens.match_one(tokens::Colon)?;

        let tag_type = TypeName::parse(tokens)?;
        let tag = RecordMember {
            name: tag_name.unwrap_identifier().to_string(),
            decl_type: tag_type,
            context: tag_name.into(),
        };

        tokens.match_one(keywords::Of)?;

        // parse variant cases
        // todo: it's legal to nest variant cases
        let cases = tokens.match_repeating(|case_num, tokens: &mut TokenStream| {
            let tag_value = match case_num {
                0 => tokens.parse()?,
                _ => {
                    if tokens.look_ahead().match_one(terminator.clone()).is_some() {
                        return Ok(None);
                    }

                    tokens.match_or_endl(tokens::Semicolon)?;
                    match Expression::parse(tokens) {
                        Err(_) => {
                            // no more cases
                            return Ok(None);
                        }

                        Ok(tag_val) => tag_val,
                    }
                }
            };

            tokens.match_one(tokens::Colon)?;
            tokens.match_one(tokens::BracketLeft)?;

            let members = Self::parse_members(tokens, tokens::BracketRight)?;

            tokens.match_one(tokens::BracketRight)?;
            tokens.match_or_endl(tokens::Semicolon)?;

            Ok(Some(RecordVariantCase {
                tag_value,
                members,
            }))
        })?;

        Ok(RecordVariantPart {
            tag,
            context: context.into(),
            cases,
        })
    }

    fn parse_members(tokens: &mut TokenStream,
                     terminator: impl Into<Matcher>)
                     -> ParseResult<Vec<RecordMember>> {
        let terminator = terminator.into();

        /* handle empty records (which is a semantic error, but should still parse OK) */
        if tokens.look_ahead().match_one(terminator.clone()).is_some() {
            return Ok(Vec::new());
        }

        tokens.match_repeating(|decl_num, tokens: &mut TokenStream| {
            let name = match decl_num {
                0 => tokens.match_one(Matcher::AnyIdentifier)?,
                _ => {
                    //found terminator instead of separator, finish here
                    if tokens.look_ahead().match_one(terminator.clone()).is_some() {
                        return Ok(None);
                    }

                    tokens.match_or_endl(tokens::Semicolon)?;
                    
                    // found terminator after separator
                    if tokens.look_ahead().match_one(terminator.clone()).is_some() {
                        return Ok(None);
                    }

                    tokens.match_one(Matcher::AnyIdentifier)?
                }
            };

            tokens.match_one(tokens::Colon)?;
            let type_name = TypeName::parse(tokens)?;

            Ok(Some(node::RecordMember {
                name: name.unwrap_identifier().to_string(),
                decl_type: type_name,
                context: ParsedContext::from(name),
            }))
        })
    }

    pub fn parse_with_name(decl_name: &str, tokens: &mut TokenStream) -> ParseResult<Self> {
        let match_kw = tokens.match_one(keywords::Record.or(keywords::Class))?;

        let kind = if match_kw.is_keyword(keywords::Class) {
            RecordKind::Class
        } else {
            RecordKind::Record
        };

        let members = Self::parse_members(tokens, keywords::End.or(keywords::Case))?;
        let variant_part = match tokens.look_ahead().match_one(keywords::Case) {
            Some(_) => Some(Self::parse_variant_part(tokens, keywords::End)?),
            None => None
        };

        tokens.match_one(keywords::End)?;

        tokens.match_or_endl(tokens::Semicolon)?;

        Ok(RecordDecl {
            name: decl_name.to_string(),
            kind,
            context: match_kw.clone().into(),
            members,
            variant_part,
        })
    }
}

impl InterfaceDecl {
    pub fn parse_with_name(name: &str, tokens: &mut TokenStream) -> ParseResult<InterfaceDecl> {
        let context = tokens.match_one(keywords::Interface)?;

        let mut functions = LinkedHashMap::new();
        loop {
            if functions.len() > 0 {
                if tokens.look_ahead().match_one(keywords::End).is_some() {
                    break;
                }

                tokens.match_or_endl(tokens::Semicolon)?;
            }

            let match_func = keywords::Function
                .or(keywords::Procedure);

            match tokens.look_ahead().match_one(match_func) {
                Some(_) => {
                    let decl = FunctionDecl::parse(tokens)?;

                    if functions.contains_key(&decl.name) {
                        return Err(ParseError::DuplicateName(decl.name, decl.context.token));
                    }

                    if decl.args.len() < 1 {
                        return Err(ParseError::MissingInterfaceArgs(decl.context.token));
                    }

                    let sig = decl.signature();
                    functions.insert(decl.name, sig);
                },
                None => break,
            }
        }

        tokens.match_one(keywords::End)?;

        Ok(InterfaceDecl {
            name: name.to_string(),
            methods: functions.into_iter().collect(),
            context: ParsedContext::from(context),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use opts::CompileOptions;

    fn parse(source: &str) -> Vec<TypeDecl> {
        let opts = CompileOptions::default();

        let stream = TokenStream::tokenize("type_decl tests", source, &opts)
            .unwrap();

        stream.parse_to_end().unwrap()
    }

    fn parse_record(source: &str) -> RecordDecl {
        let decls = parse(source);
        assert_eq!(1, decls.len(), "result should be a single record declaration");

        match decls.into_iter().next().unwrap() {
            node::TypeDecl::Record(record) => record,
            bad @ _ => panic!("expected record, got {:?}", bad)
        }
    }

    #[test]
    fn parses_empty_record() {
        let record = parse_record("type Car = record end");
        assert_eq!("Car", &record.name);
        assert_eq!(RecordKind::Record, record.kind);
        assert_eq!(0, record.members.len());
    }

    #[test]
    fn parses_record_fields_with_no_terminator() {
        let record = parse_record("type Car = record wheels: Int32 end");
        assert_eq!("Car", &record.name);
        assert_eq!(RecordKind::Record, record.kind);
        assert_eq!(1, record.members.len());
    }

    #[test]
    fn parses_record_fields_with_terminating_endl() {
        let record = parse_record(r"type Car = record wheels: Int32
        end");
        assert_eq!("Car", &record.name);
        assert_eq!(RecordKind::Record, record.kind);
        assert_eq!(1, record.members.len());
    }

    #[test]
    fn parses_record_fields_with_terminating_semicolon() {
        let record = parse_record("type Car = record wheels: Int32; end");
        assert_eq!("Car", &record.name);
        assert_eq!(RecordKind::Record, record.kind);
        assert_eq!(1, record.members.len());
    }

    #[test]
    fn parses_record_fields_with_terminating_semicolon_and_endl() {
        let record = parse_record(r"type Car = record
            wheels: Int32;
         end");
        assert_eq!("Car", &record.name);
        assert_eq!(RecordKind::Record, record.kind);
        assert_eq!(1, record.members.len());
    }
}
