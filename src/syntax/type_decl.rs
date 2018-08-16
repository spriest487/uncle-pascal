use syntax::*;
use node::{self, Identifier, RecordKind};
use keywords;
use tokens::{self, AsToken};
use operators;

pub type TypeDecl = node::TypeDecl<ParsedContext>;
pub type RecordDecl = node::RecordDecl<ParsedContext>;
pub type EnumerationDecl = node::EnumerationDecl<ParsedContext>;
pub type SetDecl = node::SetDecl<ParsedContext>;

fn any_valid_type_decl_first() -> Matcher {
    keywords::Record
        .or(keywords::Class)
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

        let mut decls = Vec::new();

        loop {
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

            decls.push(type_decl);

            /* the decl must be terminated either by a semicolon or a newline */
            tokens.match_or_endl(tokens::Semicolon)?;

            /* but if the token after that is another identifier, there's another decl
            in this type decl block */
            let next_identifier = tokens.look_ahead().match_one(tokens::Semicolon.or(Matcher::AnyIdentifier));
            if next_identifier.is_none() {
                break Ok(decls);
            }
        }
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
        let name_token_groups = tokens.match_groups(tokens::BracketLeft,
                                                    tokens::BracketRight,
                                                    tokens::Comma)?;

        name_token_groups.groups.into_iter()
            .map(|name_group| {
                if name_group.tokens.len() > 1 {
                    Err(ParseError::UnexpectedToken(name_group.tokens[1].clone(),
                                                    Some(tokens::Comma.into())))
                } else if !name_group.tokens[0].is_any_identifier() {
                    Err(ParseError::UnexpectedToken(name_group.tokens[0].clone(),
                                                    Some(Matcher::AnyIdentifier)))
                } else {
                    Ok(name_group.tokens[0].unwrap_identifier().to_string())
                }
            })
            .collect()
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
    fn parse_with_name(decl_name: &str, tokens: &mut TokenStream) -> ParseResult<Self> {
        let match_kw = tokens.match_one(keywords::Record.or(keywords::Class))?;

        let kind = if match_kw.is_keyword(keywords::Class) {
            RecordKind::Class
        } else {
            RecordKind::Record
        };

        let match_end = tokens.split_at_match(keywords::End)?;

        let mut decls_tokens = TokenStream::new(match_end.before_split, &match_kw);
        let members: Vec<VarDecl> = decls_tokens.parse()?;
        decls_tokens.finish()?;

        //after the "end", there should always be a semicolon
        //TODO: this isn't necessary, this should be up to the unit
        tokens.match_or_endl(tokens::Semicolon)?;

        Ok(RecordDecl {
            name: decl_name.to_string(),
            kind,
            context: match_kw.clone().into(),
            members,
        })
    }
}
