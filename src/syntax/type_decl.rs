use syntax::*;
use node::{self, Identifier, RecordKind};
use keywords;
use tokens::{self, AsToken};
use operators;

pub type RecordDecl = node::RecordDecl<ParsedSymbol, ParsedContext>;
pub type TypeDecl = node::TypeDecl<ParsedSymbol, ParsedContext>;
pub type EnumerationDecl = node::EnumerationDecl<ParsedContext>;

fn any_valid_type_decl_first() -> Matcher {
    keywords::Record
        .or(keywords::Class)
        .or(keywords::Function)
        .or(keywords::Procedure)
        .or(keywords::Array)
        .or(operators::Deref)
        .or(Matcher::AnyIdentifier)
        .or(tokens::BracketLeft)
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

                Some(ref t) if t.is_token(&tokens::BracketLeft) => {
                    let enum_decl =  EnumerationDecl::parse_with_name(decl_name, tokens)?;
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
    fn parse_with_name(decl_name: &str, tokens: &mut TokenStream) -> ParseResult<Self> {
        let name_token_groups = tokens.match_groups(tokens::BracketLeft,
                                                    tokens::BracketRight,
                                                    tokens::Comma)?;
        let names = name_token_groups.groups.into_iter()
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
            .collect::<ParseResult<_>>()?;

        Ok(EnumerationDecl {
            context: ParsedContext::from(name_token_groups.open),
            names,
            name: Identifier::from(decl_name),
        })
    }
}

//impl Parse for SetDecl {
//    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
//        tokens.match_sequence(Matcher::Keyword(keywords::Set)
//            .and_then(keywords::Of))?;
//
//        let enumeration_first = tokens.look_ahead()
//            .match_one(tokens::BracketLeft.or(Matcher::AnyIdentifier));
//
//        let set_type = match enumeration_first.map(|t| t.as_token()) {
//            Some(tokens::Identifier(_)) => {
//                let type_id = Identifier::parse(tokens)?;
//                TypeName::SetOf(type_id)
//            }
//
//            _ => {
//                let enumeration = parse_enumeration(tokens)?;
//                TypeName::SetOfEnumeration(enumeration)
//            }
//        };
//
//        Ok(set_type)
//    }
//}

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
            name: Identifier::from(decl_name),
            kind,
            context: match_kw.clone().into(),
            members,
        })
    }
}
