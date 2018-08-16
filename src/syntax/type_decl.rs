use syntax::*;
use node::{self, Identifier};
use keywords;
use types::RecordKind;
use tokens::{self, AsToken};
use operators;

pub type RecordDecl = node::RecordDecl<ParsedSymbol>;
pub type TypeDecl = node::TypeDecl<ParsedSymbol>;

impl TypeDecl {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Vec<Self>> {
        tokens.match_one(keywords::Type)?;

        let mut decls = Vec::new();

        loop {
            let match_name = tokens.match_sequence(Matcher::AnyIdentifier
                .and_then(operators::Equals))?;

            let decl_name = match_name[0].unwrap_identifier();

            let peek_kind = tokens.match_peek(keywords::Record.or(keywords::Class)
                .or(operators::Deref)
                .or(Matcher::AnyIdentifier))?;

            let type_decl = match peek_kind {
                Some(ref t) if t.is_keyword(keywords::Class) || t.is_keyword(keywords::Record) => {
                    let record_decl = RecordDecl::parse(decl_name, tokens)?;
                    node::TypeDecl::Record(record_decl)
                }

                _ => {
                    let alias_context = tokens.context().clone();
                    let aliased_type = ParsedType::parse(tokens)?;

                    node::TypeDecl::Alias {
                        alias: decl_name.to_string(),
                        of: aliased_type,
                        context: alias_context,
                    }
                }
            };

            decls.push(type_decl);

            /* the decl must be terminated either by a semicolon or a newline */
            let separator = tokens.match_or_endl(tokens::Semicolon)?;

            /* but if the token after that is another identifier, there's another decl
            in this type decl block */
            let next_identifier = tokens.match_peek(tokens::Semicolon.or(Matcher::AnyIdentifier))?;
            if next_identifier.is_none() {
                break Ok(decls);
            }
        }
    }
}

impl RecordDecl {
    fn parse(decl_name: &str, tokens: &mut TokenStream) -> ParseResult<Self> {
        let match_kw = tokens.match_one(keywords::Record.or(keywords::Class))?;

        let kind = if match_kw.is_keyword(keywords::Class) {
            RecordKind::Class
        } else {
            RecordKind::Record
        };

        let match_end = tokens.split_at_match(keywords::End)?;

        let mut decls_tokens = TokenStream::new(match_end.before_split, &match_kw);
        let mut decls = Vec::new();

        loop {
            /* we can have empty tokens in a field decl which is fine, it means
            there was a semicolon after the last field which we accept */
            if decls_tokens.peek().is_none() {
                break;
            }

            let decl = VarDecl::parse(&mut decls_tokens)?;
            decls.push(decl);

            /* after each field decl we expect to find either a semicolon, a newline
             or the end of the decl tokens stream */
            match decls_tokens.peek() {
                None => break,
                Some(token_after) => {
                    let skip = if token_after.is_token(&tokens::Semicolon) {
                        1
                    } else if token_after.is_any_identifier()
                        && token_after.location.line > decls_tokens.context().location.line {
                        0
                    } else {
                        let expected = Matcher::AnyIdentifier.or(tokens::Semicolon);
                        return Err(ParseError::UnexpectedToken(token_after, Some(expected)));
                    };
                }
            }
        }

        //after the "end", there should always be a semicolon
        //TODO: this isn't necessary, this should be up to the unit
        tokens.match_or_endl(tokens::Semicolon)?;

        Ok(RecordDecl {
            name: Identifier::from(decl_name),
            kind,
            context: match_kw.clone(),
            members: decls,
        })
    }
}
