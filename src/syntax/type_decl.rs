use syntax::*;
use node::{self, Identifier};
use source;
use keywords;
use types::RecordKind;
use tokens::{self, AsToken};
use operators;

pub type RecordDecl = node::RecordDecl<ParsedSymbol>;
pub type TypeDecl = node::TypeDecl<ParsedSymbol>;

impl TypeDecl {
    pub fn parse(tokens: impl IntoIterator<Item=source::Token> + 'static,
                 context: &source::Token)
                 -> ParseResult<Self> {
        let match_name = keywords::Type
            .and_then(Matcher::AnyIdentifier)
            .and_then(operators::Equals)
            .match_sequence(tokens, context)?;

        let decl_name = match_name.value[1].unwrap_identifier();

        let peek_kind = keywords::Record.or(keywords::Class)
            .or(operators::Deref)
            .or(Matcher::AnyIdentifier)
            .match_peek(match_name.next_tokens, &match_name.last_token)?;

        let type_decl = match peek_kind.value {
            Some(ref t) if t.is_keyword(keywords::Class) || t.is_keyword(keywords::Record) => {
                RecordDecl::parse(decl_name, peek_kind.next_tokens, &peek_kind.last_token)?
                    .map(|record_decl| {
                        node::TypeDecl::Record(record_decl)
                    })
            }

            _ => {
                let alias_context = peek_kind.last_token.clone();

                ParsedType::parse(peek_kind.next_tokens, &peek_kind.last_token)?
                    .map(|aliased_type| {
                        node::TypeDecl::Alias {
                            alias: decl_name.to_string(),
                            of: aliased_type,
                            context: alias_context,
                        }
                    })
            }
        };

        Ok(type_decl)
    }
}

impl RecordDecl {
    pub fn parse<TIter>(decl_name: &str,
                        in_tokens: TIter,
                        context: &source::Token) -> ParseResult<Self>
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let match_kw = keywords::Record.or(keywords::Class)
            .match_one(in_tokens, context)?;

        let kind = if match_kw.value.is_keyword(keywords::Class) {
            RecordKind::Class
        } else {
            RecordKind::Record
        };

        let match_end = keywords::End.split_at_match(match_kw.next_tokens,
                                                     &match_kw.last_token)?;

        let mut decl_next_tokens = WrapIter::new(match_end.value.before_split.into_iter());
        let mut decl_last_token = match_kw.last_token;

        let mut decls = Vec::new();
        loop {
            /* we can have empty tokens in a field decl which is fine, it means
            there was a semicolon after the last field which we accept */
            let mut peek_decl_tokens = decl_next_tokens.peekable();
            if peek_decl_tokens.peek().is_none() {
                break;
            }

            let decl = VarDecl::parse(peek_decl_tokens, &decl_last_token)?;

            decls.push(decl.value);
            decl_last_token = decl.last_token;

            /* after each field decl we expect to find either a semicolon, a newline
             or the end of the decl tokens stream */
            let mut after_field_decl = decl.next_tokens.peekable();
            match after_field_decl.peek().cloned() {
                None => break,
                Some(token_after) => {
                    let skip = if token_after.is_token(&tokens::Semicolon) {
                        1
                    } else if token_after.is_any_identifier()
                        && token_after.location.line > decl_last_token.location.line {
                        0
                    } else {
                        let expected = Matcher::AnyIdentifier.or(tokens::Semicolon);
                        return Err(ParseError::UnexpectedToken(token_after, Some(expected)));
                    };

                    decl_last_token = token_after;
                    decl_next_tokens = WrapIter::new(after_field_decl.skip(skip));
                }
            }
        }

        //after the "end", there should always be a semicolon
        //TODO: this isn't necessary, this should be up to the unit
        let terminator = tokens::Semicolon.match_or_endl(match_end.next_tokens,
                                                         &match_end.last_token)?;

        let record = RecordDecl {
            name: Identifier::from(decl_name),
            kind,
            context: match_kw.value.clone(),
            members: decls,
        };

        Ok(ParseOutput::new(record, terminator.last_token, terminator.next_tokens))
    }
}
