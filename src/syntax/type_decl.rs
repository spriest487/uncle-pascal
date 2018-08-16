use syntax::*;
use node::{self, Identifier, RecordKind};
use keywords;
use tokens::{self, AsToken};
use operators;

pub type RecordDecl = node::RecordDecl<ParsedSymbol, ParsedContext>;
pub type TypeDecl = node::TypeDecl<ParsedSymbol, ParsedContext>;

fn any_valid_type_decl_first() -> Matcher {
    keywords::Record
        .or(keywords::Class)
        .or(keywords::Function)
        .or(keywords::Procedure)
        .or(keywords::Array)
        .or(operators::Deref)
        .or(Matcher::AnyIdentifier)
}

impl Parse for Vec<TypeDecl> {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        tokens.match_one(keywords::Type)?;

        let mut decls = Vec::new();

        loop {
            let match_name = tokens.match_sequence(Matcher::AnyIdentifier
                .and_then(operators::Equals))?;

            let decl_name = match_name[0].unwrap_identifier();

            let peek_kind = tokens.match_peek(any_valid_type_decl_first())?;

            let type_decl = match peek_kind {
                Some(ref t) if t.is_keyword(keywords::Class) || t.is_keyword(keywords::Record) => {
                    let record_decl = RecordDecl::parse_with_name(decl_name, tokens)?;
                    node::TypeDecl::Record(record_decl)
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
            let next_identifier = tokens.match_peek(tokens::Semicolon.or(Matcher::AnyIdentifier))?;
            if next_identifier.is_none() {
                break Ok(decls);
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
        let mut decls = Vec::new();

        loop {
            /* we can have empty tokens in a field decl which is fine, it means
            there was a semicolon after the last field which we accept */
            if decls_tokens.peek().is_none() {
                break;
            }

            let decl: VarDecl = decls_tokens.parse()?;
            decls.push(decl);

            decls_tokens.match_or_endl(tokens::Semicolon)?;
        }

        //after the "end", there should always be a semicolon
        //TODO: this isn't necessary, this should be up to the unit
        tokens.match_or_endl(tokens::Semicolon)?;

        Ok(RecordDecl {
            name: Identifier::from(decl_name),
            kind,
            context: match_kw.clone().into(),
            members: decls,
        })
    }
}
