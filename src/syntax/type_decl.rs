use syntax::*;
use node;
use keywords;
use tokens::{self, AsToken};
use operators;

pub type RecordDecl = node::RecordDecl<node::Identifier>;

impl RecordDecl {
    pub fn parse<TIter>(in_tokens: TIter,
                    context: &TIter::Item)
                    -> ParseResult<Self, TIter::Item>
        where TIter: IntoIterator + 'static,
              TIter::Item: tokens::AsToken + 'static
    {
        let match_name = keywords::Type
            .and_then(Matcher::AnyIdentifier)
            .and_then(operators::Equals)
            .and_then(keywords::Record)
            .match_sequence(in_tokens, context)?;

        let type_name = match_name.value[1].unwrap_identifier().to_owned();

        let match_end = keywords::End.split_at_match(match_name.next_tokens,
                                                     &match_name.last_token)?;

        let mut decl_next_tokens = WrapIter::new(match_end.value.before_split.into_iter());
        let mut decl_last_token = match_name.last_token;

        let mut decls = Vec::new();
        loop {
            let decl = VarDecl::parse(decl_next_tokens, &decl_last_token)?;

            decls.push(decl.value);

            /* after each decl we expect to find either a semicolon or the end of the
            decl tokens stream */
            let mut peek_after_decl = decl.next_tokens.peekable();
            match peek_after_decl.peek().cloned() {
                Some(after_decl) => {
                    if after_decl.as_token().eq(&tokens::Semicolon) {
                        //ok, skip semicolon and move on
                        peek_after_decl.next();

                        //if there's EOF after the semicolon stop now
                        if peek_after_decl.peek().is_none() {
                            break
                        }
                        else {
                            decl_next_tokens = WrapIter::new(peek_after_decl);
                            decl_last_token = after_decl;
                        }
                    } else {
                        let expected = tokens::Semicolon.or(Matcher::AnyIdentifier);

                        return Err(ParseError::UnexpectedToken(after_decl, Some(expected)))
                    }
                },

                //done
                None => break,
            }
        }

        //after the "end", there should always be a semicolon
        //TODO: this isn't necessary, this should be up to the unit
        let terminator = tokens::Semicolon.match_one(match_end.next_tokens,
                                                     &match_end.last_token)?;

        let record = RecordDecl {
            name: type_name,
            members: decls,
        };

        Ok(ParseOutput::new(record, terminator.last_token, terminator.next_tokens))
    }
}