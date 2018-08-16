use syntax::*;
use node;
use keywords;
use tokens::{self, AsToken};
use operators;

pub type RecordDecl = node::RecordDecl<node::Identifier>;

impl RecordDecl {
    pub fn parse<TIter>(in_tokens: TIter, context: &source::Token) -> ParseResult<Self>
        where TIter: IntoIterator<Item=source::Token> + 'static
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
            name: type_name,
            context: match_name.value[0].clone(),
            members: decls,
        };

        Ok(ParseOutput::new(record, terminator.last_token, terminator.next_tokens))
    }
}

impl node::ToSource for RecordDecl {
    fn to_source(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("type {} = record", self.name));

        for member in self.members.iter() {
            lines.push(format!("\t{}: {};", member.name, member.decl_type));
        }

        lines.push("end".to_owned());
        lines.join("\n")
    }
}