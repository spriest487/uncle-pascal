use syntax::*;
use tokens;
use keywords;
use tokens::AsToken;
use node;

pub type Expression = node::Expression<node::Identifier>;
pub type ExpressionResult = Result<Expression, ParseError>;

impl Expression {
    fn parse_binary_op<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static,
    {
        let split_at_op = Matcher::AnyBinaryOperator
            .split_at_match(in_tokens, context)?;

        let lhs_expr = Expression::parse(split_at_op.value.before_split, context)?;
        let op = split_at_op.value.split_at.unwrap_binary_operator().clone();
        let rhs_expr = Expression::parse(split_at_op.next_tokens, &split_at_op.last_token)?;

        Ok(Expression::binary_op(lhs_expr, op, rhs_expr, split_at_op.value.split_at.clone()))
    }

    fn parse_identifier<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let identifier = Matcher::AnyIdentifier
            .match_one(in_tokens, context)?
            .finish()?;

        let name = identifier.as_token().unwrap_identifier();

        Ok(Expression::identifier(node::Identifier::parse(name),
                                  context.clone()))
    }

    fn parse_literal_string<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let string_token = Matcher::AnyLiteralString
            .match_one(in_tokens, context)?
            .finish()?;

        let string_value = string_token.as_token().unwrap_literal_string();

        Ok(Expression::literal_string(string_value, context.clone()))
    }

    fn parse_literal_integer<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static,
    {
        let integer_token = Matcher::AnyLiteralInteger
            .match_one(in_tokens, context)?
            .finish()?;

        Ok(Expression::literal_int(integer_token.as_token().unwrap_literal_integer(),
                                   context.clone()))
    }

    fn parse_function_call<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let func_name = Matcher::AnyIdentifier.match_one(in_tokens, context)?;

        let args = tokens::BracketLeft.terminated_by(tokens::BracketRight)
            .match_groups(tokens::Comma, func_name.next_tokens, &func_name.last_token)?;

        let all_args = args.value.groups
            .into_iter()
            .map(|arg_group| {
                Expression::parse(arg_group.tokens, &arg_group.context)
            })
            .collect::<Result<Vec<_>, _>>();

        let call_target = node::Identifier::parse(func_name.value.as_token().unwrap_identifier());

        Ok(Expression::function_call(call_target,
                                     all_args?,
                                     func_name.value.clone()))
    }

    fn parse_if<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let if_kw = keywords::If.match_one(in_tokens, context)?;

        let cond_tokens = keywords::Then.match_until(if_kw.next_tokens, &if_kw.last_token)?;
        let condition = Expression::parse(cond_tokens.value, &if_kw.last_token)?;

        let match_then_else = keywords::Then.terminated_by(keywords::Else);

        let peek_then_else = match_then_else.match_block_peek(cond_tokens.next_tokens,
                                                              &cond_tokens.last_token)?;

        if let Some(_) = peek_then_else.value {
            let then_else = match_then_else.match_block(peek_then_else.next_tokens,
                                                        &peek_then_else.last_token)?;

            let then_branch = Expression::parse(then_else.value.inner, &then_else.value.open)?;
            let else_branch = Expression::parse(then_else.next_tokens, &then_else.value.close)?;

            Ok(Expression::if_then_else(condition,
                                        then_branch,
                                        Some(else_branch),
                                        if_kw.value.clone()))
        } else {
            let then = keywords::Then.match_one(peek_then_else.next_tokens,
                                                &peek_then_else.last_token)?;
            let then_branch = Expression::parse(then.next_tokens, &then.last_token)?;

            Ok(Expression::if_then_else(condition, then_branch, None, if_kw.value))
        }
    }

    fn parse_for_loop<TIter>(in_tokens: TIter, context: &source::Token)
                             -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let for_do_pair = keywords::For.terminated_by(keywords::Do)
            .match_block(in_tokens, context)?;

        /* can't nest for loops in either the from or the to expression, so
        it's safe just to look for the next "to" */
        let split_at_to = keywords::To.split_at_match(for_do_pair.value.inner,
                                                      &for_do_pair.value.open)?;

        let from_expr = Expression::parse(split_at_to.value.before_split,
                                          &for_do_pair.value.open)?;
        let to_expr = Expression::parse(split_at_to.next_tokens,
                                        &split_at_to.value.split_at)?;

        let body_expr = Expression::parse(for_do_pair.next_tokens,
                                          &for_do_pair.last_token)?;

        Ok(Expression::for_loop(from_expr, to_expr, body_expr, for_do_pair.value.open))
    }

    pub fn parse<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        /* this matcher should cover anything which can appear at the start of an expr */
        let match_expr_start = Matcher::AnyKeyword
            .or(Matcher::AnyBinaryOperator)
            .or(Matcher::AnyIdentifier)
            .or(Matcher::AnyLiteralInteger)
            .or(Matcher::AnyLiteralString)
            .or(tokens::BracketLeft);

        let all_tokens = in_tokens.into_iter().collect::<Vec<_>>();

        //always try to parse it as a binary operation first
        Expression::parse_binary_op(all_tokens.clone(), context).or_else(|_| {
            let mut expr_first = match_expr_start.match_peek(all_tokens, context)?;

            match expr_first.value {
                Some(ref if_kw) if if_kw.as_token().is_keyword(keywords::If) => {
                    Expression::parse_if(expr_first.next_tokens,
                                         &expr_first.last_token)
                }

                Some(ref for_kw) if for_kw.is_keyword(keywords::For) => {
                    Expression::parse_for_loop(expr_first.next_tokens,
                                               &expr_first.last_token)
                }

                Some(ref begin_kw) if begin_kw.is_keyword(keywords::Begin) => {
                    let block = Block::parse(expr_first.next_tokens,
                                             &expr_first.last_token)?
                        .finish()?;

                    Ok(Expression::block(block))
                }

                Some(ref identifier) if identifier.is_any_identifier() => {
                    let identifier_tokens = expr_first.next_tokens.collect::<Vec<_>>();
                    let last_token = expr_first.last_token;

                    Expression::parse_function_call(identifier_tokens.clone(),
                                                    &last_token)
                        .or_else(|_| Expression::parse_identifier(identifier_tokens,
                                                                  &last_token))
                }

                Some(ref s) if s.is_any_literal_string() => {
                    Expression::parse_literal_string(expr_first.next_tokens,
                                                     &expr_first.last_token)
                }

                Some(ref i) if i.is_any_literal_int() => {
                    Expression::parse_literal_integer(expr_first.next_tokens,
                                                      &expr_first.last_token)
                }

                _ => {
                    let unexpected = expr_first.next_tokens.next().unwrap().clone();
                    Err(ParseError::UnexpectedToken(unexpected, Some(match_expr_start)))
                }
            }
        })
    }
}

impl node::ToSource for Expression {
    fn to_source(&self) -> String {
        match &self.value {
            &node::ExpressionValue::BinaryOperator { ref lhs, ref op, ref rhs } => {
                format!("({} {} {})", lhs.to_source(), op, rhs.to_source())
            }

            &node::ExpressionValue::Identifier(ref id) => format!("{}", id),

            &node::ExpressionValue::FunctionCall { ref target, ref args } => {
                let args_str = args.iter()
                    .map(|arg| arg.to_source())
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{}({})", target, args_str)
            }

            &node::ExpressionValue::LiteralInteger(i) => format!("{}", i),

            &node::ExpressionValue::LiteralString(ref s) =>
                format!("'{}'", tokens::LiteralString(s.clone()).to_source()),

            &node::ExpressionValue::If { ref condition, ref then_branch, ref else_branch } => {
                let mut lines = Vec::new();
                lines.push(format!("if {} then", condition.to_source()));
                lines.push(format!("\t{}", then_branch.to_source()));

                if let &Some(ref else_expr) = else_branch {
                    lines.push(format!("else\n\t{}", else_expr.to_source()))
                }

                lines.join("\n")
            }

            &node::ExpressionValue::Block(ref block) => {
                block.to_source()
            }

            &node::ExpressionValue::ForLoop { ref from, ref to, ref body } => {
                format!("for {} to {} do {}", from.to_source(), to.to_source(), body.to_source())
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tokenizer::*;

    fn parse_expr(src: &str) -> Expression {
        let context = SourceToken {
            token: tokens::Keyword(keywords::Begin),
            location: SourceLocation::new("test", 0, 0),
        };

        let tokens = tokenize("test", src).unwrap();
        let expr = Expression::parse(tokens, &context);

        assert!(expr.is_ok(), "expression source `{}` should parse correctly", src);

        expr.unwrap()
    }

    #[test]
    fn parses_nested_function_calls() {
        let expr = parse_expr("test(hello('world'), goodbye(cruel('world')))");

        assert!(expr.is_function_call(), "result should be a function call expr");
        let (test_id, test_args) = expr.unwrap_function_call();

        assert_eq!(node::Identifier::parse("test"), test_id);
        assert_eq!(2, test_args.len());

        let hello_func = test_args[0].clone();
        assert!(hello_func.is_function_call(), "first argument should be a function call expr");
        let (hello_id, hello_args) = hello_func.unwrap_function_call();
        assert_eq!(node::Identifier::parse("hello"), hello_id);
        assert_eq!(1, hello_args.len());
        assert!(hello_args[0].is_literal_string());
        assert_eq!("world", hello_args[0].clone().unwrap_literal_string());

        let goodbye_func = test_args[1].clone();
        assert!(goodbye_func.is_function_call(), "second argument should be a function call expr");
        let (goodbye_id, goodbye_args) = goodbye_func.unwrap_function_call();
        assert_eq!(node::Identifier::parse("goodbye"), goodbye_id);
        assert_eq!(1, goodbye_args.len());
        assert!(goodbye_args[0].is_function_call());

        let (cruel_id, cruel_args) = goodbye_args[0].clone().unwrap_function_call();
        assert_eq!(node::Identifier::parse("cruel"), cruel_id);
        assert_eq!(1, cruel_args.len());
        assert!(cruel_args[0].is_literal_string());
        assert_eq!("world", cruel_args[0].clone().unwrap_literal_string());
    }
}