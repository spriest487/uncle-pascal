use syntax::*;
use tokens;
use keywords;
use tokens::AsToken;
use node;
use operators;

pub type Expression = node::Expression<node::Identifier>;
pub type ExpressionResult = Result<ParseOutput<Expression>, ParseError>;

impl Expression {
    fn binary_op_by_precedence(operands: Vec<Expression>,
                               operators: Vec<operators::BinaryOperator>) -> Expression {
        assert!(operators.len() > 0);
        assert_eq!(operands.len(),
                   operators.len() + 1,
                   "operands: {:?}, operators: {:?}",
                   operands.iter().map(|expr| expr.to_string()).collect::<Vec<_>>(),
                   operators.iter().map(|op| op.to_string()).collect::<Vec<_>>());

        let (lo_op_pos, lo_op) = operators.iter()
            .enumerate()
            .max_by_key(|&(_, op)| op.precedence())
            .unwrap();

        //operands after
        let lhs = if lo_op_pos == 0 {
            operands[0].clone()
        }
        else {
            let operands_before = operands.iter().take(lo_op_pos + 1).cloned();
            let operators_before = operators.iter().take(lo_op_pos).cloned();
            Expression::binary_op_by_precedence(operands_before.collect(),
                                                operators_before.collect())
        };

        let rhs = if lo_op_pos == operators.len() - 1 {
            operands[operands.len() - 1].clone()
        } else {
            let operands_after = operands.iter().skip(lo_op_pos + 1).cloned();
            let operators_after = operators.iter().skip(lo_op_pos + 1).cloned();
            Expression::binary_op_by_precedence(operands_after.collect(),
                                                operators_after.collect())
        };

        Expression::binary_op(lhs, lo_op.clone(), rhs, operands[0].context.clone())
    }

    fn parse_binary_op<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static,
    {
        let maybe_open_bracket = tokens::BracketLeft.match_peek(in_tokens, context)?;
        match &maybe_open_bracket.value {
            &Some(_) => {
                let lhs_group = tokens::BracketLeft
                    .terminated_by(tokens::BracketRight)
                    .match_block(maybe_open_bracket.next_tokens,
                                 &maybe_open_bracket.last_token)?;

                let lhs_expr = Expression::parse(lhs_group.value.inner,
                                                 &lhs_group.value.open)?
                    .finish()?;

                let match_op = Matcher::AnyBinaryOperator.match_one(lhs_group.next_tokens,
                                                                    &lhs_group.last_token)?;

                let rhs_expr = Expression::parse(match_op.next_tokens, &match_op.last_token)?;

                let op_expr = Expression::binary_op(lhs_expr,
                                                    match_op.value.unwrap_binary_operator().clone(),
                                                    rhs_expr.value,
                                                    match_op.value);

                Ok(ParseOutput::new(op_expr, rhs_expr.last_token, rhs_expr.next_tokens))
            }
            &None => {
                /* group tokens into operators and operands - we don't know where this expression
                 will end so we need to take the entire stream */
                let all_tokens : Vec<_> = maybe_open_bracket.next_tokens.collect();

                let match_operand_groups = tokens::BracketLeft
                    .terminated_by(tokens::BracketRight)
                    .match_groups_inner(Matcher::AnyBinaryOperator,
                                        all_tokens.clone(),
                                        &maybe_open_bracket.last_token)?;

                let mut next_tokens = Vec::new();
                let mut last_token = match_operand_groups.last_token.clone();

                let operand_groups = match_operand_groups.finish()?;

                let mut operands = Vec::new();
                let mut operators = Vec::new();

                for (i, operand) in operand_groups.groups.into_iter().enumerate() {
                    if i > 0 {
                        let sep_token = &operand_groups.separators[i - 1];
                        let op = sep_token.unwrap_binary_operator().clone();
                        operators.push(op);
                    }

                    let operand_expr = Expression::parse(operand.tokens, &operand.context)?;

                    let operand_last_token = operand_expr.last_token;
                    /* if the operand expression finishes before the end of the operand tokens,
                    there's another expression after this operation in the stream starting at that
                    point */
                    let mut peek_after_operand = operand_expr.next_tokens.peekable();

                    operands.push(operand_expr.value);

                    if peek_after_operand.peek().is_some() {
                        /* collect all the tokens for everything past this operand in the stream */
                        next_tokens = all_tokens.into_iter()
                            .filter(|t| {
                                let operand_location = &operand_last_token.location;

                                t.location.col > operand_location.col ||
                                    t.location.line > operand_location.line
                            })
                            .collect();

                        last_token = operand_last_token;

                        break;
                    }
                }

                let expr = Expression::binary_op_by_precedence(operands, operators);

                Ok(ParseOutput::new(expr, last_token, next_tokens))

                /* we need to look at all ops in the token sequence and find
                the lowest-priority one to do first, the higher-priority ones
                 will be nested exprs on one side of the op */
//                let (op_pos, op) = tokens.iter()
//                    .enumerate()
//                    .filter(|&(_, token)| token.is_any_binary_operator())
//                    .map(|(pos, token)| (pos, token.unwrap_binary_operator().clone()))
//                    .max_by_key(|&(_, ref op)| op.precedence())
//                    .ok_or(ParseError::UnexpectedEOF(Matcher::AnyBinaryOperator,
//                                                     maybe_open_bracket.last_token.clone()))?;
//
//                /* TODO this needs to break on the first expression that doesn't consume
//                all the tokens in its operand */
//                let lhs_tokens: Vec<_> = tokens.iter().take(op_pos).cloned().collect();
//                let lhs_expr = Expression::parse(lhs_tokens, &maybe_open_bracket.last_token)?
//                    .finish()?;
//
//                let rhs_tokens: Vec<_> = tokens.iter().skip(op_pos + 1).cloned().collect();
//                let rhs_expr = Expression::parse(rhs_tokens, &tokens[op_pos])?;
//
//                let op_expr = Expression::binary_op(lhs_expr,
//                                                    op,
//                                                    rhs_expr.value,
//                                                    tokens[op_pos].clone());
//
//                Ok(ParseOutput::new(op_expr, rhs_expr.last_token, rhs_expr.next_tokens))
            }
        }
    }

    fn parse_identifier<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let match_id = Matcher::AnyIdentifier.match_one(in_tokens, context)?;

        Ok(match_id.map(|id_token| {
            let name = id_token.unwrap_identifier();

            Expression::identifier(node::Identifier::parse(name), context.clone())
        }))
    }

    fn parse_literal_string<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let match_str = Matcher::AnyLiteralString.match_one(in_tokens, context)?;

        Ok(match_str.map(|str_token| {
            let s = str_token.unwrap_literal_string();
            Expression::literal_string(s, context.clone())
        }))
    }

    fn parse_literal_integer<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static,
    {
        let match_int = Matcher::AnyLiteralInteger.match_one(in_tokens, context)?;

        Ok(match_int.map(|integer_token| {
            let i = integer_token.unwrap_literal_integer();
            Expression::literal_int(i, context.clone())
        }))
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
                Expression::parse(arg_group.tokens, &arg_group.context)?.finish()
            })
            .collect::<Result<Vec<_>, _>>();

        let call_target = node::Identifier::parse(func_name.value.as_token()
            .unwrap_identifier());

        let call_expr = Expression::function_call(call_target,
                                                  all_args?,
                                                  func_name.value.clone());

        Ok(ParseOutput::new(call_expr, args.last_token, args.next_tokens))
    }

    fn parse_if<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let if_kw = keywords::If.match_one(in_tokens, context)?;

        let cond_tokens = keywords::Then.match_until(if_kw.next_tokens, &if_kw.last_token)?;
        let condition = Expression::parse(cond_tokens.value,
                                          &if_kw.last_token)?.finish()?;

        let match_then_else = keywords::Then.terminated_by(keywords::Else);

        let peek_then_else = match_then_else.match_block_peek(cond_tokens.next_tokens,
                                                              &cond_tokens.last_token)?;

        if let Some(_) = peek_then_else.value {
            let then_else = match_then_else.match_block(peek_then_else.next_tokens,
                                                        &peek_then_else.last_token)?;

            let then_branch = Expression::parse(then_else.value.inner,
                                                &then_else.value.open)?.finish()?;
            let else_branch = Expression::parse(then_else.next_tokens,
                                                &then_else.value.close)?;

            let if_expr = Expression::if_then_else(condition,
                                                   then_branch,
                                                   Some(else_branch.value),
                                                   if_kw.value.clone());
            Ok(ParseOutput::new(if_expr, else_branch.last_token, else_branch.next_tokens))
        } else {
            let then = keywords::Then.match_one(peek_then_else.next_tokens,
                                                &peek_then_else.last_token)?;
            let then_branch = Expression::parse(then.next_tokens, &then.last_token)?;

            let if_expr = Expression::if_then_else(condition, then_branch.value, None, if_kw.value);

            Ok(ParseOutput::new(if_expr, then_branch.last_token, then_branch.next_tokens))
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
                                          &for_do_pair.value.open)?.finish()?;

        let to_expr = Expression::parse(split_at_to.next_tokens,
                                        &split_at_to.value.split_at)?.finish()?;

        let body_expr = Expression::parse(for_do_pair.next_tokens,
                                          &for_do_pair.last_token)?;

        let for_loop = Expression::for_loop(from_expr,
                                            to_expr,
                                            body_expr.value,
                                            for_do_pair.value.open);

        Ok(ParseOutput::new(for_loop, body_expr.last_token, body_expr.next_tokens))
    }

    pub fn parse<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        /* this matcher should cover anything which can appear at the start of an expr */
        let match_expr_start = Matcher::AnyKeyword
            .or(Matcher::AnyIdentifier)
            .or(Matcher::AnyLiteralInteger)
            .or(Matcher::AnyLiteralString)
            .or(tokens::BracketLeft);

        let all_tokens: Vec<source::Token> = in_tokens.into_iter().collect();

        let expr_first = match_expr_start.match_peek(all_tokens, context)?;
        //peeking moves all tokens, let's just put that back...
        let all_tokens: Vec<source::Token> = expr_first.next_tokens.collect();

        let contains_binary_op = all_tokens.iter()
            .any(|t| t.is_any_binary_operator());

        match expr_first.value {
            Some(ref if_kw) if if_kw.as_token().is_keyword(keywords::If) => {
                Expression::parse_if(all_tokens, context)
            }

            Some(ref for_kw) if for_kw.is_keyword(keywords::For) => {
                Expression::parse_for_loop(all_tokens, context)
            }

            Some(ref begin_kw) if begin_kw.is_keyword(keywords::Begin) => {
                let parsed_block = Block::parse(all_tokens, context)?;
                Ok(parsed_block.map(|block| {
                    Expression::block(block)
                }))
            }

            Some(_) if contains_binary_op => {
                Expression::parse_function_call(all_tokens.clone(), context)
                    .or_else(|_| Expression::parse_binary_op(all_tokens, context))
            }

            Some(ref identifier) if identifier.is_any_identifier() => {
                Expression::parse_function_call(all_tokens.clone(), context)
                    .or_else(|_| Expression::parse_identifier(all_tokens, context))
            }

            Some(ref s) if s.is_any_literal_string() => {
                Expression::parse_literal_string(all_tokens, context)
            }

            Some(ref i) if i.is_any_literal_int() => {
                Expression::parse_literal_integer(all_tokens, context)
            }

            _ => {
                /* there must be 1+ tokens or match_peek would have failed */
                let unexpected = all_tokens.get(0).unwrap().clone();
                Err(ParseError::UnexpectedToken(unexpected, Some(match_expr_start)))
            }
        }
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
    use source;
    use operators;
    use node::ToSource;

    fn parse_expr(src: &str) -> Expression {
        let context = source::test::empty_context();

        let tokens = tokenize("test", src).unwrap();

        Expression::parse(tokens, &context)
            .unwrap()
            .finish()
            .unwrap()
    }

    #[test]
    fn brackets_groups_exprs() {
        let expr = parse_expr("(1 + 2) - 3");

        assert!(expr.is_operation(&operators::Minus));
        let (lhs, _, rhs) = expr.unwrap_binary_op();

        assert!(rhs.is_literal_integer(3));
        assert!(lhs.is_operation(&operators::Plus));

        let (nested_lhs, _, nested_rhs) = lhs.unwrap_binary_op();
        assert!(nested_lhs.is_literal_integer(1));
        assert!(nested_rhs.is_literal_integer(2));
    }

    #[test]
    fn precedence() {
        for (prec_a, op_a) in operators::PRECEDENCE.iter().enumerate() {
            let other_ops = operators::PRECEDENCE.iter().enumerate()
                .filter(|&(prec, _)| prec != prec_a);

            for (prec_b, op_b) in other_ops {
                let hi_op = if prec_a < prec_b { op_a } else { op_b };
                let lo_op = if prec_a < prec_b { op_b } else { op_a };

                println!("hi: {}@{}, lo: {}@{}", hi_op, lo_op, prec_a.min(prec_b), prec_a.max(prec_b));

                /* should parse as ((1 hi_op 2) lo_op 3) */
                let expr1 = parse_expr(&format!("1 {} 2 {} 3", hi_op.to_source(), lo_op.to_source()));
                assert!(expr1.is_operation(lo_op), "{} should have precedence over {} (understood this as {})", hi_op, lo_op, expr1.to_source());
                let (lhs1, _, rhs1) = expr1.unwrap_binary_op();
                assert!(lhs1.is_operation(hi_op));
                assert!(rhs1.is_any_literal_integer());

                /* should parse as (1 lo_op (2 hi_op 3)) */
                let expr2 = parse_expr(&format!("1 {} 2 {} 3", lo_op, hi_op));
                assert!(expr2.is_operation(lo_op), "{} should have precedence over {} (understood as {})", hi_op, lo_op, expr2.to_source());
                let (lhs2, _, rhs2) = expr2.unwrap_binary_op();
                assert!(lhs2.is_any_literal_integer());
                assert!(rhs2.is_operation(hi_op));
            }
        }
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
        assert!(hello_args[0].is_any_literal_string());
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
        assert!(cruel_args[0].is_any_literal_string());
        assert_eq!("world", cruel_args[0].clone().unwrap_literal_string());
    }
}