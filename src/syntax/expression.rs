use syntax::*;
use tokens;
use keywords;
use tokens::AsToken;
use node;
use source;
use operators;

pub type Expression = node::Expression<node::Identifier>;
pub type ExpressionResult = Result<ParseOutput<Expression>, ParseError>;

#[derive(Debug, Clone)]
struct OperatorToken {
    op: operators::Operator,
    pos: operators::Position,
    token: source::Token,
}

#[derive(Debug, Clone)]
enum OperatorExpressionPart {
    Operand(Expression),
    Operator(OperatorToken),
}

impl OperatorExpressionPart {
    fn unwrap_operand(self) -> Expression {
        match self {
            OperatorExpressionPart::Operand(expr) => expr,
            _ => panic!("called unwrap_operand on something that wasn't an operand: {:?}", self)
        }
    }
}

impl Expression {
    fn resolve_ops_by_precedence(parts: Vec<OperatorExpressionPart>) -> Expression {
        assert!(parts.len() > 0, "expression must not be empty");

        if parts.len() == 1 {
            return match parts.into_iter().next().unwrap() {
                OperatorExpressionPart::Operand(expr) => expr,
                OperatorExpressionPart::Operator(op_token) =>
                    panic!("expression with one part must not be an operator (got: `{:?}`)", op_token),
            };
        }

        /* find the lowest-precedence operator in the expression, this becomes the
         outer expression */
        let (lo_op_index, lo_op) = parts.iter()
            .enumerate()
            .filter_map(|(i, part)| match part {
                &OperatorExpressionPart::Operand(_) => None,
                &OperatorExpressionPart::Operator(ref op) => Some((i, op.clone())),
            })
            .max_by_key(|&(_, ref op)| op.op.precedence(op.pos))
            .unwrap();

        match lo_op.pos {
            /* merge prefix operator with the operand that follows it*/
            operators::Position::Prefix => {
                assert!(parts.get(lo_op_index + 1).is_some(), "prefix operator must be followed by an operand");

                let (before_op, after_op) = parts.split_at(lo_op_index);

                let mut parts_after_op = after_op.iter().skip(1).cloned();

                let rhs = parts_after_op.next().unwrap().unwrap_operand();

                let op_expr = Expression::prefix_op(lo_op.op, rhs, lo_op.token);

                let merged_parts = before_op.iter()
                    .cloned()
                    .chain(vec![OperatorExpressionPart::Operand(op_expr)])
                    .chain(parts_after_op);

                Expression::resolve_ops_by_precedence(merged_parts.collect())
            }

            operators::Position::Binary => {
                let (before_op, after_op) = parts.split_at(lo_op_index);

                let lhs_operand = Expression::resolve_ops_by_precedence(Vec::from(before_op));
                let rhs_operand = Expression::resolve_ops_by_precedence(after_op.iter()
                    .skip(1)
                    .cloned()
                    .collect());

                let expr_context = lhs_operand.context.clone();
                Expression::binary_op(lhs_operand, lo_op.op, rhs_operand, expr_context)
            }
        }
    }

    fn parse_operand<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        /* if there's brackets around it, we know exactly where it begins and ends */
        let outer_brackets = tokens::BracketLeft
            .terminated_by(tokens::BracketRight)
            .match_block_peek(in_tokens, context)?;

        match outer_brackets.value {
            Some(brackets_block) => {
                let inner_len = brackets_block.inner.len();
                let inner_expr = Expression::parse(brackets_block.inner,
                                                   &brackets_block.open)?
                    .finish()?;

                let after_brackets = outer_brackets.next_tokens
                    .skip(inner_len + 2); //inner size + open + close

                Ok(ParseOutput::new(inner_expr, brackets_block.close, after_brackets))
            }

            None => {
                Expression::parse(outer_brackets.next_tokens,
                                  &outer_brackets.last_token)
            }
        }
    }

    fn parse_compound<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static,
    {
        /* group tokens into operators and operands - we don't know where this expression
         will end so we need to take the entire stream */
        let all_tokens: Vec<_> = in_tokens.into_iter().collect();
        let mut last_token = context.clone();

//        println!("ALL TOKENS: {}", source::tokens_to_string(&all_tokens));

        let mut next_operand_tokens: Vec<source::Token> = Vec::new();

        let mut parts = Vec::new();
        let mut in_operand = false;

        let mut bracket_depth = 0;

        let mut tokens_it = all_tokens.iter();

        let tokens_after = loop {
            let next_token: Option<&source::Token> = tokens_it.next();
            let finish_operand = next_token.is_none() ||
                (next_token.unwrap().is_any_operator() && bracket_depth == 0);

            if finish_operand {
                if next_operand_tokens.len() > 0 {
                    //println!("FINISHED OPERAND! {}", source::tokens_to_string(&next_operand_tokens));
                    //Expression::remove_redundant_brackets(&mut next_operand_tokens);
                    let context = next_operand_tokens[0].clone();
                    let operand_expr = Expression::parse_operand(next_operand_tokens,
                                                                 &context)?;

                    //let operand_expr = Expression::parse(next_operand_tokens, &context)?;
                    let mut after_expr = operand_expr.next_tokens.peekable();

                    parts.push(OperatorExpressionPart::Operand(operand_expr.value));

                    match after_expr.peek() {
                        None => {
                            //continue
                            next_operand_tokens = Vec::new();
                        }
                        Some(token_after) => {
                            let all_tokens_after = all_tokens.iter()
                                .filter(|t| {
                                    /* collect all the tokens for everything past this operand
                                    in the stream */
                                    t.location.line > token_after.location.line ||
                                        (t.location.line == token_after.location.line &&
                                            t.location.col >= token_after.location.col)
                                })
                                .cloned()
                                .collect();

                            /* this expression is finished, and there's tokens left over */
                            break all_tokens_after;
                        }
                    }
                }
            }

            /* update the context if the token stream isn't finished */
            if let &Some(token) = &next_token {
                last_token = token.clone();
            }

            match next_token {
                Some(bracket) if *bracket.as_token() == tokens::BracketLeft => {
                    bracket_depth += 1;

                    next_operand_tokens.push(bracket.clone());
                    in_operand = true;
                }

                Some(bracket) if *bracket.as_token() == tokens::BracketRight => {
                    match bracket_depth {
                        0 => return Err(ParseError::UnexpectedToken(bracket.clone(), None)),
                        _ => bracket_depth -= 1,
                    }

                    next_operand_tokens.push(bracket.clone());
                    in_operand = true;
                }

                Some(op_token) if bracket_depth == 0 && op_token.is_any_operator() => {
                    let pos = if in_operand {
                        operators::Position::Binary
                    } else {
                        operators::Position::Prefix
                    };

                    in_operand = false;

                    parts.push(OperatorExpressionPart::Operator(OperatorToken {
                        op: op_token.unwrap_operator().clone(),
                        token: op_token.clone(),
                        pos,
                    }))
                }

                Some(token @ _) => {
                    in_operand = true;
                    next_operand_tokens.push(token.clone());
                }

                /* finished reading the expression and there were no tokens left over */
                None => break Vec::new(),
            }
        };

        let expr = Expression::resolve_ops_by_precedence(parts);

        Ok(ParseOutput::new(expr, last_token, tokens_after))
    }

    fn parse_identifier<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let match_id = Matcher::AnyIdentifier.match_one(in_tokens, context)?;

        Ok(match_id.map(|id_token| {
            let name = id_token.unwrap_identifier();

            Expression::identifier(node::Identifier::parse(name), id_token.clone())
        }))
    }

    fn parse_literal_string<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let match_str = Matcher::AnyLiteralString.match_one(in_tokens, context)?;

        Ok(match_str.map(|str_token| {
            let s = str_token.unwrap_literal_string();
            Expression::literal_string(s, str_token.clone())
        }))
    }

    fn parse_literal_integer<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static,
    {
        let match_int = Matcher::AnyLiteralInteger.match_one(in_tokens, context)?;

        Ok(match_int.map(|integer_token| {
            let i = integer_token.unwrap_literal_integer();
            Expression::literal_int(i, integer_token.clone())
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
            .or(tokens::BracketLeft)
            .or(operators::Plus)
            .or(operators::Minus)
            .or(operators::Deref);

        let all_tokens: Vec<source::Token> = in_tokens.into_iter().collect();

        let expr_first = match_expr_start.match_peek(all_tokens, context)?;
        //peeking moves all tokens, let's just put that back...
        let all_tokens: Vec<source::Token> = expr_first.next_tokens.collect();

        let contains_operator = all_tokens.iter()
            .any(|t| t.is_any_operator());

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

            Some(_) if contains_operator => {
                Expression::parse_function_call(all_tokens.clone(), context)
                    .or_else(|_| Expression::parse_compound(all_tokens, context))
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

            &node::ExpressionValue::PrefixOperator { ref op, ref rhs } => {
                format!("({} {})", op, rhs.to_source())
            }

            &node::ExpressionValue::Member { ref of, ref name } => {
                format!("{}.{}", of, name)
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

    fn try_parse_expr(src: &str) -> ExpressionResult {
        let context = source::test::empty_context();

        let tokens = tokenize("test", src).unwrap();

        Expression::parse(tokens, &context)
    }

    fn parse_expr(src: &str) -> Expression {
        try_parse_expr(src).unwrap()
            .finish()
            .unwrap()
    }

    #[test]
    fn brackets_groups_exprs() {
        let expr = parse_expr("(1 + 2) - 3");

        assert!(expr.is_binary_op(operators::Minus));
        let (lhs, _, rhs) = expr.unwrap_binary_op();

        assert!(rhs.is_literal_integer(3));
        assert!(lhs.is_binary_op(operators::Plus));

        let (nested_lhs, _, nested_rhs) = lhs.unwrap_binary_op();
        assert!(nested_lhs.is_literal_integer(1));
        assert!(nested_rhs.is_literal_integer(2));
    }

    #[test]
    fn simple_binary_op() {
        let expr = parse_expr("1 + 2");

        assert!(expr.is_binary_op(operators::Plus));

        let (lhs, _, rhs) = expr.unwrap_binary_op();
        assert!(lhs.is_literal_integer(1));
        assert!(rhs.is_literal_integer(2));
    }

    #[test]
    fn simple_binary_op_in_brackets() {
        let expr = parse_expr("(1 + 2)");

        assert!(expr.is_binary_op(operators::Plus));

        let (lhs, _, rhs) = expr.unwrap_binary_op();
        assert!(lhs.is_literal_integer(1));
        assert!(rhs.is_literal_integer(2));
    }

    #[test]
    fn simple_prefix_op() {
        let expr = parse_expr("+1");

        assert!(expr.is_prefix_op(operators::Plus));
        let (_, rhs) = expr.unwrap_prefix_op();

        assert!(rhs.is_literal_integer(1));
    }

    #[test]
    fn parses_multiple_line_compound_expr() {
        let expr = try_parse_expr(r"a.b := 1
            b.c := 2")
            .unwrap();
        assert!(expr.value.is_binary_op(operators::Assignment));

        let remaining: Vec<_> = expr.next_tokens.collect();
        assert_eq!(3, remaining.len(), "expected `b := 2` to be left over but got tokens {:?}", remaining);

        let second_ctx = remaining[0].clone();
        let second_expr = Expression::parse(remaining, &second_ctx)
            .unwrap()
            .finish()
            .unwrap();

        assert!(second_expr.is_binary_op(operators::Assignment));
    }

    #[test]
    fn parses_binary_expr_followed_by_func_call() {
        let expr_result = try_parse_expr(r"
                ^a := ^(b + 1)

                System.FreeMem(self.Elements)")
            .unwrap();

        let expr: Expression = expr_result.value;
        match &expr.value {
            &node::ExpressionValue::BinaryOperator { .. } => (),
            _ => panic!("expected first expr in stream to be assignment, found {:?}", expr)
        }
    }

    #[test]
    fn parses_multi_line_block_without_terminators() {
        let expr = parse_expr(r"begin
            a.b := 1
            b.c := 2
            end");

        assert!(expr.is_block());
    }

    #[test]
    fn binary_op_precedence() {
        let binary_ops_precedence = operators::PRECEDENCE.iter()
            .filter_map(|&(op, pos)| if pos == operators::Position::Binary {
                Some(op)
            } else {
                None
            })
            .collect::<Vec<_>>();

        for (prec_a, op_a) in binary_ops_precedence.iter().enumerate() {
            let other_ops = binary_ops_precedence.iter().enumerate()
                .filter(|&(prec, _)| prec != prec_a);

            for (prec_b, op_b) in other_ops {
                let hi_op = *if prec_a < prec_b { op_a } else { op_b };
                let lo_op = *if prec_a < prec_b { op_b } else { op_a };

                /* should parse as ((1 hi_op 2) lo_op 3) */
                let hi_first = parse_expr(&format!("1 {} 2 {} 3", hi_op.to_source(), lo_op.to_source()));
                assert!(hi_first.is_binary_op(lo_op), "{} should have precedence over {} (understood this as {:?})", hi_op, lo_op, hi_first);

                let (hi_first_1_then_2, _, hi_first_3) = hi_first.unwrap_binary_op();
                assert!(hi_first_1_then_2.is_binary_op(hi_op));
                assert!(hi_first_3.is_literal_integer(3), "rhs should be 3, but was `{:?}`", hi_first_3);

                /* should parse as (1 lo_op (2 hi_op 3)) */
                let lo_first = parse_expr(&format!("1 {} 2 {} 3", lo_op, hi_op));
                assert!(lo_first.is_binary_op(lo_op), "{} should have precedence over {} (understood as {:?})", hi_op, lo_op, lo_first);

                let (lo_first_1, _, lo_first_2_then_3) = lo_first.unwrap_binary_op();
                assert!(lo_first_1.is_literal_integer(1), "lhs should be 1, but was `{:?}`", lo_first_1);
                assert!(lo_first_2_then_3.is_binary_op(hi_op));
            }
        }
    }

    #[test]
    fn parses_deref_on_lhs() {
        let expr = parse_expr("^a + 1");

        assert!(expr.is_binary_op(operators::Plus), "result should be plus, was: {}", expr.to_source());
        let (lhs, _, rhs) = expr.unwrap_binary_op();

        assert!(lhs.is_prefix_op(operators::Deref));
        assert!(rhs.is_literal_integer(1));
    }

    #[test]
    fn parses_assign_deref_to_deref() {
        let expr = parse_expr("^(a + 1) := ^(b + 1)");

        assert!(expr.is_binary_op(operators::Assignment), "result should be an assignment, was: {}", expr.to_source());
        let (lhs, _, rhs) = expr.unwrap_binary_op();

        assert!(lhs.is_prefix_op(operators::Deref));
        assert!(rhs.is_prefix_op(operators::Deref));
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

    #[test]
    fn parses_single_prefix_op_in_brackets() {
        let expr = parse_expr("(^a)");

        assert!(expr.is_prefix_op(operators::Deref));
    }

    #[test]
    fn parses_deref_struct_field() {
        let expr = parse_expr("(^a).b");

        assert!(expr.is_any_member());
        let (base, name) = expr.unwrap_member();
        assert!(base.is_prefix_op(operators::Deref));
        assert_eq!("b", name);
    }
}