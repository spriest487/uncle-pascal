#[cfg(test)]
mod test;

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
enum CompoundExpressionPart {
    Operand(Expression),
    Operator(OperatorToken),
}

impl CompoundExpressionPart {
    fn unwrap_operand(self) -> Expression {
        match self {
            CompoundExpressionPart::Operand(expr) => expr,
            _ => panic!("called unwrap_operand on something that wasn't an operand: {:?}", self)
        }
    }
}

fn count_operators_at_base_level<'a, TIter>(tokens: TIter) -> Result<usize, ParseError>
    where TIter: IntoIterator<Item=&'a source::Token>
{
    let mut bracket_level = 0;
    let mut ops = 0;

    for token in tokens {
        if *token.as_token() == tokens::BracketLeft {
            bracket_level += 1
        } else if *token.as_token() == tokens::BracketRight {
            if bracket_level == 0 {
                return Err(ParseError::UnexpectedToken(token.clone(), None))
            }
            bracket_level -= 1
        } else if bracket_level == 0 && token.is_any_operator() {
            ops += 1
        }
    }

    Ok(ops)
}

impl Expression {
    fn resolve_ops_by_precedence(parts: Vec<CompoundExpressionPart>) -> Expression {
        assert!(parts.len() > 0, "expression must not be empty");

        if parts.len() == 1 {
            return match parts.into_iter().next().unwrap() {
                CompoundExpressionPart::Operand(expr) => expr,
                CompoundExpressionPart::Operator(op_token) =>
                    panic!("expression with one part must not be an operator (got: `{:?}`)", op_token),
            };
        }

        /* find the lowest-precedence operator in the expression, this becomes the
         outer expression */
        let (lo_op_index, lo_op) = parts.iter()
            .enumerate()
            .filter_map(|(i, part)| match part {
                &CompoundExpressionPart::Operand(_) => None,
                &CompoundExpressionPart::Operator(ref op) => Some((i, op.clone())),
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
                    .chain(vec![CompoundExpressionPart::Operand(op_expr)])
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

                let mut after_brackets = outer_brackets.next_tokens
                    .skip(inner_len + 2) //inner size + open + close
                    .peekable();

                let inner_result = ParseOutput::new(inner_expr, brackets_block.close, after_brackets);

                Expression::parse_member_access_after(inner_result)
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

//        println!("ALL TOKENS: {}", source::tokens_to_source(&all_tokens));

        let mut next_operand_tokens: Vec<source::Token> = Vec::new();

        let mut parts = Vec::new();
        let mut in_operand = false;

        let mut bracket_depth = 0;

        let mut tokens_it = all_tokens.iter();

        let mut tokens_after = Vec::new();

        loop {
            let next_token: Option<&source::Token> = tokens_it.next();

            /* update the context if the token stream isn't finished */
            if let &Some(token) = &next_token {
                last_token = token.clone();
            }

            let finish_operand = next_token.is_none() ||
                //more tokens, but we hit an operator
                (next_token.unwrap().is_any_operator() &&
                    bracket_depth == 0);

            if finish_operand {
                if next_operand_tokens.len() > 0 {
//                    println!("FINISHED OPERAND! {}", source::tokens_to_source(&next_operand_tokens));
                    if next_operand_tokens.len() == all_tokens.len() {
                        //stop trying
//                        return Err(ParseError::UnrecognizedSequence(all_tokens.clone()))
                    }

                    let context = next_operand_tokens[0].clone();
                    let operand_expr = Expression::parse_operand(next_operand_tokens,
                                                                 &context)?;

                    let mut after_expr = operand_expr.next_tokens.peekable();

                    parts.push(CompoundExpressionPart::Operand(operand_expr.value));
                    next_operand_tokens = Vec::new();

                    if let Some(token_after) = after_expr.peek().cloned() {
                        let all_tokens_after = all_tokens.iter()
                            .filter(|t| t.location.ge(&token_after.location))
                            .cloned()
                            .collect();

//                        println!("leftovers in operand: `{}`, following tokens: `{}`",
//                                 source::tokens_to_source(&after_expr.collect::<Vec<_>>()),
//                                 source::tokens_to_source(&all_tokens_after));

                        /* this expression is finished, and there's tokens left over */
                        tokens_after = all_tokens_after;
                        break;
                    }
                }
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
                    let op = op_token.unwrap_operator().clone();
                    let pos = if in_operand {
                        operators::Position::Binary
                    } else {
                        operators::Position::Prefix
                    };

                    if !op.is_valid_in_pos(pos) {
                        tokens_after = all_tokens.iter()
                            .filter(|t| t.location.ge(&last_token.location))
                            .cloned()
                            .collect();
                        break;
                    }

                    in_operand = false;

                    parts.push(CompoundExpressionPart::Operator(OperatorToken {
                        op,
                        token: op_token.clone(),
                        pos,
                    }))
                }

                Some(token @ _) => {
                    in_operand = true;
                    next_operand_tokens.push(token.clone());
                }

                /* finished reading the expression and there were no tokens left over */
                None => break,
            }
        };

        if next_operand_tokens.len() > 0 {
//            println!("LAST OPERAND: {}", source::tokens_to_source(&next_operand_tokens));

            if next_operand_tokens.len() == all_tokens.len() {
//                return Err(ParseError::UnrecognizedSequence(all_tokens.clone()))
            }

            let last_operand_context = next_operand_tokens[0].clone();
            let last_operand_expr = Expression::parse(next_operand_tokens,
                                                      &last_operand_context)?;
            parts.push(CompoundExpressionPart::Operand(last_operand_expr.value));

            let mut after_last_operand = last_operand_expr.next_tokens.peekable();
            if after_last_operand.peek().is_some() {
                tokens_after = after_last_operand.collect();
            }
        }

        let expr = Expression::resolve_ops_by_precedence(parts);

        Ok(ParseOutput::new(expr, last_token, tokens_after))
    }

    fn parse_member_access_after(base: ParseOutput<Expression>) -> ExpressionResult {
        let mut peek_after = base.next_tokens.peekable();
        match peek_after.peek().cloned() {
            Some(ref period) if *period.as_token() == tokens::Period => {
                let member_name = node::Identifier::parse(peek_after.skip(1),
                                                          period)?;

                println!("found member name {} for base expr {:?}", member_name.value, base.value);
                let member = Expression::member_deep(base.value, member_name.value);

                Ok(ParseOutput::new(member, member_name.last_token, member_name.next_tokens))
            }

            //nope, this expr ends at the close bracket
            _ => Ok(ParseOutput::new(base.value, base.last_token, peek_after))
        }
    }

    fn parse_fn_call_after(base: ParseOutput<Expression>) -> ExpressionResult {
        let mut peek_after = base.next_tokens.peekable();
        match peek_after.peek().cloned() {
            Some(ref open_bracket) if *open_bracket.as_token() == tokens::BracketLeft => {
                let args = tokens::BracketLeft.terminated_by(tokens::BracketRight)
                    .match_groups(tokens::Comma, peek_after, &base.last_token)?;

                let all_args = args.value.groups
                    .into_iter()
                    .map(|arg_group| {
                        let arg_expr = Expression::parse_compound(arg_group.tokens, &arg_group.context)?;
                        arg_expr.finish()
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let fn_call = Expression::function_call(base.value, all_args);

                Ok(ParseOutput::new(fn_call, args.last_token, args.next_tokens))
            }

            _ => Ok(ParseOutput::new(base.value, base.last_token, peek_after))
        }
    }

    fn parse_identifier<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let parse_id = node::Identifier::parse(in_tokens, context)?;

        Ok(parse_id.map(|id| {
            Expression::identifier(id, context.clone())
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

    fn parse_base<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
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

        let contains_operator = count_operators_at_base_level(all_tokens.iter())? > 0;

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

            Some(ref compound) if contains_operator ||
                *compound.as_token() == tokens::BracketLeft => {
                Expression::parse_compound(all_tokens, context)
            }

            Some(ref identifier) if identifier.is_any_identifier() => {
                Expression::parse_identifier(all_tokens, context)
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

    pub fn parse<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static
    {
        let base = Expression::parse_base(in_tokens, context)?;

        match &base.value.value {
            &node::ExpressionValue::Identifier(_) |
            &node::ExpressionValue::BinaryOperator { .. } |
            &node::ExpressionValue::PrefixOperator { .. } |
            &node::ExpressionValue::Member { .. } |
            &node::ExpressionValue::FunctionCall { .. } => {
                let with_member_access = Expression::parse_member_access_after(base)?;
                Expression::parse_fn_call_after(with_member_access)
            }

            _ => Ok(base)
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
