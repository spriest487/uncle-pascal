#[cfg(test)]
mod test;

use syntax::*;
use tokens;
use keywords;
use tokens::AsToken;
use node;
use source;
use operators;

pub type Expression = node::Expression<ParsedSymbol>;
pub type ExpressionResult = Result<Expression, ParseError>;

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
                return Err(ParseError::UnexpectedToken(token.clone(), None));
            }
            bracket_level -= 1
        } else if bracket_level == 0 && token.is_any_operator() {
            ops += 1
        }
    }

    Ok(ops)
}

impl Expression {
    fn resolve_ops_by_precedence(parts: Vec<CompoundExpressionPart>) -> Result<Expression, ParseError> {
        assert!(parts.len() > 0, "expression must not be empty");

        if parts.len() == 1 {
            return Ok(match parts.into_iter().next().unwrap() {
                CompoundExpressionPart::Operand(expr) => expr,
                CompoundExpressionPart::Operator(op_token) =>
                    panic!("expression with one part must not be an operator (got: `{:?}`)", op_token),
            });
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

                let merged_parts: Vec<_> = before_op.iter()
                    .cloned()
                    .chain(vec![CompoundExpressionPart::Operand(op_expr)])
                    .chain(parts_after_op)
                    .collect();

                assert!(merged_parts.len() > 0);
                Expression::resolve_ops_by_precedence(merged_parts)
            }

            operators::Position::Binary => {
                let (before_op, after_op) = parts.split_at(lo_op_index);

                if before_op.len() == 0 {
                    return Err(ParseError::EmptyOperand {
                        operator: lo_op.token,
                        before: true,
                    });
                }

                //1 because the op is included in this (?)
                if after_op.len() <= 1 {
                    return Err(ParseError::EmptyOperand {
                        operator: lo_op.token,
                        before: false,
                    });
                }

                let lhs_operand = Expression::resolve_ops_by_precedence(Vec::from(before_op))?;
                let rhs_operand = Expression::resolve_ops_by_precedence(after_op.iter()
                    .skip(1)
                    .cloned()
                    .collect())?;

                let expr_context = lhs_operand.context.clone();

                Ok(Expression::binary_op(lhs_operand, lo_op.op, rhs_operand, expr_context))
            }
        }
    }

    fn parse_operand(tokens: &mut TokenStream) -> ExpressionResult {
        /* if there's brackets around it, we know exactly where it begins and ends */
        let outer_brackets = tokens.match_block_peek(tokens::BracketLeft, tokens::BracketRight)?;

        match outer_brackets {
            Some(brackets_block) => {
                let inner_len = brackets_block.inner.len();

                let mut inner_expr_tokens = TokenStream::new(brackets_block.inner,
                                                         &brackets_block.open);
                let inner_expr = Expression::parse(&mut inner_expr_tokens)?;
                inner_expr_tokens.finish()?;

                // seek to after brackets
                for _ in 0..inner_len + 2 {
                    tokens.next();
                }
                Expression::parse_member_access_after(inner_expr, tokens)
            }

            None =>  Expression::parse(tokens)
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
                None => { break; }
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

        assert!(parts.len() > 0, "expression must not be empty after {}", last_token);
        let expr = Expression::resolve_ops_by_precedence(parts)?;

        Ok(ParseOutput::new(expr, last_token, tokens_after))
    }

    fn parse_member_access_after(base: Expression, tokens_after: &mut TokenStream) -> ExpressionResult {
        match tokens_after.peek() {
            Some(ref period) if *period.as_token() == tokens::Period => {
                // skip period
                tokens_after.next();

                let member_name = node::Identifier::parse(tokens_after)?;

//                println!("found member name {} for base expr {:?}", member_name, base);

                let member = Expression::member_deep(base.value, member_name);
                Ok(member)
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

    fn parse_identifier(tokens: &mut TokenStream) -> ExpressionResult {
        let id = node::Identifier::parse(tokens)?;

        Ok(Expression::identifier(ParsedSymbol(id), tokens.context().clone()))
    }

    fn parse_literal_string(tokens: &mut TokenStream) -> ExpressionResult {
        let str_token = tokens.match_one(Matcher::AnyLiteralString)?;

        let s = str_token.unwrap_literal_string();
        Ok(Expression::literal_string(s, tokens.context().clone()))
    }

    fn parse_literal_integer(tokens: &mut TokenStream) -> ExpressionResult {
        let integer_token = tokens.match_one(Matcher::AnyLiteralInteger)?;

        let i = integer_token.unwrap_literal_integer();
        Ok(Expression::literal_int(i, integer_token.clone()))
    }

    fn parse_literal_nil(tokens: &mut TokenStream) -> ExpressionResult {
        let match_nil = tokens.match_one(keywords::Nil)?;
        Expression::literal_nil(tokens.context())
    }

    fn parse_let_binding<TIter>(in_tokens: TIter, context: &source::Token) -> ExpressionResult
        where TIter: IntoIterator<Item=source::Token> + 'static,
    {
        let binding_tokens = keywords::Let
            .and_then(Matcher::AnyIdentifier)
            .and_then(tokens::Operator(operators::Assignment))
            .match_sequence(in_tokens, context)?;

        let name = binding_tokens.value[1].unwrap_identifier();

        let value = Expression::parse(binding_tokens.next_tokens,
                                      &binding_tokens.last_token)?;

        let let_expr = Expression::let_binding(binding_tokens.value[0].clone(),
                                               name,
                                               value.value);

        Ok(ParseOutput::new(let_expr, value.last_token, value.next_tokens))
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

    fn parse_for_loop(tokens: &mut TokenStream) -> ExpressionResult {
        let for_do_pair = tokens.match_block(keywords::For, keywords::Do)?;

        /* can't nest for loops in either the from or the to expression, so
        it's safe just to look for the next "to" */
        let mut for_cond_tokens = TokenStream::new(for_do_pair.inner, for_do_pair.open);
        let split_at_to = for_cond_tokens.split_at_match(keywords::To)?;

        // the part before the "to" becomes the "from" expr
        let from_expr = Expression::parse(&mut TokenStream::new(
            split_at_to.before_split,
            split_at_to.split_at.clone()))?;

        // the part between the "to" and the "do" becomes the "to" expr
        let to_expr = Expression::parse(&mut for_cond_tokens)?;

        // there should be no more tokens before the "do"
        tokens.finish()?;

        let body_expr = Expression::parse(tokens)?;

        Ok(Expression::for_loop(from_expr,
                                to_expr,
                                body_expr.value,
                                for_do_pair.value.open))
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
            .or(keywords::Nil)
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

            Some(ref let_kw) if let_kw.is_keyword(keywords::Let) => {
                Expression::parse_let_binding(all_tokens, context)
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

            Some(ref nil) if nil.is_literal_nil() => {
                Expression::parse_literal_nil(all_tokens, context)
            }

            _ => {
                /* there must be 1+ tokens or match_peek would have failed */
                let unexpected = all_tokens.get(0).unwrap().clone();
                Err(ParseError::UnexpectedToken(unexpected, Some(match_expr_start)))
            }
        }
    }

    pub fn parse(tokens: &mut TokenStream) -> ExpressionResult {
        let base = Expression::parse_base(tokens)?;

        match &base.value.value {
            &node::ExpressionValue::Identifier(_) |
            &node::ExpressionValue::BinaryOperator { .. } |
            &node::ExpressionValue::PrefixOperator { .. } |
            &node::ExpressionValue::Member { .. } |
            &node::ExpressionValue::FunctionCall { .. } => {
                let with_member_access = Expression::parse_member_access_after(base, tokens)?;
                Expression::parse_fn_call_after(with_member_access, tokens)
            }

            _ => Ok(base)
        }
    }
}

