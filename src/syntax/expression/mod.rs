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

fn count_operators_at_base_level<'a>(tokens: impl IntoIterator<Item=&'a source::Token>)
                                     -> Result<usize, ParseError> {
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
                let brackets_block_len = brackets_block.len();
                let mut inner_expr_tokens = TokenStream::new(brackets_block.inner,
                                                             &brackets_block.open);
                let inner_expr = Expression::parse(&mut inner_expr_tokens)?;
                inner_expr_tokens.finish()?;

                // seek to after brackets
                tokens.advance(brackets_block_len);

                /* expression of the form (a).b should include the .b after the brackets too */
                Expression::parse_member_access_after(inner_expr, tokens)
            }

            None => Expression::parse(tokens)
        }
    }

    fn parse_member_access_after(base: Expression, tokens_after: &mut TokenStream) -> ExpressionResult {
        match tokens_after.peek() {
            Some(ref period) if *period.as_token() == tokens::Period => {
                // skip period
                tokens_after.next();

                let member_name = node::Identifier::parse(tokens_after)?;

//                println!("found member name {} for base expr {:?}", member_name, base);

                let member = Expression::member_deep(base, member_name);
                Ok(member)
            }

            //nope, this expr ends at the close bracket
            _ => Ok(base)
        }
    }

    fn parse_compound(tokens: &mut TokenStream) -> ExpressionResult {
        let all_tokens = tokens.peek_to_end();

        /* group tokens into operators and operands */
        let mut parts = Vec::new();
        let mut in_operand = false;

        let mut bracket_depth = 0;

        let mut next_operand_tokens = Vec::new();

        // remember where this expression started, that's the context for it
        let expr_context = tokens.context().clone();

        let mut last_token = expr_context.clone();

        loop {
            let next_token: Option<source::Token> = tokens.next();

            let finish_operand = next_token.is_none() ||
                // more tokens, but we hit an operator
                (next_token.as_ref().unwrap().is_any_operator() &&
                    bracket_depth == 0);

            if finish_operand {
                if next_operand_tokens.len() > 0 {
                    let mut operand_stream = TokenStream::from(next_operand_tokens);
                    next_operand_tokens = Vec::new();

                    let operand_expr = Expression::parse_operand(&mut operand_stream)?;
//                    println!("finished operand: `{}`, context: {}", operand_expr.to_source(), operand_expr.context);

                    parts.push(CompoundExpressionPart::Operand(operand_expr));

                    /* if there's any tokens left after an operand, they must belong to
                    the next expression, because each set of tokens between operators must
                    be a valid and complete expression. so we can stop parsing here */
                    if let Some(_token_after_operand) = operand_stream.peek() {
                        let tokens_after_operand: Vec<_> = all_tokens.iter()
                            .filter(|t| {
                                t.location.gt(&operand_stream.context().location)
                            })
                            .cloned()
                            .collect();

                        *tokens = TokenStream::new(tokens_after_operand, operand_stream.context());
                        break;
                    }
                }

                if next_token.is_none() {
                    break;
                }
            }

            match next_token {
                Some(ref bracket) if *bracket.as_token() == tokens::BracketLeft => {
                    bracket_depth += 1;

                    last_token = bracket.clone();

                    next_operand_tokens.push(bracket.clone());
                    in_operand = true;
                }

                Some(ref bracket) if *bracket.as_token() == tokens::BracketRight => {
                    match bracket_depth {
                        0 => return Err(ParseError::UnexpectedToken(bracket.clone(), None)),
                        _ => bracket_depth -= 1,
                    }

                    last_token = bracket.clone();

                    next_operand_tokens.push(bracket.clone());
                    in_operand = true;
                }

                Some(ref op_token) if bracket_depth == 0 && op_token.is_any_operator() => {
                    let op = op_token.unwrap_operator().clone();
                    let pos = if in_operand {
                        operators::Position::Binary
                    } else {
                        operators::Position::Prefix
                    };

                    if !op.is_valid_in_pos(pos) {
                        let tokens_after_operator: Vec<_> = all_tokens.iter()
                            .filter(|t|  t.location.ge(&last_token.location))
                            .cloned()
                            .collect();

                        *tokens = TokenStream::new(tokens_after_operator, &last_token);

                        break;
                    }

                    last_token = op_token.clone();
                    in_operand = false;

                    parts.push(CompoundExpressionPart::Operator(OperatorToken {
                        op,
                        token: op_token.clone(),
                        pos,
                    }))
                }

                Some(token @ _) => {
                    in_operand = true;
                    last_token = token.clone();
                    next_operand_tokens.push(token);
                }

                /* finished reading the expression and there were no tokens left over */
                None => unreachable!("already checked next_token for none")
            }
        };

        if next_operand_tokens.len() > 0 {
            let mut final_operand_tokens = TokenStream::from(next_operand_tokens);
            let final_operand_expr = Expression::parse(&mut final_operand_tokens)?;
//            println!("finished final operand: `{}`, context: {}",
//                     final_operand_expr.to_source(),
//                     final_operand_expr.context);

            parts.push(CompoundExpressionPart::Operand(final_operand_expr));

            // put unused tokens after this expression back into the stream
            tokens.rewind(final_operand_tokens);
        }

        assert!(parts.len() > 0, "expression must not be empty after {}", expr_context);
        let expr = Expression::resolve_ops_by_precedence(parts)?;

//        println!("finished compound {}, next is {:?}", expr.to_source(), tokens.peek());

        Ok(expr)
    }

    fn parse_fn_call_after(base: Expression, tokens_after: &mut TokenStream) -> ExpressionResult {
        match tokens_after.peek() {
            Some(ref open_bracket) if *open_bracket.as_token() == tokens::BracketLeft => {
                let args = tokens_after.match_groups(tokens::BracketLeft,
                                                     tokens::BracketRight,
                                                     tokens::Comma)?;

                let all_args: Vec<_> = args.groups.into_iter()
                    .map(|arg_group| {
                        let mut arg_group_stream = TokenStream::from(arg_group);
                        let arg_expr = Expression::parse_compound(&mut arg_group_stream)?;
                        arg_group_stream.finish()?;

                        Ok(arg_expr)
                    })
                    .collect::<Result<_, _>>()?;

                Ok(Expression::function_call(base, all_args))
            }

            // not a function call
            _ => Ok(base)
        }
    }

    fn parse_identifier(tokens: &mut TokenStream) -> ExpressionResult {
        /* the context of an identifier expression should be the first part of the
          identifier (if it has multiple parts) */
        let context = tokens.peek();
        let id = node::Identifier::parse(tokens)?;

        // safe to unwrap context because a token must have existed if the identifier parsed
        Ok(Expression::identifier(ParsedSymbol(id), context.unwrap()))
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
        tokens.match_one(keywords::Nil)?;
        Ok(Expression::literal_nil(tokens.context()))
    }

    fn parse_let_binding(tokens: &mut TokenStream) -> ExpressionResult {
        let binding_tokens = tokens.match_sequence(keywords::Let
            .and_then(Matcher::AnyIdentifier)
            .and_then(tokens::Operator(operators::Assignment)))?;

        let name = binding_tokens[1].unwrap_identifier();

        let value = Expression::parse(tokens)?;

        Ok(Expression::let_binding(binding_tokens[0].clone(), name, value))
    }

    fn parse_if(tokens: &mut TokenStream) -> ExpressionResult {
        let if_kw = tokens.match_one(keywords::If)?;

        /* the tokens between this "if" and the next "then" should contain exactly 1 expression
        and nothing else */
        let mut cond_tokens = TokenStream::from(tokens.match_until(keywords::Then)?);
        let condition = Expression::parse(&mut cond_tokens)?;
        cond_tokens.finish()?;

        if let Some(then_tokens) = tokens.match_block_peek(keywords::Then, keywords::Else)? {
            let then_len = then_tokens.len();

            let mut then_stream = TokenStream::new(then_tokens.inner, &then_tokens.open);
            let then_branch = Expression::parse(&mut then_stream)?;
            then_stream.finish()?;

            //advance to after the "else"
            tokens.advance(then_len);
            let else_branch = Expression::parse(tokens)?;

            Ok(Expression::if_then_else(condition, then_branch, Some(else_branch), if_kw))
        } else {
            tokens.match_one(keywords::Then)?;
            let then_branch = Expression::parse(tokens)?;

            Ok(Expression::if_then_else(condition, then_branch, None, if_kw))
        }
    }

    fn parse_for_loop(tokens: &mut TokenStream) -> ExpressionResult {
        let for_do_pair = tokens.match_block(keywords::For, keywords::Do)?;

        /* can't nest for loops in either the from or the to expression, so
        it's safe just to look for the next "to" */
        let mut for_cond_tokens = TokenStream::new(for_do_pair.inner, &for_do_pair.open);
        let split_at_to = for_cond_tokens.split_at_match(keywords::To)?;

        // the part before the "to" becomes the "from" expr
        let from_expr = Expression::parse(&mut TokenStream::new(
            split_at_to.before_split,
            &split_at_to.split_at.clone()))?;

        // the part between the "to" and the "do" becomes the "to" expr
        let to_expr = Expression::parse(&mut for_cond_tokens)?;

        // there should be no more tokens before the "do"
        for_cond_tokens.finish()?;

        let body_expr = Expression::parse(tokens)?;

        Ok(Expression::for_loop(from_expr,
                                to_expr,
                                body_expr,
                                for_do_pair.open))
    }

    fn parse_base(tokens: &mut TokenStream) -> ExpressionResult {
        /* this matcher should cover anything which can appear at the start of an expr */
        let match_expr_start = Matcher::AnyKeyword
            .or(Matcher::AnyIdentifier)
            .or(Matcher::AnyLiteralInteger)
            .or(Matcher::AnyLiteralString)
            .or(tokens::BracketLeft)
            .or(keywords::Nil)
            .or(Matcher::any_operator_in_position(operators::Position::Prefix));

        let expr_first = tokens.match_peek(match_expr_start.clone())?;

        /* we need to read the whole stream to see if there's any operators ahead */
        let all_tokens = tokens.peek_to_end();
        let contains_operator = count_operators_at_base_level(all_tokens.iter())? > 0;

        match expr_first {
            Some(ref if_kw) if if_kw.as_token().is_keyword(keywords::If) => {
                Expression::parse_if(tokens)
            }

            Some(ref for_kw) if for_kw.is_keyword(keywords::For) => {
                Expression::parse_for_loop(tokens)
            }

            Some(ref begin_kw) if begin_kw.is_keyword(keywords::Begin) => {
                let parsed_block = Block::parse(tokens)?;
                Ok(Expression::block(parsed_block))
            }

            Some(ref let_kw) if let_kw.is_keyword(keywords::Let) => {
                Expression::parse_let_binding(tokens)
            }

            Some(ref compound) if contains_operator ||
                *compound.as_token() == tokens::BracketLeft => {
                Expression::parse_compound(tokens)
            }

            Some(ref identifier) if identifier.is_any_identifier() => {
                Expression::parse_identifier(tokens)
            }

            Some(ref s) if s.is_any_literal_string() => {
                Expression::parse_literal_string(tokens)
            }

            Some(ref i) if i.is_any_literal_int() => {
                Expression::parse_literal_integer(tokens)
            }

            Some(ref nil) if nil.is_literal_nil() => {
                Expression::parse_literal_nil(tokens)
            }

            Some(unexpected) => {
                Err(ParseError::UnexpectedToken(unexpected, Some(match_expr_start)))
            }

            None => unreachable!("match_peek returns UnexpectedEOF if the stream is empty")
        }
    }

    pub fn parse(tokens: &mut TokenStream) -> ExpressionResult {
        let base = Expression::parse_base(tokens)?;

        match base.value {
            node::ExpressionValue::Identifier(_) |
            node::ExpressionValue::BinaryOperator { .. } |
            node::ExpressionValue::PrefixOperator { .. } |
            node::ExpressionValue::Member { .. } |
            node::ExpressionValue::FunctionCall { .. } => {
                let with_member_access = Expression::parse_member_access_after(base, tokens)?;
                Expression::parse_fn_call_after(with_member_access, tokens)
            }

            _ => Ok(base)
        }
    }
}

