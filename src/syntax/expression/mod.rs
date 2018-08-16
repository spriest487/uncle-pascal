#[cfg(test)]
mod test;

use syntax::*;
use tokens;
use keywords;
use tokens::AsToken;
use node;
use source;
use operators;

pub type Expression = node::Expression<ParsedSymbol, ParsedContext>;
pub type ExpressionResult = Result<Expression, ParseError>;

#[derive(Debug, Clone)]
struct OperatorToken {
    op: operators::Operator,
    pos: operators::Position,
    token: source::Token,
}

#[derive(Clone, Debug)]
struct CompoundOperand {
    expr: Expression,
    last_token: source::Token,
}

#[derive(Debug, Clone)]
enum CompoundExpressionPart {
    Operand(CompoundOperand),
    Operator(OperatorToken),
}

impl CompoundExpressionPart {
    fn unwrap_operand(self) -> CompoundOperand {
        match self {
            CompoundExpressionPart::Operand(operand) => operand,
            invalid @ _ => panic!("called unwrap_operand on {:?}", invalid)
        }
    }
}

fn any_operators_at_base_level(tokens: &mut TokenStream) -> ParseResult<bool> {
    let mut bracket_level = 0;

    for token in tokens.peeked() {
        if *token.as_token() == tokens::BracketLeft {
            bracket_level += 1
        } else if *token.as_token() == tokens::BracketRight {
            if bracket_level == 0 {
                return Err(ParseError::UnexpectedToken(token.clone(), None));
            }
            bracket_level -= 1
        } else if bracket_level == 0 && token.is_any_operator() {
            return Ok(true);
        }
    }

    Ok(false)
}

impl Expression {
    fn resolve_ops_by_precedence(parts: Vec<CompoundExpressionPart>) -> Result<Expression, ParseError> {
        assert!(parts.len() > 0, "expression must not be empty");

        if parts.len() == 1 {
            return Ok(match parts.into_iter().next().unwrap() {
                CompoundExpressionPart::Operand(CompoundOperand { expr, .. }) => expr,
                CompoundExpressionPart::Operator(op_token) =>
                    panic!("expression with one part must not be an operator (got: `{:?}`)", op_token),
            });
        }

        /* find the lowest-precedence operator in the expression, this becomes the
         outer expression */
        let (lo_op_index, lo_op) = parts.iter()
            .enumerate()
            .filter_map(|(i, part)| match part {
                &CompoundExpressionPart::Operand { .. } => None,
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

                let op_expr = Expression::prefix_op(lo_op.op, rhs.expr, lo_op.token);

                let merged_parts: Vec<_> = before_op.iter()
                    .cloned()
                    .chain(vec![CompoundExpressionPart::Operand(CompoundOperand {
                        expr: op_expr,
                        last_token: rhs.last_token,
                    })])
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
        let outer_brackets = tokens.peeked().match_block(tokens::BracketLeft, tokens::BracketRight);

        match outer_brackets {
            Some(brackets_block) => {
                let brackets_block_len = brackets_block.len();
                let mut inner_expr_tokens = TokenStream::new(brackets_block.inner,
                                                             &brackets_block.open);
                let inner_expr: Expression = inner_expr_tokens.parse_to_end()?;

                // seek to after brackets
                assert_eq!(brackets_block.open, tokens.peek().unwrap());
                tokens.advance(brackets_block_len);

                assert_eq!(brackets_block.close,
                           tokens.context().clone(),
                           "length of bracket block containing `{}` is {}, should advance to {}",
                           brackets_block_len,
                           inner_expr.to_source(),
                           brackets_block.close);

                /* expression of the form (a).b should include the .b after the brackets too */
                Expression::parse_member_access_after(inner_expr, tokens)
            }

            None => {
                tokens.parse::<Expression>()
            }
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

        /* the context e.g. the last token parsed. we have to track this separately from the
        stream because we might need to rewind the stream if it contains more than one
        expression */
        let mut expr_context = tokens.context().clone();

        let parts = tokens.match_groups_inner(tokens::BracketLeft, tokens::BracketRight, Matcher::AnyOperator)?;

        let mut operators = parts.separators.into_iter().peekable();
        let mut operands = parts.groups.into_iter().peekable();

        let mut parsed_parts: Vec<CompoundExpressionPart> = Vec::new();

        loop {
            let next_operator = operators.peek().cloned();
            let next_operand = operands.peek().map(|operand| &operand.tokens[0]).cloned();

            match (next_operator, next_operand) {
                (Some(ref operator), Some(ref operand))
                if operator.location.lt(&operand.location) => {
                    /* the next token is an operator and there's an operand following it */

                    let position = match parsed_parts.last() {
                        Some(CompoundExpressionPart::Operand { .. }) => {
                            /* we just had an operand, and there's another operand after this,
                            so it's a binary operator*/
                            operators::Position::Binary
                        }

                        None |
                        Some(CompoundExpressionPart::Operator(_)) => {
                            /* if the last part was also an operator, or we're
                            at the beginning of the expr, it must be a prefix operator */
                            operators::Position::Prefix
                        }
                    };

                    let op = operator.unwrap_operator();
                    if op.is_valid_in_pos(position) {
                        let operator = operators.next().unwrap();
                        parsed_parts.push({
                            CompoundExpressionPart::Operator(OperatorToken {
                                op: operator.unwrap_operator(),
                                pos: position,
                                token: operator,
                            })
                        });
                    } else {
                        /* this operation isn't valid, so we assume the expression must
                        terminate before this operator */
                        let last_part = parsed_parts.last();
                        if let Some(CompoundExpressionPart::Operand(operand)) = last_part {
                            /* if there's no last part we're at the beginning of the stream and
                            the expr context is already in the right place before the expr */
                            expr_context = operand.last_token.clone();
                        }

//                        println!("finished 1");
                        break;
                    }
                }

                (_, Some(_)) => {
                    /* found an operand which either occurs before the next operator, or occurs
                    after the last operator */
                    if let Some(CompoundExpressionPart::Operand { .. }) = parsed_parts.last() {
                        unreachable!("match_groups_inner should ensure all groups have at least one separator between them");
                    }

                    let operand = operands.next().unwrap();

                    let mut operand_tokens = TokenStream::new(operand.tokens, &operand.context);
                    let operand_expr = Expression::parse_operand(&mut operand_tokens)?;

                    parsed_parts.push(CompoundExpressionPart::Operand(CompoundOperand {
                        expr: operand_expr,
                        last_token: operand_tokens.context().clone(),
                    }));

                    /* if there were parts left over they must belong to the next expression,
                    stop parsing this one and set the context to the last token of this expression */
                    if let Some(_) = operand_tokens.peek() {
                        expr_context = operand_tokens.context().clone();

//                        println!("finished 2 with `{}`, context: `{}`",
//                            parsed_parts.last().cloned().unwrap().unwrap_operand().expr.to_source(),
//                            expr_context);
                        break;
                    }
                }

                /* found an operator without a following operand... there's no postfix
                operators yet so this must be part of the next expression */
                (Some(_), None) |

                /* reached the end of this expression */
                (None, None) => {
                    /* set the expr_context to the last token of the last part parsed */
                    match parsed_parts.last() {
                        Some(CompoundExpressionPart::Operand(operand)) => {
                            expr_context = operand.last_token.clone()
                        }
                        Some(CompoundExpressionPart::Operator(op)) => {
                            expr_context = op.token.clone();
                        }
                        None => {
                            //just an operator on its own!? leave the expr_context at the beginning
                        }
                    }
//                    println!("finished 3 with context: {}, last part {:?}", expr_context, parsed_parts.last().unwrap());

                    break;
                }
            }
        }

        assert!(parsed_parts.len() > 0, "expression must not be empty after {}", expr_context);

        let tokens_after: Vec<_> = all_tokens.into_iter()
            .filter(|t| t.location.gt(&expr_context.location))
            .collect();

        *tokens = TokenStream::new(tokens_after, &expr_context);

        Self::resolve_ops_by_precedence(parsed_parts)
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
        Ok(Expression::literal_nil(tokens.context().clone()))
    }

    fn parse_literal_bool(tokens: &mut TokenStream) -> ExpressionResult {
        let kw = tokens.match_one(keywords::True.or(keywords::False))?;
        let val = kw.is_keyword(keywords::True);
        Ok(Expression::literal_bool(val, kw))
    }

    fn parse_let_binding(tokens: &mut TokenStream) -> ExpressionResult {
        let binding_tokens = tokens.match_sequence(keywords::Let
            .and_then(Matcher::AnyIdentifier)
            .and_then(tokens::Operator(operators::Assignment)))?;

        let name = binding_tokens[1].unwrap_identifier();

        let value: Expression = tokens.parse()?;

        Ok(Expression::let_binding(name, value, binding_tokens[0].clone()))
    }

    fn parse_if(tokens: &mut TokenStream) -> ExpressionResult {
        let if_kw = tokens.match_one(keywords::If)?;

        /* the tokens between this "if" and the next "then" should contain exactly 1 expression
        and nothing else */
        let cond_tokens = TokenStream::from(tokens.match_until(keywords::Then)?);
        let condition: Expression = cond_tokens.parse_to_end()?;

        if let Some(then_tokens) = tokens.peeked().match_block(keywords::Then, keywords::Else) {
            let then_len = then_tokens.len();

            let then_stream = TokenStream::new(then_tokens.inner, &then_tokens.open);
            let then_branch: Expression = then_stream.parse_to_end()?;

            //advance to after the "else"
            tokens.advance(then_len);
            let else_branch: Expression = tokens.parse()?;

            Ok(Expression::if_then_else(condition, then_branch, Some(else_branch), if_kw))
        } else {
            tokens.match_one(keywords::Then)?;
            let then_branch: Expression = tokens.parse()?;

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
        let from_tokens = TokenStream::new(split_at_to.before_split,
                                           &split_at_to.split_at.clone());
        let from_expr: Expression = from_tokens.parse_to_end()?;

        // the part between the "to" and the "do" becomes the "to" expr
        let to_expr: Expression = for_cond_tokens.parse_to_end()?;

        let body_expr: Expression = tokens.parse()?;

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
            .or(keywords::True)
            .or(keywords::False)
            .or(tokens::BracketLeft)
            .or(keywords::Nil)
            .or(Matcher::any_operator_in_position(operators::Position::Prefix));

        let expr_first = tokens.peeked().match_one(match_expr_start.clone());

        let contains_operator = any_operators_at_base_level(tokens)?;

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

            Some(ref bool_kw) if bool_kw.is_keyword(keywords::True) ||
                bool_kw.is_keyword(keywords::False) => {
                Expression::parse_literal_bool(tokens)
            }

            Some(unexpected) => {
                Err(ParseError::UnexpectedToken(unexpected, Some(match_expr_start)))
            }

            None => Err(ParseError::UnexpectedEOF(match_expr_start, tokens.context().clone())),
        }
    }
}

impl Parse for Expression {
    fn parse(tokens: &mut TokenStream) -> ExpressionResult {
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

