#[cfg(test)]
mod test;

use syntax::*;
use tokens;
use keywords;
use tokens::AsToken;
use node;
use source;
use operators;

pub type Expression = node::Expression<ParsedContext>;
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

/* matches valueless statements/flow control */
fn match_statement() -> Matcher {
    keywords::Let
        .or(keywords::Begin)
        .or(keywords::If)
        .or(keywords::For)
}

/* this matcher should cover anything which can appear at the start of an expr */
fn match_expr_start() -> Matcher {
    match_statement().or(match_operand_start())
}

/* anything which can appear at the start of an operand subexpr (not let bindings
or flow control statements) */
fn match_operand_start() -> Matcher {
    Matcher::AnyIdentifier
        .or(tokens::BracketLeft)
        .or(keywords::True)
        .or(keywords::False)
        .or(Matcher::AnyLiteralInteger)
        .or(Matcher::AnyLiteralString)
        .or(Matcher::AnyLiteralFloat)
        .or(keywords::True)
        .or(keywords::False)
        .or(keywords::Nil)
        .or(Matcher::any_operator_in_position(operators::Position::Prefix))
}

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
            resolve_ops_by_precedence(merged_parts)
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

            let lhs_operand = resolve_ops_by_precedence(Vec::from(before_op))?;
            let rhs_operand = resolve_ops_by_precedence(after_op.iter()
                .skip(1)
                .cloned()
                .collect())?;

            let expr_context = lhs_operand.context.clone();

            Ok(Expression::binary_op(lhs_operand, lo_op.op, rhs_operand, expr_context))
        }
    }
}

impl Expression {
    fn parse_member_access_after(base: Expression, tokens_after: &mut TokenStream) -> ExpressionResult {
        tokens_after.match_one(tokens::Period)?;

        let member_name = node::Identifier::parse(tokens_after)?;

//      println!("found member name {} for base expr {:?}", member_name, base);
        let member = Expression::member_deep(base, member_name);
        Ok(member)
    }

    fn parse_fn_call_after(base: Expression, tokens: &mut TokenStream) -> ExpressionResult {
        tokens.match_one(tokens::BracketLeft)?;

        /* no args e.g. `a()` */
        if tokens.look_ahead().match_one(tokens::BracketRight).is_some() {
            tokens.advance(1);
            return Ok(Expression::function_call(base, vec![]))
        }

        /* parse args */
        let mut arg_exprs = Vec::new();
        loop {
            if arg_exprs.len() > 0 {
                match tokens.look_ahead().match_one(tokens::Comma) {
                    /* finished parsing one arg and there's no comma indicating another one
                    is coming, this must be the end of the args list */
                    None => break,
                    Some(_comma) => tokens.advance(1),
                }
            }

            let arg_expr = Expression::parse(tokens)?;
            arg_exprs.push(arg_expr);
        }
        tokens.match_one(tokens::BracketRight)?;

        Ok(Expression::function_call(base, arg_exprs))
    }

    fn parse_identifier(tokens: &mut TokenStream) -> ExpressionResult {
        /* the context of an identifier expression should be the first part of the
          identifier (if it has multiple parts) */
        let context = tokens.look_ahead().next();
        let id = node::Identifier::parse(tokens)?;

        // safe to unwrap context because a token must have existed if the identifier parsed
        Ok(Expression::identifier(id, context.unwrap()))
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

    fn parse_literal_float(tokens: &mut TokenStream) -> ExpressionResult {
        let float_token = tokens.match_one(Matcher::AnyLiteralFloat)?;

        let f = float_token.unwrap_literal_float();
        Ok(Expression::literal_float(f, float_token.clone()))
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
        let condition_expr: Expression = tokens.parse()?;

        tokens.match_one(keywords::Then)?;
        let then_expr: Expression = tokens.parse()?;

        // else present?
        let else_expr: Option<Expression> = match tokens.look_ahead().match_one(keywords::Else) {
            Some(_) => {
                tokens.advance(1);
                Some(tokens.parse()?)
            }

            None => None,
        };

        Ok(Expression::if_then_else(condition_expr, then_expr, else_expr, if_kw))
    }

    fn parse_for_loop(tokens: &mut TokenStream) -> ExpressionResult {
        let for_kw = tokens.match_one(keywords::For)?;
        let from_expr: Expression = tokens.parse()?;

        tokens.match_one(keywords::To)?;
        let to_expr: Expression = tokens.parse()?;

        tokens.match_one(keywords::Do)?;
        let body_expr: Expression = tokens.parse()?;

        Ok(Expression::for_loop(from_expr, to_expr, body_expr, ParsedContext::from(for_kw)))
    }
}

impl Parse for Expression {
    fn parse(tokens: &mut TokenStream) -> ExpressionResult {
        match tokens.look_ahead().match_one(match_statement()) {
            Some(ref kw) if kw.is_keyword(keywords::Let) => {
                return Self::parse_let_binding(tokens);
            }

            Some(ref kw) if kw.is_keyword(keywords::For) => {
                return Self::parse_for_loop(tokens);
            }

            Some(ref kw) if kw.is_keyword(keywords::If) => {
                return Self::parse_if(tokens);
            }

            Some(ref kw) if kw.is_keyword(keywords::Begin) => {
                let parsed_block = Block::parse(tokens)?;
                return Ok(Expression::block(parsed_block));
            }

            _ => { /* it's a value expression, keep going */ },
        }

        let mut compound_parts = Vec::new();

        /* an operand can't be followed by another operand, so each time we finish
        an operand, the next thing must be a member access, a list of arguments to
        the function the operand referred to, an array element access, or a binary
        operator connecting this operand to the next one */
        let mut last_was_operand = false;

        loop {
            let part = if !last_was_operand {
                last_was_operand = true;

                match tokens.look_ahead().match_one(match_operand_start()) {
                    /* this operand is enclosed in brackets, we expect to find an
                    entire valid subexpression then a close bracket */
                    Some(ref t) if t.is_token(&tokens::BracketLeft) => {
                        tokens.match_one(tokens::BracketLeft)?;
                        let subexpr = Expression::parse(tokens)?;
                        tokens.match_one(tokens::BracketRight)?;

                        CompoundExpressionPart::Operand(CompoundOperand {
                            expr: subexpr,
                            last_token: tokens.context().clone(),
                        })
                    }

                    /* it's an operator, but thanks to the match we know this operator
                    is valid in prefix position, so it's part of this expression */
                    Some(ref t) if t.is_any_operator() => {
                        // we need to parse another operand after this!
                        last_was_operand = false;

                        let op = tokens.match_one(Matcher::AnyOperator)?;

                        CompoundExpressionPart::Operator(OperatorToken {
                            op: op.unwrap_operator(),
                            token: op,
                            pos: operators::Position::Prefix,
                        })
                    }

                    /* the simple values */

                    Some(ref identifier) if identifier.is_any_identifier() => {
                        let expr = Expression::parse_identifier(tokens)?;

                        CompoundExpressionPart::Operand(CompoundOperand {
                            expr,
                            last_token: tokens.context().clone(),
                        })
                    }

                    Some(ref s) if s.is_any_literal_string() => {
                        let expr = Expression::parse_literal_string(tokens)?;
                        CompoundExpressionPart::Operand(CompoundOperand {
                            expr,
                            last_token: tokens.context().clone()
                        })
                    }

                    Some(ref i) if i.is_any_literal_int() => {
                        let expr = Expression::parse_literal_integer(tokens)?;
                        CompoundExpressionPart::Operand(CompoundOperand {
                            expr,
                            last_token: tokens.context().clone()
                        })
                    }

                    Some(ref f) if f.is_any_literal_float() => {
                        let expr = Expression::parse_literal_float(tokens)?;
                        CompoundExpressionPart::Operand(CompoundOperand {
                            expr,
                            last_token: tokens.context().clone()
                        })
                    }

                    Some(ref nil) if nil.is_literal_nil() => {
                        let expr = Expression::parse_literal_nil(tokens)?;
                        CompoundExpressionPart::Operand(CompoundOperand {
                            expr,
                            last_token: tokens.context().clone()
                        })
                    }

                    Some(ref bool_kw) if bool_kw.is_keyword(keywords::True) ||
                        bool_kw.is_keyword(keywords::False) => {
                        let expr = Expression::parse_literal_bool(tokens)?;
                        CompoundExpressionPart::Operand(CompoundOperand {
                            expr,
                            last_token: tokens.context().clone()
                        })
                    }

                    Some(_) => unreachable!("pattern excludes anything else"),

                    /* next token is not valid as part of an operand, so this expression
                    must end here */
                    None => break,
                }
            } else {
                let any_binary_op = Matcher::any_operator_in_position(operators::Position::Binary);
                let match_after_operand = any_binary_op
                    .or(tokens::SquareBracketLeft) // array element access
                    .or(tokens::BracketLeft) // function call argument list
                    .or(tokens::Period); // member access

                fn pop_operand(parts: &mut Vec<CompoundExpressionPart>) -> Expression {
                    match parts.pop() {
                        Some(CompoundExpressionPart::Operand(operand)) => {
                            operand.expr
                        }
                        _ => unreachable!("last should always exist and be an operand here"),
                    }
                };

                match tokens.look_ahead().match_one(match_after_operand) {
                    /* replace the last operand with a member access */
                    Some(ref t) if t.is_token(&tokens::Period) => {
                        let last_operand = pop_operand(&mut compound_parts);
                        let member_access = Self::parse_member_access_after(last_operand, tokens)?;

                        CompoundExpressionPart::Operand(CompoundOperand {
                            expr: member_access,
                            last_token: tokens.context().clone(),
                        })
                    }

                    /* replace the last operand with a function call */
                    Some(ref t) if t.is_token(&tokens::BracketLeft) => {
                        let last_operand = pop_operand(&mut compound_parts);
                        let fn_call = Self::parse_fn_call_after(last_operand, tokens)?;

                        CompoundExpressionPart::Operand(CompoundOperand {
                            expr: fn_call,
                            last_token: tokens.context().clone(),
                        })
                    }

                    /* replace the last operand with a function call */
                    Some(ref t) if t.is_token(&tokens::SquareBracketLeft) => {
                        unimplemented!("array element access")
                    }

                    // binary operator
                    Some(ref t) if t.is_any_operator() => {
                        // expect another operand afterwards
                        last_was_operand = false;
                        let op = tokens.match_one(Matcher::AnyOperator)?;

                        CompoundExpressionPart::Operator(OperatorToken {
                            op: op.unwrap_operator(),
                            token: op,
                            pos: operators::Position::Binary,
                        })
                    }

                    Some(_) => unreachable!("pattern excludes anything else"),

                    /* nothing following the last operand, which is fine, just end here */
                    None => break,
                }
            };

            compound_parts.push(part);
        }

        if compound_parts.len() == 0 {
            return Err(match tokens.look_ahead().next() {
                Some(unexpected) => {
                    ParseError::UnexpectedToken(unexpected, Some(match_expr_start()))
                },
                None => {
                    ParseError::UnexpectedEOF(match_expr_start(), tokens.context().clone())
                }
            })

        }

        resolve_ops_by_precedence(compound_parts)
    }
}
