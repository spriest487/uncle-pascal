#[cfg(test)]
mod test;

use syntax::*;
use tokens;
use keywords;
use tokens::AsToken;
use node::{
    self,
    ExpressionValue,
    SetMemberGroup,
    TypeName,
};
use source;
use operators;

pub type Expression = node::Expression<ParsedContext>;
pub type LetBinding = node::LetBinding<ParsedContext>;
pub type ObjectConstructorMember = node::ObjectConstructorMember<ParsedContext>;
pub type ObjectConstructor = node::ObjectConstructor<ParsedContext>;
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
fn match_statement_keyword() -> Matcher {
    keywords::Let
        .or(keywords::With)
        .or(keywords::Begin)
        .or(keywords::If)
        .or(keywords::While)
        .or(keywords::For)
        .or(keywords::Try)
        .or(keywords::Raise)
}

/* this matcher should cover anything which can appear at the start of an expr */
fn match_expr_start() -> Matcher {
    match_statement_keyword().or(match_operand_start())
}

/* anything which can appear at the start of an operand subexpr (not let bindings
or flow control statements) */
fn match_operand_start() -> Matcher {
    Matcher::AnyIdentifier
        // subexpr in brackets
        .or(tokens::BracketLeft)
        // literals
        .or(keywords::True)
        .or(keywords::False)
        .or(Matcher::AnyLiteralInteger)
        .or(Matcher::AnyLiteralString)
        .or(Matcher::AnyLiteralFloat)
        .or(keywords::Nil)
        // set constructor
        .or(tokens::SquareBracketLeft)
        // prefix operator applying to next operand
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

            let parts_after_op = &after_op[1..];

            let rhs = parts_after_op[0].clone().unwrap_operand();

            let op_expr = Expression::prefix_op(lo_op.op, rhs.expr, lo_op.token);

            let merged_parts: Vec<_> = before_op.iter()
                .cloned()
                .chain(vec![CompoundExpressionPart::Operand(CompoundOperand {
                    expr: op_expr,
                    last_token: rhs.last_token,
                })])
                .chain(parts_after_op[1..].iter().cloned())
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
        return Ok(Expression::function_call(base, vec![]));
    }

    /* parse args */
    let arg_exprs = tokens.match_repeating(|i, tokens| {
        if i > 0 {
            match tokens.look_ahead().match_one(tokens::Comma) {
                /* finished parsing one arg and there's no comma indicating another one
                is coming, this must be the end of the args list */
                None => return Ok(None),
                Some(_comma) => tokens.advance(1),
            }
        }

        Expression::parse(tokens).map(Option::from)
    })?;

    tokens.match_one(tokens::BracketRight)?;

    Ok(Expression::function_call(base, arg_exprs))
}

fn parse_array_element_after(base: Expression, tokens: &mut TokenStream) -> ExpressionResult {
    tokens.match_one(tokens::SquareBracketLeft)?;
    let index_expr = Expression::parse(tokens)?;
    tokens.match_one(tokens::SquareBracketRight)?;

    Ok(Expression::array_element(base, index_expr))
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
    let context = tokens.match_one(keywords::Let)?;

    // is it `let var`?
    let mutable = match tokens.look_ahead().match_one(keywords::Var) {
        | Some(_) => {
            tokens.advance(1);
            true
        }
        | None => false,
    };

    let name = tokens.match_one(Matcher::AnyIdentifier)?
        .unwrap_identifier()
        .to_string();

    /* is there an explicit type? */
    let explicit_type = match tokens.look_ahead().match_one(tokens::Colon) {
        Some(_) => {
            tokens.advance(1);
            Some(TypeName::parse(tokens)?)
        }
        None => None
    };

    /* `let` uses `= value`, `let var` uses `:= value` */
    tokens.match_one(match mutable {
        true => operators::Assignment,
        false => operators::Equals,
    })?;

    let value: Expression = tokens.parse()?;

    let binding = LetBinding {
        name,
        value: Box::new(value),
        explicit_type,
        mutable
    };

    Ok(Expression::let_binding(binding, context))
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

fn parse_while_loop(tokens: &mut TokenStream) -> ExpressionResult {
    let context = tokens.match_one(keywords::While)?;
    let condition = Expression::parse(tokens)?;
    tokens.match_one(keywords::Do)?;
    let body = Expression::parse(tokens)?;

    Ok(Expression::while_loop(condition, body, context))
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

fn parse_bracket_group(tokens: &mut TokenStream) -> ExpressionResult {
    tokens.match_one(tokens::BracketLeft)?;
    let subexpr = Expression::parse(tokens)?;
    tokens.match_one(tokens::BracketRight)?;
    Ok(subexpr)
}

fn match_statements_terminated_by(tokens: &mut TokenStream,
                                  terminator: impl Into<Matcher>)
                                  -> ParseResult<Vec<Expression>> {
    let terminator = terminator.into();
    tokens.match_repeating(|_i, tokens: &mut TokenStream| {
        // empty statements
        while tokens.look_ahead().match_one(tokens::Semicolon).is_some() {
            tokens.advance(1);
        }

        match tokens.look_ahead().next() {
            Some(ref t) if terminator.is_match(t) => return Ok(None),
            _ => {
                let expr = Expression::parse(tokens).map(Option::from)?;
                tokens.match_or_endl(tokens::Semicolon)?;
                Ok(expr)
            }
        }
    })
}

fn parse_try_except(tokens: &mut TokenStream) -> ExpressionResult {
    let context = tokens.match_one(keywords::Try)?;
    eprintln!("exception handling is not supported! found `try` block starting at {}", context);

    let try_exprs = match_statements_terminated_by(tokens, keywords::Finally.or(keywords::Except))?;

    // except block is not used but must be parsed anyway
    let _except_exprs = match tokens.look_ahead().match_one(keywords::Except) {
        Some(_) => {
            tokens.advance(1);
            match_statements_terminated_by(tokens, keywords::Finally.or(keywords::End))?
        }
        None => Vec::new(),
    };

    let finally_exprs = match tokens.look_ahead().match_one(keywords::Finally) {
        Some(_) => {
            tokens.advance(1);
            match_statements_terminated_by(tokens, keywords::End)?
        }
        None => Vec::new(),
    };

    tokens.match_one(keywords::End)?;
    tokens.match_or_endl(tokens::Semicolon)?;

    // output as block with try + finally exprs, except gets ignored
    Ok(Expression::block(Block {
        statements: {
            let mut all_exprs = try_exprs;
            all_exprs.extend(finally_exprs);
            all_exprs
        },
        context: context.into(),
    }))
}

fn parse_set_constructor(tokens: &mut TokenStream) -> ExpressionResult {
    let context = tokens.match_one(tokens::SquareBracketLeft)?;

    let groups = tokens.match_repeating(|i, tokens: &mut TokenStream| {
        if i > 0 {
            match tokens.look_ahead().match_one(tokens::Comma) {
                Some(_) => tokens.advance(1),
                None => return Ok(None),
            }
        }

        let group_expr = Expression::parse(tokens)?;
        let (from, to) = match group_expr.value {
            ExpressionValue::BinaryOperator { lhs, op: operators::RangeInclusive, rhs } => {
                (*lhs, Some(*rhs))
            }

            from_value @ _ => {
                let from_expr = Expression {
                    context: group_expr.context,
                    value: from_value,
                };
                (from_expr, None)
            }
        };

        Ok(Some(SetMemberGroup {
            from,
            to,
        }))
    })?;

    tokens.match_one(tokens::SquareBracketRight)?;

    Ok(Expression::set_constructor(groups, context))
}

fn parse_with_statement(tokens: &mut TokenStream) -> ExpressionResult {
    let context = tokens.match_one(keywords::With)?;
    let value = tokens.parse()?;
    tokens.match_one(keywords::Do)?;
    let body = tokens.parse()?;

    Ok(Expression::with_statement(value, body, context))
}

fn parse_raise(tokens: &mut TokenStream) -> ExpressionResult {
    let context = tokens.match_one(keywords::Raise)?;
    let error = Expression::parse(tokens)?;

    Ok(Expression::raise(error, context))
}

fn parse_object_constructor(tokens: &mut TokenStream) -> ExpressionResult {
    let context = tokens.match_one(tokens::BracketLeft)?;

    let members = tokens.match_repeating(|i, tokens: &mut TokenStream| {
        if tokens.look_ahead().match_one(tokens::BracketRight).is_some() {
            return Ok(None)
        }

        if i > 0 {
            tokens.match_or_endl(tokens::Semicolon)?;
        }

        /* the last member might be followed by a separator too, which is legal */
        if tokens.look_ahead().match_one(tokens::BracketRight).is_some() {
            return Ok(None)
        }

        let name = tokens.match_one(Matcher::AnyIdentifier)?
            .unwrap_identifier()
            .to_string();

        tokens.match_one(tokens::Colon)?;
        let value = Expression::parse(tokens)?;

        Ok(Some(ObjectConstructorMember {
            name,
            value,
        }))
    })?;

    let object_constructor = ObjectConstructor {
        members,
        object_type: None, // this is inferred during typechecking
    };

    tokens.match_one(tokens::BracketRight)?;
    Ok(Expression::object_constructor(object_constructor, context))
}

struct CompoundExpressionParser<'tokens> {
    tokens: &'tokens mut TokenStream,
    last_was_operand: bool,
    parts: Vec<CompoundExpressionPart>,
}

impl<'tokens> CompoundExpressionParser<'tokens> {
    fn new(tokens: &'tokens mut TokenStream) -> Self {
        CompoundExpressionParser {
            tokens,
            last_was_operand: false,
            parts: Vec::new(),
        }
    }

    fn parse(mut self) -> ExpressionResult {
        loop {
            /* an operand can't be followed by another operand, so each time we finish
            an operand, the next thing must be a member access, a list of arguments to
            the function the operand referred to, an array element access, or a binary
            operator connecting this operand to the next one */
            let more = match self.last_was_operand {
                false => self.parse_operand()?,
                true => self.parse_operator()?,
            };

            if !more {
                break {
                    if self.parts.len() == 0 {
                        let expected = match_expr_start();
                        return Err(match self.tokens.look_ahead().next() {
                            Some(unexpected) =>
                                ParseError::UnexpectedToken(unexpected, Some(expected)),
                            None => {
                                let after = self.tokens.context().clone();
                                ParseError::UnexpectedEOF(expected, after)
                            }
                        });
                    }

                    resolve_ops_by_precedence(self.parts)
                };
            }
        }
    }

    fn parse_operand(&mut self) -> ParseResult<bool> {
        self.last_was_operand = true;

        match self.tokens.look_ahead().match_one(match_operand_start()) {
            Some(ref t) if t.is_token(&tokens::BracketLeft) => {
                /* if it closes immediately, or has at least one member in the
                name: value pattern, it's a object constructor */
                let empty_obj = tokens::BracketLeft
                    .and_then(tokens::BracketRight);
                let obj_member = tokens::BracketLeft
                    .and_then(Matcher::AnyIdentifier)
                    .and_then(tokens::Colon);

                if self.tokens.look_ahead().match_sequence(empty_obj).is_some()
                    || self.tokens.look_ahead().match_sequence(obj_member).is_some() {
                    let constructor = parse_object_constructor(self.tokens)?;
                    self.add_operand(constructor);
                } else {
                    /*
                        this is a subexpression enclosed in brackets
                        parse everything until the closing bracket as part of this operand
                    */
                    let subexpr = parse_bracket_group(self.tokens)?;
                    self.add_operand(subexpr);
                }
            }

            Some(ref t) if t.is_token(&tokens::SquareBracketLeft) => {
                let constructor = parse_set_constructor(self.tokens)?;
                self.add_operand(constructor);
            }

            /* it's an operator, buet thanks to the match we know this operator
            is valid in prefix position, so it's part of this expression */
            Some(ref t) if t.is_any_operator() => {
                // we need to parse another operand after this!
                self.last_was_operand = false;
                let op = self.tokens.match_one(Matcher::AnyOperator)?;
                self.add_operator(op, operators::Position::Prefix);
            }

            /* the simple values */

            Some(ref identifier) if identifier.is_any_identifier() => {
                let expr = parse_identifier(self.tokens)?;
                self.add_operand(expr);
            }

            Some(ref s) if s.is_any_literal_string() => {
                let expr = parse_literal_string(self.tokens)?;
                self.add_operand(expr);
            }

            Some(ref i) if i.is_any_literal_int() => {
                let expr = parse_literal_integer(self.tokens)?;
                self.add_operand(expr);
            }

            Some(ref f) if f.is_any_literal_float() => {
                let expr = parse_literal_float(self.tokens)?;
                self.add_operand(expr);
            }

            Some(ref nil) if nil.is_literal_nil() => {
                let expr = parse_literal_nil(self.tokens)?;
                self.add_operand(expr);
            }

            Some(ref bool_kw) if Matcher::AnyLiteralBoolean.is_match(bool_kw.as_token()) => {
                let expr = parse_literal_bool(self.tokens)?;
                self.add_operand(expr);
            }

            Some(_) => unreachable!("pattern excludes anything else"),

            /* next token is not valid as part of an operand, so this expression
            must end here */
            None => return Ok(false),
        }

        Ok(true)
    }

    fn add_operand(&mut self, expr: Expression) {
        let part = CompoundExpressionPart::Operand(CompoundOperand {
            expr,
            last_token: self.tokens.context().clone(),
        });

        self.parts.push(part);
    }

    fn pop_operand(&mut self) -> Expression {
        match self.parts.pop() {
            Some(CompoundExpressionPart::Operand(operand)) => {
                operand.expr
            }
            _ => unreachable!("last should always exist and be an operand here"),
        }
    }

    fn parse_operator(&mut self) -> ParseResult<bool> {
        let any_binary_op = Matcher::any_operator_in_position(operators::Position::Binary);
        let match_after_operand = any_binary_op
            .or(tokens::SquareBracketLeft) // array element access
            .or(tokens::BracketLeft) // function call argument list
            .or(tokens::Period); // member access

        match self.tokens.look_ahead().match_one(match_after_operand) {
            /* replace the last operand with a member access */
            Some(ref t) if t.is_token(&tokens::Period) => {
                let last_operand = self.pop_operand();
                let member_access = parse_member_access_after(last_operand, self.tokens)?;
                self.add_operand(member_access);
            }

            /* replace the last operand with a function call */
            Some(ref t) if t.is_token(&tokens::BracketLeft) => {
                let last_operand = self.pop_operand();
                let fn_call = parse_fn_call_after(last_operand, self.tokens)?;
                self.add_operand(fn_call);
            }

            /* replace the last operand with a function call */
            Some(ref t) if t.is_token(&tokens::SquareBracketLeft) => {
                let last_operand = self.pop_operand();
                let element_expr = parse_array_element_after(last_operand, self.tokens)?;
                self.add_operand(element_expr);
            }

            // binary operator
            Some(ref t) if t.is_any_operator() => {
                // expect another operand afterwards
                self.last_was_operand = false;
                let op = self.tokens.match_one(Matcher::AnyOperator)?;
                self.add_operator(op, operators::Position::Binary);
            }

            Some(_) => unreachable!("pattern excludes anything else"),

            /* nothing following the last operand, which is fine, just end here */
            None => return Ok(false),
        }

        Ok(true)
    }

    fn add_operator(&mut self, op_token: source::Token, pos: operators::Position) {
        let part = CompoundExpressionPart::Operator(OperatorToken {
            op: op_token.unwrap_operator(),
            pos,
            token: op_token,
        });

        self.parts.push(part);
    }
}

impl Parse for Expression {
    fn parse(tokens: &mut TokenStream) -> ExpressionResult {
        let first_kw = tokens.look_ahead()
            .match_one(match_statement_keyword())
            .map(|token| token.unwrap_keyword());

        match first_kw {
            /* it's a structured statement, it must consist of this one statement
            and nothing else */
            Some(keywords::Let) => parse_let_binding(tokens),
            Some(keywords::For) => parse_for_loop(tokens),
            Some(keywords::If) => parse_if(tokens),
            Some(keywords::While) => parse_while_loop(tokens),
            Some(keywords::Try) => parse_try_except(tokens),
            Some(keywords::With) => parse_with_statement(tokens),
            Some(keywords::Raise) => parse_raise(tokens),
            Some(keywords::Begin) => {
                let parsed_block = Block::parse(tokens)?;
                Ok(Expression::block(parsed_block))
            }

            /* it's a compound value expression, parse parts until we run out
            of valid-looking tokens and try to form an expression out of them */
            _ => CompoundExpressionParser::new(tokens).parse(),
        }
    }
}
