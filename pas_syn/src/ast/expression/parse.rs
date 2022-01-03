use crate::{
    ast::{
        ArgList, BinOp, Block, Call, CollectionCtor, FunctionCall, IfCond,
        ObjectCtor, ObjectCtorArgs, TypeList, TypeName, UnaryOp, Raise,
    },
    operators::*,
    parse::*,
    token_tree::*,
    Keyword,
};
use pas_common::{span::*, TracedError};
use std::{cmp::Ordering};
use crate::ast::{CaseExpr, Expression, Literal};

// anything which can appear at the start of an operand subexpr (not let bindings
// or flow control statements)
pub fn match_operand_start() -> Matcher {
    Matcher::AnyIdent
        // subexpr in brackets or object ctor
        .or(DelimiterPair::Bracket)
        // collection constructor
        .or(DelimiterPair::SquareBracket)
        // block/control flow
        .or(DelimiterPair::BeginEnd)
        .or(Keyword::Unsafe)
        .or(Keyword::If)
        .or(Keyword::Raise)
        .or(Keyword::Case)
        // literals
        .or(Keyword::Nil)
        .or(Matcher::AnyLiteralBoolean)
        .or(Matcher::AnyLiteralInteger)
        .or(Matcher::AnyLiteralString)
        .or(Matcher::AnyLiteralReal)
        .or(Keyword::SizeOf)
        // prefix operator applying to next operand
        .or(Matcher::any_operator_in_position(Position::Prefix))
}

fn resolve_ops_by_precedence(parts: Vec<CompoundExpressionPart>) -> ParseResult<Expression<Span>> {
    assert!(!parts.is_empty(), "expression must not be empty");

    if parts.len() == 1 {
        return Ok(match parts.into_iter().next().unwrap() {
            CompoundExpressionPart::Operand(operand) => operand,
            CompoundExpressionPart::Operator(op_part) => {
                let err = ParseError::UnexpectedOperator {
                    operator: match op_part {
                        OperatorPart::Call { args, .. } => args.open,
                        OperatorPart::OperatorSymbol(op_symbol) => op_symbol.span.clone(),
                    },
                };
                return Err(TracedError::trace(err));
            }
        });
    }

    // find the lowest-precedence operator in the expression, this becomes the
    // outer expression
    let (lo_op_index, lo_op) = parts
        .iter()
        .enumerate()
        .filter_map(|(i, part)| match part {
            CompoundExpressionPart::Operand { .. } => None,
            CompoundExpressionPart::Operator(op) => Some((i, op.clone())),
        })
        .max_by(|(_, op_a), (_, op_b)| op_a.cmp_precedence(op_b))
        .unwrap();

    match lo_op {
        OperatorPart::Call { args, type_args } => {
            let (before_op, after_op) = parts.split_at(lo_op_index);

            if before_op.is_empty() {
                return Err(TracedError::trace(ParseError::EmptyOperand {
                    operator: args.list_span(),
                    before: true,
                }));
            }

            // everything on the left becomes the target
            let target = resolve_ops_by_precedence(before_op.to_vec())?;
            let span = target.annotation().span().to(&args.close);

            let op_expr = Expression::from(Call::Function(FunctionCall {
                target,
                annotation: span.clone(),
                args: args.args,
                type_args,
                args_brackets: (args.open, args.close),
            }));

            let merged_parts: Vec<_> = vec![CompoundExpressionPart::Operand(op_expr)]
                .into_iter()
                .chain(after_op[1..].iter().cloned())
                .collect();

            assert!(!merged_parts.is_empty());
            resolve_ops_by_precedence(merged_parts)
        }

        OperatorPart::OperatorSymbol(op_token) => match op_token.pos {
            // merge prefix operator with the operand that follows it
            Position::Binary => {
                let (before_op, after_op) = parts.split_at(lo_op_index);

                if before_op.is_empty() {
                    return Err(TracedError::trace(ParseError::EmptyOperand {
                        operator: op_token.span.clone(),
                        before: true,
                    }));
                }

                // 1 because the op is included in this (?)
                if after_op.len() <= 1 {
                    return Err(TracedError::trace(ParseError::EmptyOperand {
                        operator: op_token.span.clone(),
                        before: false,
                    }));
                }

                let lhs_operand = resolve_ops_by_precedence(Vec::from(before_op))?;
                let rhs_operand =
                    resolve_ops_by_precedence(after_op.iter().skip(1).cloned().collect())?;

                let span = lhs_operand.annotation().to(rhs_operand.annotation());
                let bin_op = BinOp {
                    lhs: lhs_operand,
                    op: op_token.op,
                    rhs: rhs_operand,
                    annotation: span.clone(),
                };

                Ok(Expression::from(bin_op))
            }

            Position::Prefix => {
                let (before_op, after_op) = parts.split_at(lo_op_index);

                if after_op.is_empty() {
                    return Err(TracedError::trace(ParseError::EmptyOperand {
                        operator: op_token.span.clone(),
                        before: false,
                    }));
                }

                let parts_after_op = &after_op[1..];

                let rhs = parts_after_op[0].clone().unwrap_operand();

                let op_expr = {
                    let span = op_token.span.to(rhs.annotation().span());
                    let unary_op = UnaryOp {
                        op: op_token.op,
                        operand: rhs,
                        annotation: span.clone(),
                    };
                    Expression::from(unary_op)
                };

                let merged_parts: Vec<_> = before_op
                    .iter()
                    .cloned()
                    .chain(vec![CompoundExpressionPart::Operand(op_expr)])
                    .chain(parts_after_op[1..].iter().cloned())
                    .collect();

                assert!(!merged_parts.is_empty());
                resolve_ops_by_precedence(merged_parts)
            }

            Position::Postfix => {
                let (before_op, after_op) = parts.split_at(lo_op_index);

                if before_op.is_empty() {
                    return Err(TracedError::trace(ParseError::EmptyOperand {
                        operator: op_token.span.clone(),
                        before: true,
                    }));
                }

                // everything on the left becomes the operand
                let operand = resolve_ops_by_precedence(before_op.to_vec())?;
                let span = op_token.span.to(operand.annotation().span());

                let op_expr = Expression::from(UnaryOp {
                    op: op_token.op,
                    annotation: span.clone(),
                    operand,
                });

                let merged_parts: Vec<_> = vec![CompoundExpressionPart::Operand(op_expr)]
                    .into_iter()
                    .chain(after_op[1..].iter().cloned())
                    .collect();

                assert!(!merged_parts.is_empty());
                resolve_ops_by_precedence(merged_parts)
            }
        },
    }
}

fn parse_identifier(tokens: &mut TokenStream) -> ParseResult<Expression<Span>> {
    // the context of an identifier expression should be the first part of the
    // identifier (if it has multiple parts)
    match tokens.match_one(Matcher::AnyIdent)? {
        TokenTree::Ident(ident) => {
            let span = ident.span.clone();
            Ok(Expression::Ident(ident, span))
        }
        _ => unreachable!(),
    }
}

fn parse_literal_string(tokens: &mut TokenStream) -> ParseResult<Expression<Span>> {
    match tokens.match_one(Matcher::AnyLiteralString)? {
        TokenTree::String { value, span } => {
            let str_lit = Literal::String(value);
            Ok(Expression::Literal(str_lit, span))
        }
        _ => unreachable!(),
    }
}

fn parse_literal_integer(tokens: &mut TokenStream) -> ParseResult<Expression<Span>> {
    match tokens.match_one(Matcher::AnyLiteralInteger)? {
        TokenTree::IntNumber { value, span } => {
            let int_lit = Literal::Integer(value);
            Ok(Expression::Literal(int_lit, span))
        }
        _ => unreachable!(),
    }
}

fn parse_literal_real(tokens: &mut TokenStream) -> ParseResult<Expression<Span>> {
    match tokens.match_one(Matcher::AnyLiteralReal)? {
        TokenTree::RealNumber { value, span } => {
            let real_lit = Literal::Real(value);
            Ok(Expression::Literal(real_lit, span))
        }

        _ => unreachable!(),
    }
}

fn parse_literal_bool(tokens: &mut TokenStream) -> ParseResult<Expression<Span>> {
    let kw = tokens.match_one(Keyword::True.or(Keyword::False))?;
    let val = kw.is_keyword(Keyword::True);
    let expr = Expression::Literal(Literal::Boolean(val), kw.span().clone());
    Ok(expr)
}

fn parse_size_of(tokens: &mut TokenStream) -> ParseResult<Expression<Span>> {
    let kw = tokens.match_one(Keyword::SizeOf)?;

    let (ty_tokens, close_bracket) = match tokens.match_one(DelimiterPair::Bracket)? {
        TokenTree::Delimited { inner, close, .. } =>  (inner, close),
        _ => unreachable!(),
    };

    let mut ty_token_stream = TokenStream::new(ty_tokens, kw.span().clone());
    let ty = TypeName::parse(&mut ty_token_stream)?;
    ty_token_stream.finish()?;

    let span = kw.span().to(close_bracket.span());

    Ok(Expression::Literal(
        Literal::SizeOf(Box::new(ty)),
        span,
    ))
}

pub fn parse_case_expr(tokens: &mut TokenStream) -> ParseResult<Expression<Span>> {
    let case = CaseExpr::parse(tokens)?;
    let expr = Expression::Case(Box::new(case));

    Ok(expr)
}

#[derive(Debug, Clone)]
struct SymbolOperator {
    op: Operator,
    pos: Position,
    span: Span,
}

#[derive(Debug, Clone)]
enum OperatorPart {
    // symbol operator e.g. +, *
    OperatorSymbol(SymbolOperator),

    // () operator with inner argument list
    Call {
        args: ArgList<Span>,
        type_args: Option<TypeList<TypeName>>,
    },
}

impl OperatorPart {
    fn cmp_precedence(&self, b: &Self) -> Ordering {
        let ((op_a, pos_a), (op_b, pos_b)) = match (self, b) {
            (OperatorPart::Call { .. }, OperatorPart::Call { .. }) => (
                (Operator::Call, Position::Postfix),
                (Operator::Call, Position::Postfix),
            ),
            (OperatorPart::Call { .. }, OperatorPart::OperatorSymbol(sym_op)) => {
                ((Operator::Call, Position::Postfix), (sym_op.op, sym_op.pos))
            }
            (OperatorPart::OperatorSymbol(sym_op), OperatorPart::Call { .. }) => {
                ((sym_op.op, sym_op.pos), (Operator::Call, Position::Postfix))
            }
            (OperatorPart::OperatorSymbol(sym_op_a), OperatorPart::OperatorSymbol(sym_op_b)) => {
                ((sym_op_a.op, sym_op_a.pos), (sym_op_b.op, sym_op_b.pos))
            }
        };

        let prec_a = op_a.precedence(pos_a);
        let prec_b = op_b.precedence(pos_b);
        prec_a.cmp(&prec_b)
    }
}

#[derive(Debug, Clone)]
enum CompoundExpressionPart {
    Operand(Expression<Span>),
    Operator(OperatorPart),
}

impl CompoundExpressionPart {
    fn unwrap_operand(self) -> Expression<Span> {
        match self {
            CompoundExpressionPart::Operand(expr) => expr,
            unexpected => panic!("called unwrap_operand on {:?}", unexpected),
        }
    }
}

pub struct CompoundExpressionParser<'tokens> {
    tokens: &'tokens mut TokenStream,
    last_was_operand: bool,
    parts: Vec<CompoundExpressionPart>,
}

impl<'tokens> CompoundExpressionParser<'tokens> {
    pub fn new(tokens: &'tokens mut TokenStream) -> Self {
        CompoundExpressionParser {
            tokens,
            last_was_operand: false,
            parts: Vec::new(),
        }
    }

    pub fn parse(mut self) -> ParseResult<Expression<Span>> {
        loop {
            // an operand can't be followed by another operand, so each time we finish
            // an operand, the next thing must be a member access, a list of arguments to
            // the function the operand referred to, an array element access, or a binary
            // operator connecting this operand to the next one
            let more = if !self.last_was_operand {
                self.parse_operand()?
            } else {
                self.parse_operator()?
            };

            if !more {
                break {
                    if self.parts.is_empty() {
                        let expected = match_operand_start();
                        return Err(TracedError::trace(match self.tokens.look_ahead().next() {
                            Some(unexpected) => {
                                ParseError::UnexpectedToken(Box::new(unexpected), Some(expected))
                            }
                            None => {
                                let after = self.tokens.context().clone();
                                ParseError::UnexpectedEOF(expected, after)
                            }
                        }));
                    }

                    resolve_ops_by_precedence(self.parts)
                };
            }
        }
    }

    fn parse_operand(&mut self) -> ParseResult<bool> {
        self.last_was_operand = true;

        match self.tokens.look_ahead().match_one(match_operand_start()) {
            Some(TokenTree::Delimited {
                     delim, inner, open, ..
                 }) => {
                let mut group_tokens = TokenStream::new(inner.clone(), open);

                match delim {
                    // operand is a () group: must be a sub-expression
                    DelimiterPair::Bracket => {
                        let subexpr = Expression::parse(&mut group_tokens)?;
                        group_tokens.finish()?;
                        self.tokens.advance(1);
                        self.push_operand(subexpr);
                    }

                    // operand is a [] group: must be a collection ctor
                    DelimiterPair::SquareBracket => {
                        let ctor = CollectionCtor::parse(self.tokens)?;
                        let expr = Expression::from(ctor);
                        self.push_operand(expr);
                    }

                    // operand is a begin/end group: must be a block
                    DelimiterPair::BeginEnd => {
                        let block = Block::parse(self.tokens)?;
                        let expr = Expression::from(block);
                        self.push_operand(expr);
                    }
                }
            }

            // operand is an unsafe block
            Some(tt) if tt.is_keyword(Keyword::Unsafe) => {
                let block = Block::parse(self.tokens)?;
                let expr = Expression::from(block);
                self.push_operand(expr);
            }

            // it's an operator, but thanks to the match we know this operator
            // is valid in prefix position, so it's part of this expression
            Some(TokenTree::Operator { .. }) => {
                // we need to parse another operand after this!
                self.last_was_operand = false;
                let op = self.tokens.match_one(Matcher::AnyOperator)?;
                self.push_operator_token(op, Position::Prefix);
            }

            // the simple values
            Some(TokenTree::Ident(_)) => {
                let expr = parse_identifier(self.tokens)?;
                self.push_operand(expr);
            }

            Some(TokenTree::String { .. }) => {
                let expr = parse_literal_string(self.tokens)?;
                self.push_operand(expr);
            }

            Some(TokenTree::IntNumber { .. }) => {
                let expr = parse_literal_integer(self.tokens)?;
                self.push_operand(expr);
            }

            Some(tt) if tt.is_keyword(Keyword::Nil) => {
                let nil_token = self.tokens.next().unwrap();
                self.push_operand(Expression::Literal(Literal::Nil, nil_token.span().clone()));
            }

            Some(tt) if tt.is_keyword(Keyword::SizeOf) => {
                let size_of_expr = parse_size_of(self.tokens)?;
                self.push_operand(size_of_expr);
            }

            Some(tt) if tt.is_keyword(Keyword::If) => {
                let cond = IfCond::parse(self.tokens)?;
                let expr = Expression::from(cond);
                self.push_operand(expr);
            }

            Some(tt) if tt.is_keyword(Keyword::True) || tt.is_keyword(Keyword::False) => {
                let expr = parse_literal_bool(self.tokens)?;
                self.push_operand(expr);
            }

            Some(tt) if tt.is_keyword(Keyword::Raise) => {
                let raise = Raise::parse(self.tokens)?;
                let expr = Expression::from(raise);
                self.push_operand(expr);
            }

            Some(tt) if tt.is_keyword(Keyword::Case) => {
                let expr = parse_case_expr(self.tokens)?;
                self.push_operand(expr);
            }

            Some(TokenTree::RealNumber { .. }) => {
                let expr = parse_literal_real(self.tokens)?;
                self.push_operand(expr);
            }

            Some(x) => unreachable!("got {:?} which is excluded by pattern", x),

            // next token is not valid as part of an operand, so this expression
            // must end here
            None => return Ok(false),
        }

        Ok(true)
    }

    fn push_operand(&mut self, expr: Expression<Span>) {
        let part = CompoundExpressionPart::Operand(expr);
        self.parts.push(part);
    }

    fn pop_operand(&mut self) -> Expression<Span> {
        match self.parts.pop() {
            Some(CompoundExpressionPart::Operand(operand)) => operand,
            _ => unreachable!("last should always exist and be an operand here"),
        }
    }

    fn parse_invocation(&mut self, ty_args: Option<TypeList<TypeName>>) -> ParseResult<()> {
        // replace the last operand with a function call targeting that expr
        let last_was_ident = match self.parts.last() {
            Some(CompoundExpressionPart::Operand(expr)) => expr.as_ident().is_some(),
            _ => false,
        };

        let match_arg_list = Matcher::Delimited(DelimiterPair::Bracket);
        let mut inner_tokens = match self.tokens.look_ahead().next() {
            Some(TokenTree::Delimited { inner, open, .. }) => TokenStream::new(inner, open),

            Some(bad) => {
                return Err(TracedError::trace(ParseError::UnexpectedToken(
                    Box::new(bad),
                    Some(match_arg_list),
                )))
            }
            None => {
                return Err(TracedError::trace(ParseError::UnexpectedEOF(
                    match_arg_list,
                    self.tokens.context().clone(),
                )))
            }
        };

        let ctor_matcher = Matcher::AnyIdent.and_then(Separator::Colon);

        // if the next two tokens are in the form `a:` AND the last operand is
        // an ident then it has to be an object constructor list instead of a call
        let is_ctor_ahead = inner_tokens
            .look_ahead()
            .match_sequence(ctor_matcher)
            .is_some();

        if is_ctor_ahead && last_was_ident {
            if let Some(ty_args) = ty_args {
                return Err(TracedError::trace(ParseError::CtorWithTypeArgs {
                    span: ty_args.span().clone(),
                }));
            }

            let args = ObjectCtorArgs::parse(&mut self.tokens)?;
            let ident = self.pop_operand().into_ident().unwrap();

            let span = ident.span().to(&args.close);
            let ctor = ObjectCtor {
                ident: ident.into(),
                args,
                annotation: span.clone(),
            };

            self.push_operand(Expression::from(ctor));
        } else {
            let args = ArgList::parse(&mut self.tokens)?;

            self.push_operator_call(args, ty_args);
        }

        self.last_was_operand = true;

        Ok(())
    }

    fn parse_member_access(&mut self) -> ParseResult<()> {
        let member_op_tt = self.tokens.match_one(Operator::Member)?;

        self.push_operator_token(member_op_tt, Position::Binary);

        let member_ident = self.tokens.match_one(Matcher::AnyIdent)?.into_ident().unwrap();

        self.push_operand(Expression::from(member_ident));

        self.last_was_operand = true;

        Ok(())
    }

    fn parse_operator(&mut self) -> ParseResult<bool> {
        let operator_matcher = Matcher::any_operator_in_position(Position::Binary)
            .or(Matcher::any_operator_in_position(Position::Postfix));

        let match_after_operand = operator_matcher
            .or(DelimiterPair::SquareBracket) // array element access or explicit generic args
            .or(DelimiterPair::Bracket) // function call argument list
            .or(Operator::Member); // member access

        let mut look_ahead = self.tokens.look_ahead();

        match look_ahead.match_one(match_after_operand) {
            // a bracket group starting in operator position is always a call or a ctor call
            // e.g. x.y.z(a, b, c) or A(b: 2)
            Some(TokenTree::Delimited {
                     delim: DelimiterPair::Bracket,
                     ..
                 }) => {
                let ty_args = None;
                self.parse_invocation(ty_args)?;
            }

            // a square bracket group in operator position may be an indexer:
            // * `x := a[1];`
            // * `x := b.Y()[4123];`
            //
            // or a type param list:
            // * `x := A[Integer]();`
            // * `DoSomething[Byte](255);`
            Some(TokenTree::Delimited { delim: DelimiterPair::SquareBracket, .. }) => {
                // if the square bracket group is followed by a bracket group, it must be a generic
                // function invocation or ctor call
                if look_ahead.match_one(DelimiterPair::Bracket).is_some() {
                    self.parse_invocation_with_type_args()?;
                } else {
                    self.parse_indexer()?;
                }
            }

            Some(TokenTree::Operator { op: Operator::Member, .. }) => {
                self.parse_member_access()?;
            }

            Some(TokenTree::Operator { .. }) => {
                // expect another operand afterwards

                let op = self.tokens.match_one(Matcher::AnyOperator)?;

                // assumes there are no postfix operators that are also valid in infix position
                if op.as_operator().unwrap().is_valid_in_pos(Position::Postfix) {
                    // expect another operator
                    self.last_was_operand = true;
                    self.push_operator_token(op, Position::Postfix);
                } else {
                    self.last_was_operand = false;
                    self.push_operator_token(op, Position::Binary);
                }
            }

            Some(illegal) => panic!("pattern excludes anything else: got {}", illegal),

            // nothing following the last operand, which is fine, just end here
            None => return Ok(false),
        }

        Ok(true)
    }

    fn parse_invocation_with_type_args(&mut self) -> ParseResult<()> {
        let type_args = TypeList::parse_type_args(self.tokens)?;

        self.parse_invocation(Some(type_args))?;

        Ok(())
    }

    fn parse_indexer(&mut self) -> ParseResult<()> {
        let (inner, open, _close) = match self.tokens.match_one(DelimiterPair::SquareBracket)? {
            TokenTree::Delimited { inner, open, close, .. } => (inner, open, close),
            _ => unreachable!(),
        };

        let index = {
            let mut index_tokens = TokenStream::new(inner, open.clone());
            let index_expr = Expression::parse(&mut index_tokens)?;
            index_tokens.finish()?;

            index_expr
        };

        let indexer_part = OperatorPart::OperatorSymbol(SymbolOperator {
            op: Operator::Index,
            pos: Position::Binary,
            span: open,
        });
        self.parts.push(CompoundExpressionPart::Operator(indexer_part));

        self.push_operand(index);

        self.last_was_operand = true;

        Ok(())
    }

    fn push_operator_call(&mut self, args: ArgList<Span>, type_args: Option<TypeList<TypeName>>) {
        let op_call = OperatorPart::Call { args, type_args };
        self.parts.push(CompoundExpressionPart::Operator(op_call));
    }

    fn push_operator_token(&mut self, op_token: TokenTree, pos: Position) {
        let op_token = OperatorPart::OperatorSymbol(SymbolOperator {
            op: op_token.as_operator().unwrap(),
            pos,
            span: op_token.span().clone(),
        });

        let part = CompoundExpressionPart::Operator(op_token);
        self.parts.push(part);
    }
}