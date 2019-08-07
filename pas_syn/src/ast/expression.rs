#[cfg(test)]
pub(crate) mod test;

use crate::{
    ast::{
        Annotation,
        BinOp,
        Block,
        Call,
        CollectionCtor,
        FunctionCall,
        IfCond,
        Indexer,
        ObjectCtor,
        ObjectCtorArgs,
        UnaryOp,
    },
    consts::*,
    ident::*,
    operators::*,
    parse::*,
    token_tree::*,
    Keyword,
};
use pas_common::{
    span::*,
    TracedError,
};
use std::{
    cmp::Ordering,
    fmt,
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Literal {
    Nil,
    Integer(IntConstant),
    Real(RealConstant),
    String(String),
    Boolean(bool),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Nil => write!(f, "nil"),
            Literal::Integer(x) => write!(f, "{}", x),
            Literal::Real(x) => write!(f, "{}", x),
            Literal::String(s) => write!(f, "'{}'", s),
            Literal::Boolean(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression<A: Annotation> {
    BinOp(Box<BinOp<A>>),
    UnaryOp(Box<UnaryOp<A>>),
    Literal(Literal, A),
    Ident(Ident, A),
    Call(Box<Call<A>>),
    ObjectCtor(Box<ObjectCtor<A>>),
    CollectionCtor(Box<CollectionCtor<A>>),
    IfCond(Box<IfCond<A>>),
    Block(Box<Block<A>>),
    Indexer(Box<Indexer<A>>),
}

impl<A: Annotation> From<BinOp<A>> for Expression<A> {
    fn from(bin_op: BinOp<A>) -> Self {
        Expression::BinOp(Box::new(bin_op))
    }
}

impl<A: Annotation> From<UnaryOp<A>> for Expression<A> {
    fn from(unary_op: UnaryOp<A>) -> Self {
        Expression::UnaryOp(Box::new(unary_op))
    }
}

impl<A: Annotation> From<Call<A>> for Expression<A> {
    fn from(call: Call<A>) -> Self {
        Expression::Call(Box::new(call))
    }
}

impl<A: Annotation> From<ObjectCtor<A>> for Expression<A> {
    fn from(ctor: ObjectCtor<A>) -> Self {
        Expression::ObjectCtor(Box::new(ctor))
    }
}

impl<A: Annotation> From<CollectionCtor<A>> for Expression<A> {
    fn from(ctor: CollectionCtor<A>) -> Self {
        Expression::CollectionCtor(Box::new(ctor))
    }
}

impl<A: Annotation> From<IfCond<A>> for Expression<A> {
    fn from(cond: IfCond<A>) -> Self {
        Expression::IfCond(Box::new(cond))
    }
}

impl<A: Annotation> From<Block<A>> for Expression<A> {
    fn from(block: Block<A>) -> Self {
        Expression::Block(Box::new(block))
    }
}

impl<A: Annotation> From<Indexer<A>> for Expression<A> {
    fn from(indexer: Indexer<A>) -> Self {
        Expression::Indexer(Box::new(indexer))
    }
}

impl<A: Annotation> Expression<A> {
    pub fn annotation(&self) -> &A {
        match self {
            Expression::BinOp(bin_op) => &bin_op.annotation,
            Expression::UnaryOp(unary_op) => &unary_op.annotation,
            Expression::Literal(_, annotation) => annotation,
            Expression::Ident(_, annotation) => annotation,
            Expression::Call(call) => &call.annotation(),
            Expression::IfCond(cond) => &cond.annotation,
            Expression::Block(block) => &block.annotation,
            Expression::CollectionCtor(ctor) => &ctor.annotation,
            Expression::ObjectCtor(ctor) => &ctor.annotation,
            Expression::Indexer(indexer) => &indexer.annotation,
        }
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Expression::Ident(ident, _) => Some(ident),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<Ident> {
        match self {
            Expression::Ident(ident, _) => Some(ident),
            _ => None,
        }
    }

    pub fn as_object_ctor(&self) -> Option<&ObjectCtor<A>> {
        match self {
            Expression::ObjectCtor(ctor) => Some(ctor),
            _ => None,
        }
    }

    pub fn as_call(&self) -> Option<&Call<A>> {
        match self {
            Expression::Call(call) => Some(call),
            _ => None,
        }
    }

    pub fn as_literal(&self) -> Option<&Literal> {
        match self {
            Expression::Literal(lit, _) => Some(lit),
            _ => None,
        }
    }
}

impl Expression<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let parser = CompoundExpressionParser::new(tokens);
        parser.parse()
    }
}

impl<A: Annotation> fmt::Display for Expression<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(ident, _) => write!(f, "{}", ident),
            Expression::Literal(lit, _) => write!(f, "{}", lit),
            Expression::BinOp(op) => write!(f, "{}", op),
            Expression::Call(call) => write!(f, "{}", call),
            Expression::ObjectCtor(ctor) => write!(f, "{}", ctor),
            Expression::CollectionCtor(ctor) => write!(f, "{}", ctor),
            Expression::IfCond(if_cond) => write!(f, "{}", if_cond),
            Expression::Block(block) => write!(f, "{}", block),
            Expression::UnaryOp(op) => write!(f, "{}", op),
            Expression::Indexer(indexer) => write!(f, "{}", indexer),
        }
    }
}

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
        .or(Keyword::If)
        // literals
        .or(Keyword::Nil)
        .or(Matcher::AnyLiteralBoolean)
        .or(Matcher::AnyLiteralInteger)
        .or(Matcher::AnyLiteralString)
        .or(Matcher::AnyLiteralReal)
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
                        OperatorPart::Call { args_open, .. } => args_open,
                        OperatorPart::Symbol(op_symbol) => op_symbol.token.span().clone(),
                    },
                };
                return Err(TracedError::trace(err));
            },
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
        OperatorPart::Call {
            args,
            args_open,
            args_close,
        } => {
            let (before_op, after_op) = parts.split_at(lo_op_index);

            if before_op.is_empty() {
                return Err(TracedError::trace(ParseError::EmptyOperand {
                    operator: args_open.to(&args_close),
                    before: true,
                }));
            }

            // everything on the left becomes the target
            let target = resolve_ops_by_precedence(before_op.to_vec())?;
            let span = target.annotation().span().to(&args_close);

            let op_expr = Expression::from(Call::Function(FunctionCall {
                target,
                annotation: span.clone(),
                args,
                args_brackets: (args_open, args_close),
            }));

            let merged_parts: Vec<_> = vec![CompoundExpressionPart::Operand(op_expr)]
                .into_iter()
                .chain(after_op[1..].iter().cloned())
                .collect();

            assert!(!merged_parts.is_empty());
            resolve_ops_by_precedence(merged_parts)
        },

        OperatorPart::Symbol(op_token) => match op_token.pos {
            // merge prefix operator with the operand that follows it
            Position::Binary => {
                let (before_op, after_op) = parts.split_at(lo_op_index);

                if before_op.is_empty() {
                    return Err(TracedError::trace(ParseError::EmptyOperand {
                        operator: op_token.token.span().clone(),
                        before: true,
                    }));
                }

                // 1 because the op is included in this (?)
                if after_op.len() <= 1 {
                    return Err(TracedError::trace(ParseError::EmptyOperand {
                        operator: op_token.token.span().clone(),
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
            },

            Position::Prefix => {
                let (before_op, after_op) = parts.split_at(lo_op_index);

                if after_op.is_empty() {
                    return Err(TracedError::trace(ParseError::EmptyOperand {
                        operator: op_token.token.span().clone(),
                        before: false,
                    }));
                }

                let parts_after_op = &after_op[1..];

                let rhs = parts_after_op[0].clone().unwrap_operand();

                let op_expr = {
                    let span = op_token.token.span().to(rhs.annotation().span());
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
            },

            Position::Postfix => {
                let (before_op, after_op) = parts.split_at(lo_op_index);

                if before_op.is_empty() {
                    return Err(TracedError::trace(ParseError::EmptyOperand {
                        operator: op_token.token.span().clone(),
                        before: true,
                    }));
                }

                // everything on the left becomes the operand
                let operand = resolve_ops_by_precedence(before_op.to_vec())?;
                let span = op_token.token.span().to(operand.annotation().span());

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
            },
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
        },
        _ => unreachable!(),
    }
}

fn parse_literal_string(tokens: &mut TokenStream) -> ParseResult<Expression<Span>> {
    match tokens.match_one(Matcher::AnyLiteralString)? {
        TokenTree::String { value, span } => {
            let str_lit = Literal::String(value);
            Ok(Expression::Literal(str_lit, span))
        },
        _ => unreachable!(),
    }
}

fn parse_literal_integer(tokens: &mut TokenStream) -> ParseResult<Expression<Span>> {
    match tokens.match_one(Matcher::AnyLiteralInteger)? {
        TokenTree::IntNumber { value, span } => {
            let int_lit = Literal::Integer(value);
            Ok(Expression::Literal(int_lit, span))
        },
        _ => unreachable!(),
    }
}

fn parse_literal_real(tokens: &mut TokenStream) -> ParseResult<Expression<Span>> {
    match tokens.match_one(Matcher::AnyLiteralReal)? {
        TokenTree::RealNumber { value, span } => {
            let real_lit = Literal::Real(value);
            Ok(Expression::Literal(real_lit, span))
        },

        _ => unreachable!(),
    }
}

fn parse_literal_bool(tokens: &mut TokenStream) -> ParseResult<Expression<Span>> {
    let kw = tokens.match_one(Keyword::True.or(Keyword::False))?;
    let val = kw.is_keyword(Keyword::True);
    let expr = Expression::Literal(Literal::Boolean(val), kw.span().clone());
    Ok(expr)
}

#[derive(Debug, Clone)]
struct SymbolOperator {
    op: Operator,
    pos: Position,
    token: TokenTree,
}

#[derive(Debug, Clone)]
enum OperatorPart {
    // symbol operator e.g. +, *
    Symbol(SymbolOperator),

    // () operator with inner argument list
    Call {
        args: Vec<Expression<Span>>,
        args_open: Span,
        args_close: Span,
    },
}

impl OperatorPart {
    fn cmp_precedence(&self, b: &Self) -> Ordering {
        let (op_a, pos_a, op_b, pos_b) = match (self, b) {
            (OperatorPart::Call { .. }, OperatorPart::Call { .. }) => (
                Operator::Call,
                Position::Postfix,
                Operator::Call,
                Position::Postfix,
            ),
            (OperatorPart::Call { .. }, OperatorPart::Symbol(sym_op)) => {
                (Operator::Call, Position::Postfix, sym_op.op, sym_op.pos)
            },
            (OperatorPart::Symbol(sym_op), OperatorPart::Call { .. }) => {
                (sym_op.op, sym_op.pos, Operator::Call, Position::Postfix)
            },
            (OperatorPart::Symbol(sym_op_a), OperatorPart::Symbol(sym_op_b)) => {
                (sym_op_a.op, sym_op_a.pos, sym_op_b.op, sym_op_b.pos)
            },
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

    fn parse(mut self) -> ParseResult<Expression<Span>> {
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
                            },
                            None => {
                                let after = self.tokens.context().clone();
                                ParseError::UnexpectedEOF(expected, after)
                            },
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
                    DelimiterPair::Bracket => {
                        let subexpr = Expression::parse(&mut group_tokens)?;
                        group_tokens.finish()?;
                        self.tokens.advance(1);
                        self.add_operand(subexpr);
                    },

                    DelimiterPair::SquareBracket => {
                        let ctor = CollectionCtor::parse(self.tokens)?;
                        let expr = Expression::from(ctor);
                        self.add_operand(expr);
                    },

                    DelimiterPair::BeginEnd => {
                        let block = Block::parse(self.tokens)?;
                        let expr = Expression::from(block);
                        self.add_operand(expr);
                    },
                }
            },

            // it's an operator, but thanks to the match we know this operator
            // is valid in prefix position, so it's part of this expression
            Some(TokenTree::Operator { .. }) => {
                // we need to parse another operand after this!
                self.last_was_operand = false;
                let op = self.tokens.match_one(Matcher::AnyOperator)?;
                self.add_operator_token(op, Position::Prefix);
            },

            // the simple values
            Some(TokenTree::Ident(_)) => {
                let expr = parse_identifier(self.tokens)?;
                self.add_operand(expr);
            },

            Some(TokenTree::String { .. }) => {
                let expr = parse_literal_string(self.tokens)?;
                self.add_operand(expr);
            },

            Some(TokenTree::IntNumber { .. }) => {
                let expr = parse_literal_integer(self.tokens)?;
                self.add_operand(expr);
            },

            Some(TokenTree::Keyword {
                kw: Keyword::Nil, ..
            }) => {
                let nil_token = self.tokens.next().unwrap();
                self.add_operand(Expression::Literal(Literal::Nil, nil_token.span().clone()));
            },

            Some(TokenTree::Keyword {
                kw: Keyword::If, ..
            }) => {
                let cond = IfCond::parse(self.tokens)?;
                let expr = Expression::from(cond);
                self.add_operand(expr);
            },

            Some(TokenTree::Keyword {
                kw: Keyword::False, ..
            })
            | Some(TokenTree::Keyword {
                kw: Keyword::True, ..
            }) => {
                let expr = parse_literal_bool(self.tokens)?;
                self.add_operand(expr);
            },

            Some(TokenTree::RealNumber { .. }) => {
                let expr = parse_literal_real(self.tokens)?;
                self.add_operand(expr);
            },

            Some(x) => unreachable!("got {:?} which is excluded by pattern", x),

            // next token is not valid as part of an operand, so this expression
            // must end here
            None => return Ok(false),
        }

        Ok(true)
    }

    fn add_operand(&mut self, expr: Expression<Span>) {
        let part = CompoundExpressionPart::Operand(expr);
        self.parts.push(part);
    }

    fn pop_operand(&mut self) -> Expression<Span> {
        match self.parts.pop() {
            Some(CompoundExpressionPart::Operand(operand)) => operand,
            _ => unreachable!("last should always exist and be an operand here"),
        }
    }

    fn parse_operator(&mut self) -> ParseResult<bool> {
        let operator_matcher = Matcher::any_operator_in_position(Position::Binary)
            .or(Matcher::any_operator_in_position(Position::Postfix));

        let match_after_operand = operator_matcher
            .or(DelimiterPair::SquareBracket) // array element access
            .or(DelimiterPair::Bracket) // function call argument list
            .or(Operator::Member); // member access

        match self.tokens.look_ahead().match_one(match_after_operand) {
            Some(TokenTree::Delimited {
                delim: DelimiterPair::Bracket,
                open,
                close,
                inner,
                ..
            }) => {
                // if the next two tokens are in the form `a:` AND the last operand is
                // an ident then it has to be an object constructor list instead of a call
                let mut inner_tokens = TokenStream::new(inner, open.clone());
                let ctor_matcher = Matcher::AnyIdent.and_then(Separator::Colon);
                let args_matches_ctor = inner_tokens
                    .look_ahead()
                    .match_sequence(ctor_matcher)
                    .is_some();

                // replace the last operand with a function call targetting that expr
                let last_was_ident = match self.parts.last() {
                    Some(CompoundExpressionPart::Operand(expr)) => expr.as_ident().is_some(),
                    _ => false,
                };

                if args_matches_ctor && last_was_ident {
                    let args = ObjectCtorArgs::parse(&mut self.tokens)?;
                    let ident = self.pop_operand().into_ident().unwrap();

                    let span = ident.span().to(&args.close);
                    let ctor = ObjectCtor {
                        ident: ident.into(),
                        args,
                        annotation: span.clone(),
                    };

                    self.add_operand(Expression::from(ctor));
                } else {
                    let args = Call::parse_arg_list(&mut self.tokens)?;
                    self.add_operator_call(args, open, close);
                }
            },

            Some(TokenTree::Delimited {
                delim: DelimiterPair::SquareBracket,
                open,
                inner,
                close,
                ..
            }) => {
                let base = self.pop_operand();

                let mut index_tokens = TokenStream::new(inner, open);
                let index_expr = Expression::parse(&mut index_tokens)?;
                index_tokens.finish()?;

                self.tokens.advance(1);
                self.last_was_operand = true;

                let span = base.annotation().span().to(close.span());

                let indexer = Indexer {
                    base,
                    annotation: span.clone(),
                    index: index_expr,
                };

                self.add_operand(Expression::from(indexer));
            },

            Some(TokenTree::Operator { .. }) => {
                // expect another operand afterwards

                let op = self.tokens.match_one(Matcher::AnyOperator)?;

                // assumes there are no postfix operators that are also valid in infix position
                if op.as_operator().unwrap().is_valid_in_pos(Position::Postfix) {
                    // expect another operator
                    self.last_was_operand = true;
                    self.add_operator_token(op, Position::Postfix);
                } else {
                    self.last_was_operand = false;
                    self.add_operator_token(op, Position::Binary);
                }
            },

            Some(_) => unreachable!("pattern excludes anything else"),

            // nothing following the last operand, which is fine, just end here
            None => return Ok(false),
        }

        Ok(true)
    }

    fn add_operator_call(
        &mut self,
        args: Vec<Expression<Span>>,
        args_open: Span,
        args_close: Span,
    ) {
        let op_call = OperatorPart::Call {
            args,
            args_open,
            args_close,
        };
        self.parts.push(CompoundExpressionPart::Operator(op_call));
    }

    fn add_operator_token(&mut self, op_token: TokenTree, pos: Position) {
        let op_token = OperatorPart::Symbol(SymbolOperator {
            op: op_token.as_operator().unwrap(),
            pos,
            token: op_token,
        });

        let part = CompoundExpressionPart::Operator(op_token);
        self.parts.push(part);
    }
}
