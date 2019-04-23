use crate::{
    ast::{
        Annotation,
        BinOp,
        Block,
        Call,
        FunctionCall,
        IfCond,
        ObjectCtor,
        CollectionCtor,
        ObjectCtorArgs,
        UnaryOp,
        Indexer,
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
pub struct ExpressionNode<A: Annotation> {
    pub expr: Box<Expression<A>>,
    pub annotation: A,
}

impl<A: Annotation> ExpressionNode<A> {
    pub fn new(expr: impl Into<Expression<A>>, annotation: impl Into<A>) -> Self {
        Self {
            expr: Box::new(expr.into()),
            annotation: annotation.into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression<A: Annotation> {
    BinOp(BinOp<A>),
    UnaryOp(UnaryOp<A>),
    Literal(Literal),
    Ident(Ident),
    Call(Call<A>),
    ObjectCtor(ObjectCtor<A>),
    CollectionCtor(CollectionCtor<A>),
    IfCond(IfCond<A>),
    Block(Block<A>),
    Indexer(Indexer<A>),
}

impl<A: Annotation> From<BinOp<A>> for Expression<A> {
    fn from(bin_op: BinOp<A>) -> Self {
        Expression::BinOp(bin_op)
    }
}

impl<A: Annotation> From<UnaryOp<A>> for Expression<A> {
    fn from(unary_op: UnaryOp<A>) -> Self {
        Expression::UnaryOp(unary_op)
    }
}

impl<A: Annotation> From<Literal> for Expression<A> {
    fn from(lit: Literal) -> Self {
        Expression::Literal(lit)
    }
}

impl<A: Annotation> From<Ident> for Expression<A> {
    fn from(ident: Ident) -> Self {
        Expression::Ident(ident)
    }
}

impl<A: Annotation> From<Call<A>> for Expression<A> {
    fn from(call: Call<A>) -> Self {
        Expression::Call(call)
    }
}

impl<A: Annotation> From<ObjectCtor<A>> for Expression<A> {
    fn from(ctor: ObjectCtor<A>) -> Self {
        Expression::ObjectCtor(ctor)
    }
}

impl<A: Annotation> From<CollectionCtor<A>> for Expression<A> {
    fn from(ctor: CollectionCtor<A>) -> Self {
        Expression::CollectionCtor(ctor)
    }
}

impl<A: Annotation> From<IfCond<A>> for Expression<A> {
    fn from(cond: IfCond<A>) -> Self {
        Expression::IfCond(cond)
    }
}

impl<A: Annotation> From<Block<A>> for Expression<A> {
    fn from(block: Block<A>) -> Self {
        Expression::Block(block)
    }
}

impl<A: Annotation> From<Indexer<A>> for Expression<A> {
    fn from(indexer: Indexer<A>) -> Self {
        Expression::Indexer(indexer)
    }
}

impl<A: Annotation> Expression<A> {
    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            Expression::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<Ident> {
        match self {
            Expression::Ident(ident) => Some(ident),
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
            Expression::Literal(lit) => Some(lit),
            _ => None,
        }
    }
}

impl ExpressionNode<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let parser = CompoundExpressionParser::new(tokens);
        parser.parse()
    }
}

impl<A: Annotation> fmt::Display for Expression<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(ident) => write!(f, "{}", ident),
            Expression::Literal(lit) => write!(f, "{}", lit),
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

impl<A: Annotation> fmt::Display for ExpressionNode<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expr)
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

fn resolve_ops_by_precedence(
    parts: Vec<CompoundExpressionPart>,
) -> ParseResult<ExpressionNode<Span>> {
    assert!(!parts.is_empty(), "expression must not be empty");

    if parts.len() == 1 {
        return Ok(match parts.into_iter().next().unwrap() {
            CompoundExpressionPart::Operand(operand) => operand,
            CompoundExpressionPart::Operator(op_token) => panic!(
                "expression with one part must not be an operator (got: `{:?}`)",
                op_token
            ),
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
        OperatorPart::Call(args_span, args) => {
            let (before_op, after_op) = parts.split_at(lo_op_index);

            if before_op.is_empty() {
                return Err(TracedError::trace(ParseError::EmptyOperand {
                    operator: args_span.clone(),
                    before: true,
                }));
            }

            // everything on the left becomes the target
            let target = resolve_ops_by_precedence(before_op.to_vec())?;
            let span = args_span.to(target.annotation.span());

            let op_expr = ExpressionNode::new(
                Call::Function(FunctionCall {
                    target,
                    annotation: span.clone(),
                    args,
                }),
                span,
            );

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

                let span = lhs_operand.annotation.to(&rhs_operand.annotation);
                let bin_op = BinOp {
                    lhs: lhs_operand,
                    op: op_token.op,
                    rhs: rhs_operand,
                    annotation: span.clone(),
                };

                Ok(ExpressionNode::new(bin_op, span))
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
                    let span = op_token.token.span().to(rhs.annotation.span());
                    let unary_op = UnaryOp {
                        op: op_token.op,
                        operand: rhs,
                        annotation: span.clone(),
                    };
                    ExpressionNode::new(unary_op, span)
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
                let span = op_token.token.span().to(operand.annotation.span());

                let op_expr = ExpressionNode::new(
                    UnaryOp {
                        op: op_token.op,
                        annotation: span.clone(),
                        operand,
                    },
                    span,
                );

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

fn parse_identifier(tokens: &mut TokenStream) -> ParseResult<ExpressionNode<Span>> {
    // the context of an identifier expression should be the first part of the
    // identifier (if it has multiple parts)
    match tokens.match_one(Matcher::AnyIdent)? {
        TokenTree::Ident(ident) => {
            let span = ident.span.clone();
            Ok(ExpressionNode::new(ident, span))
        },
        _ => unreachable!(),
    }
}

fn parse_literal_string(tokens: &mut TokenStream) -> ParseResult<ExpressionNode<Span>> {
    match tokens.match_one(Matcher::AnyLiteralString)? {
        TokenTree::String { value, span } => {
            let str_lit = Literal::String(value);
            Ok(ExpressionNode::new(str_lit, span))
        },
        _ => unreachable!(),
    }
}

fn parse_literal_integer(tokens: &mut TokenStream) -> ParseResult<ExpressionNode<Span>> {
    match tokens.match_one(Matcher::AnyLiteralInteger)? {
        TokenTree::IntNumber { value, span } => {
            let int_lit = Literal::Integer(value);
            Ok(ExpressionNode::new(int_lit, span))
        },
        _ => unreachable!(),
    }
}

fn parse_literal_real(tokens: &mut TokenStream) -> ParseResult<ExpressionNode<Span>> {
    match tokens.match_one(Matcher::AnyLiteralReal)? {
        TokenTree::RealNumber { value, span } => {
            let real_lit = Literal::Real(value);
            Ok(ExpressionNode::new(real_lit, span))
        },

        _ => unreachable!(),
    }
}

fn parse_literal_bool(tokens: &mut TokenStream) -> ParseResult<ExpressionNode<Span>> {
    let kw = tokens.match_one(Keyword::True.or(Keyword::False))?;
    let val = kw.is_keyword(Keyword::True);
    let expr = Expression::Literal(Literal::Boolean(val));
    Ok(ExpressionNode::new(expr, kw.span().clone()))
}

#[derive(Debug, Clone)]
struct OperatorToken {
    op: Operator,
    pos: Position,
    token: TokenTree,
}

#[derive(Debug, Clone)]
enum OperatorPart {
    // symbol operator e.g. +, *
    Symbol(OperatorToken),

    // () operator with inner argument list
    Call(Span, Vec<ExpressionNode<Span>>),
}

impl OperatorPart {
    fn cmp_precedence(&self, b: &Self) -> Ordering {
        match (self, b) {
            (OperatorPart::Call(_, _), OperatorPart::Call(_, _)) => Ordering::Equal,
            (OperatorPart::Call(_, _), OperatorPart::Symbol(_)) => Ordering::Greater,
            (OperatorPart::Symbol(_), OperatorPart::Call(_, _)) => Ordering::Less,
            (OperatorPart::Symbol(op_a), OperatorPart::Symbol(op_b)) => {
                let prec_a = op_a.op.precedence(op_a.pos);
                let prec_b = op_b.op.precedence(op_b.pos);
                prec_a.cmp(&prec_b)
            },
        }
    }
}

#[derive(Debug, Clone)]
enum CompoundExpressionPart {
    Operand(ExpressionNode<Span>),
    Operator(OperatorPart),
}

impl CompoundExpressionPart {
    fn unwrap_operand(self) -> ExpressionNode<Span> {
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

    fn parse(mut self) -> ParseResult<ExpressionNode<Span>> {
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
                                ParseError::UnexpectedToken(unexpected, Some(expected))
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
                        let subexpr = ExpressionNode::parse(&mut group_tokens)?;
                        group_tokens.finish()?;
                        self.tokens.advance(1);
                        self.add_operand(subexpr);
                    },

                    DelimiterPair::SquareBracket => {
                        let ctor = CollectionCtor::parse(self.tokens)?;
                        let span = ctor.annotation.clone();
                        let expr = ExpressionNode::new(ctor, span);
                        self.add_operand(expr);
                    },

                    DelimiterPair::BeginEnd => {
                        let block = Block::parse(self.tokens)?;
                        let span = block.annotation.clone();
                        let expr = ExpressionNode::new(block, span);
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
                self.add_operand(ExpressionNode::new(Literal::Nil, nil_token.span().clone()));
            },

            Some(TokenTree::Keyword {
                kw: Keyword::If, ..
            }) => {
                let cond = IfCond::parse(self.tokens)?;
                let span = cond.span().clone();
                let expr = ExpressionNode::new(cond, span);
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

    fn add_operand(&mut self, expr: ExpressionNode<Span>) {
        let part = CompoundExpressionPart::Operand(expr);
        self.parts.push(part);
    }

    fn pop_operand(&mut self) -> ExpressionNode<Span> {
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
                    Some(CompoundExpressionPart::Operand(expr)) => expr.expr.as_ident().is_some(),
                    _ => false,
                };

                if args_matches_ctor && last_was_ident {
                    let args = ObjectCtorArgs::parse(&mut self.tokens)?;
                    let ident = self.pop_operand().expr.into_ident().unwrap();

                    let span = ident.span().to(&args.close);
                    let ctor = ObjectCtor {
                        ident: ident.into(),
                        args,
                        annotation: span.clone(),
                    };

                    self.add_operand(ExpressionNode::new(ctor, span));
                } else {
                    let args = Call::parse_arg_list(&mut self.tokens)?;
                    let span = open.to(&close);
                    self.add_operator_call(span, args);
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
                let index_expr = ExpressionNode::parse(&mut index_tokens)?;
                index_tokens.finish()?;

                self.tokens.advance(1);
                self.last_was_operand = true;

                let span = base.annotation.span().to(close.span());

                let indexer = Indexer {
                    base,
                    annotation: span.clone(),
                    index: index_expr,
                };

                self.add_operand(ExpressionNode::new(indexer, span));
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

    fn add_operator_call(&mut self, span: Span, args: Vec<ExpressionNode<Span>>) {
        let op_call = OperatorPart::Call(span, args);
        self.parts.push(CompoundExpressionPart::Operator(op_call));
    }

    fn add_operator_token(&mut self, op_token: TokenTree, pos: Position) {
        let op_token = OperatorPart::Symbol(OperatorToken {
            op: op_token.as_operator().unwrap(),
            pos,
            token: op_token,
        });

        let part = CompoundExpressionPart::Operator(op_token);
        self.parts.push(part);
    }
}
