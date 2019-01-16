use {
    std::fmt,
    pas_common::{
        TracedError,
        span::*,
    },
    crate::{
        consts::*,
        token_tree::*,
        ident::*,
        operators::*,
        parse::*,
        ast::{
            Annotation,
            Call,
            ObjectCtor,
            ObjectCtorArgs,
        },
    },
};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct BinOp<A: Annotation> {
    pub lhs: ExpressionNode<A>,
    pub op: Operator,
    pub rhs: ExpressionNode<A>,
}

impl<A: Annotation> fmt::Display for BinOp<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.op, self.rhs)
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
    LiteralInt(IntConstant),
    LiteralString(String),
    Ident(Ident),
    Call(Call<A>),
    ObjectCtor(ObjectCtor<A>),
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
            Expression::LiteralInt(i) => write!(f, "{}", i),
            Expression::BinOp(op) => write!(f, "{}", op),
            Expression::LiteralString(s) => write!(f, "'{}'", s),
            Expression::Call(call) => write!(f, "{}", call),
            Expression::ObjectCtor(ctor) => write!(f, "{}", ctor),
        }
    }
}

impl<A: Annotation> fmt::Display for ExpressionNode<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}

/* anything which can appear at the start of an operand subexpr (not let bindings
or flow control statements) */
pub fn match_operand_start() -> Matcher {
    Matcher::AnyIdent
        // subexpr in brackets or object ctor
        .or(DelimiterPair::Bracket)
        // collection constructor
        .or(DelimiterPair::SquareBracket)
        // literals
        .or(Matcher::AnyLiteralBoolean)
        .or(Matcher::AnyLiteralInteger)
        .or(Matcher::AnyLiteralString)
        .or(Matcher::AnyLiteralReal)
//        .or(keywords::Nil)
        // prefix operator applying to next operand
        .or(Matcher::any_operator_in_position(Position::Prefix))
}

fn resolve_ops_by_precedence(parts: Vec<CompoundExpressionPart>) -> ParseResult<ExpressionNode<Span>> {
    assert!(!parts.is_empty(), "expression must not be empty");

    if parts.len() == 1 {
        return Ok(match parts.into_iter().next().unwrap() {
            CompoundExpressionPart::Operand(operand) => *operand.expr,
            CompoundExpressionPart::Operator(op_token) =>
                panic!("expression with one part must not be an operator (got: `{:?}`)", op_token),
        });
    }

    /* find the lowest-precedence operator in the expression, this becomes the
     outer expression */
    let (lo_op_index, lo_op) = parts.iter()
        .enumerate()
        .filter_map(|(i, part)| match part {
            CompoundExpressionPart::Operand { .. } => None,
            CompoundExpressionPart::Operator(op) => Some((i, op.clone())),
        })
        .max_by_key(|&(_, ref op)| op.op.precedence(op.pos))
        .unwrap();

    match lo_op.pos {
        /* merge prefix operator with the operand that follows it*/
        Position::Prefix => {
            assert!(parts.get(lo_op_index + 1).is_some(), "prefix operator must be followed by an operand");
            unimplemented!("unary op expr")

//            let (before_op, after_op) = parts.split_at(lo_op_index);
//
//            let parts_after_op = &after_op[1..];
//
//            let rhs = parts_after_op[0].clone().unwrap_operand();
//
//            let op_expr = {
//                ExpressionNode::new(Expression::UnaryOp(lo_op.op, *rhs.expr, lo_op.token));
//            };

//            let merged_parts: Vec<_> = before_op.iter()
//                .cloned()
//                .chain(vec![CompoundExpressionPart::Operand(CompoundOperand {
//                    expr: Box::new(op_expr),
//                    last_token: rhs.last_token,
//                })])
//                .chain(parts_after_op[1..].iter().cloned())
//                .collect();
//
//            assert!(!merged_parts.is_empty());
//            resolve_ops_by_precedence(merged_parts)
        }

        Position::Binary => {
            let (before_op, after_op) = parts.split_at(lo_op_index);

            if before_op.is_empty() {
                return Err(TracedError::trace(ParseError::EmptyOperand {
                    operator: lo_op.token,
                    before: true,
                }));
            }

            //1 because the op is included in this (?)
            if after_op.len() <= 1 {
                return Err(TracedError::trace(ParseError::EmptyOperand {
                    operator: lo_op.token,
                    before: false,
                }));
            }

            let lhs_operand = resolve_ops_by_precedence(Vec::from(before_op))?;
            let rhs_operand = resolve_ops_by_precedence(after_op.iter()
                .skip(1)
                .cloned()
                .collect())?;

            let span = lhs_operand.annotation.to(&rhs_operand.annotation);

            let bin_op = BinOp {
                lhs: lhs_operand,
                op: lo_op.op,
                rhs: rhs_operand,
            };

            Ok(ExpressionNode::new(Expression::BinOp(bin_op), span))
        }
    }
}

fn parse_identifier(tokens: &mut TokenStream) -> ParseResult<ExpressionNode<Span>> {
    /* the context of an identifier expression should be the first part of the
      identifier (if it has multiple parts) */
    match tokens.match_one(Matcher::AnyIdent)? {
        TokenTree::Ident(ident) => {
            let span = ident.span.clone();
            Ok(ExpressionNode::new(Expression::Ident(ident), span))
        }
        _ => unreachable!(),
    }
}

fn parse_literal_string(tokens: &mut TokenStream) -> ParseResult<ExpressionNode<Span>> {
    match tokens.match_one(Matcher::AnyLiteralString)? {
        TokenTree::String { value, span } => {
            Ok(ExpressionNode::new(Expression::LiteralString(value), span))
        }
        _ => unreachable!()
    }
}

fn parse_literal_integer(tokens: &mut TokenStream) -> ParseResult<ExpressionNode<Span>> {
    match tokens.match_one(Matcher::AnyLiteralInteger)? {
        TokenTree::IntNumber { value, span } => {
            Ok(ExpressionNode::new(Expression::LiteralInt(value), span))
        }
        _ => unreachable!(),
    }
}

//fn parse_literal_float(tokens: &mut TokenStream) -> ParseResult<ExpressionNode<Span> {
//    let float_token = tokens.match_one(Matcher::AnyLiteralFloat)?;
//
//    let f = float_token.unwrap_literal_float();
//    Ok(Expression::literal_float(f, float_token.clone()))
//}
//
//fn parse_literal_nil(tokens: &mut TokenStream) -> ParseResult<ExpressionNode<Span> {
//    tokens.match_one(keywords::Nil)?;
//    Ok(Expression::literal_nil(tokens.context().clone()))
//}
//
//fn parse_literal_bool(tokens: &mut TokenStream) -> ParseResult<ExpressionNode<Span> {
//    let kw = tokens.match_one(keywords::True.or(keywords::False))?;
//    let val = kw.is_keyword(keywords::True);
//    Ok(Expression::literal_bool(val, kw))
//}

#[derive(Debug, Clone)]
struct OperatorToken {
    op: Operator,
    pos: Position,
    token: TokenTree,
}

#[derive(Clone, Debug)]
struct CompoundOperand {
    expr: Box<ExpressionNode<Span>>,
}

#[derive(Debug, Clone)]
enum CompoundExpressionPart {
    Operand(CompoundOperand),
    Operator(OperatorToken),
}

//impl CompoundExpressionPart {
//    fn unwrap_operand(self) -> CompoundOperand {
//        match self {
//            CompoundExpressionPart::Operand(operand) => operand,
//            invalid => panic!("called unwrap_operand on {:?}", invalid)
//        }
//    }
//}

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
            /* an operand can't be followed by another operand, so each time we finish
            an operand, the next thing must be a member access, a list of arguments to
            the function the operand referred to, an array element access, or a binary
            operator connecting this operand to the next one */
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
                            Some(unexpected) =>
                                ParseError::UnexpectedToken(unexpected, Some(expected)),
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
            Some(TokenTree::Delimited { delim, inner, open, .. }) => {
                let mut group_tokens = TokenStream::new(inner.clone(), open);

                match delim {
                    DelimiterPair::Bracket => {
                        let subexpr = ExpressionNode::parse(&mut group_tokens)?;
                        self.add_operand(subexpr);
                    }

                    DelimiterPair::SquareBracket => {
                        let _ctor = unimplemented!("collection ctor");
//                        self.add_operand(ctor);
                    }

                    DelimiterPair::BeginEnd => unreachable!("not in matcher"),
                }
            }

            /* it's an operator, but thanks to the match we know this operator
            is valid in prefix position, so it's part of this expression */
            Some(ref t) if Matcher::AnyOperator.is_match(t) => {
                // we need to parse another operand after this!
                self.last_was_operand = false;
                let op = self.tokens.match_one(Matcher::AnyOperator)?;
                self.add_operator(op, Position::Prefix);
            }

            /* the simple values */

            Some(ref ident) if ident.as_ident().is_some() => {
                let expr = parse_identifier(self.tokens)?;
                self.add_operand(expr);
            }

            Some(ref s) if s.as_literal_string().is_some() => {
                let expr = parse_literal_string(self.tokens)?;
                self.add_operand(expr);
            }

            Some(ref i) if i.as_literal_int().is_some() => {
                let expr = parse_literal_integer(self.tokens)?;
                self.add_operand(expr);
            }

//            Some(ref f) if f.is_any_literal_real() => {
//                let expr = parse_literal_float(self.tokens)?;
//                self.add_operand(expr);
//            }

//            Some(ref nil) if nil.is_literal_nil() => {
//                let expr = parse_literal_nil(self.tokens)?;
//                self.add_operand(expr);
//            }

//            Some(ref bool_kw) if Matcher::AnyLiteralBoolean.is_match(bool_kw) => {
//                let expr = parse_literal_bool(self.tokens)?;
//                self.add_operand(expr);
//            }

            Some(_) => unreachable!("pattern excludes anything else"),

            /* next token is not valid as part of an operand, so this expression
            must end here */
            None => return Ok(false),
        }

        Ok(true)
    }

    fn add_operand(&mut self, expr: ExpressionNode<Span>) {
        let part = CompoundExpressionPart::Operand(CompoundOperand {
            expr: Box::new(expr),
        });

        self.parts.push(part);
    }

    fn pop_operand(&mut self) -> ExpressionNode<Span> {
        match self.parts.pop() {
            Some(CompoundExpressionPart::Operand(operand)) => {
                *operand.expr
            }
            _ => unreachable!("last should always exist and be an operand here"),
        }
    }

    fn parse_operator(&mut self) -> ParseResult<bool> {
        let any_binary_op = Matcher::any_operator_in_position(Position::Binary);
        let match_after_operand = any_binary_op
            .or(DelimiterPair::SquareBracket) // array element access
            .or(DelimiterPair::Bracket) // function call argument list
            .or(Operator::Member); // member access

        match self.tokens.look_ahead().match_one(match_after_operand) {
            Some(TokenTree::Delimited { delim: DelimiterPair::Bracket, open, close, inner, .. }) => {
                /* replace the last operand with a function call targetting that expr */
                let target = self.pop_operand();

                /* if the next two tokens are in the form `a:` AND the last operand is
                an ident then it has to be an object constructor list instead of a call */
                let mut inner_tokens = TokenStream::new(inner, open);
                let ctor_matcher = Matcher::AnyIdent.and_then(Separator::Colon);
                let args_matches_ctor = inner_tokens.look_ahead()
                    .match_sequence(ctor_matcher)
                    .is_some();
                let target_is_ident = target.expr.as_ident()
                    .is_some();

                let (operand, span) = if args_matches_ctor && target_is_ident {
                    let args = ObjectCtorArgs::parse(&mut self.tokens)?;
                    let ident = (*target.expr).into_ident().unwrap();
                    let span = ident.span().to(&args.close);
                    let ctor = ObjectCtor {
                        ident,
                        args,
                        annotation: span.clone(),
                    };

                    (Expression::ObjectCtor(ctor), span)
                } else {
                    let args = Call::parse_arg_list(&mut self.tokens)?;
                    let span = target.annotation.to(&close);
                    let call = Call {
                        target,
                        args,
                        annotation: span.clone(),
                    };

                    (Expression::Call(call), span)
                };

                self.add_operand(ExpressionNode::new(operand, span))
            }

            Some(TokenTree::Delimited { delim: DelimiterPair::SquareBracket, .. }) => {
                /* replace the last operand with an array element access of that expr */
                let _last_operand = self.pop_operand();
//                let element_expr = parse_array_element_after(last_operand, self.tokens)?;
                let _element_expr = unimplemented!("array element access expr");
//                self.add_operand(element_expr);
            }

            Some(TokenTree::Operator { .. }) => {
                // expect another operand afterwards
                self.last_was_operand = false;
                let op = self.tokens.match_one(Matcher::AnyOperator)?;
                self.add_operator(op, Position::Binary);
            }

            Some(_) => unreachable!("pattern excludes anything else"),

            /* nothing following the last operand, which is fine, just end here */
            None => return Ok(false),
        }

        Ok(true)
    }

    fn add_operator(&mut self, op_token: TokenTree, pos: Position) {
        let part = CompoundExpressionPart::Operator(OperatorToken {
            op: op_token.as_operator().unwrap(),
            pos,
            token: op_token,
        });

        self.parts.push(part);
    }
}