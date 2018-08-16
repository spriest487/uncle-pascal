use std::fmt;

use operators;
use source;

pub trait ToSource {
    fn to_source(&self) -> String;
}

pub trait Symbol {
    type Type: Clone + fmt::Debug;
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct Identifier {
    pub namespace: Vec<String>,
    pub name: String,
}

impl Symbol for Identifier {
    type Type = Self;
}

impl<'a> From<&'a str> for Identifier {
    fn from(from: &'a str) -> Self {
        Identifier::parse(from)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.namespace.len() > 0 {
            write!(f, "{}.", self.namespace.join("."))?;
        }
        write!(f, "{}", self.name)
    }
}

impl Identifier {
    pub fn parse(source: &str) -> Self {
        let mut parts: Vec<String> = source.split('.')
            .map(|part: &str| part.to_owned())
            .collect();

        let name = parts.pop().unwrap_or(String::new());

        Identifier {
            namespace: parts,
            name,
        }
    }

    pub fn child(&self, child_name: &str) -> Identifier {
        let mut child_ns = self.namespace.clone();
        child_ns.push(self.name.clone());

        Identifier {
            name: child_name.to_owned(),
            namespace: child_ns,
        }
    }

    pub fn parent(&self) -> Option<Identifier> {
        if self.namespace.len() > 0 {
            let parent_namespace = self.namespace[0..self.namespace.len() - 1]
                .to_vec();

            let parent_name = self.namespace.last().unwrap().clone();

            Some(Identifier {
                namespace: parent_namespace,
                name: parent_name,
            })
        } else {
            None
        }
    }

    pub fn append(&self, other: &Identifier) -> Self {
        let mut result_ns = self.namespace.clone();
        result_ns.push(self.name.clone());
        result_ns.extend(other.namespace.clone());

        Identifier {
            name: other.name.clone(),
            namespace: result_ns,
        }
    }

    pub fn head(&self) -> String {
        self.namespace.first()
            .cloned()
            .unwrap_or_else(|| self.name.clone())
    }

    pub fn tail(&self) -> Option<Identifier> {
        if self.namespace.len() > 0 {
            Some(Identifier {
                name: self.name.clone(),
                namespace: self.namespace.iter().cloned().skip(1).collect(),
            })
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub struct UnitReference {
    pub name: Identifier,
    pub context: source::Token,
}

impl fmt::Display for UnitReference {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ({})", self.name, self.context)
    }
}

#[derive(Clone, Debug)]
pub enum UnitDeclaration<TSymbol>
    where TSymbol: Symbol
{
    Function(FunctionDecl<TSymbol>),
    Record(RecordDecl<TSymbol>),
    Vars(VarDecls<TSymbol>),
}

#[derive(Clone, Debug)]
pub struct Program<TSymbol>
    where TSymbol: Symbol
{
    pub name: String,

    pub uses: Vec<UnitReference>,
    pub decls: Vec<UnitDeclaration<TSymbol>>,

    pub program_block: Block<TSymbol>,
}

#[derive(Clone, Debug)]
pub enum ExpressionValue<TSymbol> {
    BinaryOperator {
        lhs: Box<Expression<TSymbol>>,
        op: operators::BinaryOperator,
        rhs: Box<Expression<TSymbol>>,
    },
    FunctionCall {
        target: TSymbol,
        args: Vec<Expression<TSymbol>>,
    },
    LiteralInteger(i64),
    LiteralString(String),
    Identifier(TSymbol),
    If {
        condition: Box<Expression<TSymbol>>,
        then_branch: Box<Expression<TSymbol>>,
        else_branch: Option<Box<Expression<TSymbol>>>,
    },
    Block(Block<TSymbol>),
    ForLoop {
        from: Box<Expression<TSymbol>>,
        to: Box<Expression<TSymbol>>,
        body: Box<Expression<TSymbol>>,
    },
}

#[derive(Clone, Debug)]
pub struct Expression<TSymbol> {
    pub value: ExpressionValue<TSymbol>,
    pub context: source::Token,
}

impl<TSymbol> fmt::Display for Expression<TSymbol>
    where TSymbol: fmt::Debug
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "expression: {:?} ({})", self.value, self.context) //TODO better display
    }
}

#[allow(dead_code)]
impl<TSymbol> Expression<TSymbol>
    where TSymbol: fmt::Debug
{
    pub fn binary_op(lhs: Self,
                     op: operators::BinaryOperator,
                     rhs: Self,
                     context: source::Token) -> Self {
        Expression {
            value: ExpressionValue::BinaryOperator {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
            context,
        }
    }

    pub fn function_call<TIter>(target: TSymbol,
                                args: TIter,
                                context: source::Token) -> Self
        where TIter: IntoIterator<Item=Self>
    {
        Expression {
            value: ExpressionValue::FunctionCall {
                target,
                args: args.into_iter().collect(),
            },
            context,
        }
    }

    pub fn literal_int(i: i64, context: source::Token) -> Self {
        Expression {
            value: ExpressionValue::LiteralInteger(i),
            context,
        }
    }

    pub fn literal_string(s: &str, context: source::Token) -> Self {
        Expression {
            value: ExpressionValue::LiteralString(s.to_owned()),
            context,
        }
    }

    pub fn identifier(id: TSymbol, context: source::Token) -> Self {
        Expression {
            value: ExpressionValue::Identifier(id),
            context,
        }
    }

    pub fn if_then_else(condition: Self,
                        then_branch: Self,
                        else_branch: Option<Self>,
                        context: source::Token) -> Self {
        Expression {
            value: ExpressionValue::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: else_branch.map(Box::new),
            },
            context,
        }
    }

    pub fn for_loop(from: Self, to: Self, body: Self, context: source::Token) -> Self {
        Expression {
            value: ExpressionValue::ForLoop {
                from: Box::new(from),
                to: Box::new(to),
                body: Box::new(body),
            },
            context,
        }
    }

    pub fn block(block: Block<TSymbol>) -> Self {
        Expression {
            context: block.context.clone(),
            value: ExpressionValue::Block(block),
        }
    }

    pub fn unwrap_function_call(self) -> (TSymbol, Vec<Self>) {
        match self.value {
            ExpressionValue::FunctionCall { target, args } => (target, args),
            _ => panic!("called unwrap_function_call on something other than a function call expr"),
        }
    }

    pub fn is_function_call(&self) -> bool {
        match &self.value {
            &ExpressionValue::FunctionCall { .. } => true,
            _ => false,
        }
    }

    pub fn unwrap_literal_string(self) -> String {
        match self.value {
            ExpressionValue::LiteralString(s) => s,
            _ => panic!("called unwrap_literal_string on something other than a string literal expr")
        }
    }

    pub fn is_any_literal_string(&self) -> bool {
        match &self.value {
            &ExpressionValue::LiteralString(_) => true,
            _ => false,
        }
    }

    pub fn is_literal_integer(&self, val: i64) -> bool {
        match &self.value {
            &ExpressionValue::LiteralInteger(i) => i == val,
            _ => false,
        }
    }

    pub fn is_any_literal_integer(&self) -> bool {
        match &self.value {
            &ExpressionValue::LiteralInteger(_) => true,
            _ => false
        }
    }

    pub fn is_operation(&self, op: &operators::BinaryOperator) -> bool {
        match &self.value {
            &ExpressionValue::BinaryOperator { op: ref expr_op, .. } => {
                expr_op == op
            }
            _ => false
        }
    }

    pub fn unwrap_binary_op(self) -> (Self, operators::BinaryOperator, Self) {
        match self.value {
            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                (*lhs, op, *rhs)
            }
            _ => panic!("called unwrap_binary_op on {}", self)
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDecl<TSymbol>
    where TSymbol: Symbol
{
    pub name: String,
    pub context: source::Token,

    pub return_type: Option<TSymbol::Type>,

    pub args: VarDecls<TSymbol>,
    pub local_vars: VarDecls<TSymbol>,

    pub body: Block<TSymbol>,
}

#[derive(Clone, Debug)]
pub struct Block<TSymbol> {
    pub context: source::Token,
    pub statements: Vec<Expression<TSymbol>>,
}

#[derive(Clone, Debug)]
pub struct RecordDecl<TSymbol>
    where TSymbol: Symbol
{
    pub name: String,
    pub context: source::Token,
    pub members: Vec<VarDecl<TSymbol>>,
}

#[derive(Clone, Debug)]
pub struct VarDecl<TSymbol>
    where TSymbol: Symbol
{
    pub name: String,
    pub context: source::Token,

    pub decl_type: TSymbol::Type,
}

#[derive(Clone, Debug)]
pub struct VarDecls<TSymbol>
    where TSymbol: Symbol
{
    pub decls: Vec<VarDecl<TSymbol>>,
}

impl<TSymbol> Default for VarDecls<TSymbol>
    where TSymbol: Symbol
{
    fn default() -> Self {
        Self { decls: Vec::new() }
    }
}