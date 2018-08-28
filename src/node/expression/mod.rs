pub mod constructor;
pub mod call;

use std::fmt;

use operators;
use node::*;
use tokens;
use consts::{
    IntConstant,
    FloatConstant,
    EnumConstant,
    SetConstant,
};
use types::{
    Type,
    ParameterizedName,
};

pub use self::{
    constructor::{
        ObjectConstructor,
        ObjectConstructorMember,
        CollectionConstructor,
        CollectionMember,
    },

    call::FunctionCall,
};

#[derive(Clone, Debug, PartialEq)]
pub enum ExpressionValue<TContext>
    where TContext: Context
{
    /**
        operator applied to a following operand
        `-1`
        `@dog`
    */
    PrefixOperator {
        op: operators::Operator,
        rhs: Box<Expression<TContext>>,
    },

    /**
        operator applied to two operands
        `1 + 2`
        `x..y`
        `true and false`
    */
    BinaryOperator {
        lhs: Box<Expression<TContext>>,
        op: operators::Operator,
        rhs: Box<Expression<TContext>>,
    },

    /**
        a call to a function with an argument list of 0 or more arguments.
        `x(y)`
        `x(y, z)`
        `x()`
        `x` where x is a function taking no arguments, or where all arguments have defaults
    */
    FunctionCall(FunctionCall<TContext>),

//    VirtualCall(VirtualCall<TContext>),

    /**
        conversion between castable types.
        `Int32($0)`
        `Byte(1)`
        we disambiguate this from a regular function call during typechecking.
    */
    TypeCast {
        target_type: TContext::Type,
        from_value: Box<Expression<TContext>>,
    },

    /**
        a constant value. expressions of this type can be used in array range
        declarations, constant bindings, and other context where the value must
        be known at compile time.

        operator expressions composed exclusively of constant expressions are
        also resolvable to constant values, depending on the operator and operand
        types.

        constant int, uint and char: `1`, `$1`, `#1`
        constant float: `1.0`, `1E100`, `1.0E100`
        constant string: `'hello world'`
        constant nil: `nil`
        constant boolean: `true`, `false`
        constant enum: `Apple`
        constant set: `[Apple, Banana]`. `[Apple..Pear, Cherry]`, `[]`
    */
    Constant(ConstExpression),

    /**
        a local or fully-qualified name
        `a`
        `System.a.b`

        note that before typechecking, we don't know if a name is just a name
        or if it's a function call (when there's no args list) or a record
        member (when the member expression is a valid identifier on its own).
    */
    Identifier(Identifier),

    /**
        binds a value to a new name into the current scope.
        immutable binding: `let x = foo()`
        mutable binding: `let var x := foo()`
        a `let` binding's value cannot change after it is bound (like a const) but
        does not have to be a compile-time constant expression (unlike a const).
        a `let var` binding can be assigned to.

        the type of the introduced name is inferred from the value of the binding.
        this statement itself has no type
    */
    LetBinding(LetBinding<TContext>),

    /**
        the value of a member of a record or class e.g. `dog.Name`
        OR a function invocation on a record or class using UFCS e.g. `vector.Add(x)`
    */
    Member {
        of: Box<Expression<TContext>>,
        name: String,
    },

    /**
        the value of an element in an array at a given index.
        `x[index]`

        the type of this expression is the element type of the array (if the target
        array is single-dimensional), or an array of the same type with one less
        dimension (if the target is multi-dimensional)
    */
    ArrayElement {
        of: Box<Expression<TContext>>,
        index_expr: Box<Expression<TContext>>,
    },

    /**
        conditional branch. there are two possible forms:
        `if x then ...`
        `if x then ... else ...`

        `condition` must be an expression which has boolean type.
        the `then` and `else` branch expressions can be statements of any type.

        this expression has no type and can only be used as a statement.
    */
    If {
        condition: Box<Expression<TContext>>,
        then_branch: Box<Expression<TContext>>,
        else_branch: Option<Box<Expression<TContext>>>,
    },

    /**
        iterate while a condition is true.
        `while condition do ...`

        `condition` must be an expression which has boolean type.

        this expression has no type and can only be used as a statement.
    */
    While {
        condition: Box<Expression<TContext>>,
        body: Box<Expression<TContext>>,
    },

    /**
        group a list of enclosed statements together into a single expression.
        `begin (statements) end`
        statements are separated by newlines or semicolons. empty statements
        are allowed and ignored.

        this expression has no type and can only be used as a statement.
    */
    Block(Block<TContext>),

    /**
        iterate over a range. there are two possible forms:
        `for x := 0 to 10 do ...`
        `for let x = 0 to 10 do ...`
        the first assigns to and increments an existing variable during
        the loop. the latter binds the loop counter to a new name which
        is only visible during the loop.

        in either form the counter variable must be an expression of
        integer type.

        this expression has no type and can only be used as a statement.
    */
    ForLoop {
        from: Box<Expression<TContext>>,
        to: Box<Expression<TContext>>,
        body: Box<Expression<TContext>>,
    },

    /**

        expression constructing a new set or array. the element type is inferred from the context
        it appears in or the type of the first element (following elements, if any,
        must be of a type automatically convertible to the type of the first element).
        ```
        // array[0..2] of Int32
        let vals = [1, 2, 3]

        // array[1..3] of byte
        let vals: array[1..3] of Int32 = [1, 2, 3]
        ```

        // set of Color
        let vals: set of Color = [Color.Red, Color.Blue]

        a value of a set type which can include multiple member
        groups. a member group can be a single value or an inclusive range.
        the type of this expression is dependent on the type expressed in
        its member groups (which must be uniform).

        ```
        // set of Int32
        let vals = [0..1024]
        ```

        an empty collection (`[]`) can be assigned to any set.
    */
    CollectionConstructor(CollectionConstructor<TContext>),

    /**
        an object constructor, the type of which must be inferred from the
        context it appears in.

        members not appearing in the expression are default-initialized.
        ```
        let point: Point2D = (X: 1, X: 10)
        let zeroPoint: Point2D = ()
        ```
    */
    ObjectConstructor(ObjectConstructor<TContext>),

    /**
        `with (value) do (body)`, where `value` is a class or record.
        `body` is evaluated in the scope of `value`, e.g.
        `with dog do WriteLn(Name)`
        and
        `WriteLn(dog.Name)`
        are equivalent.

        this exists for compatibility with existing FPC and Delphi code and
        should not be used
    */
    With {
        value: Box<Expression<TContext>>,
        body: Box<Expression<TContext>>,
    },

    /**
        raises an error.
        `raise 'dead'`

        this statement terminates the program
    */
    Raise(Box<Expression<TContext>>),

    Exit(Option<Box<Expression<TContext>>>),
}

#[derive(Clone, Debug)]
pub struct Expression<TContext>
    where TContext: Context,
{
    pub value: ExpressionValue<TContext>,
    pub context: TContext,
}

impl<TContext> PartialEq for Expression<TContext>
    where TContext: Context
{
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LetBinding<TContext>
    where TContext: Context
{
    pub name: String,
    pub value: Box<Expression<TContext>>,
    pub explicit_type: Option<TContext::Type>,
    pub mutable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstExpression {
    Integer(IntConstant),
    Float(FloatConstant),
    String(String),
    Boolean(bool),
    Enum(EnumConstant),
    Set(SetConstant),
    Nil,
}

impl From<IntConstant> for ConstExpression {
    fn from(i: IntConstant) -> Self {
        ConstExpression::Integer(i)
    }
}

impl From<FloatConstant> for ConstExpression {
    fn from(f: FloatConstant) -> Self {
        ConstExpression::Float(f)
    }
}

impl ConstExpression {
    pub fn value_type(&self, hint: Option<&Type>) -> Type {
        match self {
            ConstExpression::String(_) => {
                Type::class_ref(ParameterizedName::new_simple("System.String"))
            }

            ConstExpression::Integer(int_const) => {
                match (hint, int_const) {
                    | (Some(hint), _) if hint.is_numeric() && int_const.is_zero() => hint.clone(),
                    | (Some(Type::Byte), _) if int_const.as_u8().is_some() => Type::Byte,
                    | (Some(Type::Int32), _) if int_const.as_i32().is_some() => Type::Int32,
                    | (Some(Type::UInt32), _) if int_const.as_u32().is_some() => Type::UInt32,
                    | (Some(Type::Int64), _) if int_const.as_i64().is_some() => Type::Int64,
                    | (Some(Type::UInt64), _) if int_const.as_u64().is_some() => Type::Int64,
                    | (Some(Type::NativeInt), _) if int_const.as_isize().is_some() => Type::NativeInt,
                    | (Some(Type::NativeUInt), _) if int_const.as_usize().is_some() => Type::NativeUInt,

                    //invalid type hint, ignore it
                    | (Some(_), _) => self.value_type(None),

                    | (None, IntConstant::Char(_)) => Type::Byte,
                    | (None, IntConstant::I32(_)) => Type::Int32,
                    | (None, IntConstant::U32(_)) => Type::UInt32,
                    | (None, IntConstant::I64(_)) => Type::Int64,
                    | (None, IntConstant::U64(_)) => Type::UInt64,
                }
            }

            ConstExpression::Enum(enum_const) =>
                Type::Enumeration(enum_const.enumeration.clone()),

            ConstExpression::Float(float_const) =>
                match float_const {
                    FloatConstant::F64(_) => Type::Float64,
                }

            ConstExpression::Boolean(_) =>
                Type::Boolean,

            ConstExpression::Set(set_const) =>
                Type::Set(set_const.set.clone()),

            ConstExpression::Nil =>
                Type::Nil,
        }
    }
}

impl fmt::Display for ConstExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConstExpression::Integer(int) => write!(f, "{}", int),
            ConstExpression::Float(float) => write!(f, "{}", float),
            ConstExpression::Nil => write!(f, "nil"),
            ConstExpression::Boolean(val) => write!(f, "{}", val),
            ConstExpression::Enum(enum_val) => write!(f, "{}", enum_val),
            ConstExpression::Set(set_val) => write!(f, "{}", set_val),
            ConstExpression::String(s) => write!(f, "'{}'", s),
        }
    }
}

impl<TContext> fmt::Display for Expression<TContext>
    where TContext: Context,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<C> fmt::Display for ExpressionValue<C>
    where C: Context
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                write!(f, "({} {} {})", lhs, op, rhs)
            }

            ExpressionValue::Identifier(id) => f.write_str(&id.to_string()),

            ExpressionValue::FunctionCall(call) => {
                let args_str = call.args().iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                match call {
                    FunctionCall::Function { target, .. } => {
                        write!(f, "{}({})", target, args_str)
                    }

                    FunctionCall::Method { interface_id, func_name, .. } => {
                        write!(f, "{}.{}({})", interface_id, func_name, args_str)
                    }

                    FunctionCall::Extension { self_expr, func_name, .. } => {
                        write!(f, "{}.{}({})", self_expr, func_name, args_str)
                    }
                }
            }

            ExpressionValue::TypeCast { target_type, from_value } => {
                write!(f, "{}({})", target_type, from_value)
            }

            ExpressionValue::PrefixOperator { op, rhs } => {
                write!(f, "({} {})", op, rhs)
            }

            ExpressionValue::Member { of, name } => {
                write!(f, "{}.{}", of, name)
            }

            ExpressionValue::ArrayElement { of, index_expr } => {
                write!(f, "{}[{}]", of, index_expr)
            }

            ExpressionValue::LetBinding(binding) => {
                if binding.mutable {
                    write!(f, "let var {} := {}", binding.name, binding.value)
                } else {
                    write!(f, "let {} = {}", binding.name, binding.value)
                }
            }

            ExpressionValue::Constant(ConstExpression::Integer(i)) => write!(f, "{}", i),
            ExpressionValue::Constant(ConstExpression::Float(float)) => write!(f, "{}", float),

            ExpressionValue::Constant(ConstExpression::String(s)) =>
                write!(f, "{}", tokens::LiteralString(s.clone())),

            ExpressionValue::Constant(ConstExpression::Boolean(b)) =>
                write!(f, "{}", if *b { "true" } else { "false" }),

            ExpressionValue::Constant(ConstExpression::Nil) =>
                f.write_str("nil"),

            ExpressionValue::Constant(ConstExpression::Enum(e)) =>
                write!(f, "{}({})", e.enumeration, e.ordinal),

            ExpressionValue::Constant(ConstExpression::Set(set)) =>
                write!(f, "{}", set.set),

            ExpressionValue::If { condition, then_branch, else_branch } => {
                writeln!(f, "if {} then", condition)?;
                writeln!(f, "\t{}", then_branch)?;

                if let Some(else_expr) = else_branch {
                    writeln!(f, "else")?;
                    writeln!(f, "\t{}", else_expr)?;
                }

                Ok(())
            }

            ExpressionValue::While { condition, body } => {
                writeln!(f, "while {} do {}", condition, body)
            }

            ExpressionValue::Block(block) => {
                writeln!(f, "{}", block)
            }

            ExpressionValue::ForLoop { from, to, body } => {
                writeln!(f, "for {} to {} do {}", from, to, body)
            }

            ExpressionValue::CollectionConstructor(ctor) => {
                write!(f, "{}", ctor)
            }

            ExpressionValue::ObjectConstructor(members) => {
                write!(f, "{}", members)
            }

            ExpressionValue::With { value, body } => {
                write!(f, "with {} do {}", value, body)
            }

            ExpressionValue::Raise(error) => {
                write!(f, "raise {}", error)
            }

            ExpressionValue::Exit(exit_val) => {
                match exit_val {
                    Some(val) => write!(f, "exit {}", val),
                    None => write!(f, "exit")
                }
            }
        }
    }
}

#[allow(dead_code)]
impl<TContext> Expression<TContext>
    where TContext: Context + Clone + fmt::Debug
{
    pub fn prefix_op(op: operators::Operator, rhs: Self, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::PrefixOperator {
                rhs: Box::from(rhs),
                op,
            },
            context: context.into(),
        }
    }

    pub fn binary_op(lhs: Self,
                     op: operators::Operator,
                     rhs: Self,
                     context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::BinaryOperator {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
            context: context.into(),
        }
    }

    pub fn function_call(target: Self, args: impl IntoIterator<Item=Self>) -> Self {
        let context = target.context.clone();

        Expression {
            value: ExpressionValue::FunctionCall(FunctionCall::Function {
                target: Box::new(target),
                args: args.into_iter().collect(),
            }),
            context,
        }
    }

    pub fn method_call(interface_id: impl Into<Identifier>,
                       func_name: impl Into<String>,
                       for_type: impl Into<TContext::Type>,
                       args: impl IntoIterator<Item=Self>,
                       context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::FunctionCall(FunctionCall::Method {
                interface_id: interface_id.into(),
                func_name: func_name.into(),
                for_type: for_type.into(),
                args: args.into_iter().collect(),
            }),
            context: context.into(),
        }
    }

    pub fn extension_call(self_expr: impl Into<Self>,
                          func_name: impl Into<String>,
                          for_type: impl Into<TContext::Type>,
                          args: impl IntoIterator<Item=Self>) -> Self {
        let self_expr = self_expr.into();
        let context = self_expr.context.clone();

        Expression {
            value: ExpressionValue::FunctionCall(FunctionCall::Extension {
                self_expr: Box::new(self_expr),
                args: args.into_iter().collect(),
                for_type: for_type.into(),
                func_name: func_name.into(),
            }),
            context,
        }
    }

    pub fn type_cast(target_type: impl Into<TContext::Type>,
                     from_value: impl Into<Self>,
                     context: impl Into<TContext>)
                     -> Self {
        Expression {
            value: ExpressionValue::TypeCast {
                target_type: target_type.into(),
                from_value: Box::new(from_value.into()),
            },
            context: context.into(),
        }
    }

    pub fn let_binding(binding: impl Into<LetBinding<TContext>>,
                       context: impl Into<TContext>)
                       -> Self {
        Expression {
            value: ExpressionValue::LetBinding(binding.into()),
            context: context.into(),
        }
    }

    pub fn literal_int(i: IntConstant, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstExpression::Integer(i)),
            context: context.into(),
        }
    }

    pub fn literal_float(f: FloatConstant, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstExpression::Float(f)),
            context: context.into(),
        }
    }

    pub fn literal_enumeration(e: EnumConstant, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstExpression::Enum(e)),
            context: context.into(),
        }
    }

    pub fn literal_set(set_const: SetConstant, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstExpression::Set(set_const)),
            context: context.into(),
        }
    }

    pub fn literal_string(s: impl Into<String>, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstExpression::String(s.into())),
            context: context.into(),
        }
    }

    pub fn identifier(id: impl Into<Identifier>, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Identifier(id.into()),
            context: context.into(),
        }
    }

    pub fn const_value(const_val: impl Into<ConstExpression>,
                       context: impl Into<TContext>)
                       -> Self {
        Expression {
            value: ExpressionValue::Constant(const_val.into()),
            context: context.into(),
        }
    }

    pub fn member(of: Self, name: &str) -> Self {
        let context = of.context.clone();

        Expression {
            value: ExpressionValue::Member {
                of: Box::from(of),
                name: name.to_owned(),
            },
            context,
        }
    }

    pub fn array_element(of: Self, index_expr: Self) -> Self {
        let context = of.context.clone();

        Expression {
            value: ExpressionValue::ArrayElement {
                of: Box::new(of),
                index_expr: Box::new(index_expr),
            },
            context,
        }
    }

    pub fn collection_ctor(ctor: impl Into<CollectionConstructor<TContext>>,
                           context: impl Into<TContext>)
                           -> Self {
        Expression {
            value: ExpressionValue::CollectionConstructor(ctor.into()),
            context: context.into(),
        }
    }

    pub fn object_ctor(members: impl Into<ObjectConstructor<TContext>>,
                       context: impl Into<TContext>)
                       -> Self {
        Expression {
            value: ExpressionValue::ObjectConstructor(members.into()),
            context: context.into(),
        }
    }

    pub fn member_deep(of: Self, names: impl IntoIterator<Item=String>) -> Self {
        let mut member = of;
        for name in names {
            member = Expression::member(member, &name);
        }

        member
    }

    pub fn with_statement(value: Self, body: Self, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::With {
                value: Box::new(value),
                body: Box::new(body),
            },
            context: context.into(),
        }
    }

    pub fn raise(error: Self, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Raise(Box::new(error)),
            context: context.into(),
        }
    }

    pub fn exit(with_val: Option<Self>, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Exit(with_val.map(Box::new)),
            context: context.into(),
        }
    }

    pub fn is_exit(&self) -> bool {
        match &self.value {
            ExpressionValue::Exit(_) => true,
            _ => false,
        }
    }

    pub fn is_any_member(&self) -> bool {
        match &self.value {
            ExpressionValue::Member { .. } => true,
            _ => false,
        }
    }

    pub fn as_member(&self) -> Option<(&Self, &str)> {
        match &self.value {
            ExpressionValue::Member { of, name } => Some((of.as_ref(), name.as_str())),
            _ => None,
        }
    }

    pub fn if_then_else(condition: Self,
                        then_branch: Self,
                        else_branch: Option<Self>,
                        context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: else_branch.map(Box::new),
            },
            context: context.into(),
        }
    }

    pub fn while_loop(condition: Self, body: Self, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::While {
                condition: Box::new(condition),
                body: Box::new(body),
            },
            context: context.into(),
        }
    }

    pub fn for_loop(from: Self, to: Self, body: Self, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::ForLoop {
                from: Box::new(from),
                to: Box::new(to),
                body: Box::new(body),
            },
            context: context.into(),
        }
    }

    pub fn block(block: Block<TContext>) -> Self {
        Expression {
            context: block.context.clone(),
            value: ExpressionValue::Block(block),
        }
    }

    pub fn literal_nil(context: impl Into<TContext>) -> Self {
        Expression {
            context: context.into(),
            value: ExpressionValue::Constant(ConstExpression::Nil),
        }
    }

    pub fn literal_bool(val: bool, context: impl Into<TContext>) -> Self {
        Expression {
            context: context.into(),
            value: ExpressionValue::Constant(ConstExpression::Boolean(val)),
        }
    }

    pub fn unwrap_function_call(self) -> FunctionCall<TContext> {
        match self.value {
            ExpressionValue::FunctionCall(call) => call,
            _ => panic!("called unwrap_function_call on something other than a function call expr"),
        }
    }

    pub fn is_function_call(&self) -> bool {
        match &self.value {
            ExpressionValue::FunctionCall(_) => true,
            _ => false,
        }
    }

    pub fn as_function_call(&self) -> Option<&FunctionCall<TContext>> {
        match &self.value {
            ExpressionValue::FunctionCall(call) => Some(call),
            _ => None,
        }
    }

    pub fn is_if(&self) -> bool {
        match &self.value {
            ExpressionValue::If { .. } => true,
            _ => false,
        }
    }

    pub fn is_let_binding(&self) -> bool {
        match &self.value {
            ExpressionValue::LetBinding { .. } => true,
            _ => false
        }
    }

    pub fn as_let_binding(&self) -> Option<&LetBinding<TContext>> {
        match &self.value {
            ExpressionValue::LetBinding(binding) => Some(binding),
            _ => None
        }
    }

    pub fn unwrap_let_binding(self) -> LetBinding<TContext> {
        match self.value {
            ExpressionValue::LetBinding(binding) => binding,
            _ => panic!("called unwrap_let_binding on {}", self)
        }
    }

    pub fn unwrap_literal_string(self) -> String {
        match self.value {
            ExpressionValue::Constant(ConstExpression::String(s)) => s,
            _ => panic!("called unwrap_literal_string on something other than a string literal expr")
        }
    }

    pub fn is_any_literal_string(&self) -> bool {
        match &self.value {
            ExpressionValue::Constant(ConstExpression::String(_)) => true,
            _ => false,
        }
    }

    pub fn is_literal_integer(&self, val: impl Into<IntConstant>) -> bool {
        match &self.value {
            ExpressionValue::Constant(ConstExpression::Integer(i)) => *i == val.into(),
            _ => false,
        }
    }

    pub fn is_any_literal_integer(&self) -> bool {
        match &self.value {
            ExpressionValue::Constant(ConstExpression::Integer(_)) => true,
            _ => false
        }
    }

    pub fn is_binary_op(&self, op: operators::Operator) -> bool {
        match &self.value {
            ExpressionValue::BinaryOperator { op: expr_op, .. } => {
                *expr_op == op
            }
            _ => false
        }
    }

    pub fn is_prefix_op(&self, op: operators::Operator) -> bool {
        match &self.value {
            ExpressionValue::PrefixOperator { op: expr_op, .. } => {
                *expr_op == op
            }
            _ => false
        }
    }

    pub fn as_binary_op(&self) -> Option<(&Self, operators::Operator, &Self)> {
        match &self.value {
            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                Some((lhs.as_ref(), *op, rhs.as_ref()))
            }

            _ => None,
        }
    }

    pub fn unwrap_prefix_op(self) -> (operators::Operator, Self) {
        match self.value {
            ExpressionValue::PrefixOperator { op, rhs } => {
                (op, *rhs)
            }
            _ => panic!("called unwrap_prefix_op on {}", self)
        }
    }

    pub fn is_block(&self) -> bool {
        match &self.value {
            ExpressionValue::Block(_) => true,
            _ => false
        }
    }

    pub fn unwrap_block(self) -> Block<TContext> {
        match self.value {
            ExpressionValue::Block(block) => block,
            _ => panic!("called unwrap_block on {}", self)
        }
    }

    pub fn as_object_constructor(&self) -> Option<&ObjectConstructor<TContext>> {
        match &self.value {
            ExpressionValue::ObjectConstructor(ctor) => Some(ctor),
            _ => None,
        }
    }
}

#[allow(dead_code)]
impl<TContext> Expression<TContext>
    where TContext: Context
{
    pub fn is_any_identifier(&self) -> bool {
        match &self.value {
            | ExpressionValue::Identifier(_) => true,
            _ => false,
        }
    }

    pub fn is_identifier(&self, id: &Identifier) -> bool {
        match &self.value {
            | ExpressionValue::Identifier(expr_id) => expr_id == id,
            _ => false,
        }
    }
}
