use std::fmt;

use operators;
use node::*;
use consts::{
    IntConstant,
    FloatConstant,
    EnumConstant,
    SetConstant,
};
use types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct SetMemberGroup<TContext>
    where TContext: Context
{
    pub from: Expression<TContext>,
    pub to: Option<Expression<TContext>>,
}

#[derive(Clone, Debug)]
pub struct ObjectConstructorMember<TContext>
    where TContext: Context
{
    pub name: String,
    pub value: Expression<TContext>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjectConstructor<TContext>
    where TContext: Context
{
    pub members: Vec<ObjectConstructorMember<TContext>>,
    pub object_type: Option<Type>,
}

impl<TContext> PartialEq for ObjectConstructorMember<TContext>
    where TContext: Context
{
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(&other.name) && self.value.eq(&other.value)
    }
}

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
    FunctionCall {
        target: Box<Expression<TContext>>,
        args: Vec<Expression<TContext>>,
    },

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
    Constant(ConstantExpression),

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
        a value of a set type which can include multiple member
        groups. a member group can be a single value or an inclusive range.
        the type of this expression is dependent on the type expressed in
        its member groups (which must be uniform).

        an empty set constructor is the empty set type, which can be
        assigned to all set values.

        e.g. `[0, 1, 2]`, `[0..10]`
    */
    SetConstructor(Vec<SetMemberGroup<TContext>>),

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
    //    pub explicit_type: Option<TContext::Type>,
    pub mutable: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstantExpression {
    Integer(IntConstant),
    Float(FloatConstant),
    String(String),
    Boolean(bool),
    Enum(EnumConstant),
    Set(SetConstant),
    Nil,
}

impl ConstantExpression {
    pub fn value_type(&self) -> Type {
        match self {
            ConstantExpression::String(_) => {
                Type::Class(Identifier::from("System.String"))
            }

            ConstantExpression::Integer(int_const) =>
                match int_const {
                    IntConstant::Char(_) => Type::Byte,
                    IntConstant::I32(_) => Type::Int32,
                    IntConstant::U32(_) => Type::UInt32,
                    IntConstant::I64(_) => Type::Int64,
                    IntConstant::U64(_) => Type::UInt64,
                },

            ConstantExpression::Enum(enum_const) =>
                Type::Enumeration(enum_const.enumeration.clone()),

            ConstantExpression::Float(float_const) =>
                match float_const {
                    FloatConstant::F64(_) => Type::Float64,
                }

            ConstantExpression::Boolean(_) =>
                Type::Boolean,

            ConstantExpression::Set(set_const) =>
                Type::Set(set_const.set.clone()),

            ConstantExpression::Nil =>
                Type::Nil,
        }
    }
}

impl<TContext> fmt::Display for Expression<TContext>
    where TContext: Context + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "expression: {:?} ({})", self.value, self.context.token()) //TODO better display
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
            value: ExpressionValue::FunctionCall {
                target: Box::new(target),
                args: args.into_iter().collect(),
            },
            context: context.into(),
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
            value: ExpressionValue::Constant(ConstantExpression::Integer(i)),
            context: context.into(),
        }
    }

    pub fn literal_float(f: FloatConstant, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstantExpression::Float(f)),
            context: context.into(),
        }
    }

    pub fn literal_enumeration(e: EnumConstant, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstantExpression::Enum(e)),
            context: context.into(),
        }
    }

    pub fn literal_set(set_const: SetConstant, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstantExpression::Set(set_const)),
            context: context.into(),
        }
    }

    pub fn literal_string(s: &str, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(ConstantExpression::String(s.to_owned())),
            context: context.into(),
        }
    }

    pub fn identifier(id: Identifier, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Identifier(id),
            context: context.into(),
        }
    }

    pub fn const_value(const_val: ConstantExpression, context: impl Into<TContext>) -> Self {
        Expression {
            value: ExpressionValue::Constant(const_val),
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

    pub fn set_constructor(members: Vec<SetMemberGroup<TContext>>,
                           context: impl Into<TContext>)
                           -> Self {
        Expression {
            value: ExpressionValue::SetConstructor(members),
            context: context.into(),
        }
    }

    pub fn object_constructor(members: impl Into<ObjectConstructor<TContext>>,
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

    pub fn is_any_member(&self) -> bool {
        match &self.value {
            &ExpressionValue::Member { .. } => true,
            _ => false,
        }
    }

    pub fn unwrap_member(self) -> (Self, String) {
        match self.value {
            ExpressionValue::Member { of, name } => (*of, name),
            _ => panic!("called unwrap_member on {}", self),
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
            value: ExpressionValue::Constant(ConstantExpression::Nil),
        }
    }

    pub fn literal_bool(val: bool, context: impl Into<TContext>) -> Self {
        Expression {
            context: context.into(),
            value: ExpressionValue::Constant(ConstantExpression::Boolean(val)),
        }
    }

    pub fn unwrap_function_call(self) -> (Self, Vec<Self>) {
        match self.value {
            ExpressionValue::FunctionCall { target, args } => (*target, args),
            _ => panic!("called unwrap_function_call on something other than a function call expr"),
        }
    }

    pub fn is_function_call(&self) -> bool {
        match &self.value {
            &ExpressionValue::FunctionCall { .. } => true,
            _ => false,
        }
    }

    pub fn is_if(&self) -> bool {
        match &self.value {
            &ExpressionValue::If { .. } => true,
            _ => false,
        }
    }

    pub fn is_let_binding(&self) -> bool {
        match &self.value {
            &ExpressionValue::LetBinding { .. } => true,
            _ => false
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
            ExpressionValue::Constant(ConstantExpression::String(s)) => s,
            _ => panic!("called unwrap_literal_string on something other than a string literal expr")
        }
    }

    pub fn is_any_literal_string(&self) -> bool {
        match &self.value {
            ExpressionValue::Constant(ConstantExpression::String(_)) => true,
            _ => false,
        }
    }

    pub fn is_literal_integer(&self, val: impl Into<IntConstant>) -> bool {
        match &self.value {
            ExpressionValue::Constant(ConstantExpression::Integer(i)) => *i == val.into(),
            _ => false,
        }
    }

    pub fn is_any_literal_integer(&self) -> bool {
        match &self.value {
            &ExpressionValue::Constant(ConstantExpression::Integer(_)) => true,
            _ => false
        }
    }

    pub fn is_binary_op(&self, op: operators::Operator) -> bool {
        match &self.value {
            &ExpressionValue::BinaryOperator { op: expr_op, .. } => {
                expr_op == op
            }
            _ => false
        }
    }

    pub fn is_prefix_op(&self, op: operators::Operator) -> bool {
        match &self.value {
            &ExpressionValue::PrefixOperator { op: expr_op, .. } => {
                expr_op == op
            }
            _ => false
        }
    }

    pub fn unwrap_binary_op(self) -> (Self, operators::Operator, Self) {
        match self.value {
            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                (*lhs, op, *rhs)
            }
            _ => panic!("called unwrap_binary_op on {}", self)
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
            &ExpressionValue::Block(_) => true,
            _ => false
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

pub fn try_visit_expressions<TContext, TErr>(
    root_expr: &Expression<TContext>,
    mut visit: &mut FnMut(&Expression<TContext>) -> Result<(), TErr>)
    -> Result<(), TErr>
    where TContext: Context
{
    match &root_expr.value {
        | ExpressionValue::BinaryOperator { lhs, rhs, .. } => {
            try_visit_expressions(lhs.as_ref(), &mut visit)?;
            try_visit_expressions(rhs.as_ref(), &mut visit)?;
        }
        | ExpressionValue::PrefixOperator { rhs, .. } => {
            try_visit_expressions(rhs.as_ref(), &mut visit)?;
        }
        | ExpressionValue::TypeCast { from_value, .. } => {
            try_visit_expressions(from_value.as_ref(), &mut visit)?;
        }

        | ExpressionValue::FunctionCall { target, args } => {
            try_visit_expressions(target.as_ref(), &mut visit)?;
            for arg in args.iter() {
                try_visit_expressions(arg, &mut visit)?;
            }
        }

        | ExpressionValue::LetBinding(binding) => {
            try_visit_expressions(binding.value.as_ref(), &mut visit)?;
        }

        | ExpressionValue::Member { of, .. } => {
            try_visit_expressions(of.as_ref(), &mut visit)?;
        }

        | ExpressionValue::ArrayElement { of, index_expr } => {
            try_visit_expressions(of.as_ref(), &mut visit)?;
            try_visit_expressions(index_expr.as_ref(), &mut visit)?;
        }

        | ExpressionValue::If { condition, then_branch, else_branch } => {
            try_visit_expressions(condition.as_ref(), &mut visit)?;
            try_visit_expressions(then_branch.as_ref(), &mut visit)?;

            if let Some(expr) = else_branch.as_ref() {
                try_visit_expressions(expr.as_ref(), &mut visit)?;
            }
        }

        | ExpressionValue::While { condition, body } => {
            try_visit_expressions(condition.as_ref(), &mut visit)?;
            try_visit_expressions(body.as_ref(), &mut visit)?;
        }

        | ExpressionValue::Block(block) => {
            for statement in block.statements.iter() {
                try_visit_expressions(statement, &mut visit)?;
            }
        }

        | ExpressionValue::ForLoop { from, to, body } => {
            try_visit_expressions(from.as_ref(), &mut visit)?;
            try_visit_expressions(to.as_ref(), &mut visit)?;
            try_visit_expressions(body.as_ref(), &mut visit)?;
        }

        | ExpressionValue::With { value, body } => {
            try_visit_expressions(value.as_ref(), &mut visit)?;
            try_visit_expressions(body.as_ref(), &mut visit)?;
        }
        | ExpressionValue::Raise(expr) => {
            try_visit_expressions(expr.as_ref(), &mut visit)?;
        }

        | ExpressionValue::ObjectConstructor(obj) => {
            for member in obj.members.iter() {
                try_visit_expressions(&member.value, &mut visit)?;
            }
        }

        | ExpressionValue::Identifier(_)
        | ExpressionValue::Constant(_)
        | ExpressionValue::SetConstructor(_)
        => {}
    }

    visit(&root_expr)
}

pub fn transform_expressions<TContext>(
    root_expr: Expression<TContext>,
    replace: &mut FnMut(Expression<TContext>) -> Expression<TContext>)
    -> Expression<TContext>
    where TContext: Context + Clone + fmt::Debug
{
    match root_expr.value {
        ExpressionValue::BinaryOperator { lhs, op, rhs } => {
            let lhs = transform_expressions(*lhs, replace);
            let rhs = transform_expressions(*rhs, replace);

            replace(Expression::binary_op(lhs, op, rhs, root_expr.context))
        }

        ExpressionValue::Block(block) => {
            let statements = block.statements.into_iter()
                .map(|stmt| transform_expressions(stmt, replace))
                .collect();

            replace(Expression::block(Block {
                context: block.context,
                statements,
            }))
        }

        ExpressionValue::ForLoop { from, to, body } => {
            let from = transform_expressions(*from, replace);
            let to = transform_expressions(*to, replace);
            let body = transform_expressions(*body, replace);

            replace(Expression::for_loop(from, to, body, root_expr.context))
        }

        ExpressionValue::If { condition, then_branch, else_branch } => {
            let cond = transform_expressions(*condition, replace);
            let if_branch = transform_expressions(*then_branch, replace);
            let else_branch = else_branch.map(|else_expr| {
                transform_expressions(*else_expr, replace)
            });

            replace(Expression::if_then_else(cond, if_branch, else_branch, root_expr.context))
        }

        ExpressionValue::While { condition, body } => {
            let condition = transform_expressions(*condition, replace);
            let body = transform_expressions(*body, replace);

            replace(Expression::while_loop(condition, body, root_expr.context))
        }

        ExpressionValue::PrefixOperator { op, rhs } => {
            let rhs = transform_expressions(*rhs, replace);

            replace(Expression::prefix_op(op, rhs, root_expr.context))
        }

        ExpressionValue::LetBinding(mut binding) => {
            binding.value = Box::new(transform_expressions(*binding.value, replace));
            replace(Expression::let_binding(binding, root_expr.context))
        }

        ExpressionValue::Member { of, name } => {
            let of = transform_expressions(*of, replace);
            replace(Expression::member(of, &name))
        }

        ExpressionValue::ArrayElement { of, index_expr } => {
            let of = transform_expressions(*of, replace);
            let index_expr = transform_expressions(*index_expr, replace);
            replace(Expression::array_element(of, index_expr))
        }

        ExpressionValue::Identifier(name) => {
            replace(Expression::identifier(name, root_expr.context))
        }

        ExpressionValue::Constant(const_expr) => {
            replace(Expression::const_value(const_expr, root_expr.context))
        }

        ExpressionValue::FunctionCall { target, args } => {
            let target = transform_expressions(*target, replace);
            let args: Vec<_> = args.into_iter().map(|arg| transform_expressions(arg, replace))
                .collect();

            replace(Expression::function_call(target, args))
        }

        ExpressionValue::TypeCast { target_type, from_value } => {
            let from_value = transform_expressions(*from_value, replace);

            replace(Expression::type_cast(target_type, from_value, root_expr.context))
        }

        ExpressionValue::SetConstructor(members) => {
            let members = members.into_iter()
                .map(|member| {
                    let from = transform_expressions(member.from, replace);
                    let to = member.to.map(|to| transform_expressions(to, replace));

                    SetMemberGroup { from, to }
                })
                .collect();

            replace(Expression::set_constructor(members, root_expr.context))
        }

        ExpressionValue::ObjectConstructor(obj) => {
            let members: Vec<_> = obj.members.into_iter()
                .map(|member| {
                    let value = transform_expressions(member.value, replace);
                    ObjectConstructorMember {
                        name: member.name,
                        value,
                    }
                })
                .collect();

            let obj: ObjectConstructor<TContext> = ObjectConstructor {
                members,
                object_type: obj.object_type,
            };

            replace(Expression::object_constructor(obj, root_expr.context))
        }

        ExpressionValue::With { value, body } => {
            let value = transform_expressions(*value, replace);
            let body = transform_expressions(*body, replace);

            replace(Expression::with_statement(value, body, root_expr.context))
        }

        ExpressionValue::Raise(error) => {
            let error = transform_expressions(*error, replace);
            replace(Expression::raise(error, root_expr.context))
        }
    }
}