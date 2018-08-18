use std::{
    fmt::{self, Write},
};

use target_c::ast::{
    rc_release,
    rc_retain,
    TranslationResult,
    Name,
    TranslationUnit,
    Block,
    CType,
};
use semantic::{
    self,
    BindingKind,
    arc_transform::{
        RcSubValuePath,
        RcStatement,
        BoundName,
        extract_stmt_rc_bindings,
    },
};
use node::{
    Context,
    Identifier,
    ExpressionValue,
    ConstExpression,
    FunctionCall,
};
use types::{
    Type,
    ParameterizedName,
};
use consts::{
    IntConstant,
    FloatConstant,
};
use operators;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum CastKind {
    Static,
    Reinterpret,
    Const,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Raw(String),
    Name(Name),
    Block(Block),
    LocalDecl {
        ctype: CType,
        name: Name,
        value: Option<Box<Expression>>,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
    },
    While {
        condition: Box<Expression>,
        body: Box<Expression>,
    },
    ForLoop {
        init: Box<Expression>,
        continue_while: Box<Expression>,
        after_each: Box<Expression>,
        body: Box<Expression>,
    },
    FunctionCall {
        target: Box<Expression>,
        args: Vec<Expression>,
    },
    UnaryOp {
        op: String,
        operand: Box<Expression>,
        before: bool,
    },
    BinaryOp {
        lhs: Box<Expression>,
        op: String,
        rhs: Box<Expression>,
    },
    Cast {
        target_type: CType,
        value: Box<Expression>,
        kind: CastKind,
    },
    SizeLiteral(usize),
    StringLiteral(String),
    Member {
        of: Box<Expression>,
        name: String,
    },
    Return(Box<Expression>),
    True,
    False,
    Null,
}

impl From<Name> for Expression {
    fn from(name: Name) -> Self {
        Expression::Name(name)
    }
}

impl From<Vec<Expression>> for Expression {
    fn from(statements: Vec<Expression>) -> Self {
        Expression::Block(Block::new(statements))
    }
}

impl Expression {
    pub fn if_then(condition: impl Into<Self>, then_branch: impl Into<Self>) -> Self {
        Expression::If {
            condition: Box::new(condition.into()),
            then_branch: Box::new(then_branch.into()),
            else_branch: None,
        }
    }

    pub fn if_then_else(condition: impl Into<Self>,
                        then_branch: impl Into<Self>,
                        else_branch: impl Into<Self>) -> Self {
        Expression::If {
            condition: Box::new(condition.into()),
            then_branch: Box::new(then_branch.into()),
            else_branch: Some(Box::new(else_branch.into())),
        }
    }

    pub fn function_call(target: impl Into<Self>, args: impl IntoIterator<Item=Self>) -> Self {
        Expression::FunctionCall {
            target: Box::new(target.into()),
            args: args.into_iter().collect(),
        }
    }

    pub fn string_literal(s: &str) -> Self {
        Expression::StringLiteral(s.replace("\"", "\\\""))
    }

    fn while_loop(condition: impl Into<Self>, body: impl Into<Self>) -> Self {
        Expression::While {
            condition: Box::new(condition.into()),
            body: Box::new(body.into()),
        }
    }

    fn for_loop(init: impl Into<Self>,
                continue_while: impl Into<Self>,
                after_each: impl Into<Self>,
                body: impl Into<Self>) -> Self {
        Expression::ForLoop {
            init: Box::new(init.into()),
            continue_while: Box::new(continue_while.into()),
            after_each: Box::new(after_each.into()),
            body: Box::new(body.into()),
        }
    }

    pub fn deref(self) -> Self {
        Self::unary_op("*", self, true)
    }

    pub fn unary_op(op: impl Into<String>, operand: impl Into<Self>, before: bool) -> Self {
        Expression::UnaryOp {
            op: op.into(),
            operand: Box::new(operand.into()),
            before,
        }
    }

    pub fn binary_op(lhs: impl Into<Self>, op: impl Into<String>, rhs: impl Into<Self>) -> Self {
        Expression::BinaryOp {
            lhs: Box::new(lhs.into()),
            op: op.into(),
            rhs: Box::new(rhs.into()),
        }
    }

    pub fn cast(target_type: impl Into<CType>, value: impl Into<Self>, kind: CastKind) -> Self {
        Expression::Cast {
            target_type: target_type.into(),
            value: Box::new(value.into()),
            kind,
        }
    }

    pub fn local_decl(ctype: impl Into<CType>,
                      name: impl Into<Name>,
                      value: Option<Self>) -> Self {
        Expression::LocalDecl {
            ctype: ctype.into(),
            name: name.into(),
            value: value.map(Box::new),
        }
    }

    pub fn member(of: impl Into<Self>, name: impl Into<String>) -> Self {
        Expression::Member {
            of: Box::new(of.into()),
            name: name.into(),
        }
    }

    pub fn return_value(value: impl Into<Self>) -> Self {
        Expression::Return(Box::new(value.into()))
    }

    pub fn translate_rc_value_expr(path: &RcSubValuePath,
                                   base: impl Into<Expression>)
        -> Expression {
        match path {
            RcSubValuePath::This { parent: None } =>
                base.into(),
            RcSubValuePath::This { parent: Some(parent) } =>
                Self::translate_rc_value_expr(parent, base),

            RcSubValuePath::Member { parent: None, name } =>
                Expression::member(base, name.clone()),
            RcSubValuePath::Member { parent: Some(parent), name } =>
                Expression::member(Self::translate_rc_value_expr(parent, base), name.clone()),

            RcSubValuePath::ArrayElement { parent: None, index } =>
                unimplemented!("rc subvalue path <this>[{}]", index),
            RcSubValuePath::ArrayElement { parent: Some(parent), index } =>
                unimplemented!("rc subvalue path {}[{}]", parent, index),
        }
    }

    pub fn translate_rc_statement(stmt: RcStatement,
                                         unit: &mut TranslationUnit)
                                         -> TranslationResult<Vec<Expression>> {
        match stmt {
            RcStatement::Block(statements) => {
                let block = Block::translate_rc_block(statements, None, unit)?;

                Ok(vec![Expression::Block(block)])
            }

            RcStatement::Statement { body, rc_bindings, bound_name, rc_assignments } => {
                /*
                    translate the main statement into one or more c statements - a simple expression with no
                    rc to handle will translate to one c statement, otherwise this may include rc statements
                    to release the old value and retain the new one
                */
                let mut lines = Vec::new();


                /* if we're going to bind this to a name, declare that before the temporaries
                block so the name is visible outside */
                if let Some(bound_name) = &bound_name {
                    let local_type = CType::translate(
                        &bound_name.bound_type,
                        body.scope(),
                        unit,
                    )?;

                    lines.push(Expression::local_decl(
                        local_type,
                        Name::local(bound_name.name.as_str()),
                        None)
                    );
                }

                let body_expr = match &bound_name {
                    Some(BoundName { name, .. }) => Expression::binary_op(
                        Name::local(name.as_str()),
                        "=",
                        Self::translate_expression(&body, unit)?,
                    ),

                    None => Self::translate_expression(&body, unit)?,
                };
                let rc_val_exprs: Vec<Expression> = rc_assignments.iter()
                    .map(|expr| Self::translate_expression(expr, unit))
                    .collect::<TranslationResult<_>>()?;

                for (i, rc_val) in rc_assignments.iter().enumerate() {
                    /* if it's an assignment to a variable which is already initialized */
                    if let ExpressionValue::Identifier(name) = &rc_val.value {
                        if rc_val.scope().get_symbol(name).unwrap().initialized() {
                            // if (lhs) release(lhs)
                            let val_expr = &rc_val_exprs[i];

                            lines.push(Expression::if_then(
                                val_expr.clone(),
                                rc_release(val_expr.clone()),
                            ));
                        }
                    }
                }

                if !rc_bindings.is_empty() {
                    let mut temp_block = Vec::new();

                    // ...then release temp bindings and close the block
                    let rc_subvals: Vec<Expression> = rc_bindings.iter()
                        .enumerate()
                        .flat_map(|(tmp_id, rc_binding)| {
                            rc_binding.rc_subvalues.iter()
                                .map(move |val_path| {
                                    let tmp_name = Name::local_internal(format!("rc_{}", tmp_id));
                                    Self::translate_rc_value_expr(val_path, tmp_name)
                                })
                        })
                        .collect();

                    // bind RC temporaries to local names...
                    for (tmp_id, tmp_val) in rc_bindings.iter().enumerate() {
                        let tmp_type = tmp_val.base_value.expr_type()
                            .expect("temporary rc values should always be valid types")
                            .expect("temporary rc values should never have no type");

                        let val_c_type = CType::translate(&tmp_type, tmp_val.base_value.scope(), unit)?;
                        let tmp_name = Name::local_internal(format!("rc_{}", tmp_id));
                        let tmp_val_expr = Expression::translate_expression(
                            &tmp_val.base_value,
                            unit,
                        )?;

                        temp_block.push(Expression::local_decl(
                            val_c_type,
                            tmp_name,
                            Some(tmp_val_expr),
                        ));
                    }

                    // ...the original exprs in the middle...
                    temp_block.push(body_expr);

                    /* if this statement binds an expression to a name, the rc values
                    in this statement need to outlive it so add a reference */
                    if let Some(_bound_name) = &bound_name {
                        temp_block.extend(rc_subvals.iter().cloned().map(rc_retain));
                    }

                    // ...retain newly-assigned values...
                    for rc_val in rc_val_exprs {
                        temp_block.push(rc_retain(rc_val));
                    }

                    // release statement temporaries
                    temp_block.extend(rc_subvals.iter().cloned().map(rc_release));

                    lines.push(Expression::Block(Block::new(temp_block)));
                } else {
                    lines.push(body_expr);

                    for rc_val in rc_val_exprs {
                        lines.push(rc_retain(rc_val));
                    }
                }

                Ok(lines)
            }
        }
    }

    pub fn translate_statement(stmt: &semantic::Expression,
                               unit: &mut TranslationUnit)
                               -> TranslationResult<Vec<Self>> {
        let rc_block = extract_stmt_rc_bindings(stmt.clone(), 0);

        Self::translate_rc_statement(rc_block, unit)
    }

    pub fn translate_expression(expr: &semantic::Expression,
                                unit: &mut TranslationUnit)
                                -> TranslationResult<Self> {
        match &expr.value {
            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                let string_type = Some(Type::Class(ParameterizedName {
                    name: Identifier::from("System.String"),
                    type_args: Vec::new(),
                }));

                if *op == operators::Plus
                    && lhs.expr_type().unwrap() == string_type
                    && rhs.expr_type().unwrap() == string_type {
                    return Ok(Expression::function_call(
                        Name::user_symbol(&Identifier::from("System.StringConcat")),
                        vec![
                            Expression::translate_expression(lhs, unit)?,
                            Expression::translate_expression(rhs, unit)?,
                        ],
                    ));
                }

                if *op == operators::In {
                    unimplemented!("`in` operator (c++ backend")
                }

                let c_op = match op {
                    operators::Assignment => "=",
                    operators::Equals => "==",
                    operators::NotEquals => "!=",
                    operators::Minus => "-",
                    operators::Plus => "+",
                    operators::Multiply => "*",
                    operators::Divide => "/",
                    operators::And => "&&",
                    operators::Or => "||",
                    operators::Lt => "<",
                    operators::Lte => "<=",
                    operators::Gt => ">",
                    operators::Gte => ">=",

                    operators::Not |
                    operators::RangeInclusive |
                    operators::AddressOf |
                    operators::Deref => panic!("bad binary operator type: {}", op),

                    operators::In => unreachable!("special cased above"),
                };

                Ok(Expression::binary_op(
                    Expression::translate_expression(lhs, unit)?,
                    c_op,
                    Expression::translate_expression(rhs, unit)?,
                ))
            }

            ExpressionValue::PrefixOperator { op, rhs } => {
                let c_op = match op {
                    operators::Plus => "+",
                    operators::Minus => "-",
                    operators::Deref => "*",
                    operators::AddressOf => "&",
                    operators::Not => "!",

                    operators::In |
                    operators::RangeInclusive |
                    operators::And |
                    operators::Or |
                    operators::Equals |
                    operators::NotEquals |
                    operators::Multiply |
                    operators::Divide |
                    operators::Gt |
                    operators::Gte |
                    operators::Lt |
                    operators::Lte |
                    operators::Assignment => panic!("bad prefix operator type: {}", op),
                };

                Ok(Expression::unary_op(c_op, Expression::translate_expression(rhs, unit)?, true))
            }

            ExpressionValue::FunctionCall(call) => {
                let args: Vec<_> = call.args().iter()
                    .map(|arg_expr| {
                        Expression::translate_expression(arg_expr, unit)
                    })
                    .collect::<TranslationResult<_>>()?;

                let target = match call {
                    FunctionCall::Function { target, .. } => {
                        Expression::translate_expression(target, unit)?
                    }

                    FunctionCall::Method { interface_id, func_name, for_type, .. } => {
                        match for_type {
                            Type::AnyImplementation(_) => {
                                let call_name = unit.method_call_name(interface_id, func_name)
                                    .unwrap();

                                Expression::Name(call_name.clone())
                            }

                            _ => {
                                let type_name = expr.scope().full_type_name(for_type).unwrap();
                                let call_name = Name::interface_call(
                                    interface_id,
                                    &type_name,
                                    func_name.clone(),
                                );

                                Expression::Name(call_name)
                            }
                        }
                    }
                };

                Ok(Expression::function_call(target, args))
            }

            ExpressionValue::TypeCast { target_type, from_value } => {
                let target_ctype = CType::translate(target_type, expr.scope(), unit)?;
                let from_value = Expression::translate_expression(from_value, unit)?;

                Ok(Expression::cast(target_ctype, from_value, CastKind::Static))
            }

            ExpressionValue::LetBinding(binding) => {
                let value_type = binding.value.expr_type().unwrap().unwrap();
                let value = Expression::translate_expression(binding.value.as_ref(), unit)?;

                Ok(Expression::local_decl(
                    CType::translate(&value_type, expr.scope(), unit)?,
                    Name::local(binding.name.as_str()),
                    Some(value),
                ))
            }

            ExpressionValue::Constant(const_expr) => {
                match const_expr {
                    ConstExpression::Integer(i) => {
                        let int_type = const_expr.value_type(expr.context.type_hint());
                        let cast_target = CType::translate(&int_type, expr.scope(), unit)?;

                        let cast_val = Expression::Raw(match i {
                            IntConstant::U32(i) => format!("0x{:x}", i),
                            IntConstant::U64(i) => format!("0x{:x}", i),
                            IntConstant::I32(i) => format!("{}", i),
                            IntConstant::I64(i) => format!("{}ll", i),
                            IntConstant::Char(c) => format!("{}", c),
                        });

                        Ok(Expression::cast(cast_target, cast_val, CastKind::Static))
                    }

                    ConstExpression::Enum(e) => {
                        let enum_name = Name::user_type(
                            &ParameterizedName::new_simple(e.enumeration.clone()));
                        Ok(Expression::cast(
                            CType::Named(enum_name),
                            /* todo: check for overflow if usize != u64 */
                            Expression::SizeLiteral(e.ordinal as usize),
                            CastKind::Static,
                        ))
                    }

                    ConstExpression::Set(_set) => {
                        unimplemented!("set literals (c++ backend)")
                    }

                    ConstExpression::Float(f) => {
                        let float_type = const_expr.value_type(expr.context.type_hint());

                        let cast_target = CType::translate(&float_type, expr.scope(), unit)?;
                        let cast_value = match f {
                            FloatConstant::F64(val) => format!("{:e}", val),
                        };

                        Ok(Expression::cast(
                            cast_target,
                            Expression::Raw(cast_value),
                            CastKind::Static,
                        ))
                    }

                    ConstExpression::String(s) => {
                        Ok(Expression::Name(unit.string_literal_name(s)))
                    }

                    ConstExpression::Boolean(val) => {
                        if *val {
                            Ok(Expression::True)
                        } else {
                            Ok(Expression::False)
                        }
                    }

                    ConstExpression::Nil => {
                        Ok(Expression::Null)
                    }
                }
            }

            ExpressionValue::If { condition, then_branch, else_branch } => {
                let condition = Expression::translate_expression(condition, unit)?;
                let then_branch = Expression::translate_statement(then_branch, unit)?;

                let if_expr = match else_branch {
                    Some(stmt) => {
                        let else_stmts = Expression::translate_statement(stmt.as_ref(), unit)?;

                        Expression::if_then_else(
                            condition,
                            Expression::Block(Block::new(then_branch)),
                            Expression::Block(Block::new(else_stmts)),
                        )
                    }
                    None => {
                        Expression::if_then(condition, then_branch)
                    }
                };

                Ok(if_expr)
            }

            ExpressionValue::While { condition, body } => {
                let condition = Expression::translate_expression(condition, unit)?;
                let body = Expression::translate_statement(body, unit)?;

                Ok(Expression::while_loop(condition, body))
            }

            ExpressionValue::ForLoop { from, to, body } => {
                let iter_expr = match &from.as_ref().value {
                    ExpressionValue::BinaryOperator { lhs, op, .. }
                    if *op == operators::Assignment => {
                        Expression::translate_expression(lhs, unit)?
                    }
                    ExpressionValue::LetBinding(binding) => {
                        Expression::Name(Name::local(binding.name.clone()))
                    }
                    _ => panic!("for loop 'from' clause must be an assignment or a let binding")
                };

                let init = Expression::translate_expression(from, unit)?;

                let to = Expression::translate_expression(to, unit)?;
                let continue_while = Expression::binary_op(iter_expr.clone(), "<", to);

                let after_each = Expression::binary_op(
                    iter_expr.clone(),
                    "+=",
                    Expression::SizeLiteral(1),
                );

                let body = Expression::translate_statement(body, unit)?;

                Ok(Expression::for_loop(init, continue_while, after_each, body))
            }

            ExpressionValue::Identifier(sym) => {
                let scoped_sym = expr.scope().get_symbol(sym).unwrap();
                match scoped_sym.binding_kind() {
                    | BindingKind::Uninitialized
                    | BindingKind::Immutable
                    | BindingKind::Mutable
                    => Ok(Expression::Name(Name::local(scoped_sym.name().name.clone()))),

                    | BindingKind::Global
                    | BindingKind::Function
                    => Ok(Expression::Name(Name::user_symbol(&scoped_sym.name()))),

                    | BindingKind::Internal
                    => Ok(Expression::Name(Name::local_internal(scoped_sym.name().name.clone())))
                }
            }

            ExpressionValue::Member { of, name } => {
                //panic!("of: {:?}, name: {}", of, name);
                let mut of_type: Type = of.expr_type()
                    .unwrap()
                    .expect("target of member expression must exist");

                /* accessing a member through a pointer automatically dereferences the pointer */
                let mut of_expr = Expression::translate_expression(of, unit)?;
                while let Type::Pointer(of_target) = of_type {
                    of_expr = of_expr.deref();
                    of_type = *of_target;
                }

                /* ...but pascal classes in the C++ backend have one extra level of indirection */
                if of_type.is_class() {
                    of_expr = of_expr.deref();
                };

                Ok(Expression::member(of_expr, name.clone()))
            }

            ExpressionValue::ArrayElement { of, index_expr } => {
                let mut out = String::new();
                write!(out, "(")?;
                Expression::translate_expression(of, unit)?.write(&mut out)?;
                write!(out, "[")?;
                Expression::translate_expression(index_expr, unit)?.write(&mut out)?;
                write!(out, "])")?;

                Ok(Expression::Raw(out))
            }

            ExpressionValue::Block(block) => {
                let block = Block::translate(block, None, unit)?;
                Ok(Expression::Block(block))
            }

            ExpressionValue::ObjectConstructor(obj) => {
                let (obj_id, obj_decl) = match obj.object_type.as_ref() {
                    | Some(Type::Class(name)) => {
                        expr.scope().get_class_specialized(name).unwrap()
                    }
                    | Some(Type::Record(name)) => {
                        expr.scope().get_record_specialized(name).unwrap()
                    }
                    | _ => panic!("invalid type for object constructor: {:?}", obj.object_type),
                };

                /* todo: support the variant part */
                let ctor_args: Vec<Expression> = obj_decl.all_members()
                    .map(|member| {
                        match obj.get_member(&member.name) {
                            /* the typechecker should have already thrown an error if the type
                             isn't default-able */
                            None => Ok(Expression::Name(Name::internal_symbol("None"))),
                            Some(ctor_val) => Self::translate_expression(&ctor_val.value, unit),
                        }
                    })
                    .collect::<TranslationResult<_>>()?;

                let ctor_name = Name::constructor(&obj_id);

                Ok(Expression::function_call(ctor_name, ctor_args))
            }

            ExpressionValue::SetConstructor(_member_groups) => {
                unimplemented!("set constructor (c++ backend)");
            }

            ExpressionValue::With { .. } => {
                unreachable!("with statements should be removed during semantic analysis");
            }

            ExpressionValue::Raise(error) => {
                let location = &expr.context.token().location;
                let msg = error.to_string();

                /* with absolutely no exception support the best thing we can do is convert
                the error to a string */
                let raise_args = vec![
                    Expression::string_literal(&location.file),
                    Expression::SizeLiteral(location.line),
                    Expression::SizeLiteral(location.col),
                    Expression::string_literal(&msg)
                ];

                Ok(Expression::function_call(Name::internal_symbol("Raise"), raise_args))
            }
        }
    }

    pub fn write(&self, out: &mut fmt::Write) -> fmt::Result {
        match self {
            Expression::True => out.write_str("true"),
            Expression::False => out.write_str("false"),
            Expression::Null => out.write_str("nullptr"),

            Expression::Block(block) => block.write(out),
            Expression::Raw(expr) => out.write_str(expr),
            Expression::Name(name) => write!(out, "{}", name),

            Expression::If { condition, then_branch, else_branch } => {
                write!(out, "if (")?;
                condition.write(out)?;
                writeln!(out, ") {{")?;

                then_branch.write(out)?;
                writeln!(out, "; }}")?;

                if let Some(ref else_expr) = else_branch {
                    write!(out, " else {{")?;
                    else_expr.write(out)?;
                    writeln!(out, "; }}")?;
                }
                Ok(())
            }

            Expression::FunctionCall { target, args } => {
                target.write(out)?;

                write!(out, "(")?;
                for (arg_num, arg) in args.iter().enumerate() {
                    if arg_num > 0 {
                        write!(out, ", ")?;
                    }

                    arg.write(out)?;
                }
                write!(out, ")")
            }

            Expression::BinaryOp { lhs, op, rhs } => {
                write!(out, "((")?;
                lhs.write(out)?;
                write!(out, ")")?;

                write!(out, " {} (", op)?;
                rhs.write(out)?;
                write!(out, "))")
            }

            Expression::UnaryOp { operand, op, before } => {
                write!(out, "(")?;
                if *before {
                    write!(out, "{}", op)?;
                    write!(out, "{}", operand)?;
                } else {
                    write!(out, "{}", operand)?;
                    write!(out, "{}", op)?;
                }
                write!(out, ")")
            }

            Expression::While { condition, body } => {
                write!(out, "while (")?;
                condition.write(out)?;
                writeln!(out, ") {{")?;
                body.write(out)?;
                write!(out, "}}")
            }

            Expression::ForLoop { init, continue_while, after_each, body } => {
                write!(out, "for (")?;
                init.write(out)?;
                write!(out, "; ")?;
                continue_while.write(out)?;
                write!(out, "; ")?;
                after_each.write(out)?;
                write!(out, ") {{")?;
                body.write(out)?;
                write!(out, "}}")
            }

            Expression::Cast { target_type, value, kind } => {
                let cast = match kind {
                    CastKind::Static => "static_cast",
                    CastKind::Reinterpret => "reinterpret_cast",
                    CastKind::Const => "const_cast",
                };

                write!(out, "{}<{}>(", cast, target_type)?;
                value.write(out)?;
                write!(out, ")")
            }

            Expression::StringLiteral(s) => {
                write!(out, "\"{}\"", s)
            }

            Expression::SizeLiteral(size) => {
                write!(out, "{}", size)
            }

            Expression::LocalDecl { ctype, name, value } => {
                write!(out, "{} {}", ctype, name)?;
                if let Some(value) = value {
                    write!(out, " = {}", value)?;
                }
                Ok(())
            }

            Expression::Member { of, name } => {
                write!(out, "{}.{}", of, Name::member(name.clone()))
            }

            Expression::Return(value) => {
                write!(out, "return {}", value)
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.write(f)
    }
}