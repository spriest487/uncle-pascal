use std::{
    rc::Rc,
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
    Variable,
};
use semantic::{
    self,
    BindingKind,
    SemanticContext,
};
use node::{
    self,
    Context,
    Identifier,
    ExpressionValue,
    ConstExpression,
    FunctionCall,
};
use types::Type;
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

    pub fn unary_op(op: impl ToString, operand: impl Into<Self>, before: bool) -> Self {
        Expression::UnaryOp {
            op: op.to_string(),
            operand: Box::new(operand.into()),
            before,
        }
    }

    pub fn binary_op(lhs: impl Into<Self>, op: impl ToString, rhs: impl Into<Self>) -> Self {
        Expression::BinaryOp {
            lhs: Box::new(lhs.into()),
            op: op.to_string(),
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
                      name: impl ToString,
                      value: Option<Self>) -> Self {
        Expression::LocalDecl {
            ctype: ctype.into(),
            name: Name::local(name),
            value: value.map(|val| Box::new(val)),
        }
    }

    pub fn member(of: impl Into<Self>, name: impl ToString) -> Self {
        Expression::Member {
            of: Box::new(of.into()),
            name: name.to_string(),
        }
    }

    pub fn return_value(value: impl Into<Self>) -> Self {
        Expression::Return(Box::new(value.into()))
    }

    pub fn translate_statement(stmt: &semantic::Expression,
                               unit: &mut TranslationUnit)
                               -> TranslationResult<Vec<Self>> {
        if let ExpressionValue::Block(block) = &stmt.value {
            let block = Block::translate(block, None, unit)?;
            return Ok(vec![Expression::Block(block)]);
        }

        let mut bindings = Vec::new();

        let mut lines = Vec::new();

        /*
            if the statement is a let binding, we turn it into a local variable decl plus an
            assignment to that variable first of all. this is because we might create a block around
            the whole statement to manage RC variable names, and so we need to make sure the
            let-bound name exists outside that scope
        */

        let stmt_after_let = match stmt.clone() {
            semantic::Expression { value: ExpressionValue::LetBinding(binding), context } => {
                let binding_type: Type = binding.value.expr_type()
                    .expect("let binding target must be a valid type")
                    .expect("let binding type must not be None");

                // create the let-bound local var name
                let local_var = Variable {
                    name: Name::local(&binding.name),
                    ctype: CType::translate(&binding_type, binding.value.scope()),
                    default_value: None,
                    array_size: None,
                };
                lines.push(local_var.decl_statement());

                let binding_id = Identifier::from(&binding.name);

                /* the scope of the assignment needs the let-bound variable added to it,
                because a let binding doesn't have itself in scope */
                let context = semantic::SemanticContext::new(
                    context.token().clone(),
                    Rc::new(stmt.scope().clone()
                        .with_binding(&binding.name, binding_type, BindingKind::Uninitialized)),
                );
                let binding_id_expr = semantic::Expression::identifier(binding_id, context.clone());

                let binary_op = semantic::Expression::binary_op(binding_id_expr,
                                                                operators::Assignment,
                                                                *binding.value,
                                                                context);
                binary_op
            }

            stmt @ _ => stmt,
        };

        /* bind the results of any function calls which return RC objects to temporary bindings so we
         can release them later. nothing but function calls returning RC objects can leak an RC,
         because local vars and func args are already managed elsewhere */
        let stmt_simplified = node::transform_expressions(stmt_after_let, &mut |subexpr: semantic::Expression| {
            // call expression...
            let call_func = match subexpr.value.clone() {
                ExpressionValue::FunctionCall(FunctionCall::Function { target, .. }) => {
                    target.function_type()
                        .expect("called function must have a valid type")
                        .expect("called function target must be of function type")
                }
                ExpressionValue::FunctionCall(FunctionCall::Method { interface_id, func_name, for_type, .. }) => {
                    // todo: getting the sig for an interface call is duplicated in call_type
                    match for_type {
                        Type::AnyImplementation(_) => {
                            let (_, interface) = subexpr.scope().get_interface(&interface_id).unwrap();
                            interface.decl.methods.get(&func_name).cloned().unwrap()
                        }

                        _ => {
                            let (_, func) = subexpr.scope().get_interface_impl(&for_type, &interface_id, &func_name)
                                .expect("interface call target must exist");

                            func.signature()
                        }
                    }
                }

                _ => return subexpr,
            };


            // returning an instance of an rc class..
            if !call_func.return_type.as_ref().map(|t| t.is_ref_counted()).unwrap_or(false) {
                return subexpr;
            }

            let name = format!("rc_{}", bindings.len());

            /* the temp binding needs to be added to scope in case anything typechecks again after
            this point*/
            let binding_scope = {
                let decl_type = call_func.return_type.as_ref().cloned().unwrap();

                subexpr.scope().clone()
                    .with_binding(&name, decl_type, BindingKind::Internal)
            };

            let binding_context = SemanticContext::new(subexpr.context.token().clone(), binding_scope);

            /* construct a new expression referring to the temp binding instead of the original
            expression by its name */
            let binding_expr = semantic::Expression::identifier(
                Identifier::from(&name),
                binding_context
            );

            /* store the original expression value for later when we write out the temp bindings */
            bindings.push(subexpr);
            binding_expr
        });

        /*
            translate the main statement into one or more c statements - a simple expression with no
            rc to handle will translate to one c statement, otherwise this may include rc statements
            to release the old value and retain the new one
        */
        let translated_exprs = match &stmt_simplified.value {
            /* if assigning a new value to an rc variable, release the old value first */
            ExpressionValue::BinaryOperator { lhs, op, .. } => {
                let mut translated_exprs = Vec::new();

                let lhs_type = lhs.expr_type().unwrap().unwrap();

                let is_rc_assignment = lhs_type.is_ref_counted() && *op == operators::Assignment;

                let mut lhs_expr = Self::translate_expression(lhs, unit)?;

                /*
                    release the old value of the variable, if it's initialized
                 */
                if is_rc_assignment {
                    /* if it's an assignment to a variable which is already initialized */
                    if let ExpressionValue::Identifier(name) = &lhs.value {
                        if lhs.scope().get_symbol(name).unwrap().initialized() {
                            // if (lhs) release(lhs)
                            translated_exprs.push(Expression::if_then(
                                lhs_expr.clone(),
                                rc_release(lhs_expr.clone()),
                            ));
                        }
                    }
                }

                // do the thing
                translated_exprs.push(Expression::translate_expression(&stmt_simplified, unit)?);

                if is_rc_assignment {
                    /* retain rc variables: if it came from a function call, it'll be released once
                    (from the code above which releases all rc results from functions), and if it's an
                    existing value we now have two references to it */
                    translated_exprs.push(rc_retain(lhs_expr.clone()));
                }

                translated_exprs
            }

            _ => {
                vec![Self::translate_expression(&stmt_simplified, unit)?]
            }
        };

        lines.extend(if bindings.len() > 0 {
            //write a block around this statement for the scope of the temp bindings
            let mut bindings_block = Vec::new();

            // bind RC temporaries to local names...
            for (tmp_id, tmp_val) in bindings.iter().enumerate() {
                let tmp_type = tmp_val.expr_type()
                    .expect("temporary rc values should always be valid types")
                    .expect("temporary rc values should never have no type");
                let val_c_type = CType::translate(&tmp_type, tmp_val.scope());

                let tmp_name = Name::local_internal(format!("rc_{}", tmp_id));

                let tmp_val_expr = Expression::translate_expression(tmp_val, unit)?;
                let tmp_var = Variable {
                    name: tmp_name.clone(),
                    ctype: val_c_type,
                    default_value: None,
                    array_size: None,
                };

                bindings_block.push(tmp_var.decl_statement());
                bindings_block.push(Expression::binary_op(tmp_name, "=", tmp_val_expr));
            }

            // ...the original exprs in the middle...
            bindings_block.extend(translated_exprs);

            // ...then release temp bindings and close the block
            for (tmp_id, _tmp_val) in bindings.iter().enumerate() {
                let tmp_name = Name::local_internal(format!("rc_{}", tmp_id));
                bindings_block.push(rc_release(tmp_name));
            }

            vec![Expression::Block(Block::new(bindings_block))]
        } else {
            translated_exprs
        });

        Ok(lines)
    }

    pub fn translate_expression(expr: &semantic::Expression,
                                unit: &mut TranslationUnit)
                                -> TranslationResult<Self> {
        match &expr.value {
            ExpressionValue::BinaryOperator { lhs, op, rhs } => {
                let string_type = Some(Type::Class(Identifier::from("System.String")));
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
                                    func_name,
                                );

                                Expression::Name(call_name)
                            }
                        }
                    }
                };

                Ok(Expression::function_call(target, args))
            }

            ExpressionValue::TypeCast { target_type, from_value } => {
                let target_ctype = CType::translate(target_type, expr.scope());
                let from_value = Expression::translate_expression(from_value, unit)?;

                Ok(Expression::cast(target_ctype, from_value, CastKind::Static))
            }

            ExpressionValue::LetBinding(binding) => {
                let value_type = binding.value.expr_type().unwrap().unwrap();
                let value = Expression::translate_expression(binding.value.as_ref(), unit)?;

                Ok(Expression::local_decl(
                    CType::translate(&value_type, expr.scope()),
                    binding.name.clone(),
                    Some(value)
                ))
            }

            ExpressionValue::Constant(const_expr) => {
                match const_expr {
                    ConstExpression::Integer(i) => {
                        let int_type = const_expr.value_type();
                        let cast_target = CType::translate(&int_type, expr.scope());

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
                        let enum_name = Name::user_type(&e.enumeration);
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
                        let float_type = const_expr.value_type();

                        let cast_target = CType::translate(&float_type, expr.scope());
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
                        match *val {
                            true => Ok(Expression::True),
                            false => Ok(Expression::False),
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
                        Expression::Name(Name::local(&binding.name))
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
                    => Ok(Expression::Name(Name::local(&scoped_sym.name().name))),

                    | BindingKind::Global
                    | BindingKind::Function
                    => Ok(Expression::Name(Name::user_symbol(&scoped_sym.name()))),

                    | BindingKind::Internal
                    => Ok(Expression::Name(Name::local_internal(&scoped_sym.name())))
                }
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

            ExpressionValue::Member { of, name } => {
                //panic!("of: {:?}, name: {}", of, name);
                let mut of_type: Type = of.expr_type()
                    .unwrap()
                    .expect("target of member expression must exist");

                /* accessing a member through a pointer automatically dereferences the pointer */
                let mut of_expr = Expression::translate_expression(of, unit)?;
                loop {
                    match of_type {
                        Type::Pointer(of_target) => {
                            of_expr = of_expr.deref();
                            of_type = *of_target;
                        }

                        _ => break,
                    }
                }

                /* ...but pascal classes in the C++ backend have one extra level of indirection */
                if of_type.is_class() {
                    of_expr = of_expr.deref();
                };

                Ok(Expression::member(of_expr, &name))
            }

            ExpressionValue::Block(block) => {
                let block = Block::translate(block, None, unit)?;
                Ok(Expression::Block(block))
            }

            ExpressionValue::ObjectConstructor(obj) => {
                let (obj_id, obj_decl) = match obj.object_type.as_ref() {
                    | Some(Type::Class(id)) => expr.scope().get_class(id).unwrap(),
                    | Some(Type::Record(id)) => expr.scope().get_record(id).unwrap(),
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

                if let &Some(ref else_expr) = else_branch {
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
                write!(out, "{}.{}", of, Name::member(name))
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