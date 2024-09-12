pub use crate::ast::call::{
    typecheck_call, Call, Invocation,
};
use crate::{
    ast::{
        cast::typecheck_cast_expr, const_eval::ConstEval, match_block::MatchExpr, typecheck_bin_op,
        typecheck_block, typecheck_case_expr, typecheck_collection_ctor, typecheck_exit,
        typecheck_func_expr, typecheck_if_cond_expr, typecheck_match_expr, typecheck_object_ctor,
        typecheck_raise, typecheck_unary_op, VarBinding,
    },
    ast::{Block, CaseExpr, CaseStmt, IfCond, MatchStmt, Stmt},
    string_type,
    ty::FunctionParamSig,
    typecheck_type, Context, Decl, FunctionTyped, NameError, Primitive, ScopeMemberRef, Type,
    Typed, TypecheckError, TypecheckResult, TypedValue, ValueKind,
};
use pas_common::span::*;
use pas_syn::{ast, ast::FunctionParamMod, Ident, IdentPath, IntConstant};
use crate::ast::FunctionCallNoArgs;

pub type Expr = ast::Expr<Typed>;
pub type Literal = ast::Literal<Type>;

pub fn const_eval_string(expr: &Expr, ctx: &Context) -> TypecheckResult<String> {
    match expr.const_eval(ctx) {
        Some(Literal::String(src_str)) => Ok((*src_str).clone()),

        _ => Err(TypecheckError::InvalidConstExpr {
            expr: Box::new(expr.clone()),
        }),
    }
}

pub fn const_eval_integer(expr: &Expr, ctx: &Context) -> TypecheckResult<IntConstant> {
    match expr.const_eval(ctx) {
        Some(Literal::Integer(int_const)) => Ok(int_const),

        _ => Err(TypecheckError::InvalidConstExpr {
            expr: Box::new(expr.clone()),
        }),
    }
}

pub fn typecheck_expr(
    expr_node: &ast::Expr<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Expr> {
    match expr_node {
        ast::Expr::Literal(lit, span) => typecheck_literal(lit, expect_ty, span, ctx),

        ast::Expr::Ident(ident, span) => typecheck_ident(ident, expect_ty, span, ctx),

        ast::Expr::BinOp(bin_op) => typecheck_bin_op(bin_op, expect_ty, ctx),

        ast::Expr::UnaryOp(unary_op) => {
            let unary_op = typecheck_unary_op(unary_op, expect_ty, ctx)?;
            Ok(ast::Expr::from(unary_op))
        },

        ast::Expr::Call(call) => {
            let expr = match typecheck_call(call, expect_ty, ctx)? {
                Invocation::Call(call) => ast::Expr::from(*call),
                Invocation::Ctor(ctor) => ast::Expr::from(*ctor),
            };
            Ok(expr)
        },

        ast::Expr::ObjectCtor(ctor) => {
            let span = ctor.annotation.span().clone();
            let ctor = typecheck_object_ctor(ctor, span, expect_ty, ctx)?;
            Ok(ast::Expr::from(ctor))
        },

        ast::Expr::CollectionCtor(ctor) => {
            let ctor = typecheck_collection_ctor(ctor, expect_ty, ctx)?;
            Ok(ast::Expr::from(ctor))
        },

        ast::Expr::IfCond(if_cond) => {
            let if_cond = typecheck_if_cond_expr(if_cond, expect_ty, ctx)?;
            Ok(ast::Expr::from(if_cond))
        },

        ast::Expr::Block(block) => {
            let block = typecheck_block(block, expect_ty, ctx)?;
            Ok(ast::Expr::from(block))
        },

        ast::Expr::Raise(raise) => {
            let raise = typecheck_raise(raise, expect_ty, ctx)?;
            Ok(ast::Expr::from(raise))
        },

        ast::Expr::Case(case) => {
            let case = typecheck_case_expr(case, expect_ty, ctx)?;
            Ok(ast::Expr::from(case))
        },

        ast::Expr::Match(match_expr) => {
            let match_expr = typecheck_match_expr(match_expr, expect_ty, ctx)?;
            Ok(ast::Expr::from(match_expr))
        },

        ast::Expr::Exit(exit) => {
            let exit = typecheck_exit(exit, expect_ty, ctx)?;
            Ok(ast::Expr::from(exit))
        },

        ast::Expr::Cast(cast) => {
            let cast = typecheck_cast_expr(cast, ctx)?;
            Ok(ast::Expr::from(cast))
        },

        ast::Expr::AnonymousFunction(def) => {
            let anon_func = typecheck_func_expr(def, expect_ty, ctx)?;
            Ok(ast::Expr::from(anon_func))
        },
    }
}

fn typecheck_literal(
    lit: &ast::Literal<ast::TypeName>,
    expect_ty: &Type,
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<Expr> {
    match lit {
        ast::Literal::String(s) => {
            let binding = ValueKind::Immutable;
            let annotation = TypedValue {
                ty: string_type(ctx)?,
                value_kind: binding,
                span: span.clone(),
                decl: None,
            }
            .into();

            Ok(Expr::Literal(Literal::String(s.clone()), annotation))
        },

        ast::Literal::Boolean(b) => {
            let annotation = TypedValue {
                ty: Type::Primitive(Primitive::Boolean),
                value_kind: ValueKind::Immutable,
                span: span.clone(),
                decl: None,
            }
            .into();

            Ok(Expr::Literal(Literal::Boolean(*b), annotation))
        },

        ast::Literal::Integer(i) => typecheck_literal_int(i, expect_ty, span.clone()),

        ast::Literal::Real(x) => {
            let ty = if x.as_f32().is_some() {
                Type::from(Primitive::Real32)
            } else {
                unimplemented!("real literal outside range of f32")
            };

            let annotation = TypedValue {
                ty,
                value_kind: ValueKind::Immutable,
                span: span.clone(),
                decl: None,
            }
            .into();

            Ok(ast::Expr::Literal(
                ast::Literal::Real(x.clone()),
                annotation,
            ))
        },

        ast::Literal::Nil => {
            let ty = match expect_ty {
                ptr @ Type::Pointer(..) => ptr.clone(),
                _ => Type::Nil,
            };

            let annotation = TypedValue {
                ty,
                value_kind: ValueKind::Temporary,
                span: span.clone(),
                decl: None,
            };

            Ok(ast::Expr::Literal(Literal::Nil, annotation.into()))
        },

        ast::Literal::SizeOf(size_of_ty) => {
            let ty = typecheck_type(&size_of_ty, ctx)?;
            let annotation = TypedValue {
                ty: Type::Primitive(Primitive::Int32),
                span: span.clone(),
                decl: None,
                value_kind: ValueKind::Temporary,
            };

            Ok(Expr::Literal(
                Literal::SizeOf(Box::new(ty)),
                annotation.into(),
            ))
        },
    }
}

fn typecheck_literal_int(i: &IntConstant, expect_ty: &Type, span: Span) -> TypecheckResult<Expr> {
    let ty = match expect_ty {
        Type::Primitive(Primitive::UInt8) => {
            try_map_primitive_int(i, Primitive::UInt8, IntConstant::as_u8)
        },
        Type::Primitive(Primitive::Int8) => {
            try_map_primitive_int(i, Primitive::Int8, IntConstant::as_i8)
        },
        Type::Primitive(Primitive::Int16) => {
            try_map_primitive_int(i, Primitive::Int16, IntConstant::as_i16)
        },
        Type::Primitive(Primitive::UInt16) => {
            try_map_primitive_int(i, Primitive::UInt16, IntConstant::as_u16)
        },
        Type::Primitive(Primitive::Int32) => {
            try_map_primitive_int(i, Primitive::Int32, IntConstant::as_i32)
        },
        Type::Primitive(Primitive::UInt32) => {
            try_map_primitive_int(i, Primitive::UInt32, IntConstant::as_u32)
        },
        Type::Primitive(Primitive::Int64) => {
            try_map_primitive_int(i, Primitive::Int64, IntConstant::as_i64)
        },
        Type::Primitive(Primitive::UInt64) => {
            try_map_primitive_int(i, Primitive::UInt64, IntConstant::as_u64)
        },
        Type::Primitive(Primitive::NativeInt) => {
            try_map_primitive_int(i, Primitive::NativeInt, IntConstant::as_isize)
        },
        Type::Primitive(Primitive::NativeUInt) => {
            try_map_primitive_int(i, Primitive::NativeUInt, IntConstant::as_usize)
        },

        Type::Primitive(Primitive::Real32) => {
            try_map_primitive_int(i, Primitive::Real32, IntConstant::as_f32)
        },

        _ => match i.as_i32() {
            Some(_) => Type::from(Primitive::Int32),
            None => unimplemented!("integer literals outside range of i32"),
        },
    };

    let annotation = TypedValue {
        ty,
        value_kind: ValueKind::Immutable,
        span,
        decl: None,
    }
    .into();

    Ok(ast::Expr::Literal(ast::Literal::Integer(*i), annotation))
}

fn try_map_primitive_int<F, T>(i: &IntConstant, primitive_ty: Primitive, f: F) -> Type
where
    F: Fn(&IntConstant) -> Option<T>,
{
    match f(&i) {
        Some(..) => Type::Primitive(primitive_ty),
        None => Type::Primitive(Primitive::Int32),
    }
}

fn typecheck_ident(
    ident: &Ident,
    expect_ty: &Type,
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<Expr> {
    let decl = match ctx.find_name(ident) {
        Some(decl) => decl,
        None => {
            let not_found_ident = ident.clone().into();
            return Err(TypecheckError::NameError {
                err: NameError::NotFound {
                    ident: not_found_ident,
                },
                span: span.clone(),
            });
        },
    };

    match decl {
        // const values from any scope can be transformed directly into literals
        ScopeMemberRef::Decl {
            value: Decl::Const { ty, val, .. },
            key,
            ..
        } => {
            let annotation = TypedValue {
                ty: ty.clone(),
                decl: Some(key.clone()),
                span: span.clone(),
                value_kind: ValueKind::Temporary,
            };
            Ok(ast::Expr::Literal(val.clone(), annotation.into()))
        },

        // an ident referencing a function with no parameters is interpreted as a call to
        // that function, but we wrap it in the NoArgs type so it can be unwrapped if this
        // expression appears in an actual call node
        ScopeMemberRef::Decl {
            value: Decl::Function { sig, .. },
            parent_path,
            ..
        } if sig.should_call_noargs_in_expr(expect_ty, &Type::Nothing) => {
            let annotation = TypedValue {
                decl: None,
                span: span.clone(),
                ty: sig.return_ty.clone(),
                value_kind: ValueKind::Temporary,
            };

            let func_annotation = FunctionTyped {
                ident: parent_path.to_namespace().child(ident.clone()),
                sig: sig.clone(),
                span: span.clone(),
                type_args: None,
            };

            let call = ast::Call::FunctionNoArgs(FunctionCallNoArgs {
                target: ast::Expr::Ident(ident.clone(), func_annotation.into()),
                annotation: annotation.into(),
            });

            Ok(Expr::from(call))
        },

        member => {
            let annotation = member_annotation(member, span.clone(), ctx);

            if let Some(decl_name) = annotation.decl() {
                if let Some(decl_scope) = ctx.get_decl_scope(decl_name) {
                    let decl_scope_id = decl_scope.id();
                    if let Some(closure_scope_id) = ctx.get_closure_scope().map(|s| s.id()) {
                        if decl_scope_id < closure_scope_id {
                            ctx.add_closure_capture(decl_name, &annotation.ty());
                        }
                    }
                }
            }

            Ok(ast::Expr::Ident(ident.clone(), annotation))
        },
    }
}

pub fn member_annotation(member: ScopeMemberRef, span: Span, ctx: &Context) -> Typed {
    match member {
        ScopeMemberRef::Decl {
            value: Decl::Alias(aliased),
            ..
        } => {
            let alias_ref = ctx
                .find_path(aliased)
                .unwrap_or_else(|| panic!("invalid alias to {}", aliased));

            member_annotation(alias_ref, span, ctx)
        },

        ScopeMemberRef::Decl {
            value: Decl::BoundValue(binding),
            ..
        } => TypedValue {
            span,
            ty: binding.ty.clone(),
            value_kind: binding.kind,
            decl: binding.def.clone(),
        }
        .into(),

        ScopeMemberRef::Decl {
            value: Decl::Function { sig, .. },
            ref parent_path,
            key,
        } => {
            if parent_path.as_slice().is_empty() {
                panic!("empty path for decl {}", key);
            }

            FunctionTyped {
                span,
                ident: parent_path.to_namespace().child(key.clone()),
                sig: sig.clone(),
                // the named version of the function never has type args, the caller will have
                // to specialize the expr to add some
                type_args: None,
            }
            .into()
        },

        ScopeMemberRef::Decl {
            value: Decl::Const { ty, .. },
            key,
            ..
        } => TypedValue {
            ty: ty.clone(),
            decl: Some(key.clone()),
            span,
            value_kind: ValueKind::Immutable,
        }
        .into(),

        ScopeMemberRef::Decl {
            value: Decl::Type { ty, .. },
            ..
        } => Typed::Type(ty.clone(), span),

        ScopeMemberRef::Decl {
            value: Decl::Namespace(path),
            ..
        } => Typed::Namespace(path.clone(), span),
        ScopeMemberRef::Scope { path } => {
            Typed::Namespace(IdentPath::from_parts(path.keys().cloned()), span)
        },
    }
}

pub fn expect_stmt_initialized(stmt: &Stmt, ctx: &Context) -> TypecheckResult<()> {
    match stmt {
        ast::Stmt::Ident(ident, annotation) => expect_ident_initialized(ident, annotation, ctx),

        ast::Stmt::Call(call) => expect_call_initialized(call, ctx),

        ast::Stmt::If(if_stmt) => expect_if_stmt_initialized(if_stmt, ctx),

        ast::Stmt::Block(block) => expect_block_initialized(block, ctx),

        ast::Stmt::LocalBinding(binding) => expect_binding_initialized(binding, ctx),

        ast::Stmt::ForLoop(for_loop) => {
            match &for_loop.init {
                ast::ForLoopInit::Binding(init_binding) => {
                    expect_binding_initialized(init_binding, ctx)?
                },
                ast::ForLoopInit::Assignment { counter: _, value } => {
                    // only the initial value needs to be initialized - we (re)initialize the counter in the loop
                    expect_expr_initialized(value, ctx)?;
                },
            }
            expect_expr_initialized(&for_loop.to_expr, ctx)?;
            expect_stmt_initialized(&for_loop.body, ctx)?;
            Ok(())
        },

        ast::Stmt::WhileLoop(while_loop) => {
            expect_expr_initialized(&while_loop.condition, ctx)?;
            expect_stmt_initialized(&while_loop.body, ctx)?;
            Ok(())
        },

        ast::Stmt::Exit(exit) => match exit.as_ref() {
            ast::Exit::WithoutValue(_) => Ok(()),
            ast::Exit::WithValue(exit_val, ..) => expect_expr_initialized(exit_val, ctx),
        },

        ast::Stmt::Assignment(assignment) => expect_expr_initialized(&assignment.rhs, ctx),

        ast::Stmt::CompoundAssignment(assignment) => {
            expect_expr_initialized(&assignment.lhs, ctx)?;
            expect_expr_initialized(&assignment.rhs, ctx)?;
            Ok(())
        },

        ast::Stmt::Break(..) | ast::Stmt::Continue(..) => Ok(()),

        ast::Stmt::Raise(raise) => expect_expr_initialized(&raise.value, ctx),

        ast::Stmt::Case(case) => expect_case_stmt_initialized(case, ctx),

        ast::Stmt::Match(match_stmt) => expect_match_stmt_initialized(match_stmt, ctx),
    }
}

fn expect_ident_initialized(
    ident: &Ident,
    annotation: &Typed,
    ctx: &Context,
) -> TypecheckResult<()> {
    match annotation.value_kind() {
        Some(ValueKind::Uninitialized) => {
            let decl_ident = ctx.find_decl(ident).unwrap_or(ident);
            Err(TypecheckError::NotInitialized {
                ident: decl_ident.clone(),
                usage: ident.span().clone(),
            })
        },

        _ => Ok(()),
    }
}

pub fn expect_expr_initialized(expr: &Expr, ctx: &Context) -> TypecheckResult<()> {
    match expr {
        ast::Expr::Ident(ident, annotation) => expect_ident_initialized(ident, annotation, ctx),

        ast::Expr::Literal(..) => Ok(()),

        ast::Expr::Block(block) => expect_block_initialized(block, ctx),

        ast::Expr::IfCond(cond) => expect_if_expr_initialized(cond, ctx),

        ast::Expr::ObjectCtor(ctor) => {
            for member in &ctor.args.members {
                expect_expr_initialized(&member.value, ctx)?;
            }
            Ok(())
        },

        ast::Expr::CollectionCtor(ctor) => {
            for el in &ctor.elements {
                expect_expr_initialized(&el.value, ctx)?;
            }
            Ok(())
        },

        ast::Expr::Call(call) => expect_call_initialized(call, ctx),

        ast::Expr::BinOp(bin_op) => {
            expect_expr_initialized(&bin_op.lhs, ctx)?;
            expect_expr_initialized(&bin_op.rhs, ctx)?;
            Ok(())
        },

        ast::Expr::UnaryOp(unary_op) => expect_expr_initialized(&unary_op.operand, ctx),

        ast::Expr::Raise(raise) => expect_expr_initialized(&raise.value, ctx),

        ast::Expr::Case(case) => expect_case_expr_initialized(&case, ctx),

        ast::Expr::Match(match_expr) => expect_match_expr_initialized(&match_expr, ctx),

        ast::Expr::Exit(exit) => match exit.as_ref() {
            ast::Exit::WithValue(exit_val, _) => expect_expr_initialized(exit_val, ctx),
            ast::Exit::WithoutValue(_) => Ok(()),
        },

        ast::Expr::Cast(cast) => expect_expr_initialized(&cast.expr, ctx),

        ast::Expr::AnonymousFunction(_) => Ok(()),
    }?;

    Ok(())
}

fn expect_binding_initialized(binding: &VarBinding, ctx: &Context) -> TypecheckResult<()> {
    if let Some(init_val) = &binding.val {
        expect_expr_initialized(init_val, ctx)?;
    }
    Ok(())
}

fn expect_args_initialized(
    params: &[FunctionParamSig],
    args: &[Expr],
    ctx: &Context,
) -> TypecheckResult<()> {
    assert_eq!(
        params.len(),
        args.len(),
        "function call with wrong number of args shouldn't pass type checking. got:\n{}\nexpected:\n{}",
        args.iter().map(Expr::to_string).collect::<Vec<_>>().join("; "),
        params.iter().map(|param| param.ty.to_string()).collect::<Vec<_>>().join("; "),
    );

    for (arg, param) in args.iter().zip(params.iter()) {
        if param.modifier != Some(FunctionParamMod::Out) {
            expect_expr_initialized(arg, ctx)?;
        }
    }

    Ok(())
}

fn expect_call_initialized(call: &Call, ctx: &Context) -> TypecheckResult<()> {
    match call {
        ast::Call::FunctionNoArgs(call) => {
            expect_expr_initialized(&call.target, ctx)?;
        }

        ast::Call::MethodNoArgs(call) => {
            expect_expr_initialized(&call.target, ctx)?;
            expect_expr_initialized(&call.self_arg, ctx)?;
        }

        ast::Call::Function(func_call) => {
            expect_expr_initialized(&func_call.target, ctx)?;

            let target_ty = func_call.target.annotation().ty();

            let params = match target_ty.as_ref() {
                Type::Function(sig) => &sig.params,
                _ => panic!(
                    "function call with wrong target type shouldn't pass type checking (was: {:#?})",
                    func_call.target.annotation().ty()
                ),
            };
            expect_args_initialized(params, &func_call.args, ctx)?;
        },

        ast::Call::Method(method_call) => {
            let params = match &method_call.func_type {
                Type::Function(sig) => &sig.params,
                _ => panic!(
                    "function call with wrong target type shouldn't pass type checking (was: {:#?})",
                    method_call.func_type,
                ),
            };

            expect_args_initialized(params, &method_call.args, ctx)?;
        },

        ast::Call::VariantCtor(ctor_call) => {
            if let Some(arg_expr) = &ctor_call.arg {
                expect_expr_initialized(&arg_expr, ctx)?;
            }
        },
    }

    Ok(())
}

fn expect_if_expr_initialized(if_stmt: &IfCond<Expr>, ctx: &Context) -> TypecheckResult<()> {
    expect_expr_initialized(&if_stmt.cond, ctx)?;
    expect_expr_initialized(&if_stmt.then_branch, ctx)?;
    if let Some(else_branch) = &if_stmt.else_branch {
        expect_expr_initialized(else_branch, ctx)?;
    }
    Ok(())
}

fn expect_if_stmt_initialized(if_stmt: &IfCond<Stmt>, ctx: &Context) -> TypecheckResult<()> {
    expect_expr_initialized(&if_stmt.cond, ctx)?;
    expect_stmt_initialized(&if_stmt.then_branch, ctx)?;
    if let Some(else_branch) = &if_stmt.else_branch {
        expect_stmt_initialized(else_branch, ctx)?;
    }
    Ok(())
}

fn expect_block_initialized(block: &Block, ctx: &Context) -> TypecheckResult<()> {
    for stmt in &block.stmts {
        expect_stmt_initialized(stmt, ctx)?;
    }
    if let Some(output) = &block.output {
        expect_expr_initialized(output, ctx)?;
    }
    Ok(())
}

fn expect_case_stmt_initialized(case: &CaseStmt, ctx: &Context) -> TypecheckResult<()> {
    expect_expr_initialized(&case.cond_expr, ctx)?;

    for branch in &case.branches {
        expect_expr_initialized(&branch.value, ctx)?;
        expect_stmt_initialized(&branch.item, ctx)?;
    }

    if let Some(else_branch) = &case.else_branch {
        expect_stmt_initialized(else_branch, ctx)?;
    }

    Ok(())
}

fn expect_case_expr_initialized(case: &CaseExpr, ctx: &Context) -> TypecheckResult<()> {
    expect_expr_initialized(&case.cond_expr, ctx)?;

    for branch in &case.branches {
        expect_expr_initialized(&branch.value, ctx)?;
        expect_expr_initialized(&branch.item, ctx)?;
    }

    if let Some(else_branch) = &case.else_branch {
        expect_expr_initialized(else_branch, ctx)?;
    }

    Ok(())
}

fn expect_match_expr_initialized(match_expr: &MatchExpr, ctx: &Context) -> TypecheckResult<()> {
    expect_expr_initialized(&match_expr.cond_expr, ctx)?;

    for branch in &match_expr.branches {
        expect_expr_initialized(&branch.item, ctx)?;
    }

    if let Some(else_branch) = &match_expr.else_branch {
        expect_expr_initialized(else_branch, ctx)?;
    }

    Ok(())
}

fn expect_match_stmt_initialized(match_stmt: &MatchStmt, ctx: &Context) -> TypecheckResult<()> {
    expect_expr_initialized(&match_stmt.cond_expr, ctx)?;

    for branch in &match_stmt.branches {
        expect_stmt_initialized(&branch.item, ctx)?;
    }

    if let Some(else_branch) = &match_stmt.else_branch {
        expect_stmt_initialized(else_branch, ctx)?;
    }

    Ok(())
}
