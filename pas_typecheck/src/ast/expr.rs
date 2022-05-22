pub use crate::ast::call::{
    typecheck_call, Call, FunctionCall, Invocation, MethodCall, VariantCtorCall,
};
use crate::{ast::{
    cast::typecheck_cast_expr, const_eval::ConstEval, match_block::MatchExpr, typecheck_bin_op,
    typecheck_block, typecheck_case_expr, typecheck_collection_ctor, typecheck_exit,
    typecheck_func_expr, typecheck_if_cond_expr, typecheck_match_expr, typecheck_object_ctor,
    typecheck_raise, typecheck_unary_op, VarBinding,
}, string_type, ty::FunctionParamSig, typecheck_type, Context, Decl, FunctionAnnotation, Primitive, ScopeMemberRef, Type, TypeAnnotation, TypecheckError, TypecheckResult, TypedValueAnnotation, ValueKind, NameError};
use pas_common::span::*;
use pas_syn::ast::FunctionParamMod;
use pas_syn::{ast, IdentPath, IntConstant};
use crate::ast::{Block, CaseExpr, CaseStmt, IfCond, MatchStmt, Stmt};

pub type Expr = ast::Expr<TypeAnnotation>;
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
    let span = expr_node.annotation().clone();

    match expr_node {
        ast::Expr::Literal(ast::Literal::String(s), _) => {
            let binding = ValueKind::Immutable;
            let annotation = TypedValueAnnotation {
                ty: string_type(ctx)?,
                value_kind: binding,
                span: span.clone(),
                decl: None,
            }
            .into();

            Ok(ast::Expr::Literal(
                ast::Literal::String(s.clone()),
                annotation,
            ))
        },

        ast::Expr::Literal(ast::Literal::Boolean(b), _) => {
            let annotation = TypedValueAnnotation {
                ty: Type::Primitive(Primitive::Boolean),
                value_kind: ValueKind::Immutable,
                span,
                decl: None,
            }
            .into();

            Ok(ast::Expr::Literal(ast::Literal::Boolean(*b), annotation))
        },

        ast::Expr::Literal(ast::Literal::Integer(i), _) => {
            typecheck_literal_int(i, expect_ty, span)
        },

        ast::Expr::Literal(ast::Literal::Real(x), _) => {
            let ty = if x.as_f32().is_some() {
                Type::from(Primitive::Real32)
            } else {
                unimplemented!("real literal outside range of f32")
            };

            let annotation = TypedValueAnnotation {
                ty,
                value_kind: ValueKind::Immutable,
                span,
                decl: None,
            }
            .into();

            Ok(ast::Expr::Literal(
                ast::Literal::Real(x.clone()),
                annotation,
            ))
        },

        ast::Expr::Literal(ast::Literal::Nil, _) => {
            let ty = match expect_ty {
                ptr @ Type::Pointer(..) => ptr.clone(),
                _ => Type::Nil,
            };

            let annotation = TypedValueAnnotation {
                ty,
                value_kind: ValueKind::Temporary,
                span,
                decl: None,
            }
            .into();
            Ok(ast::Expr::Literal(ast::Literal::Nil, annotation))
        },

        ast::Expr::Literal(ast::Literal::SizeOf(size_of_ty), span) => {
            let ty = typecheck_type(&size_of_ty, ctx)?;

            Ok(Expr::Literal(
                Literal::SizeOf(Box::new(ty)),
                TypedValueAnnotation {
                    ty: Type::Primitive(Primitive::Int32),
                    span: span.clone(),
                    decl: None,
                    value_kind: ValueKind::Temporary,
                }
                .into(),
            ))
        },

        ast::Expr::Ident(ident, _) => match ctx.find_name(&ident) {
            // const values from any scope can be transformed directly into literals
            Some(ScopeMemberRef::Decl {
                value: Decl::Const { ty, val, .. },
                key,
                ..
            }) => Ok(ast::Expr::Literal(
                val.clone(),
                TypedValueAnnotation {
                    ty: ty.clone(),
                    decl: Some(key.clone()),
                    span: expr_node.span().clone(),
                    value_kind: ValueKind::Immutable,
                }
                .into(),
            )),

            Some(member) => {
                let annotation = member_annotation(member, ident.span().clone(), ctx);

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

            _ => {
                let not_found_ident = ident.clone().into();
                Err(TypecheckError::NameError {
                    err: NameError::NotFound {
                        ident: not_found_ident,
                    },
                    span: expr_node.span().clone(),
                })
            },
        },

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

    let annotation = TypedValueAnnotation {
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

pub fn member_annotation(member: ScopeMemberRef, span: Span, ctx: &Context) -> TypeAnnotation {
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
        } => TypedValueAnnotation {
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

            FunctionAnnotation {
                span,
                ns: IdentPath::from_parts(parent_path.keys().cloned()),
                name: key.clone(),
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
        } => TypedValueAnnotation {
            ty: ty.clone(),
            decl: Some(key.clone()),
            span,
            value_kind: ValueKind::Immutable,
        }
        .into(),

        ScopeMemberRef::Decl {
            value: Decl::Type { ty, .. },
            ..
        } => TypeAnnotation::Type(ty.clone(), span),

        ScopeMemberRef::Decl {
            value: Decl::Namespace(path),
            ..
        } => TypeAnnotation::Namespace(path.clone(), span),
        ScopeMemberRef::Scope { path } => {
            TypeAnnotation::Namespace(IdentPath::from_parts(path.keys().cloned()), span)
        },
    }
}

pub fn expect_stmt_initialized(stmt: &Stmt, ctx: &Context) -> TypecheckResult<()> {
    match stmt {
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

pub fn expect_expr_initialized(expr: &Expr, ctx: &Context) -> TypecheckResult<()> {
    match expr {
        ast::Expr::Ident(ident, ..) => match expr.annotation().value_kind() {
            Some(ValueKind::Uninitialized) => {
                let decl_ident = ctx.find_decl(ident).unwrap_or(ident);
                Err(TypecheckError::NotInitialized {
                    ident: decl_ident.clone(),
                    usage: ident.span().clone(),
                })
            },

            _ => Ok(()),
        },

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
    }
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
