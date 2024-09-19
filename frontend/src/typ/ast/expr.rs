mod init;
mod literal;

use crate::ast;
use crate::ast::FunctionCallNoArgs;
pub use crate::typ::ast::call::typecheck_call;
pub use crate::typ::ast::call::Invocation;
use crate::typ::ast::cast::typecheck_cast_expr;
use crate::typ::ast::const_eval::ConstEval;
use crate::typ::ast::typecheck_bin_op;
use crate::typ::ast::typecheck_block;
use crate::typ::ast::typecheck_case_expr;
use crate::typ::ast::typecheck_collection_ctor;
use crate::typ::ast::typecheck_exit;
use crate::typ::ast::typecheck_func_expr;
use crate::typ::ast::typecheck_if_cond_expr;
use crate::typ::ast::typecheck_match_expr;
use crate::typ::ast::typecheck_object_ctor;
use crate::typ::ast::typecheck_raise;
use crate::typ::ast::typecheck_unary_op;
use crate::typ::Context;
use crate::typ::Decl;
use crate::typ::FunctionTyped;
use crate::typ::NameError;
use crate::typ::ScopeMemberRef;
use crate::typ::Type;
use crate::typ::TypecheckError;
use crate::typ::TypecheckResult;
use crate::typ::Typed;
use crate::typ::TypedValue;
use crate::typ::ValueKind;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::IntConstant;
pub use init::*;
pub use literal::*;
use common::span::*;

pub type Expr = ast::Expr<Typed>;

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
        ast::Expr::Literal(lit, span) => literal::typecheck_literal(lit, expect_ty, span, ctx),

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
