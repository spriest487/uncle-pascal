use crate::ast::call;
use crate::ast::implicit_conversion;
use crate::ast::typecheck_expr;
use crate::ast::Expr;
use crate::Context;
use crate::FunctionParamSig;
use crate::FunctionSig;
use crate::GenericError;
use crate::GenericTarget;
use crate::GenericTypeHint;
use crate::Specializable;
use crate::Type;
use crate::TypeArgsResolver;
use crate::TypeArgsResult;
use crate::TypeParamType;
use crate::TypecheckError;
use crate::TypecheckResult;
use crate::ValueKind;
use pas_common::span::Span;
use pas_syn::ast;
use pas_syn::ast::FunctionParamMod;
use pas_syn::ast::TypeList;
use std::borrow::Cow;

pub struct SpecializedCallArgs {
    pub sig: FunctionSig,
    pub type_args: Option<TypeList<Type>>,
    pub actual_args: Vec<Expr>,
}

struct PartiallySpecializedTypeArgsList<'a> {
    items: &'a [Option<Type>],
}

impl<'a> TypeArgsResolver for PartiallySpecializedTypeArgsList<'a> {
    fn resolve(&self, param: &TypeParamType) -> Cow<Type> {
        match self.items.get(param.pos) {
            Some(Some(specialized_ty)) => Cow::Borrowed(specialized_ty),
            _ => Cow::Owned(Type::GenericParam(Box::new(param.clone()))),
        }
    }

    fn get_specialized(&self, pos: usize) -> Option<&Type> {
        self.items.get(pos)?.as_ref()
    }

    fn len(&self) -> usize {
        self.items.len()
    }
}

fn specialize_arg<ArgProducer>(
    param_ty: &Type,
    inferred_ty_args: &mut [Option<Type>],
    produce_expr: ArgProducer,
    _span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<Expr>
// (expected type, ctx) -> expr val
where
    ArgProducer: FnOnce(&Type, &mut Context) -> TypecheckResult<Expr>,
{
    let partial_args_resolver = PartiallySpecializedTypeArgsList {
        items: inferred_ty_args,
    };

    let mut expect_ty = param_ty
        .clone()
        .substitute_type_args(&partial_args_resolver);
    
    if expect_ty.is_generic_param() || expect_ty.is_unspecialized_generic() {
        // not enough info to resolve this generic type fully yet: arg expr ty drives param type
        expect_ty = Type::Nothing;
    }

    let actual_arg =  produce_expr(&expect_ty, ctx)?;
    let actual_ty = actual_arg.annotation().ty().clone();
    
    infer_from_structural_ty_args(param_ty, &actual_ty, inferred_ty_args);

    Ok(actual_arg)
}

fn infer_from_structural_ty_args(
    param_ty: &Type,
    actual_ty: &Type,
    inferred_ty_args: &mut [Option<Type>],
) {
    // in param_ty, find all the references to generic params within the type and their
    // corresponding values in the actual type, in the order they appear.
    // for example if the expected type is `array of T`, and the actual
    // arg is `array of Int32`, `T` is added to the param ty args and `Int32` is added to the 
    // actual ty args.
    // at this point, if the types are fundamentally incompatible we should bail
    let (param_ty_args, actual_ty_args) = match (param_ty, actual_ty) {
        // plain generic param can be substituted for anything
        (Type::GenericParam(..), actual) => {
            (vec![param_ty.clone()], vec![actual.clone()])
        }

        // static-length arrays can be substituted into a generic array of the same length 
        (Type::Array(param_array_ty), Type::Array(actual_array_ty)) => {
            if actual_array_ty.dim != param_array_ty.dim {
                return;
            }

            (
                vec![param_array_ty.element_ty.clone()],
                vec![actual_array_ty.element_ty.clone()],
            )
        },

        // dynarrays only need element type to match
        (Type::DynArray { element: param_el }, Type::DynArray { element: actual_el }) => {
            (vec![*param_el.clone()], vec![*actual_el.clone()])
        },

        _ => {
            // all other types: must be the exact same declaration (struct, enum etc)
            if !param_ty.same_decl_type(actual_ty) {
                return;
            }

            let param_ty_args = match param_ty.type_args() {
                TypeArgsResult::Specialized(args) => args.items.clone(),
                _ => return,
            };
            let actual_ty_args = match actual_ty.type_args() {
                TypeArgsResult::Specialized(args) => args.items.clone(),
                _ => return,
            };
            (param_ty_args, actual_ty_args)
        },
    };

    let all_ty_args = param_ty_args.iter().zip(actual_ty_args.iter());
    for (param_ty_arg, actual_ty_arg) in all_ty_args {
        match param_ty_arg {
            Type::GenericParam(param_generic) => {
                let pos = param_generic.pos;
                let inferred = &mut inferred_ty_args[pos];
                assert!(
                    inferred.is_none() || inferred.as_ref().unwrap() == actual_ty_arg, 
                    "previously inferred type for this position ({}) must either be None for the first occurrence, or the same type as this inference ({})",
                    pos,
                    actual_ty_arg
                );

                *inferred = Some(actual_ty_arg.clone());
            },

            _ => infer_from_structural_ty_args(param_ty_arg, actual_ty_arg, inferred_ty_args),
        }
    }
}

/// for a function decl, figure out the actual arg types and values from the provided
/// expressions. evaluate arguments left-to-right until we either resolve a signature that
/// matches the args, or conclude that the args don't match the call and return an error.
///
/// for example `function A[T](t: T; t2: array[2] of T)`' s actual signature can be deduced
/// from the actual type of `t`, and `t2`'s value can be checked against that.
pub fn specialize_call_args(
    decl_sig: &FunctionSig,
    args: &[ast::Expr<Span>],
    self_arg: Option<&Expr>,
    explicit_ty_args: Option<TypeList<Type>>,
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<SpecializedCallArgs> {
    if decl_sig.type_params.is_none() {
        if let Some(explicit_ty_args) = explicit_ty_args {
            return Err(TypecheckError::from_generic_err(
                GenericError::ArgsLenMismatch {
                    expected: 0,
                    actual: explicit_ty_args.len(),
                    target: GenericTarget::FunctionSig(decl_sig.clone()),
                },
                span.clone(),
            ));
        }

        // no type params
        let actual_args = call::build_args_for_params(&decl_sig.params, args, self_arg, span, ctx)?;

        return Ok(SpecializedCallArgs {
            sig: decl_sig.clone(),
            actual_args,
            type_args: None,
        });
    }

    if let Some(explicit_ty_args) = explicit_ty_args {
        // we have explicit args, don't try to infer types
        let sig = decl_sig
            .clone()
            .specialize_generic(&explicit_ty_args, ctx)
            .map_err(|err| TypecheckError::from_generic_err(err, span.clone()))?;

        let actual_args = call::build_args_for_params(&sig.params, args, self_arg, span, ctx)?;

        Ok(SpecializedCallArgs {
            sig,
            actual_args,
            type_args: Some(explicit_ty_args),
        })
    } else {
        // we haven't checked arg length matches yet because we haven't typechecked args
        let self_arg_len = if self_arg.is_some() { 1 } else { 0 };
        if args.len() + self_arg_len != decl_sig.params.len() {
            // this is an inferral error because we don't have enough information to report
            // it as an arg type mismatch
            return Err(TypecheckError::from_generic_err(
                GenericError::CannotInferArgs {
                    target: GenericTarget::FunctionSig(decl_sig.clone()),
                    hint: GenericTypeHint::Unknown,
                },
                span.clone(),
            ));
        }

        // try to infer type from args, left to right
        let mut inferred_ty_args = vec![None; decl_sig.type_params.as_ref().unwrap().len()];
        let mut actual_args = Vec::new();

        if let Some(self_arg) = self_arg.cloned() {
            let decl_self_param = &decl_sig.params[0];
            let actual_self_arg = specialize_arg(
                &decl_self_param.ty,
                &mut inferred_ty_args,
                |_expect_ty, _ctx| Ok(self_arg),
                span,
                ctx,
            )?;

            actual_args.push(actual_self_arg);
        }

        for (i, arg) in args.iter().enumerate() {
            let param_ty = &decl_sig.params[i + self_arg_len].ty;
            let actual_arg = specialize_arg(
                param_ty,
                &mut inferred_ty_args,
                |expect_ty, ctx| typecheck_expr(arg, expect_ty, ctx),
                span,
                ctx,
            )?;

            actual_args.push(actual_arg);
        }

        let inferred_ty_args =
            unwrap_inferred_args(decl_sig, inferred_ty_args, &actual_args, span)?;

        let actual_sig = decl_sig
            .clone()
            .specialize_generic(&inferred_ty_args, ctx)
            .map_err(|err| TypecheckError::from_generic_err(err, span.clone()))?;

        // eprintln!("INFERRED ARGS:\n\tdecl: {}\n\tinferred:{}\n\tfinal sig: {}", decl_sig, inferred_ty_args, actual_sig);

        Ok(SpecializedCallArgs {
            type_args: Some(inferred_ty_args),
            sig: actual_sig,
            actual_args,
        })
    }
}

/// unwrap all the type args or raise an error if any of them can't be unwrapped because
/// we haven't successfully inferred them
fn unwrap_inferred_args(
    decl_sig: &FunctionSig,
    inferred_args: Vec<Option<Type>>,
    actual_args: &[Expr],
    span: &Span,
) -> TypecheckResult<TypeList<Type>> {
    let mut items = Vec::new();

    for arg in inferred_args {
        match arg {
            Some(arg) => {
                items.push(arg);
            },

            None => {
                let arg_tys = actual_args
                    .iter()
                    .map(|a| a.annotation().ty().into_owned())
                    .collect();

                return Err(TypecheckError::from_generic_err(
                    GenericError::CannotInferArgs {
                        target: GenericTarget::FunctionSig(decl_sig.clone()),
                        hint: GenericTypeHint::ArgTypes(arg_tys),
                    },
                    span.clone(),
                ));
            },
        }
    }

    Ok(TypeList::new(items, span.clone()))
}

/// final arg validation when all arg types are known and specialized correctly, and any
/// self-args have been processed into normal arguments to match the sig
/// - insert any required implicit type conversions
/// - validate length
/// - ensure that the expressions provided for out/var refs are mutable l-values
/// - mark the names referenced in out vars as initialized
pub fn validate_args(
    args: &mut [Expr],
    params: &[FunctionParamSig],
    span: &Span,
    ctx: &mut Context,
) -> TypecheckResult<()> {
    if args.len() != params.len() {
        return Err(call::invalid_args(args.to_vec(), params, span.clone()));
    }

    for i in 0..params.len() {
        // only do implicit conversion for non-ref params
        if params[i].is_by_ref() {
            continue;
        }

        args[i] =
            implicit_conversion(args[i].clone(), &params[i].ty, ctx).map_err(|err| match err {
                TypecheckError::TypeMismatch { .. } => {
                    call::invalid_args(args.to_vec(), params, span.clone())
                },
                err => err,
            })?;
    }

    let args_and_params = args.iter().zip(params.iter());
    for (arg, param) in args_and_params {
        let (is_in_ref, is_out_ref) = match &param.modifier {
            None => (false, false),
            Some(FunctionParamMod::Out) => (false, true),
            Some(FunctionParamMod::Var) => (true, true),
        };

        if is_in_ref || is_out_ref {
            match arg.annotation().value_kind() {
                // in-refs shouldn't really allow uninitialized values here but we do init checking
                // in a separate pass
                Some(ValueKind::Mutable) | Some(ValueKind::Uninitialized) => {},
                _ => {
                    return Err(TypecheckError::NotMutable {
                        expr: Box::new(arg.clone()),
                        decl: None,
                    });
                },
            }

            if is_out_ref {
                if let Expr::Ident(ref_ident, ..) = &arg {
                    ctx.initialize(ref_ident);
                }
            }
        }
    }

    Ok(())
}
