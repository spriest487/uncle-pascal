use crate::ast;
use crate::ast::TypeList;
use crate::ast::Visibility;
use crate::typ::ast::{check_implicit_conversion, MethodDecl};
use crate::typ::ast::specialize_call_args;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::Expr;
use crate::typ::ast::FunctionDecl;
use crate::typ::Context;
use crate::typ::InstanceMethod;
use crate::typ::NameError;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use common::span::Span;
use common::span::Spanned;
use std::fmt;
use std::rc::Rc;

#[derive(Debug)]
pub struct Overload {
    pub selected_sig: usize,
    pub args: Vec<Expr>,
    pub type_args: Option<TypeList<Type>>,
}

impl Overload {
    pub fn type_args_len(&self) -> usize {
        self.type_args.as_ref().map(|list| list.len()).unwrap_or(0)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum OverloadCandidate {
    Function {
        decl_name: Symbol,
        decl: Rc<FunctionDecl>,
        visibility: Visibility,
    },
    Method {
        iface_ty: Type,
        self_ty: Type,

        index: usize,

        decl: MethodDecl,
    },
}

impl OverloadCandidate {
    pub fn from_instance_method(im: InstanceMethod) -> Self {
        match im {
            InstanceMethod::Method { self_ty, iface_ty, index, method } => {
                OverloadCandidate::Method {
                    self_ty,
                    iface_ty,
                    index,
                    decl: method.clone(),
                }
            }

            InstanceMethod::FreeFunction { func_name, decl, visibility } => {
                OverloadCandidate::Function {
                    decl_name: func_name,
                    decl,
                    visibility,
                }
            },
        }
    }

    pub fn decl(&self) -> &Rc<FunctionDecl> {
        match self {
            OverloadCandidate::Function { decl, .. } => decl,
            OverloadCandidate::Method { decl, .. } => &decl.func_decl,
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            OverloadCandidate::Function { decl, .. } => decl.span(),
            OverloadCandidate::Method { decl, .. } => decl.func_decl.span(),
        }
    }
}

impl fmt::Display for OverloadCandidate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OverloadCandidate::Function { decl_name, .. } => {
                write!(f, "function {}", decl_name)
            },
            OverloadCandidate::Method { iface_ty, decl, .. } => {
                write!(f, "method {}.{}", iface_ty, decl.func_decl.ident())
            },
        }
    }
}

pub fn try_resolve_overload(
    candidates: &[OverloadCandidate],
    args: &[ast::Expr<Span>],
    type_args: Option<&TypeArgList>,
    self_arg: Option<&Expr>,
    span: &Span,
    ctx: &mut Context,
) -> Option<Overload> {
    let mut tmp_ctx = ctx.clone();

    let overload = resolve_overload(
        candidates,
        args,
        type_args,
        self_arg,
        span,
        &mut tmp_ctx
    ).ok()?;
    
    ctx.consolidate_branches(&[tmp_ctx]);
    
    Some(overload)
}

pub fn resolve_overload(
    candidates: &[OverloadCandidate],
    args: &[ast::Expr<Span>],
    type_args: Option<&TypeArgList>,
    self_arg: Option<&Expr>,
    span: &Span,
    ctx: &mut Context,
) -> TypeResult<Overload> {
    if candidates.is_empty() {
        panic!("overload resolution requires at least 1 candidate");
    }

    let candidate_sigs: Vec<_> = match self_arg {
        Some(self_arg) => {
            let self_ty = self_arg.annotation().ty();
            candidates
                .iter()
                .map(|c| c.decl().sig().with_self(&self_ty))
                .collect()
        },

        None => candidates.iter().map(|c| c.decl().sig().clone()).collect(),
    };

    // no overload resolution needed, we can use the param type hint for all args
    if candidates.len() == 1 {
        let specialized = specialize_call_args(
            candidates[0].decl(),
            args,
            self_arg,
            type_args.cloned(),
            span,
            ctx
        )?;

        let args = specialized.actual_args;
        let sig= specialized.sig;

        let actual_arg_tys: Vec<_> = args
            .iter()
            .map(|arg_expr| arg_expr.annotation().ty().into_owned())
            .collect();

        // if it's a direct interface method invocation (e.g. `Comparable.Compare(1, 2)`),
        // we don't actually know at this point whether that implementation exists!
        if self_arg.is_none() {
            if let OverloadCandidate::Method {
                iface_ty: iface_ty @ Type::Interface(..),
                decl: method_decl,
                ..
            } = &candidates[0] {
                let self_ty = sig.self_ty_from_args(&actual_arg_tys).ok_or_else(|| {
                    TypeError::AmbiguousSelfType {
                        iface: iface_ty.clone(),
                        span: span.clone(),
                        method: method_decl.func_decl.ident().clone(),
                    }
                })?;
                
                let is_impl = ctx
                    .is_implementation(self_ty, iface_ty)
                    .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

                if !is_impl {
                    return Err(TypeError::from_name_err(NameError::NoImplementationFound {
                        owning_ty: iface_ty.clone(),
                        impl_ty: self_ty.clone(),
                    }, span.clone()))
                }
            }
        }

        return Ok(Overload {
            selected_sig: 0,
            args,
            type_args: specialized.type_args,
        });
    }

    // full overload resolution: while > 1 candidates remain, process an additional arg
    // left-to-right, without a type hint, and look at the type of the resulting expr to eliminate
    // candidates. as soon as 1 candidate remains, process the rest of the arguments using that
    // sig. if 0 candidates remain after an arg is processed, the call is ambiguous
    let mut actual_args = Vec::new();
    let mut valid_candidates: Vec<_> = (0..candidates.len()).collect();

    let mut param_index = 0;

    // do the self-arg (which has a known type already) first
    if let Some(self_arg) = self_arg {
        actual_args.push(self_arg.clone());

        valid_candidates.retain(|i| {
            let sig = &candidate_sigs[*i];

            let self_arg_ty = self_arg.annotation().ty();

            if sig.params.len() < 1 {
                //                println!("discarding {} as candidate for {}, not enough params", sig, span);
                false
            } else {
                let self_param_ty = &sig.params[0].ty;

                check_implicit_conversion(&self_arg_ty, &self_param_ty, span, ctx).is_ok()
            }
        });

        param_index += 1;
    }
    
    // disqualify any sigs that don't match the argument count
    valid_candidates.retain(|index| {
        let sig = &candidate_sigs[*index];

        sig.params.len() == args.len() + self_arg.iter().len()
    });

    let arg_count = args.len();
    let mut arg_index = 0;

    loop {
        // eprintln!("resolve_overload: {} candidates remaining @ {span}", valid_candidates.len());
        
        // did we find a best match? try to typecheck args as if this is the sig to be called
        if valid_candidates.len() == 1 {
            let selected_sig = valid_candidates[0];
            // println!("selected {} as candidate for {}", candidates[selected_sig].sig(), span);

            break Ok(Overload {
                selected_sig,
                args: actual_args,
                // TODO: can we resolve overloads for generic methods?
                type_args: None, 
            });
        }

        if valid_candidates.len() == 0 {
            // couldn't resolve a single sig
            break Err(TypeError::AmbiguousFunction {
                candidates: candidates.to_vec(),
                span: span.clone(),
            });
        }

        // checked all arguments and more than one candidate is still valid
        if arg_index >= arg_count {
            // println!("ran out of args or candidates (arg {}/{}), {} candidates remain)", arg_index + 1, arg_count, valid_candidates.len());

            // discard any that are inaccessible methods or invisible functions
            // we don't do this until the very last minute so that at any earlier stage we can 
            // still match one of these and show an accessibility/visibility error, 
            // rather than acting like it doesn't exist
            disqualify_inaccessible(candidates, &mut valid_candidates, ctx);

            if valid_candidates.len() == 1 {
                break Ok(Overload {
                    selected_sig: valid_candidates[0],
                    args: actual_args,
                    type_args: None,
                });
            } else {
                break Err(TypeError::AmbiguousFunction {
                    candidates: candidates.to_vec(),
                    span: span.clone(),
                });
            }
        }

        // println!("matching {} (arg {}/{}), {} candidates remain)", args[arg_index], arg_index + 1, arg_count, valid_candidates.len());

        // we still don't know the sig to use, so evaluate another argument and use its type
        // to disqualify any sigs that wouldn't be compatible with that type
        let arg = typecheck_expr(&args[arg_index], &Type::Nothing, ctx)?;
        let arg_span = args[arg_index].span();
        let arg_ty = arg.annotation().ty().into_owned();
        actual_args.push(arg);

        valid_candidates.retain(|i| {
            let sig = &candidate_sigs[*i];
            
            let sig_param = &sig.params[param_index];
            check_implicit_conversion(&arg_ty, &sig_param.ty, arg_span, ctx).is_ok()
        });

        param_index += 1;
        arg_index += 1;
    }
}

fn disqualify_inaccessible(
    candidates: &[OverloadCandidate],
    valid: &mut Vec<usize>,
    ctx: &Context,
) {
    valid.retain(|i| {
        let candidate = &candidates[*i];

        match candidate {
            OverloadCandidate::Function { visibility, decl_name, .. } => {
                *visibility >= Visibility::Interface
                    || ctx.is_current_namespace_child(&decl_name.full_path)
            },

            OverloadCandidate::Method { decl, iface_ty, .. } => {
                let accessible = iface_ty.get_current_access(ctx) >= decl.access;

                if !accessible {
                    // eprintln!("disqualifying {}: inaccessible from this context", candidate.sig());
                }

                accessible
            }
        }
    });
}
