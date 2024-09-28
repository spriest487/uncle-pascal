use crate::typ::ast::call;
use crate::typ::ast::check_implicit_conversion;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::Expr;
use crate::typ::ast::FunctionDecl;
use crate::typ::{Context, NameError};
use crate::typ::FunctionSig;
use crate::typ::InstanceMethod;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use common::span::Span;
use common::span::Spanned;
use crate::ast;
use crate::ast::{FunctionName, TypeList};
use crate::ast::Ident;
use crate::ast::IdentPath;
use std::fmt;
use std::rc::Rc;

pub struct Overload {
    pub selected_sig: usize,
    pub args: Vec<Expr>,
    pub type_args: Option<TypeList<Type>>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum OverloadCandidate {
    Function {
        decl_name: IdentPath,
        sig: Rc<FunctionSig>,
    },
    Method {
        iface_ty: Type,
        ident: Ident,
        decl: FunctionDecl,
        sig: Rc<FunctionSig>,
    },
}

impl OverloadCandidate {
    pub fn from_instance_method(im: InstanceMethod) -> Self {
        match im {
            InstanceMethod::Method { owning_ty: iface_ty, decl } => {
                let ident = decl.name.ident().clone();
                let sig = FunctionSig::of_decl(&decl);
                
                OverloadCandidate::Method {
                    iface_ty,
                    ident,
                    sig: Rc::new(sig),
                    decl,
                }
            },

            InstanceMethod::FreeFunction { func_name, sig } => {
                OverloadCandidate::Function {
                    decl_name: func_name,
                    sig,
                }
            },
        }
    }

    pub fn sig(&self) -> &Rc<FunctionSig> {
        match self {
            OverloadCandidate::Function { sig, .. } => sig,
            OverloadCandidate::Method { sig, .. } => sig,
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            OverloadCandidate::Function { decl_name, .. } => decl_name.span(),
            OverloadCandidate::Method { decl, .. } => decl.span(),
        }
    }
}

impl fmt::Display for OverloadCandidate {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OverloadCandidate::Function { decl_name, .. } => {
                write!(f, "function {}", decl_name)
            },
            OverloadCandidate::Method { iface_ty, ident, .. } => {
                write!(f, "method {}.{}", iface_ty, ident)
            },
        }
    }
}

pub fn resolve_overload(
    candidates: &[OverloadCandidate],
    args: &[ast::Expr<Span>],
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
                .map(|c| c.sig().with_self(&self_ty))
                .collect()
        },

        None => candidates.iter().map(|c| (**c.sig()).clone()).collect(),
    };

    // no overload resolution needed, we can use the param type hint for all args
    if candidates.len() == 1 {
        let sig = &candidate_sigs[0];

        let args = call::build_args_for_params(&sig.params, args, self_arg, span, ctx)?;
        let actual_arg_tys: Vec<_> = args
            .iter()
            .map(|arg_expr| arg_expr.annotation().ty().into_owned())
            .collect();

        // if it's a direct interface method invocation (e.g. `Comparable.Compare(1, 2)`),
        // we don't actually know at this point whether that implementation exists!
        if self_arg.is_none() {
            if let OverloadCandidate::Method {
                iface_ty,
                ident: method_ident,
                ..
            } = &candidates[0]
            {
                let self_ty = sig.self_ty_from_args(&actual_arg_tys).ok_or_else(|| {
                    TypeError::AmbiguousSelfType {
                        iface: iface_ty.clone(),
                        span: span.clone(),
                        method: method_ident.clone(),
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
            type_args: None, //todo
        });
    }

    // full overload resolution: while > 1 candidates remain, typ an additional arg
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

    let arg_count = args.len();
    let mut arg_index = 0;

    loop {
        // did we find a best match? try to typ args as if this is the sig to be called
        if valid_candidates.len() == 1 {
            let selected_sig = valid_candidates[0];
            //            println!("selected {} as candidate for {}", candidates[selected_sig].sig(), span);

            break Ok(Overload {
                selected_sig,
                args: actual_args,
                type_args: None, //todo
            });
        }

        // ran out of candidates or arguments, couldn't resolve a single sig
        if arg_index >= arg_count || valid_candidates.len() == 0 {
            // println!("ran out of args or candidates (arg {}/{}), {} candidates remain)", arg_index + 1, arg_count, valid_candidates.len());
            // dbg!(&candidates);

            break Err(TypeError::AmbiguousFunction {
                candidates: candidates.to_vec(),
                span: span.clone(),
            });
        }

        // println!("matching {} (arg {}/{}), {} candidates remain)", args[arg_index], arg_index + 1, arg_count, valid_candidates.len());

        let arg = typecheck_expr(&args[arg_index], &Type::Nothing, ctx)?;
        let arg_span = args[arg_index].span();
        let arg_ty = arg.annotation().ty().into_owned();
        actual_args.push(arg);

        valid_candidates.retain(|i| {
            let sig = &candidate_sigs[*i];

            if param_index > sig.params.len() {
                //                println!("discarding {} as candidate for {}: not enough params", sig, span);
                false
            } else {
                let sig_param = &sig.params[param_index];
                check_implicit_conversion(&arg_ty, &sig_param.ty, arg_span, ctx).is_ok()
            }
        });

        param_index += 1;
        arg_index += 1;
    }
}
