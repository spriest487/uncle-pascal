use crate::{
    ast::{FunctionDecl, FunctionParam},
    Context, GenericError, GenericResult, GenericTarget, Type, TypeList, TypeParamList,
};
use pas_common::span::Spanned;
use pas_syn::ast::{self, FunctionParamMod};
use std::fmt;

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct FunctionParamSig {
    pub modifier: Option<FunctionParamMod>,
    pub ty: Type,
}

impl FunctionParamSig {
    pub fn by_val(ty: Type) -> Self {
        Self { ty, modifier: None }
    }

    pub fn inout(ty: Type) -> Self {
        Self {
            ty,
            modifier: Some(FunctionParamMod::Var),
        }
    }

    pub fn out(ty: Type) -> Self {
        Self {
            ty,
            modifier: Some(FunctionParamMod::Out),
        }
    }

    pub fn is_by_ref(&self) -> bool {
        match &self.modifier {
            Some(FunctionParamMod::Out) | Some(FunctionParamMod::Var) => true,
            _ => false,
        }
    }
}

impl From<FunctionParam> for FunctionParamSig {
    fn from(param: FunctionParam) -> Self {
        Self {
            ty: param.ty,
            modifier: param.modifier,
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct FunctionSigTypeParam {
    pub is_ty: Type,
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct FunctionSig {
    pub return_ty: Type,
    pub params: Vec<FunctionParamSig>,
    pub type_params: Option<ast::TypeList<FunctionSigTypeParam>>,
}

impl FunctionSig {
    pub fn new(
        return_ty: Type,
        params: Vec<FunctionParamSig>,
        type_params: Option<TypeParamList>,
    ) -> Self {
        let params = params
            .into_iter()
            .map(|p| FunctionParamSig {
                ty: p.ty.clone(),
                modifier: p.modifier.clone(),
            })
            .collect();

        let type_params = match type_params {
            Some(type_params) => {
                let items: Vec<_> = type_params
                    .items
                    .iter()
                    .map(|decl_param| {
                        let is_ty = decl_param
                            .constraint
                            .as_ref()
                            .map(|c| c.is_ty.clone())
                            .unwrap_or(Type::Any);

                        FunctionSigTypeParam { is_ty }
                    })
                    .collect();

                Some(ast::TypeList::new(items, type_params.span().clone()))
            },
            None => None,
        };

        Self {
            return_ty,
            params,
            type_params,
        }
    }

    pub fn of_decl(decl: &FunctionDecl) -> Self {
        let return_ty = decl.return_ty.clone().unwrap_or(Type::Nothing);
        let param_sigs = decl.params.iter().cloned().map(FunctionParamSig::from).collect();

        Self::new(return_ty, param_sigs, decl.type_params.clone())
    }

    /// test if a list of type args that could be used to specialize this function are applicable
    /// e.g. this sig has type params, the number of type params is the same as the number
    /// of args provided, and that each arg passes the constraints for its corresponding param
    pub fn validate_type_args(&self, type_args: &TypeList, ctx: &Context) -> GenericResult<()> {
        let expected_type_args_len = match self.type_params.as_ref() {
            Some(type_params) => type_params.len(),
            None => 0,
        };

        if type_args.len() != expected_type_args_len {
            return Err(GenericError::ArgsLenMismatch {
                expected: expected_type_args_len,
                actual: type_args.len(),
                target: GenericTarget::FunctionSig(self.clone()),
            });
        }

        let type_params = self
            .type_params
            .as_ref()
            .expect("must have type params or previous check would fail");

        for arg_pos in 0..type_params.len() {
            match &type_params.items[arg_pos].is_ty {
                Type::Any => {
                    // nothing to validate
                },

                Type::Interface(is_iface_ident) => {
                    let actual_ty = &type_args.items[arg_pos];
                    if !ctx.is_iface_impl(actual_ty, is_iface_ident) {
                        return Err(GenericError::ArgConstraintNotSatisfied {
                            is_not_ty: Type::Interface(is_iface_ident.clone()),
                            arg_ty: actual_ty.clone(),
                        });
                    }
                },

                bad => panic!(
                    "unsupported type in signature type param constraint: {}",
                    bad
                ),
            }
        }

        Ok(())
    }

    pub fn specialize_generic(&self, type_args: &TypeList, ctx: &Context) -> GenericResult<Self> {
        self.validate_type_args(type_args, ctx)?;

        let params = self
            .params
            .iter()
            .map(|sig_param| {
                let ty = sig_param.ty.clone().substitute_type_args(type_args);
                Ok(FunctionParamSig {
                    ty,
                    ..sig_param.clone()
                })
            })
            .collect::<GenericResult<_>>()?;

        let return_ty = self.return_ty.clone().substitute_type_args(type_args);

        let specialized_sig = FunctionSig {
            return_ty,
            params,
            type_params: self.type_params.clone(),
        };

        Ok(specialized_sig)
    }

    /// replace all `Self`-typed args with `self_ty`
    pub fn with_self(&self, self_ty: &Type) -> Self {
        let mut result = self.clone();
        for param in &mut result.params {
            if param.ty == Type::MethodSelf {
                param.ty = self_ty.clone();
            }
        }

        result
    }

    /// given that `self` is the sig of an interface method with one
    /// or more `Self`-typed arguments, find the expected self-type for
    pub fn self_ty_from_args<'arg>(&self, args: &'arg [Type]) -> Option<&'arg Type> {
        if args.len() != self.params.len() {
            return None;
        }

        let self_arg_pos = self
            .params
            .iter()
            .position(|arg| arg.ty == Type::MethodSelf)?;

        Some(&args[self_arg_pos])
    }

    /// given that `self` is the sig of an interface method,
    /// for what type does `impl_func` implement this method, if any?
    pub fn impl_ty<'func>(&self, impl_func: &'func Self) -> Option<&'func Type> {
        if self.params.len() != impl_func.params.len() {
            return None;
        }

        let self_type = if self.return_ty == Type::MethodSelf {
            &impl_func.return_ty
        } else {
            self.params
                .iter()
                .position(|param| param.ty == Type::MethodSelf)
                .map(|pos| &impl_func.params[pos].ty)?
        };

        // `Nothing` can't have interface impls
        if *self_type == Type::Nothing {
            return None;
        }

        let self_positions: Vec<_> = self
            .params
            .iter()
            .enumerate()
            .filter_map(|(pos, param)| {
                if param.ty == Type::MethodSelf {
                    Some(pos)
                } else {
                    None
                }
            })
            .collect();

        for pos in 0..self.params.len() {
            if self_positions.contains(&pos) {
                // self-typed params must all be the same type as either the
                // first such param, or the return type if it's self-typed too
                if impl_func.params[pos].ty != *self_type {
                    return None;
                }
            } else if impl_func.params[pos] != self.params[pos] {
                // non-self params must match exactly
                return None;
            }
        }

        Some(self_type)
    }
}

impl fmt::Display for FunctionSig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function (")?;

        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            if let Some(modifier) = &param.modifier {
                write!(f, "{} ", modifier)?;
            }

            write!(f, "{}", param.ty)?;
        }
        write!(f, ")")?;

        match &self.return_ty {
            Type::Nothing => Ok(()),
            ty => write!(f, ": {}", ty),
        }
    }
}
