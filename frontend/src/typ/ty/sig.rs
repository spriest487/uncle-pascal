use crate::ast::FunctionParamMod;
use crate::typ::ast::AnonymousFunctionDef;
use crate::typ::ast::Expr;
use crate::typ::ast::FunctionCall;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionParam;
use crate::typ::ast::MethodCallNoArgs;
use crate::typ::{GenericError, Specializable, TypeParamContainer};
use crate::typ::GenericResult;
use crate::typ::GenericTarget;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeArgResolver;
use crate::typ::TypeParamList;
use crate::typ::Typed;
use crate::typ::{Context, TypeParam};
use crate::ast;
use common::span::Span;
use common::span::Spanned;
use std::fmt;
use std::rc::Rc;

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

impl FunctionSigTypeParam {
    pub fn from_type_param(param: TypeParam) -> Self {
        FunctionSigTypeParam {
            is_ty: param.constraint
                .as_ref()
                .map(|constraint| &constraint.is_ty)
                .cloned()
                .unwrap_or(Type::Nothing)
        }
    }
}

pub type FunctionSigTypeParamList = ast::TypeList<FunctionSigTypeParam>;

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
        let return_ty = decl.return_ty.clone();
        let param_sigs = decl
            .params
            .iter()
            .cloned()
            .map(FunctionParamSig::from)
            .collect();

        Self::new(return_ty, param_sigs, decl.type_params.clone())
    }

    pub fn of_anonymous_func(func: &AnonymousFunctionDef) -> Rc<Self> {
        func.annotation
            .ty()
            .as_func()
            .cloned()
            .expect("anonymous function must have function type")
    }

    /// test if a list of type args that could be used to specialize this function are applicable
    /// e.g. this sig has type params, the number of type params is the same as the number
    /// of args provided, and that each arg passes the constraints for its corresponding param
    pub fn validate_type_args(
        &self,
        type_args: &impl TypeArgResolver,
        ctx: &Context,
    ) -> GenericResult<()> {
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
            let constraint_ty = &type_params[arg_pos].is_ty;
            
            let actual_ty = type_args
                .find_by_pos(arg_pos)
                .expect("already checked the length matches");

            if !actual_ty.match_constraint(constraint_ty, ctx) {
                return Err(GenericError::ConstraintNotSatisfied {
                    is_not_ty: constraint_ty.clone(),
                    actual_ty: Some(actual_ty.clone()),
                });
            }
        }

        Ok(())
    }
    
    pub fn contains_generic_params(&self, ctx: &Context) -> bool {
        if self.return_ty.contains_unresolved_params(ctx) {
            return true;
        }
        
        self.params
            .iter()
            .any(|param| param.ty.contains_unresolved_params(ctx))
    }

    pub fn specialize_generic(
        &self,
        type_args: &impl TypeArgResolver,
        ctx: &Context,
    ) -> GenericResult<Self> {
        self.validate_type_args(type_args, ctx)?;
        
        let specialized_sig = self.substitute_type_args(type_args);
        Ok(specialized_sig)
    }
    
    pub fn type_params_len(&self) -> usize {
        self.type_params.as_ref().map(|list| list.len()).unwrap_or(0)
    }
    
    pub fn apply_ty_args(&self, ty_params: &impl TypeParamContainer, args: &impl TypeArgResolver) -> Self {
        let params = self
            .params
            .iter()
            .map(|sig_param| {
                let ty = sig_param.ty
                    .clone()
                    .apply_type_args_by_name(ty_params, args);

                FunctionParamSig {
                    ty,
                    ..sig_param.clone()
                }
            })
            .collect();

        let return_ty = self.return_ty
            .clone()
            .apply_type_args_by_name(ty_params, args);

        let sig_ty_params = match &self.type_params {
            Some(type_params) => {
                let mut items = Vec::with_capacity(type_params.len());
                for item in &type_params.items {
                    items.push(FunctionSigTypeParam {
                        is_ty: item
                            .is_ty.clone()
                            .apply_type_args_by_name(ty_params, args),
                    });
                }
                Some(ast::TypeList::new(items, type_params.span().clone()))
            },
            None => None,
        };

        let sig = FunctionSig {
            return_ty,
            params,
            type_params: sig_ty_params,
        };
        sig
    } 

    pub fn substitute_type_args(&self, type_args: &impl TypeArgResolver) -> Self {
        let params = self
            .params
            .iter()
            .map(|sig_param| {
                let ty = sig_param.ty.clone().substitute_type_args(type_args);
                FunctionParamSig {
                    ty,
                    ..sig_param.clone()
                }
            })
            .collect();

        let return_ty = self.return_ty.clone().substitute_type_args(type_args);

        let type_params = match &self.type_params {
            Some(type_params) => {
                let mut items = Vec::with_capacity(type_params.len());
                for item in &type_params.items {
                    items.push(FunctionSigTypeParam {
                        is_ty: item.is_ty.clone().substitute_type_args(type_args),
                    });
                }
                Some(ast::TypeList::new(items, type_params.span().clone()))
            },
            None => None,
        };

        let sig = FunctionSig {
            return_ty,
            params,
            type_params,
        };
        sig
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

    /// when the contextual type hint means we're expecting a function value that matches
    /// the signature of the function, the expression evaluates to the function itself rather
    /// than a call to the function. it could never refer to the return value of the function,
    /// since it's impossible for a function to return a function with its own signature, which
    /// would be a recursive type
    pub fn should_call_noargs_in_expr(&self, expect_ty: &Type, self_arg_ty: &Type) -> bool {
        // if we expect a self-param (eg the `a` in the no-args call `a.B`), it can only be a
        // no-args call if there is exactly one param. 
        // if there's no self-param, there must be zero actual args
        let with_params_len = match self_arg_ty {
            Type::Nothing => 0,
            _ => 1,
        };

        if self.params.len() != with_params_len {
            return false;
        }

        match expect_ty {
            // does the expected type of this value match the full sig (after including self, if
            // applicable)? if so we aren't calling the function, we're referencing it
            Type::Function(expect_sig) => match self_arg_ty {
                Type::Nothing => **expect_sig != *self,

                ty => {
                    let actual_sig = self.with_self(ty);
                    **expect_sig != actual_sig
                }
            },

            _ => true,
        }
    }

    pub fn new_function_call(
        &self,
        target: Expr,
        span: Span,
        args: impl IntoIterator<Item = Expr>,
        args_span: Span,
        type_args: Option<TypeArgList>,
    ) -> FunctionCall {
        let func_val_annotation = match &self.return_ty {
            Type::Nothing => Typed::Untyped(span),
            return_ty => Typed::new_temp_val(return_ty.clone(), span),
        };

        FunctionCall {
            annotation: func_val_annotation,
            args: args.into_iter().collect(),
            args_span,
            target,
            type_args,
        }
    }

    pub fn new_no_args_method_call(&self,
        target: Expr,
        owning_type: Type,
        self_arg: Option<Expr>,
        type_args: Option<TypeArgList>,
    ) -> MethodCallNoArgs {
        let span = target.span().clone();
        let func_val_annotation = match &self.return_ty {
            Type::Nothing => Typed::Untyped(span),
            return_ty => Typed::new_temp_val(return_ty.clone(), span),
        };
        
        MethodCallNoArgs {
            annotation: func_val_annotation,
            self_arg,
            owning_type,
            target,
            type_args,
        }
    }
    
    fn type_param_name(pos: usize) -> String {
        format!("T{pos}")
    }
}

impl fmt::Display for FunctionSig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function")?;
        
        if let Some(type_params) = &self.type_params {
            write!(f, "[")?;
            for i in 0..type_params.len() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", Self::type_param_name(i))?;
            }
            write!(f, "]")?;
        }

        if !self.params.is_empty() {
            write!(f, "(")?;
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
        }
        
        if self.return_ty != Type::Nothing {
            write!(f, ": {}", self.return_ty)?;
        }

        if let Some(type_params) = &self.type_params {
            if type_params.iter().any(|p| p.is_ty != Type::Any) {
                write!(f, " where ")?;

                let mut constraint_count = 0;
                for i in 0..type_params.len() {
                    if type_params[i].is_ty == Type::Any {
                        continue;
                    }

                    if constraint_count > 0 {
                        write!(f, "; ")?;
                    }

                    write!(f, "T{i} is {}", type_params[i].is_ty)?;
                    constraint_count += 1;
                }
            }
        }
        
        Ok(())
    }
}
