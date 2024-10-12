use crate::ast;
use crate::ast::IdentPath;
use crate::typ::ast::apply_func_decl_named_ty_args;
use crate::typ::ast::infer_from_structural_ty_args;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::Method;
use crate::typ::ast::StructDef;
use crate::typ::ast::TypedFunctionName;
use crate::typ::ast::VariantDef;
use crate::typ::validate_ty_args;
use crate::typ::Context;
use crate::typ::FunctionSig;
use crate::typ::GenericError;
use crate::typ::GenericResult;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeArgResolver;
use crate::typ::TypeParamContainer;
use crate::typ::TypeParamList;
use crate::typ::TypeParamType;
use common::span::Span;
use common::span::Spanned;
use std::borrow::Cow;
use std::rc::Rc;

pub trait Specializable {
    type GenericID: PartialEq + Clone;

    fn is_unspecialized_generic(&self) -> bool;
    fn name(&self) -> Cow<Self::GenericID>;

    fn infer_specialized_from_hint<'a, 'b>(&'a self, hint: &'a Self) -> Option<&'b Self>
    where 'a: 'b
    {
        if self.is_unspecialized_generic() {
            let is_specialization = self.is_unspecialized_generic()
                && !hint.is_unspecialized_generic()
                && self.name() == hint.name();

            if is_specialization {
                Some(hint)
            } else {
                None
            }
        } else {
            Some(self)
        }
    }

    fn apply_type_args_by_name(self, params: &impl TypeParamContainer, args: &impl TypeArgResolver) -> Self;
}


pub fn specialize_generic_name<'a>(
    name: &'a Symbol,
    args: &TypeArgList,
    ctx: &Context,
) -> GenericResult<Cow<'a, Symbol>> {
    let type_params = match name.type_params.as_ref() {
        None => return Ok(Cow::Borrowed(name)),
        Some(type_params) => type_params,
    };

    if args.len() != type_params.items.len() {
        return Err(GenericError::ArgsLenMismatch {
            target: GenericTarget::Name(name.full_path.clone()),
            expected: type_params.items.len(),
            actual: args.len(),
        });
    }

    validate_ty_args(args, type_params, ctx)?;

    let type_args = if let Some(existing_args) = &name.type_args {
        existing_args
            .clone()
            .map(|arg, _pos| arg.apply_type_args_by_name(type_params, args))
    } else {
        let mut resolved_args = Vec::with_capacity(type_params.len());

        for (i, param) in type_params.items.iter().enumerate() {
            let is_iface = param.constraint
                .clone()
                .map(|constraint| Rc::new(constraint.is_ty));

            let arg = args.resolve(&TypeParamType {
                name: param.name.clone(),
                is_iface,
                pos: i,
            });

            resolved_args.push(arg.into_owned());
        }
        TypeArgList::new(resolved_args, name.span().clone())
    };

    let name = Symbol {
        type_args: Some(type_args),
        ..name.clone()
    };

    Ok(Cow::Owned(name))
}

pub fn specialize_struct_def<'a>(
    generic_def: &Rc<StructDef>,
    ty_args: &TypeArgList,
    ctx: &Context,
) -> GenericResult<Rc<StructDef>> {
    let struct_ty_params = match &generic_def.name.type_params {
        None => return Ok(generic_def.clone()),
        Some(param_list) => param_list,
    };

    let specialized_name= specialize_generic_name(&generic_def.name, ty_args, ctx)?
        .into_owned();

    let implements = specialize_implements_clause(
        &generic_def.implements,
        struct_ty_params,
        ty_args
    );

    let fields: Vec<_> = generic_def
        .fields()
        .map(|generic_field| {
            let ty = generic_field.ty
                .clone()
                .apply_type_args_by_name(struct_ty_params, ty_args);

            Ok(ast::Field {
                ty,
                ..generic_field.clone()
            })
        })
        .collect::<GenericResult<_>>()?;

    let methods: Vec<_> = generic_def
        .methods()
        .map(|generic_method| {
            let self_ty = Type::from_struct_type(
                specialized_name.clone(),
                generic_def.kind
            );

            let specialized_decl = specialize_method_decl(
                &generic_def.name.full_path,
                self_ty,
                &generic_method.decl,
                struct_ty_params,
                ty_args,
            )?;
            
            Ok(Method {
                access: generic_method.access,
                decl: Rc::new(specialized_decl),
            })
        })
        .collect::<GenericResult<_>>()?;

    Ok(Rc::new(StructDef {
        name: specialized_name,
        implements,
        fields,
        methods,
        span: generic_def.span.clone(),
        kind: generic_def.kind,
        forward: generic_def.forward,
    }))
}

pub fn specialize_variant_def(
    variant: &VariantDef,
    args: &TypeArgList,
    ctx: &Context,
) -> GenericResult<VariantDef> {
    let variant_ty_params = match &variant.name.type_params {
        None => return Ok(variant.clone()),
        Some(param_list) => param_list,
    };

    let parameterized_name= specialize_generic_name(&variant.name, args, ctx)?
        .into_owned();
    
    let implements = specialize_implements_clause(
        &variant.implements,
        variant_ty_params,
        args
    );

    let cases: Vec<_> = variant
        .cases
        .iter()
        .map(|case| {
            let data_ty = match &case.data_ty {
                None => None,
                Some(ty) => {
                    let ty = ty.clone().substitute_type_args(args);
                    Some(ty)
                },
            };

            Ok(ast::VariantCase {
                data_ty,
                ..case.clone()
            })
        })
        .collect::<GenericResult<_>>()?;
    
    let mut methods = Vec::new();

    let self_ty = Type::variant(parameterized_name.clone());
    for method in &variant.methods{ 
        let specialized_decl = specialize_method_decl(
            &parameterized_name.full_path,
            self_ty.clone(),
            &method.decl,
            variant_ty_params,
            args
        )?;
        
        methods.push(Method {
            decl: Rc::new(specialized_decl),
            access: method.access,
        });
    }

    Ok(VariantDef {
        name: parameterized_name,
        span: variant.span().clone(),
        forward: variant.forward,
        cases,
        implements,
        methods,
    })
}

fn specialize_implements_clause(
    implements: &[Type],
    params: &TypeParamList,
    args: &TypeArgList
) -> Vec<Type> {
    implements
        .iter()
        .map(|implements_ty| {
            implements_ty
                .clone()
                .apply_type_args_by_name(params, args)
        })
        .collect()
}

fn specialize_method_decl(
    owning_ty_generic_name: &IdentPath,
    self_ty: Type,
    generic_method: &FunctionDecl,
    struct_ty_params: &TypeParamList,
    ty_args: &TypeArgList
) -> GenericResult<FunctionDecl> {
    let mut method = apply_func_decl_named_ty_args(
        generic_method.clone(),
        struct_ty_params,
        ty_args
    );

    // specialize the owning type of all methods
    method.name = TypedFunctionName {
        ident: method.name.ident.clone(),
        span: method.name.span.clone(),
        owning_ty: match &method.name.owning_ty {
            Some(ty) => {
                assert_eq!(
                    ty.full_path().map(Cow::into_owned).as_ref(),
                    Some(owning_ty_generic_name),
                    "owning type of a method must always be the type it's declared in"
                );

                Some(self_ty)
            },

            None => None,
        },
    };

    Ok(method)
}

pub fn specialize_by_return_ty<'a>(
    name: &'a Symbol,
    generic_sig: &FunctionSig,
    expect_return_ty: &Type,
    span: &Span,
    ctx: &Context
) -> GenericResult<Cow<'a, Symbol>> {
    if !name.is_unspecialized_generic() {
        return Ok(Cow::Borrowed(name));
    }

    let ty_params = name.type_params.as_ref().unwrap();

    let mut inferred_ty_args = vec![None; ty_params.len()];
    infer_from_structural_ty_args(&generic_sig.return_ty, expect_return_ty, &mut inferred_ty_args);

    let mut ty_args = Vec::new();
    for (pos, arg) in inferred_ty_args.into_iter().enumerate() {
        let valid_arg = match arg {
            None => None,

            Some(arg) => {
                if let Some(constraint) = &ty_params.items[pos].constraint {
                    if arg.match_constraint(&constraint.is_ty, ctx) {
                        Some(arg)
                    } else {
                        None
                    }
                } else {
                    Some(arg)
                }
            },
        };
        
        match valid_arg {
            Some(ty) => { 
                ty_args.push(ty); 
            }
            None => {
                return Err(GenericError::CannotInferArgs {
                    target: GenericTarget::FunctionSig(generic_sig.clone()),
                    hint: GenericTypeHint::ExpectedReturnType(expect_return_ty.clone()),
                })
            }
        }
    }

    let specialized = name
        .clone()
        .with_ty_args(Some(TypeArgList::new(ty_args, span.clone())));

    Ok(Cow::Owned(specialized))
}
