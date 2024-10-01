use crate::ast;
use crate::typ::ast::apply_func_decl_named_ty_args;
use crate::typ::ast::StructDef;
use crate::typ::ast::TypedFunctionName;
use crate::typ::ast::VariantDef;
use crate::typ::validate_ty_args;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::GenericResult;
use crate::typ::GenericTarget;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeArgResolver;
use crate::typ::TypeParamContainer;
use crate::typ::TypeParamType;
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
        let specialized_args = existing_args
            .items
            .iter()
            .cloned()
            .map(|arg| arg.apply_type_args_by_name(type_params, args));

        TypeArgList::new(specialized_args, existing_args.span().clone())
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
    generic_class: &Rc<StructDef>,
    ty_args: &TypeArgList,
    ctx: &Context,
) -> GenericResult<Rc<StructDef>> {
    let struct_ty_params = match &generic_class.name.type_params {
        None => return Ok(generic_class.clone()),
        Some(param_list) => param_list,
    };

    let specialized_name = specialize_generic_name(&generic_class.name, ty_args, ctx)?
        .into_owned();

    let implements: Vec<Type> = generic_class.implements
        .iter()
        .map(|implements_ty| {
            let specialized = implements_ty
                .clone()
                .apply_type_args_by_name(struct_ty_params, ty_args);
            Ok(specialized)
        })
        .collect::<GenericResult<_>>()?;

    let members: Vec<_> = generic_class
        .members
        .iter()
        .map(|member| {
            match member {
                ast::StructMember::Field(field) => {
                    let ty = field.ty
                        .clone()
                        .apply_type_args_by_name(struct_ty_params, ty_args);

                    Ok(ast::StructMember::Field(ast::Field {
                        ty,
                        ..field.clone()
                    }))
                }

                ast::StructMember::MethodDecl(generic_method) => {
                    let mut method = apply_func_decl_named_ty_args(
                        (**generic_method).clone(),
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
                                    Some(&generic_class.name.full_path),
                                    "owning type of a method must always be the type it's declared in"
                                );

                                Some(Type::struct_type(
                                    specialized_name.clone(),
                                    generic_class.kind
                                ))
                            },
                            None => None,
                        },
                    };

                    Ok(ast::StructMember::MethodDecl(Rc::new(method)))
                }
            }
        })
        .collect::<GenericResult<_>>()?;

    Ok(Rc::new(StructDef {
        name: specialized_name,
        implements,
        members,
        span: generic_class.span.clone(),
        kind: generic_class.kind,
    }))
}

pub fn specialize_variant_def(
    variant: &VariantDef,
    args: &TypeArgList,
    ctx: &Context,
) -> GenericResult<VariantDef> {
    let parameterized_name = specialize_generic_name(&variant.name, args, ctx)?;

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

    Ok(VariantDef {
        name: parameterized_name.into_owned(),
        span: variant.span().clone(),
        cases,
    })
}
