#[cfg(test)]
mod test;

use crate::ast;
use crate::ast::Ident;
use crate::ast::Visibility;
use crate::ast::{FunctionName, StructKind};
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_func_decl;
use crate::typ::ast::InterfaceMethodDecl;
use crate::typ::ast::const_eval_integer;
use crate::typ::typecheck_type;
use crate::typ::Context;
use crate::typ::NameError;
use crate::typ::Primitive;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypecheckError;
use crate::typ::TypecheckResult;
use crate::typ::Typed;
use crate::IntConstant;
use common::span::Span;
use common::span::Spanned;

pub type StructDef = ast::StructDef<Typed>;
pub type StructMember = ast::StructMember<Typed>;
pub type Field = ast::Field<Typed>;
pub type InterfaceDecl = ast::InterfaceDecl<Typed>;
pub type VariantDef = ast::VariantDef<Typed>;
pub type AliasDecl = ast::AliasDecl<Typed>;
pub type EnumDecl = ast::EnumDecl<Typed>;
pub type EnumDeclItem = ast::EnumDeclItem<Typed>;

pub const VARIANT_TAG_TYPE: Type = Type::Primitive(Primitive::Int32);

pub fn typecheck_struct_decl(
    name: Symbol,
    class: &ast::StructDef<Span>,
    ctx: &mut Context,
) -> TypecheckResult<StructDef> {
    let self_ty = match class.kind {
        StructKind::Record | StructKind::PackedRecord => Type::Record(Box::new(name.clone())),
        StructKind::Class => Type::Class(Box::new(name.clone())),
    };

    ctx.declare_self_ty(self_ty.clone(), name.span().clone())?;
    ctx.declare_type(
        class.name.ident.clone(),
        self_ty.clone(),
        Visibility::Implementation,
    )?;

    let mut members = Vec::new();
    for member in &class.members {
        match member {
            ast::StructMember::Field(field) => {
                let ty = typecheck_type(&field.ty, ctx)?.clone();

                let is_unsized = ctx
                    .is_unsized_ty(&ty)
                    .map_err(|err| TypecheckError::from_name_err(err, class.span().clone()))?;

                if is_unsized {
                    return Err(TypecheckError::UnsizedMember {
                        decl: name.qualified,
                        member: field.ident.clone(),
                        member_ty: ty,
                    });
                }

                members.push(Field {
                    ty,
                    span: field.span.clone(),
                    ident: field.ident.clone(),
                }.into());
            }
            
            ast::StructMember::MethodDecl(decl) => {
                let decl = typecheck_func_decl(decl, ctx)?;

                if !decl.mods.is_empty() {
                    return Err(TypecheckError::InvalidMethodModifiers {
                        mods: decl.mods.clone(),
                        span: {
                            let start = decl.mods[0].span();
                            let end = decl.mods[decl.mods.len() - 1].span();
                            start.to(end)
                        },
                    });
                }
                
                members.push(decl.into());
            }
        }
    }

    Ok(StructDef {
        kind: class.kind,
        name,
        span: class.span.clone(),
        members,
    })
}

pub fn typecheck_iface(
    name: Symbol,
    iface: &ast::InterfaceDecl<Span>,
    ctx: &mut Context,
) -> TypecheckResult<InterfaceDecl> {
    // declare Self type - type decls are always in their own scope so we don't need to push
    // another one
    ctx.declare_self_ty(Type::MethodSelf, iface.name.span().clone())?;

    let mut methods: Vec<InterfaceMethodDecl> = Vec::new();
    for method in &iface.methods {
        if let Some(existing) = methods
            .iter()
            .find(|other| other.decl.name.ident == method.decl.name.ident)
        {
            let method_path = name
                .qualified
                .clone()
                .child(method.decl.name.ident().clone());

            return Err(TypecheckError::NameError {
                err: NameError::AlreadyDefined {
                    ident: method_path,
                    existing: existing.span().clone(),
                },
                span: method.decl.name.span().clone(),
            });
        }

        let method_decl = typecheck_func_decl(&method.decl, ctx)?;

        methods.push(ast::InterfaceMethodDecl { decl: method_decl });
    }

    Ok(InterfaceDecl {
        name,
        span: iface.span.clone(),
        methods,
    })
}

pub fn typecheck_variant(
    name: Symbol,
    variant: &ast::VariantDef<Span>,
    ctx: &mut Context,
) -> TypecheckResult<VariantDef> {
    if variant.cases.is_empty() {
        return Err(TypecheckError::EmptyVariant(Box::new(variant.clone())));
    }

    let mut cases = Vec::with_capacity(variant.cases.len());
    for case in &variant.cases {
        let data_ty = match &case.data_ty {
            Some(data_ty) => Some(typecheck_type(data_ty, ctx)?),
            None => None,
        };

        cases.push(ast::VariantCase {
            ident: case.ident.clone(),
            span: case.span.clone(),
            data_ty,
        });
    }

    Ok(VariantDef {
        name,
        cases,
        span: variant.span().clone(),
    })
}

pub fn typecheck_alias(
    name: Symbol,
    alias: &ast::AliasDecl<Span>,
    ctx: &mut Context,
) -> TypecheckResult<AliasDecl> {
    let ty = typecheck_type(&alias.ty, ctx)?;

    Ok(AliasDecl {
        name,
        ty: Box::new(ty),
        span: alias.span.clone(),
    })
}

pub fn typecheck_enum_decl(
    name: Symbol,
    enum_decl: &ast::EnumDecl<Span>,
    ctx: &mut Context,
) -> TypecheckResult<EnumDecl> {
    if name.decl_name.type_params.is_some() {
        return Err(TypecheckError::EnumDeclWithTypeParams {
            span: name.span().clone(),
        });
    }
    assert!(name.type_args.is_none());

    let mut prev_item: Option<(Ident, i128)> = None;

    let mut items = Vec::with_capacity(enum_decl.items.len());
    for item in &enum_decl.items {
        let ord_val = match &item.value {
            Some(val_expr) => {
                let val_expr =
                    typecheck_expr(&val_expr, &Type::Primitive(Primitive::NativeInt), ctx)?;
                let item_ord_val = const_eval_integer(&val_expr, ctx)?.as_i128();

                if let Some((prev_ident, prev_ord_val)) = &prev_item {
                    if item_ord_val <= *prev_ord_val {
                        return Err(TypecheckError::EnumValuesMustBeAscending {
                            span: item.span().clone(),
                            prev_ident: prev_ident.clone(),
                            prev_val: *prev_ord_val,
                            next_ident: item.ident.clone(),
                            next_val: item_ord_val,
                        });
                    }
                }

                item_ord_val
            },

            None => prev_item
                .map(|(_prev_ident, prev_ord_val)| prev_ord_val + 1)
                .unwrap_or(0),
        };

        let item = EnumDeclItem {
            ident: item.ident.clone(),
            span: item.span.clone(),
            value: Some(IntConstant::from(ord_val)),
        };

        prev_item = Some((item.ident.clone(), ord_val));

        items.push(item);
    }

    let enum_decl = EnumDecl {
        name,
        items,
        span: enum_decl.span.clone(),
    };
    Ok(enum_decl)
}
