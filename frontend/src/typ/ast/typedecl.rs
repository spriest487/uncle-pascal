#[cfg(test)]
mod test;

use crate::ast;
use crate::ast::FunctionName;
use crate::ast::Ident;
use crate::ast::StructKind;
use crate::ast::Visibility;
use crate::typ::ast::const_eval_integer;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_func_decl;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::InterfaceMethodDecl;
use crate::typ::typecheck_type;
use crate::typ::Context;
use crate::typ::FunctionSig;
use crate::typ::MissingImplementation;
use crate::typ::NameError;
use crate::typ::Primitive;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Typed;
use crate::IntConstant;
use common::span::Span;
use common::span::Spanned;
use std::rc::Rc;

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
    struct_def: &ast::StructDef<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<StructDef> {
    let self_ty = match struct_def.kind {
        StructKind::Record | StructKind::PackedRecord => Type::record(name.clone()),
        StructKind::Class => Type::class(name.clone()),
    };

    let implements: Vec<Type> = struct_def.implements
        .iter()
        .map(|implements_ty| typecheck_type(implements_ty, ctx))
        .collect::<TypeResult<_>>()?;

    ctx.declare_self_ty(self_ty.clone(), name.span().clone())?;
    ctx.declare_type(
        struct_def.name.ident.clone(),
        self_ty.clone(),
        visibility,
        true,
    )?;

    let mut members = Vec::new();
    for member in &struct_def.members {
        match member {
            ast::StructMember::Field(field) => {
                let field = typecheck_field(&field, ctx)?; 
                members.push(field.into());
            }
            
            ast::StructMember::MethodDecl(decl) => {
                let decl = typecheck_method(decl, ctx)?;
                
                members.push(decl.into());
            }
        }
    }
    
    for iface_ty in implements.iter() {
        if let Type::Interface(iface_name) = &iface_ty {
            let iface = ctx.find_iface_def(iface_name.as_ref())
                .map_err(|e| TypeError::from_name_err(e, struct_def.name.span.clone()))?;
            
            let mut missing_methods = Vec::new();
            
            for method in &iface.methods {
                let expect_sig = FunctionSig::of_decl(&method.decl)
                    .with_self(&self_ty);
                
                let matching_method = members
                    .iter()
                    .filter_map(|m| match m {
                        ast::StructMember::MethodDecl(method) => Some(method),
                        _ => None,
                    })
                    .any(|decl| {
                        if decl.name.ident() != method.ident() {
                            return false;
                        }
                        
                        let actual_sig = FunctionSig::of_decl(decl);
                        actual_sig == expect_sig
                    });
                
                if !matching_method {
                    missing_methods.push(MissingImplementation {
                        iface_ty: iface_ty.clone(),
                        method: method.ident().clone(),
                        sig: expect_sig, 
                    });
                }
            }
            
            if !missing_methods.is_empty() {
                return Err(TypeError::IncompleteImplementation {
                    ty: self_ty,
                    span: Span::range(&struct_def.implements).unwrap_or(struct_def.name.span.clone()),
                    missing: missing_methods
                });
            }
        }
    }

    Ok(StructDef {
        kind: struct_def.kind,
        name,
        span: struct_def.span.clone(),
        implements,
        members,
        forward: struct_def.forward,
    })
}

fn typecheck_method(decl: &ast::FunctionDecl, ctx: &mut Context) -> TypeResult<FunctionDecl> {
    let decl = typecheck_func_decl(decl, false, ctx)?;

    if !decl.mods.is_empty() {
        return Err(TypeError::InvalidMethodModifiers {
            mods: decl.mods.clone(),
            span: {
                let start = decl.mods[0].span();
                let end = decl.mods[decl.mods.len() - 1].span();
                start.to(end)
            },
        });
    }
    
    Ok(decl)
}

fn typecheck_field(
    field: &ast::Field,
    ctx: &mut Context
) -> TypeResult<Field> {
    let ty = typecheck_type(&field.ty, ctx)?.clone();

    ty.expect_sized(ctx, &field.span)?;

    let field = Field {
        ty,
        span: field.span.clone(),
        ident: field.ident.clone(),
    };
    
    Ok(field)
}

pub fn typecheck_iface(
    name: Symbol,
    iface: &ast::InterfaceDecl<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<InterfaceDecl> {
    // declare Self type - type decls are always in their own scope so we don't need to push
    // another one
    ctx.declare_self_ty(Type::MethodSelf, iface.name.span().clone())?;
    ctx.declare_type(
        iface.name.ident.clone(),
        Type::interface(name.full_path.clone()),
        visibility,
        true
    )?;

    let mut methods: Vec<InterfaceMethodDecl> = Vec::new();
    for method in &iface.methods {
        if let Some(existing) = methods
            .iter()
            .find(|other| other.decl.name.ident == method.decl.name.ident)
        {
            let method_path = name
                .full_path
                .clone()
                .child(method.decl.name.ident().clone());

            return Err(TypeError::NameError {
                err: NameError::AlreadyDefined {
                    ident: method_path,
                    existing: existing.span().clone(),
                },
                span: method.decl.name.span().clone(),
            });
        }

        let method_decl = typecheck_func_decl(&method.decl, false, ctx)?;

        methods.push(ast::InterfaceMethodDecl { decl: Rc::new(method_decl) });
    }

    Ok(InterfaceDecl {
        name,
        forward: iface.forward,
        span: iface.span.clone(),
        methods,
    })
}

pub fn typecheck_variant(
    name: Symbol,
    variant_def: &ast::VariantDef<Span>,
    visibility: Visibility,
    ctx: &mut Context,
) -> TypeResult<VariantDef> {
    if variant_def.cases.is_empty() {
        return Err(TypeError::EmptyVariant(Box::new(variant_def.clone())));
    }
    
    ctx.declare_self_ty(Type::MethodSelf, variant_def.name.span().clone())?;
    ctx.declare_type(
        variant_def.name.ident.clone(),
        Type::variant(name.full_path.clone()),
        visibility,
        true
    )?;

    let mut cases = Vec::with_capacity(variant_def.cases.len());
    for case in &variant_def.cases {
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
        forward: variant_def.forward,
        cases,
        span: variant_def.span().clone(),
    })
}

pub fn typecheck_alias(
    name: Symbol,
    alias: &ast::AliasDecl<Span>,
    ctx: &mut Context,
) -> TypeResult<AliasDecl> {
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
) -> TypeResult<EnumDecl> {
    if name.type_params.is_some() {
        return Err(TypeError::EnumDeclWithTypeParams {
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
                        return Err(TypeError::EnumValuesMustBeAscending {
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
