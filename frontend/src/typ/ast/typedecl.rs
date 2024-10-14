#[cfg(test)]
mod test;

use crate::ast;
use crate::ast::FunctionName;
use crate::ast::Ident;
use crate::ast::StructKind;
use crate::ast::Visibility;
use crate::typ::ast::const_eval_integer;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::InterfaceMethodDecl;
use crate::typ::typecheck_type;
use crate::typ::Context;
use crate::typ::FunctionSig;
use crate::typ::MismatchedImplementation;
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
pub type Method = ast::Method<Typed>;
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

    let mut implements = Vec::new();
    for implements_name in &struct_def.implements {
        let implements_ty = typecheck_type(implements_name, ctx)?;
        match implements_ty {
            iface_ty @ Type::Interface(..) => implements.push(iface_ty),

            invalid_base_ty => return Err(TypeError::InvalidBaseType {
                invalid_base_ty,
                span: implements_name.span().clone(),
                ty: self_ty.clone(),
            }),
        }
    }

    ctx.declare_self_ty(self_ty.clone(), name.span().clone())?;
    ctx.declare_type(
        struct_def.name.ident.clone(),
        self_ty.clone(),
        visibility,
        true,
    )?;

    let mut fields = Vec::new();
    for src_field in &struct_def.fields {
        let field = typecheck_field(src_field, ctx)?;
        fields.push(field);
    }

    let mut methods: Vec<Method> = Vec::new();
    for method in &struct_def.methods {
        let decl = typecheck_method(&method.decl, ctx)?;
        
        let existing: Vec<_> = methods
            .iter()
            .filter(|m| m.decl.ident() == decl.ident())
            .collect();
        
        if !existing.is_empty() {
            
        }

        methods.push(Method {
            access: method.access,
            decl: Rc::new(decl),
        });
    } 
    
    for iface_ty in implements.iter() {
        if let Type::Interface(iface_name) = &iface_ty {
            let iface = ctx.find_iface_def(iface_name.as_ref())
                .map_err(|e| TypeError::from_name_err(e, struct_def.name.span.clone()))?;
            
            let mut missing_methods = Vec::new();
            let mut mismatched_methods = Vec::new();
            
            for iface_method in &iface.methods {
                let expect_sig = FunctionSig::of_decl(&iface_method.decl)
                    .with_self(&self_ty);
                
                let actual_method = methods
                    .iter()
                    .find(|method| {
                        method.decl.name.ident() == iface_method.ident()
                    });
                
                match actual_method {
                    None => {
                        missing_methods.push(MissingImplementation {
                            method_name: iface_method.decl.name.clone(),
                            sig: expect_sig,
                        });
                    }
                    
                    Some(impl_method) => {
                        let actual_sig = FunctionSig::of_decl(&impl_method.decl);

                        if actual_sig != expect_sig {                            
                            mismatched_methods.push(MismatchedImplementation {
                                impl_method_name: impl_method.decl.name.clone(),
                                iface_method_name: iface_method.decl.name.clone(),
                                expect_sig,
                                actual_sig,
                            });
                        }
                    }
                }
            }
            
            if !missing_methods.is_empty() || !mismatched_methods.is_empty() {
                return Err(TypeError::InvalidImplementation {
                    ty: self_ty,
                    span: Span::range(&struct_def.implements).unwrap_or(struct_def.name.span.clone()),
                    missing: missing_methods,
                    mismatched: mismatched_methods,
                });
            }
        } else {
            unreachable!("already checked that only valid types are accepted")
        }
    }

    Ok(StructDef {
        kind: struct_def.kind,
        name,
        span: struct_def.span.clone(),
        implements,
        fields,
        methods,
        forward: struct_def.forward,
    })
}

fn typecheck_method(decl: &ast::FunctionDecl, ctx: &mut Context) -> TypeResult<FunctionDecl> {
    let decl = FunctionDecl::typecheck(decl, false, ctx)?;    
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
        access: field.access,
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

        let method_decl = FunctionDecl::typecheck(&method.decl, false, ctx)?;

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
    
    let mut implements = Vec::new();
    for implements_ty in &variant_def.implements {
        implements.push(typecheck_type(implements_ty, ctx)?);
    }
    
    let variant_ty = Type::variant(name.clone());

    ctx.declare_self_ty(variant_ty.clone(), variant_def.name.span().clone())?;
    ctx.declare_type(
        variant_def.name.ident.clone(),
        variant_ty,
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

    let mut methods = Vec::new();
    for method in &variant_def.methods {
        let decl = typecheck_method(&method.decl, ctx)?;
        methods.push(Method {
            decl: Rc::new(decl),
            access: method.access,
        });
    }

    Ok(VariantDef {
        name: Rc::new(name),
        forward: variant_def.forward,
        cases,
        implements,
        methods,
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
