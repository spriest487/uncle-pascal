use crate::module_builder::ModuleBuilder;
use crate::syn;
use crate::typ;
use crate::Metadata;
use ir_lang::*;
use std::borrow::Cow;
use typ::Specializable;

pub trait NamePathExt {
    fn from_decl(name: typ::Symbol, metadata: &Metadata) -> Self;
    fn from_ident_path(ident: &syn::IdentPath, type_args: Option<Vec<Type>>) -> Self;
    fn from_parts<Iter: IntoIterator<Item = String>>(iter: Iter) -> Self;
    
    fn to_pretty_string<'a, TyFormat>(&self, ty_format: TyFormat) -> String
        where TyFormat: Fn(&Type) -> Cow<'a, str>;
}

impl NamePathExt for NamePath {
    fn from_decl(name: typ::Symbol, metadata: &Metadata) -> Self {
        let path_parts = name
            .qualified
            .into_parts()
            .into_iter()
            .map(|ident| ident.to_string());

        let type_args = match name.type_args {
            Some(name_type_args) => {
                let types = name_type_args
                    .items
                    .iter()
                    .map(|arg| metadata.find_type(arg))
                    .collect();

                Some(types)
            },

            None => None,
        };

        NamePath {
            path: path_parts.collect(),
            type_args,
        }
    }

    fn from_ident_path(ident: &syn::IdentPath, type_args: Option<Vec<Type>>) -> Self {
        let path = ident.iter()
            .map(|ident| ident.to_string())
            .collect();

        NamePath { path, type_args }
    }

    fn from_parts<Iter: IntoIterator<Item = String>>(iter: Iter) -> Self {
        NamePath {
            path: iter.into_iter().collect(),
            type_args: None,
        }
    }

    fn to_pretty_string<'a, TyFormat>(&self, ty_format: TyFormat) -> String
        where TyFormat: Fn(&Type) -> Cow<'a, str>,
    {
        let mut buf = self.path.join("::");

        if let Some(type_args) = self.type_args.as_ref() {
            buf.push('<');
            for (i, ty_arg) in type_args.iter().enumerate() {
                if i > 0 {
                    buf.push_str(", ");
                }

                let ty_name = ty_format(ty_arg);
                buf.push_str(&ty_name);
            }
            buf.push('>');
        }

        buf
    }
}

pub fn translate_name(
    name: &typ::Symbol,
    type_args: Option<&typ::TypeList>,
    module: &mut ModuleBuilder,
) -> NamePath {
    if name.is_unspecialized_generic() {
        panic!("can't translate unspecialized generic name: {}", name);
    }

    if let Some(name_type_args) = name.type_args.as_ref() {
        if let Some(t) = name_type_args.items.iter().find(|t| t.is_generic_param()) {
            panic!(
                "can't translate name containing generic parameters (found {}): {}",
                t, name
            );
        }
    }

    let path = name
        .qualified
        .clone()
        .into_parts()
        .into_iter()
        .map(|ident| ident.to_string())
        .collect();

    let type_args = name.type_args.as_ref().map(|name_type_args_list| {
        name_type_args_list
            .items
            .iter()
            .map(|arg| module.translate_type(arg, type_args))
            .collect()
    });

    NamePath {
        path,
        type_args,
    }
}
