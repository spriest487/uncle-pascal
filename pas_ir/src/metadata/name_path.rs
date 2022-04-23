use std::borrow::Cow;
use crate::metadata::ty::*;
use crate::{InstructionFormatter, Metadata, Module, pas_ty, RawInstructionFormatter};
use pas_syn::{self as syn, Path};
use std::fmt;
use pas_typecheck::Specializable;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct NamePath {
    pub path: Path<String>,
    pub type_args: Option<Vec<Type>>,
}

impl fmt::Display for NamePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        RawInstructionFormatter.format_name(self, f)
    }
}

impl NamePath {
    pub fn from_decl(name: pas_ty::Symbol, metadata: &Metadata) -> Self {
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
            }

            None => None,
        };

        NamePath {
            path: Path::from_parts(path_parts),
            type_args,
        }
    }

    pub fn from_ident_path(ident: &syn::IdentPath, type_args: Option<Vec<Type>>) -> Self {
        let path = Path::from_parts(ident.iter().map(|ident| ident.to_string()));

        NamePath { path, type_args }
    }

    pub fn from_parts<Iter: IntoIterator<Item = String>>(iter: Iter) -> Self {
        NamePath {
            path: Path::from_parts(iter),
            type_args: None,
        }
    }

    pub fn to_pretty_string<'a, TyFormat>(&self, ty_format: TyFormat) -> String
    where
        TyFormat: Fn(&Type) -> Cow<'a, str>,
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
    name: &pas_ty::Symbol,
    type_args: Option<&pas_ty::TypeList>,
    module: &mut Module,
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

    let path_parts = name
        .qualified
        .clone()
        .into_parts()
        .into_iter()
        .map(|ident| ident.to_string());

    let type_args = name.type_args.as_ref().map(|name_type_args_list| {
        name_type_args_list
            .items
            .iter()
            .map(|arg| module.translate_type(arg, type_args))
            .collect()
    });

    NamePath {
        path: pas_syn::Path::from_parts(path_parts),
        type_args,
    }
}
