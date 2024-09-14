use std::fmt;
use pas_common::span::{Span, Spanned};
use pas_syn::ast::{DeclNamed, TypeDeclName};
use pas_syn::{Ident, IdentPath};
use crate::{GenericError, GenericResult, Specializable, Type, TypeList};

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Symbol {
    pub decl_name: TypeDeclName,
    pub qualified: IdentPath,

    pub type_args: Option<TypeList>,
}

impl Symbol {
    pub fn expect_not_unspecialized(&self) -> GenericResult<()> {
        if !self.is_unspecialized_generic() {
            Ok(())
        } else {
            Err(GenericError::IllegalUnspecialized {
                ty: Type::Class(Box::new(self.clone())),
            })
        }
    }
}

impl Specializable for Symbol {
    type GenericID = IdentPath;

    /// is this either a type without type args, or does it already have all the type args it needs?
    fn is_unspecialized_generic(&self) -> bool {
        if self.decl_name.type_params.is_none() {
            return false;
        }

        match &self.type_args {
            None => true,
            Some(type_args) => type_args
                .items
                .iter()
                .any(|arg| arg.is_unspecialized_generic()),
        }
    }

    fn name(&self) -> IdentPath {
        self.qualified.clone()
    }
}

impl DeclNamed for Symbol {
    fn as_local(&self) -> &TypeDeclName {
        &self.decl_name
    }

    fn decl_ty_params(&self) -> &[Ident] {
        match self.decl_name.type_params.as_ref() {
            Some(type_params) => &type_params.items,
            None => &[],
        }
    }
}

impl Spanned for Symbol {
    fn span(&self) -> &Span {
        self.decl_name.span()
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(type_args) = &self.type_args {
            write!(f, "{}{}", self.qualified, type_args)?;
        } else if let Some(type_params) = &self.decl_name.type_params {
            write!(f, "{}{}", self.qualified, type_params)?;
        } else {
            write!(f, "{}", self.qualified)?;
        }

        Ok(())
    }
}