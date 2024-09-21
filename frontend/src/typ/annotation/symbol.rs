use std::fmt;
use common::span::Span;
use common::span::Spanned;
use crate::ast::ParameterizedName;
use crate::ast::TypeDeclName;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::typ::Type;
use crate::typ::Specializable;
use crate::typ::GenericResult;
use crate::typ::GenericError;
use crate::typ::TypeList;

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Symbol {
    // TODO: can we get rid of this and just store the type params etc separately?
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

impl From<IdentPath> for Symbol {
    fn from(value: IdentPath) -> Self {
        Symbol {
            decl_name: TypeDeclName {
                span: value.path_span(),
                ident: value.last().clone(),
                type_params: None,
            },
            qualified: value,
            type_args: None,
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

impl ParameterizedName for Symbol {
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
