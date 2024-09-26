use crate::ast::IdentPath;
use crate::ast::TypeDeclName;
use crate::typ::{typecheck_type_params, Context};
use crate::typ::GenericError;
use crate::typ::GenericResult;
use crate::typ::Specializable;
use crate::typ::Type;
use crate::typ::TypeList;
use crate::typ::TypeParamList;
use crate::typ::TypeResult;
use common::span::Span;
use common::span::Spanned;
use std::fmt;
use crate::Ident;

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Symbol {
    pub qualified: IdentPath,

    pub type_params: Option<TypeParamList>,
    pub type_args: Option<TypeList>,
}

impl Symbol {
    /// Creates a symbol of the given decl, using the context's current local namespace 
    /// to create the fully qualified name
    pub fn from_local_decl_name(decl_name: &TypeDeclName, ctx: &mut Context) -> TypeResult<Self> {
        let type_params = match &decl_name.type_params {
            Some(params) => {
                let params = typecheck_type_params(params, ctx)?;
                Some(params)
            },

            None => None,
        };

        let sym = Symbol {
            qualified: ctx.qualify_name(decl_name.ident.clone()),
            type_args: None,
            type_params,
        };
        
        Ok(sym)
    }
    
    pub fn expect_not_unspecialized(&self) -> GenericResult<()> {
        if !self.is_unspecialized_generic() {
            Ok(())
        } else {
            Err(GenericError::IllegalUnspecialized {
                ty: Type::Class(Box::new(self.clone())),
            })
        }
    }
    
    pub fn ident(&self) -> &Ident {
        self.qualified.last()
    }
}

impl From<IdentPath> for Symbol {
    fn from(value: IdentPath) -> Self {
        Symbol {
            qualified: value,
            
            type_args: None,
            type_params: None,
        }
    }
}

impl Specializable for Symbol {
    type GenericID = IdentPath;

    /// is this either a type without type args, or does it already have all the type args it needs?
    fn is_unspecialized_generic(&self) -> bool {
        if self.type_params.is_none() {
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

impl Spanned for Symbol {
    fn span(&self) -> &Span {
        self.qualified.span()
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.qualified)?;

        if let Some(type_args) = &self.type_args {
            write!(f, "{}", type_args)?;
        } else if let Some(type_params) = &self.type_params {
            write!(f, "{}", type_params)?;
        }

        Ok(())
    }
}
