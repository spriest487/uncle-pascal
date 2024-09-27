use crate::ast::IdentPath;
use crate::ast::TypeDeclName;
use crate::typ::{typecheck_type_params, Context, TypeArgsResolver};
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
    pub full_path: IdentPath,

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
            full_path: ctx.qualify_name(decl_name.ident.clone()),
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

    pub fn substitute_ty_args(self, args: &impl TypeArgsResolver) -> Self {
        let new_args = self
            .type_args
            .as_ref()
            .and_then(|name_type_args| {
                let items = name_type_args
                    .items
                    .iter()
                    .cloned()
                    .map(|arg| arg.substitute_type_args(args));

                Some(TypeList::new(items, name_type_args.span().clone()))
            });

        Symbol {
            type_args: new_args,
            ..self
        }
    }
    
    pub fn ident(&self) -> &Ident {
        self.full_path.last()
    }
}

impl From<IdentPath> for Symbol {
    fn from(value: IdentPath) -> Self {
        Symbol {
            full_path: value,
            
            type_args: None,
            type_params: None,
        }
    }
}

impl Specializable for Symbol {
    type GenericID = IdentPath;

    /// is this type missing specialization e.g. it has type params and not type arguments
    fn is_unspecialized_generic(&self) -> bool {
        if self.type_params.is_none() {
            return false;
        }
        self.type_args.is_none()
    }

    fn name(&self) -> IdentPath {
        self.full_path.clone()
    }
}

impl Spanned for Symbol {
    fn span(&self) -> &Span {
        self.full_path.span()
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.full_path)?;

        if let Some(type_args) = &self.type_args {
            write!(f, "{}", type_args)?;
        } else if let Some(type_params) = &self.type_params {
            write!(f, "{}", type_params)?;
        }

        Ok(())
    }
}
