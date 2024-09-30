use crate::ast::{IdentPath, TypeConstraint};
use crate::ast::TypeDeclName;
use crate::typ::{typecheck_type_params, Context, TypeArgResolver, TypeParam, TypeParamContainer};
use crate::typ::GenericError;
use crate::typ::GenericResult;
use crate::typ::Specializable;
use crate::typ::Type;
use crate::typ::TypeArgList;
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
    pub type_args: Option<TypeArgList>,
}

impl Symbol {
    pub fn with_ty_params(self, type_params: Option<TypeParamList>) -> Self {
        Self {
            type_params,
            ..self
        }
    }
    
    pub fn with_ty_args(self, type_args: Option<TypeArgList>) -> Self {
        Self {
            type_args,
            ..self
        }
    }
    
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

    pub fn substitute_ty_args(self, args: &impl TypeArgResolver) -> Self {
        let new_args = self
            .type_args
            .as_ref()
            .and_then(|name_type_args| {
                let items = name_type_args
                    .items
                    .iter()
                    .cloned()
                    .map(|arg| arg.substitute_type_args(args));

                let span = name_type_args.span().clone();

                Some(TypeArgList::new(items, span))
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

    fn apply_type_args_by_name(self, params: &impl TypeParamContainer, args: &impl TypeArgResolver) -> Self {
        let sym_ty_args = match self.type_args {
            Some(args_list) => {
                let items = args_list.items
                    .into_iter()
                    .map(|arg| arg.apply_type_args_by_name(params, args));
                
                Some(TypeArgList::new(items, args_list.span))
            },
            None => None,
        };
        
        let sym_ty_params = match self.type_params {
            Some(params_list) => {
                let items = params_list.items
                    .into_iter()
                    .map(|param| TypeParam {
                        name: param.name,
                        constraint: param.constraint
                            .map(|c| TypeConstraint {
                                is_ty: c.is_ty.apply_type_args_by_name(params, args),
                                param_ident: c.param_ident,
                                span: c.span,
                            })
                    });

                Some(TypeParamList::new(items, params_list.span))
            },
            None => None,
        };
        
        Symbol {
            full_path: self.full_path,
            type_args: sym_ty_args,
            type_params: sym_ty_params,
        }
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
