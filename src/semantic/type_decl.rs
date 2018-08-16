use std::rc::Rc;
use node::{self, Identifier};
use semantic::*;
use syntax;

pub type TypeDecl = node::TypeDecl<ScopedSymbol, SemanticContext>;
pub type RecordDecl = node::RecordDecl<ScopedSymbol, SemanticContext>;
pub type EnumerationDecl = node::EnumerationDecl<SemanticContext>;
pub type SetDecl = node::SetDecl<SemanticContext>;

impl TypeDecl {
    pub fn annotate(decl: &syntax::TypeDecl, scope: Rc<Scope>) -> SemanticResult<Self> {
        match decl {
            node::TypeDecl::Record(record_decl) => {
                let record_decl = RecordDecl::annotate(record_decl, scope.clone())?;
                Ok(node::TypeDecl::Record(record_decl))
            }

            node::TypeDecl::Enumeration(enum_decl) => {
                let enum_decl = EnumerationDecl::annotate(enum_decl, scope.clone())?;
                Ok(node::TypeDecl::Enumeration(enum_decl))
            }

            node::TypeDecl::Set(set_decl) => {
                let set_decl = SetDecl::annotate(set_decl, scope.clone())?;
                Ok(node::TypeDecl::Set(set_decl))
            }

            node::TypeDecl::Alias { alias, of, context } => {
                let context = SemanticContext {
                    scope: scope.clone(),
                    token: context.token().clone(),
                };

                let aliased_type = scope.get_type(&of)
                    .map_err(|not_found| {
                        SemanticError::unknown_type(not_found, context.clone())
                    })?;

                Ok(node::TypeDecl::Alias {
                    alias: alias.clone(),
                    of: aliased_type,
                    context,
                })
            }
        }
    }
}

impl RecordDecl {
    pub fn annotate(decl: &syntax::RecordDecl,
                    scope: Rc<Scope>) -> Result<Self, SemanticError> {
        let context = SemanticContext {
            token: decl.context.token().clone(),
            scope: scope.clone(),
        };

        if decl.members.len() == 0 {
            Err(SemanticError::empty_record(scope.qualify_local_name(&decl.name), context))
        } else {
            let members = decl.members.iter()
                .map(|member| {
                    VarDecl::annotate(member, scope.clone())
                })
                .collect::<Result<_, _>>()?;

            Ok(RecordDecl {
                name: decl.name.clone(),
                kind: decl.kind,
                context,
                members,
            })
        }
    }

    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }

    pub fn qualified_name(&self) -> Identifier {
        self.scope().qualify_local_name(&self.name)
    }
}

impl EnumerationDecl {
    pub fn annotate(enumeration: &syntax::EnumerationDecl, scope: Rc<Scope>) -> SemanticResult<Self> {
        let context = SemanticContext {
            scope: scope.clone(),
            token: enumeration.context.token().clone(),
        };

        Ok(EnumerationDecl {
            name: enumeration.name.clone(),
            names: enumeration.names.clone(),
            context,
        })
    }
}

impl SetDecl {
    pub fn annotate(set_decl: &syntax::SetDecl,
                    scope: Rc<Scope>) -> SemanticResult<Self> {
        let context = SemanticContext {
            scope: scope.clone(),
            token: set_decl.context.token().clone(),
        };

        let enumeration = match &set_decl.enumeration {
            node::SetEnumeration::Named(enum_name) => {
                let (enum_id, _) = scope.get_enumeration(enum_name)
                    .ok_or_else(|| {
                        SemanticError::unknown_type(enum_name.clone(), context.clone())
                    })?;

                node::SetEnumeration::Named(enum_id)
            }

            inline @ node::SetEnumeration::Inline(_) => inline.clone(),
        };

        Ok(node::SetDecl{
            name: set_decl.name.clone(),
            enumeration,
            context,
        })
    }
}