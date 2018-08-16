use std::rc::Rc;
use node;
use semantic::*;
use syntax;

pub type TypeDecl = node::TypeDecl<ScopedSymbol, SemanticContext>;
pub type RecordDecl = node::RecordDecl<ScopedSymbol, SemanticContext>;

impl TypeDecl {
    pub fn annotate(decl: &syntax::TypeDecl, scope: Rc<Scope>) -> SemanticResult<Self> {
        match decl {
            node::TypeDecl::Record(record_decl) => {
                let record_decl = RecordDecl::annotate(record_decl, scope.clone())?;

                Ok(node::TypeDecl::Record(record_decl))
            },

            node::TypeDecl::Alias { alias, of, context } => {
                let context = SemanticContext {
                    scope: scope.clone(),
                    token: context.token().clone(),
                };

                let aliased_type = scope.get_type(&of)
                    .ok_or_else(|| {
                        SemanticError::unknown_type(of.clone(), context.clone())
                    })?;

                Ok(node::TypeDecl::Alias {
                    alias: alias.clone(),
                    of: aliased_type.with_indirection(of.indirection),
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
            Err(SemanticError::empty_record(decl.name.clone(), context))
        } else {
            let members = decl.members.iter()
                .map(|member| VarDecl::annotate(member, scope.clone(), SemanticVarsKind::Local))
                .collect::<Result<_, _>>()?;

            let qualified_name = if decl.name.namespace.len() == 0 {
                scope.qualify_local_name(&decl.name.name)
            } else {
                return Err(SemanticError::illegal_name(decl.name.to_string(), context));
            };

            Ok(RecordDecl {
                name: qualified_name,
                kind: decl.kind,
                context,
                members
            })
        }
    }

    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }
}
