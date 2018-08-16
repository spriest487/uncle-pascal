use node;
use semantic::*;
use syntax;
use types;

pub type RecordDecl = node::RecordDecl<ScopedSymbol>;

impl RecordDecl {
    pub fn annotate(decl: &syntax::RecordDecl,
                    scope: &Scope) -> Result<Self, SemanticError> {
        if decl.members.len() == 0 {
            Err(SemanticError::empty_record(decl.name.clone(), decl.context.clone()))
        } else {
            let members = decl.members.iter()
                .map(|member| VarDecl::annotate(member, scope, SemanticVarsKind::Local))
                .collect::<Result<_, _>>()?;

            let qualified_name = if decl.name.namespace.len() == 0 {
                scope.qualify_local_name(&decl.name.name)
            } else {
                return Err(SemanticError::illegal_name(decl.name.to_string(),
                                                       decl.context.clone()));
            };

            Ok(RecordDecl {
                name: qualified_name,
                context: decl.context.clone(),
                members
            })
        }
    }

    pub fn record_type(&self) -> types::DeclaredType {
        let record = types::DeclaredRecord {
            name: self.name.clone(),
            members: self.members.iter()
                .map(|member| {
                    types::Symbol::new(member.name.clone(), member.decl_type.clone())
                })
                .collect()
        };

        types::DeclaredType::Record(record)
    }
}
