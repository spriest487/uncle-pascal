use node;
use syntax;
use semantic::*;
use types;

pub type RecordDecl = node::RecordDecl<ScopedSymbol>;

impl RecordDecl {
    pub fn annotate(decl: &syntax::RecordDecl,
                    scope: &Scope) -> Result<Self, SemanticError> {
        if decl.members.len() == 0 {
            Err(SemanticError::empty_record(decl.name.clone(), decl.context.clone()))
        } else {
            let members = decl.members.iter()
                .map(|member| VarDecl::annotate(member, scope))
                .collect::<Result<_, _>>()?;

            let qualified_name = scope.qualify_local_name(&decl.name);

            Ok(RecordDecl {
                name: qualified_name.to_string(),
                context: decl.context.clone(),
                members
            })
        }
    }

    pub fn record_type(&self) -> types::DeclaredType {
        let record = types::DeclaredRecord {
            name: node::Identifier::from(self.name.as_str()),
            members: self.members.iter()
                .map(|member| {
                    types::Symbol::new(member.name.as_str(), member.decl_type.clone())
                })
                .collect()
        };

        types::DeclaredType::Record(record)
    }
}
