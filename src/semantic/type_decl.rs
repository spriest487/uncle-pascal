use node;
use syntax;
use semantic::*;
use types;

pub type RecordDecl = node::RecordDecl<ScopedSymbol>;

impl RecordDecl {
    pub fn annotate(decl: &syntax::RecordDecl, scope: &Scope) -> Result<Self, SemanticError> {
        if decl.members.len() == 0 {
            Err(SemanticError::empty_record(decl.name.clone(), decl.context.clone()))
        } else {
            let members = decl.members.iter()
                .map(|member| VarDecl::annotate(member, scope))
                .collect::<Result<_, _>>()?;

            Ok(RecordDecl {
                name: decl.name.clone(),
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
                    types::Symbol::new(member.name.as_str(), member.decl_type.clone())
                })
                .collect()
        };

        types::DeclaredType::Record(record)
    }
}