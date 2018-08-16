use node;
use syntax;
use semantic::*;

pub type RecordDecl = node::RecordDecl<Symbol>;

impl RecordDecl {
    pub fn annotate(decl: &syntax::RecordDecl, scope: &Scope) -> Result<Self, SemanticError> {
        let members = decl.members.iter()
            .map(|member| VarDecl::annotate(member, scope))
            .collect::<Result<_, _>>()?;

        Ok(RecordDecl {
            name: decl.name.clone(),
            members
        })
    }
}
