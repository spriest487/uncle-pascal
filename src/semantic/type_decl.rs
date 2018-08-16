use std::rc::Rc;
use node::{self, Identifier};
use semantic::*;
use syntax;

pub type TypeDecl = node::TypeDecl<SemanticContext>;
pub type RecordDecl = node::RecordDecl<SemanticContext>;
pub type RecordMember = node::RecordMember<SemanticContext>;
pub type RecordVariantPart = node::RecordVariantPart<SemanticContext>;
pub type RecordVariantCase = node::RecordVariantCase<SemanticContext>;
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

                let aliased_type = of.resolve(scope.clone())?;

                Ok(node::TypeDecl::Alias {
                    alias: alias.clone(),
                    of: aliased_type,
                    context,
                })
            }
        }
    }
}

impl RecordMember {
    pub fn annotate(member: &syntax::RecordMember, scope: Rc<Scope>) -> SemanticResult<Self> {
        let context = SemanticContext {
            token: member.context.token().clone(),
            scope: scope.clone(),
        };

        let decl_type = member.decl_type.resolve(scope)?;

        Ok(RecordMember {
            name: member.name.clone(),
            decl_type,
            context,
        })
    }
}

impl RecordVariantPart {
    pub fn annotate(part: &syntax::RecordVariantPart, scope: Rc<Scope>) -> SemanticResult<Self> {
        let context = SemanticContext {
            token: part.context.token().clone(),
            scope: scope.clone(),
        };

        let tag = RecordMember::annotate(&part.tag, scope.clone())?;

        let cases = part.cases.iter()
            .map(|case| {
                let tag_value = Expression::annotate(&case.tag_value, scope.clone())?;

                let members = case.members.iter()
                    .map(|case_member| RecordMember::annotate(case_member, scope.clone()))
                    .collect::<SemanticResult<_>>()?;

                Ok(RecordVariantCase {
                    tag_value,
                    members,
                })
            })
            .collect::<SemanticResult<_>>()?;

        Ok(RecordVariantPart {
            tag,
            cases,
            context,
        })
    }
}

impl RecordDecl {
    pub fn annotate(decl: &syntax::RecordDecl,
                    scope: Rc<Scope>) -> SemanticResult<Self> {
        let context = SemanticContext {
            token: decl.context.token().clone(),
            scope: scope.clone(),
        };

        if decl.members.len() == 0 {
            Err(SemanticError::empty_record(scope.qualify_local_name(&decl.name), context))
        } else {
            let members = decl.members.iter()
                .map(|member| {
                    RecordMember::annotate(member, scope.clone())
                })
                .collect::<Result<_, _>>()?;

            let variant_part = match &decl.variant_part {
                Some(part) => Some(RecordVariantPart::annotate(part, scope)?),
                None => None,
            };

            Ok(RecordDecl {
                name: decl.name.clone(),
                kind: decl.kind,
                context,
                members,
                variant_part,
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