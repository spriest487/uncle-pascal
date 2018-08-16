use std::rc::Rc;
use linked_hash_map::LinkedHashMap;

use node::{
    self,
    Identifier,
    TypeName,
};
use semantic::*;
use syntax;
use types::Type;

pub type TypeDecl = node::TypeDecl<SemanticContext>;
pub type RecordDecl = node::RecordDecl<SemanticContext>;
pub type RecordMember = node::RecordMember<SemanticContext>;
pub type RecordVariantPart = node::RecordVariantPart<SemanticContext>;
pub type RecordVariantCase = node::RecordVariantCase<SemanticContext>;
pub type EnumerationDecl = node::EnumerationDecl<SemanticContext>;
pub type InterfaceDecl = node::InterfaceDecl<SemanticContext>;
pub type SetDecl = node::SetDecl<SemanticContext>;

impl TypeDecl {
    /**
        Adds type information to a parsed TypeDecl.
        Returns None if the TypeDecl doesn't produce a semantically distinct type (e.g. an alias
        which doesn't declare anything new).
    */
    pub fn annotate(decl: &syntax::TypeDecl,
                    scope: Rc<Scope>)
                    -> SemanticResult<(Option<Self>, Rc<Scope>)> {
        match decl {
            node::TypeDecl::Record(record_decl) => {
                let (record_decl, scope) = RecordDecl::annotate(record_decl, scope.clone())?;


                Ok((Some(node::TypeDecl::Record(record_decl)), scope))
            }

            node::TypeDecl::Enumeration(enum_decl) => {
                let enum_decl = EnumerationDecl::annotate(enum_decl, scope.clone())?;

                let scope = scope.as_ref().clone()
                    .with_enumeration(enum_decl.clone());

                Ok((Some(node::TypeDecl::Enumeration(enum_decl)), Rc::new(scope)))
            }

            node::TypeDecl::Set(set_decl) => {
                let set_decl = SetDecl::annotate(set_decl, scope.clone())?;

                let scope = scope.as_ref().clone()
                    .with_set(set_decl.clone());

                Ok((Some(node::TypeDecl::Set(set_decl)), Rc::new(scope)))
            }

            node::TypeDecl::Alias { alias, of, .. } => {
                let aliased_type = of.resolve(scope.clone())?;

                let scope = scope.as_ref().clone().with_type_alias(alias, aliased_type);

                Ok((None, Rc::new(scope)))
            }

            node::TypeDecl::Interface(interface_decl) => {
                let (interface_decl, scope) = InterfaceDecl::annotate(interface_decl, scope)?;

                Ok((Some(node::TypeDecl::Interface(interface_decl)), scope))
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
                let tag_type = Some(&tag.decl_type);
                let (tag_value, _) = Expression::annotate(&case.tag_value, tag_type, scope.clone())?;

                let members = case.members.iter()
                    .map(|case_member| RecordMember::annotate(case_member, scope.clone()))
                    .collect::<SemanticResult<_>>()?;

                Ok(RecordVariantCase {
                    tag_value: tag_value.into_const_expr(tag_type)?,
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
                    scope: Rc<Scope>) -> SemanticResult<(Self, Rc<Scope>)> {
        let context = SemanticContext {
            token: decl.context.token().clone(),
            scope: scope.clone(),
        };

        if decl.members.len() == 0 && decl.variant_part.is_none() {
            return Err(SemanticError::empty_record(scope.namespace_qualify(&decl.name), context));
        }

        let members = decl.members.iter()
            .map(|member| {
                RecordMember::annotate(member, scope.clone())
            })
            .collect::<Result<_, _>>()?;

        let variant_part = match &decl.variant_part {
            Some(part) => Some(RecordVariantPart::annotate(part, scope.clone())?),
            None => None,
        };

        let record_decl = RecordDecl {
            name: decl.name.clone(),
            kind: decl.kind,
            context,
            members,
            variant_part,
        };

        let scope = match record_decl.kind {
            node::RecordKind::Record => scope.as_ref().clone()
                .with_record(record_decl.clone()),
            node::RecordKind::Class => scope.as_ref().clone()
                .with_class(record_decl.clone()),
        };

        Ok((record_decl, Rc::new(scope)))
    }

    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }

    pub fn qualified_name(&self) -> Identifier {
        self.scope().namespace_qualify(&self.name)
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

    pub fn qualified_name(&self) -> Identifier {
        self.context.scope.namespace_qualify(&self.name)
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

                node::SetEnumeration::Named(enum_id.clone())
            }

            inline @ node::SetEnumeration::Inline(_) => inline.clone(),
        };

        Ok(node::SetDecl {
            name: set_decl.name.clone(),
            enumeration,
            context,
        })
    }

    pub fn qualified_name(&self) -> Identifier {
        self.context.scope.namespace_qualify(&self.name)
    }
}

impl InterfaceDecl {
    fn valid_self_arg(arg: &syntax::FunctionArgSignature) -> bool {
        /* the first parameter should have no modifiers and use the magic typename "Self" */
        let scalar = match arg.decl_type.as_scalar() {
            Some(scalar) => scalar,
            None => return false,
        };

        if scalar.name.namespace.len() > 0 || scalar.name.name != "Self" {
            return false;
        }

        if arg.modifier.is_some() {
            return false;
        }

        true
    }

    pub fn get_implementation_sig(&self, name: &str, self_type: Type) -> Option<FunctionSignature> {
        let mut sig = self.methods.get(name).cloned()?;
        sig.args[0] = FunctionArgSignature {
            decl_type: self_type,
            modifier: None,
        };

        Some(sig)
    }

    pub fn qualified_name(&self) -> Identifier {
        self.scope().namespace_qualify(&self.name)
    }

    pub fn annotate(interface_decl: &syntax::InterfaceDecl,
                    scope: Rc<Scope>)
                    -> SemanticResult<(Self, Rc<Scope>)> {
        let context = SemanticContext {
            token: interface_decl.context.token().clone(),
            scope: scope.clone(),
        };

        let mut methods = LinkedHashMap::new();

        let full_name = scope.namespace_qualify(&interface_decl.name);

        for (method_name, parsed_sig) in interface_decl.methods.iter() {
            assert!(!methods.contains_key(method_name),
                    "interface should not contain duplicate methods");
            assert!(parsed_sig.args.len() >= 1,
                    "arg list length should be >= 1 for parsed interface methods");

            if !Self::valid_self_arg(&parsed_sig.args[0]) {
                return Err(SemanticError::invalid_self_arg(parsed_sig.args[0].clone(), context));
            }

            /* hack: replace `Self` with `var` as a placeholder so this passes typechecking
            (the actual type we'll store is this interface itself, which isn't in scope yet, can
            we forward declare it?) */
            let mut untyped_sig = parsed_sig.clone();
            untyped_sig.args[0] = syntax::FunctionArgSignature {
                decl_type: TypeName::UntypedRef {
                    context: parsed_sig.args[0].decl_type.context().clone()
                },
                modifier: None,
            };

            let mut func_sig = FunctionSignature::annotate(&untyped_sig, scope.clone())?;

            /* add the proper type for `Self` */
            func_sig.args[0] = FunctionArgSignature {
                decl_type: Type::AnyImplementation(full_name.clone()),
                modifier: None,
            };

            methods.insert(method_name.to_string(), func_sig);
        }

        let interface = InterfaceDecl {
            name: interface_decl.name.clone(),
            methods,
            context,
        };

        let scope = Rc::new(scope.as_ref().clone()
            .with_interface(interface.clone())?);

        Ok((interface, scope))
    }

    pub fn scope(&self) -> &Scope {
        self.context.scope.as_ref()
    }
}