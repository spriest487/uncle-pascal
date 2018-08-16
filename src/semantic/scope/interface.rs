use std::{
    collections::hash_map::{
        HashMap,
        Entry
    },
};
use super::{
    NamedFunction,
    expect_overload_ok,
};
use node::{
    Identifier,
};
use semantic::{
    InterfaceDecl,
    FunctionDecl,
    SemanticResult,
    SemanticError,
};

#[derive(Clone, Debug, Default)]
pub struct InterfaceMember {
    pub impls_by_type: HashMap<Identifier, NamedFunction>,
}

#[derive(Clone, Debug)]
pub struct Interface {
    pub decl: InterfaceDecl,
    pub members: HashMap<String, InterfaceMember>,
}

impl Interface {
    pub fn new(decl: InterfaceDecl) -> Self {
        Interface {
            members: decl.functions.iter()
                .map(|(fn_name, _)| (fn_name.clone(), InterfaceMember::default()))
                .collect(),
            decl,
        }
    }

    /* if impl_entry returns None, the function name isn't part of this interface */
    fn impl_entry(&mut self, for_type: Identifier, func: &str)
                  -> Option<Entry<Identifier, NamedFunction>> {
        let member = self.members.get_mut(func)?;

        Some(member.impls_by_type.entry(for_type))
    }

    pub fn get_impl(&self, for_type: &Identifier, func: &str) -> Option<&FunctionDecl> {
        self.members.get(func)
            .and_then(|member| member.impls_by_type.get(for_type))
            .map(|member_for_type| &member_for_type.decl)
    }

    fn qualified_name(&self) -> Identifier {
        self.decl.scope().namespace_qualify(&self.decl.name)
    }

    pub fn add_impl(&mut self,
                impl_type: Identifier,
                new_func: NamedFunction)
                -> SemanticResult<()> {
        let interface_id = self.qualified_name();

        let impl_entry = self.impl_entry(impl_type, &new_func.decl.name)
            .ok_or_else(|| SemanticError::unknown_symbol(
                interface_id.child(&new_func.decl.name),
                new_func.decl.context.clone(),
            ))?;

        match impl_entry {
            Entry::Occupied(mut slot) => {
                expect_overload_ok(
                    &new_func,
                    slot.get(),
                    Some(&interface_id),
                )?;

                let existing = slot.get_mut();
                existing.defined = existing.defined || new_func.defined;
            }

            Entry::Vacant(slot) => {
                slot.insert(new_func);
            }
        }

        Ok(())
    }

    pub fn impls_for_type(&self, type_id: &Identifier) -> Vec<&FunctionDecl> {
        let mut result = Vec::new();
        for (_fn_name, member) in self.members.iter() {
            if let Some(impl_for_type) = member.impls_by_type.get(type_id) {
                result.push(&impl_for_type.decl)
            }
        }

        result
    }
}
