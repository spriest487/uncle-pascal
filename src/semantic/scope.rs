use std::collections::hash_map::*;

use node;
use types::*;

#[derive(Clone, Debug)]
pub enum Named {
    Type(DeclaredType),
    Symbol(DeclaredType),
}

#[derive(Clone, Debug)]
pub struct Scope {
    names: HashMap<node::Identifier, Named>
}

impl Default for Scope {
    fn default() -> Self {
        let scope = Self {
            names: HashMap::new(),
        };

        scope.with_type(builtin_names::system_integer(), DeclaredType::Integer)
            .with_type(builtin_names::system_string(), DeclaredType::String)
            .with_type(builtin_names::system_pointer(), DeclaredType::Pointer)
    }
}

impl Scope {
    pub fn with_type(mut self, name: node::Identifier, named_type: DeclaredType) -> Self {
        self.names.insert(name, Named::Type(named_type));
        self
    }

    pub fn with_symbol(mut self, name: node::Identifier, named_symbol: DeclaredType) -> Self {
        self.names.insert(name, Named::Symbol(named_symbol));
        self
    }

    pub fn find_type(&self, name: &node::Identifier) -> Option<&DeclaredType> {
        self.names.get(name).and_then(|named| match named {
            &Named::Type(ref named_type) => Some(named_type),
            _ => None,
        })
    }
}
