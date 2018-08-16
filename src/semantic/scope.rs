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
            .with_type(builtin_names::system_boolean(), DeclaredType::Boolean)
            .with_symbol(Symbol::new(
                node::Identifier::parse("WriteLn"),
                FunctionSignature {
                    name: "WriteLn".to_owned(),
                    arg_types: vec![DeclaredType::String],
                    return_type: DeclaredType::None,
                }))
            .with_symbol(Symbol::new(
                node::Identifier::parse("GetMem"),
                FunctionSignature {
                    name: "GetMem".to_owned(),
                    arg_types: vec![DeclaredType::Integer],
                    return_type: DeclaredType::Pointer,
                }))
            .with_symbol(Symbol::new(
                node::Identifier::parse("Dispose"),
                FunctionSignature {
                    name: "Dispose".to_owned(),
                    arg_types: vec![DeclaredType::Pointer],
                    return_type: DeclaredType::None,
                }))
    }
}

impl Scope {
    pub fn with_type(mut self, name: node::Identifier, named_type: DeclaredType) -> Self {
        self.names.insert(name, Named::Type(named_type));
        self
    }

    pub fn with_symbol(mut self, symbol: Symbol) -> Self {
        self.names.insert(symbol.name, Named::Symbol(symbol.decl_type));
        self
    }

    pub fn with_symbols<TIter>(mut self, symbols: TIter) -> Self
        where TIter: IntoIterator,
              TIter::Item: Into<Symbol>
    {
        for symbol in symbols {
            self = self.with_symbol(symbol.into());
        }
        self
    }

    pub fn get_symbol(&self, name: &node::Identifier) -> Option<Symbol> {
        match self.names.get(name) {
            Some(&Named::Symbol(ref symbol_type)) => {
                Some(Symbol::new(name.clone(), symbol_type.clone()))
            },
            _ => None
        }
    }

    pub fn get_type(&self, name: &node::Identifier) -> Option<&DeclaredType> {
        match self.names.get(name) {
            Some(&Named::Type(ref result)) => Some(result),
            _ => None
        }
    }
}
