use std::fmt;

use types::*;
use std::collections::hash_map::*;
use syntax;

#[derive(Clone, Debug)]
pub enum SemanticError {
    UnknownType(Identifier),
}

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &SemanticError::UnknownType(ref missing_type) => {
                write!(f, "type not found: {}", missing_type)
            }
        }
    }
}

#[derive(Clone, Debug)]
enum Named {
    Type(DeclaredType),
    Symbol(DeclaredType),
}

#[derive(Clone, Debug)]
struct Scope {
    names: HashMap<Identifier, Named>
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
    fn with_type(mut self, name: Identifier, named_type: DeclaredType) -> Self {
        self.names.insert(name, Named::Type(named_type));
        self
    }

    fn with_symbol(mut self, name: Identifier, named_symbol: DeclaredType) -> Self {
        self.names.insert(name, Named::Symbol(named_symbol));
        self
    }

    fn find_type(&self, name: &Identifier) -> Option<&DeclaredType> {
        self.names.get(name).and_then(|named| match named {
            &Named::Type(ref named_type) => Some(named_type),
            _ => None,
        })
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    vars: Vec<Symbol>,
}

pub fn type_check(program: syntax::Program) -> Result<Program, SemanticError> {
    let global_scope = Scope::default();

    let typed_vars = program.vars.decls.iter()
        .map(|v| -> Result<Symbol, SemanticError> {
            let var_type = global_scope
                .find_type(&v.decl_type)
                .cloned()
                .ok_or_else(|| SemanticError::UnknownType(v.decl_type.clone()))?;

            Ok(Symbol::new(&v.name, var_type))
        })
        .collect::<Result<Vec<_>, _>>();

    Ok(Program {
        vars: typed_vars?
    })
}