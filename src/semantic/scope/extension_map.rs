use std::{
    collections::hash_map::{
        Entry,
        HashMap,
    },
};
use node::{
    Identifier,
    SELF_ARG_NAME,
};
use types::Type;
use semantic::{
    scope::NamedFunction,
    SemanticResult,
    SemanticError,
    FunctionDecl,
};

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
struct ExtensionKey {
    name: String,
//    sig: FunctionSignature,
}

impl ExtensionKey {
    pub fn new(from_decl: &FunctionDecl) -> Self {
        Self {
            name: from_decl.name.clone(),
        }
    }
}

#[derive(Clone, Debug)]
struct TypeExtensions {
    by_key: HashMap<ExtensionKey, NamedFunction>,
}

impl TypeExtensions {
    pub fn new() -> Self {
        Self {
            by_key: HashMap::new()
        }
    }

    pub fn add_extension(&mut self, extension_func: NamedFunction) -> SemanticResult<()> {
        let key = ExtensionKey::new(&extension_func.decl);

        match self.by_key.entry(key) {
            Entry::Vacant(entry) => { entry.insert(extension_func); }
            Entry::Occupied(mut entry) => {
                if entry.get().defined {
                    return Err(SemanticError::multiple_function_def(
                        extension_func.decl,
                        entry.get().decl.clone(),
                        Option::None::<Identifier>,
                    ));
                }

                entry.insert(extension_func);
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct ExtensionMap {
    by_type: HashMap<Type, TypeExtensions>
}

impl ExtensionMap {
    pub fn new() -> Self {
        Self {
            by_type: HashMap::new(),
        }
    }

    pub fn add_extension(&mut self, for_type: Type, extension: NamedFunction) -> SemanticResult<()> {
        assert!(!extension.decl.args.is_empty());
        assert_eq!(SELF_ARG_NAME, extension.decl.args[0].name);
        assert_eq!(None, extension.decl.implements);

        self.by_type.entry(for_type)
            .or_insert_with(TypeExtensions::new)
            .add_extension(extension)
    }

    pub fn reference_all(&mut self, other: &ExtensionMap) {
        for for_type in other.by_type.keys() {
            self.reference(other, for_type);
        }
    }

    pub fn reference(&mut self, other: &ExtensionMap, for_type: &Type) {
        if let Some(extensions) = other.by_type.get(for_type) {
            for (key, extension) in &extensions.by_key {
                let mut type_extensions = self.by_type.entry(for_type.clone())
                    .or_insert_with(TypeExtensions::new);

                type_extensions.by_key.insert(key.clone(), extension.clone());
            }
        }
    }

    pub fn find(&self, for_type: &Type, name: &str) -> Option<&FunctionDecl> {
        let key = ExtensionKey { name: name.to_string() };
        self.by_type.get(for_type)
            .and_then(|extensions| extensions.by_key.get(&key))
            .map(|extension| &extension.decl)
    }
}