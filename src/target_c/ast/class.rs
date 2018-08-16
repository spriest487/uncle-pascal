use std::fmt;

use semantic::{
    self,
    Scope,
};
use node::Identifier;
use types::Type;
use target_c::{
    ast::{
        TranslationResult,
        FunctionDecl,
    }
};

pub struct Class {
    pascal_name: String,
    destructor: Option<String>,
}

impl Class {
    pub fn translate(class: &semantic::RecordDecl, unit_scope: &Scope) -> TranslationResult<Self> {
        let (full_name, _) = unit_scope.get_class(&Identifier::from(&class.name)).unwrap();
        let destructor = unit_scope.get_destructor(&Type::Class(full_name.clone()))
            .map(|(_, dtor_func)| FunctionDecl::translate_name(dtor_func));

        Ok(Class {
            pascal_name: full_name.to_string(),
            destructor,
        })
    }

    pub fn write_init(&self, mut out: impl fmt::Write) -> fmt::Result {
        let destructor_ptr = match &self.destructor {
            Some(destructor) => {
                format!("(System_Internal_Destructor)&{}", destructor)
            }
            None =>
                "nullptr".to_string(),
        };

        writeln!(out, "System_Internal_InitClass(\"{}\", {});", self.pascal_name, destructor_ptr)
    }
}