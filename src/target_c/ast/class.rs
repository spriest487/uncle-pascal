use std::fmt;

use semantic;
use target_c::{
    identifier_to_c,
    ast::{
        TranslationResult
    }
};

pub struct Class {
//    name: String,
    pascal_name: String,
    destructor: Option<String>,
}

impl Class {
    pub fn translate(class: &semantic::RecordDecl) -> TranslationResult<Self> {
        let full_name = class.scope().namespace_qualify(&class.name);

        let destructor = class.scope().get_destructor(&full_name)
            .map(|(dtor_id, _dtor_func)| identifier_to_c(dtor_id));

        Ok(Class {
//            name: identifier_to_c(&full_name),
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