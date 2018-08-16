use std::fmt;
use linked_hash_set::LinkedHashSet;
use target_c::ast::{
    FunctionDecl,
};

pub struct ModuleGlobals {
    function_decls: Vec<FunctionDecl>,
    string_literals: LinkedHashSet<String>,
}

impl ModuleGlobals {
    pub fn new() -> Self {
        ModuleGlobals {
            string_literals: LinkedHashSet::new(),
            function_decls: Vec::new(),
        }
    }

    pub fn declare_function(&mut self, func: FunctionDecl) {
        self.function_decls.push(func);
    }

    pub fn write_forward_funcs(&self, mut out: impl fmt::Write) -> fmt::Result {
        for func in self.function_decls.iter() {
            func.write_forward(&mut out)?;
        }

        Ok(())
    }

    pub fn write_func_impls(&self, mut out: impl fmt::Write) -> fmt::Result {
        for func in self.function_decls.iter() {
            func.write_impl(&mut out)?;
        }

        Ok(())
    }

    fn string_literal_index_to_name(index: usize) -> String {
        format!("System_Internal_StringLiteral_{}", index)
    }

    pub fn string_literal_name(&mut self, string_literal: &str) -> String {
        self.string_literals.insert_if_absent(string_literal.to_string());

        let index = self.string_literals.iter()
            .enumerate()
            .filter_map(|(index, existing)| if existing == string_literal {
                Some(index)
            } else {
                None
            })
            .next()
            .unwrap();

        Self::string_literal_index_to_name(index)
    }

    pub fn declare_string_literals(&self, mut out: impl fmt::Write) -> fmt::Result {
        for (index, _) in self.string_literals.iter().enumerate() {
            let name = Self::string_literal_index_to_name(index);
            writeln!(out, "static System_String* {};", name)?;
        }

        Ok(())
    }

    pub fn init_string_literals(&self, mut out: impl fmt::Write) -> fmt::Result {
        for (index, literal) in self.string_literals.iter().enumerate() {
            let name = Self::string_literal_index_to_name(index);
            writeln!(out, "{} = System_StringFromBytes((System_Byte*)\"{}\", (System_NativeInt){});",
                     name,
                     literal,
                     literal.len())?;
        }

        Ok(())
    }
}
