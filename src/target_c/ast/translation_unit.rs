use std::fmt;
use linked_hash_set::LinkedHashSet;
use node::{
    Identifier,
    Implementation,
};
use target_c::{
    ast::{
        TranslationResult,
        Declaration,
        Block,
        Class,
        Expression,
    },
};
use semantic;

pub struct TranslationUnit {
    decls: Vec<Declaration>,

    classes: Vec<Class>,
    string_literals: LinkedHashSet<String>,

    initialization: Vec<Block>,
    finalization: Vec<Block>,
}

impl TranslationUnit {
    pub fn decls(&self) -> &[Declaration] {
        &self.decls
    }

    pub fn initialization(&self) -> &[Block] {
        &self.initialization
    }

    pub fn finalization(&self) -> &[Block] {
        &self.finalization
    }

    pub fn classes(&self) -> &[Class] {
        &self.classes
    }

    fn add_implementation_decl(&mut self,
                               decl: &semantic::Implementation)
                               -> TranslationResult<()> {
        let impl_decl = Declaration::translate_impl(decl, self)?;
        self.decls.extend(impl_decl);

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

    fn add_unit(&mut self, unit: &semantic::Unit) -> TranslationResult<()> {
        for decl in unit.interface.iter() {
            let new_decls = Declaration::translate_decl(decl, self)?;
            self.decls.extend(new_decls);
        }

        for decl in unit.implementation.iter() {
            let new_impls = Declaration::translate_impl(decl, self)?;
            self.decls.extend(new_impls);
        }

        if let Some(init_block) = unit.initialization.as_ref() {
            let init_block = Block::translate(init_block, None, self)?;
            self.initialization.push(init_block);
        }

        if let Some(final_block) = unit.finalization.as_ref() {
            let final_block = Block::translate(final_block, None, self)?;
            self.finalization.push(final_block);
        }

        let string_name = Identifier::from("System.String");

        let class_decls = unit.interface.iter()
            .filter_map(|decl| decl.as_class_decl())
            .chain(unit.implementation.iter()
                .filter_map(|impl_decl| match impl_decl {
                    Implementation::Decl(decl) => decl.as_class_decl(),
                    _ => None,
                }))
            .filter(|class| {
                /* string is magically initialized earlier */
                class.qualified_name() != string_name
            });

        self.classes.extend(class_decls
            .map(|class_record| {
                Class::translate(class_record)
            })
            .collect::<TranslationResult<Vec<_>>>()?
        );

        Ok(())
    }

    pub fn from_program(module: &semantic::ProgramModule) -> TranslationResult<Self> {
        let mut result = TranslationUnit {
            decls: Vec::new(),

            classes: Vec::new(),

            string_literals: LinkedHashSet::new(),

            initialization: Vec::new(),
            finalization: Vec::new(),
        };

        for unit in module.units.iter() {
            result.add_unit(unit)?;
        }

        for impl_decl in module.program.decls.iter() {
            result.add_implementation_decl(impl_decl)?;
        }

        let global_vars: Vec<_> = module.units.iter()
            .flat_map(|unit| unit.vars())
            .chain(module.program.vars())
            .cloned()
            .collect();

        /* zero-initialize global variables */
        result.initialization.push(Block::new(global_vars.iter()
            .map(|var_decl| {
                let name = &var_decl.name;
                Expression::Raw(format!("memset(&{}, 0, sizeof({}))", name, name))
            })
            .collect()));

        let main = Block::translate(&module.program.program_block, Some(&global_vars), &mut result)?;
        result.initialization.push(main);

        Ok(result)
    }
}
