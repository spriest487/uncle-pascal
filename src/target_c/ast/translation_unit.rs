use std::fmt;
use target_c::{
    ast::{
        CType,
        FunctionDecl,
        FunctionDefinition,
        TranslationResult,
    }
};
use semantic;

pub struct Variable {
    name: String,
    ctype: CType,
}

pub struct Record {
    name: String,
    members: Vec<Variable>,
}

pub enum Declaration {
    Variable(String, CType),
    Function(FunctionDecl),
    Record(Record),
}

pub struct TranslationUnit {
    decls: Vec<Declaration>,
    functions: Vec<FunctionDefinition>,
}

impl TranslationUnit {
    fn from_unit(unit: &semantic::Unit) -> TranslationResult<Self> {
        for decl in unit.interface.iter() {
            unimplemented!()
        }

        for decl in unit.implementation.iter() {
            unimplemented!()
        }

        unimplemented!()
    }

    fn from_program(program: &semantic::Program) -> TranslationResult<Self> {
        unimplemented!()
    }

    fn write(&self, mut out: impl fmt::Write) -> fmt::Result {
        unimplemented!()
    }
}
