use std::{collections::HashMap, fmt};

use pas_ir::{
    self as ir,
    metadata::FieldID,
};

pub use self::{
    function::*,
    stmt::*,
    ty::*,
};

mod stmt;
mod function;
mod ty;

pub struct Module {
    functions: Vec<FunctionDef>,
    static_array_types: HashMap<ArraySig, Type>,
    struct_defs: Vec<StructDef>,
}

impl Module {
    pub fn new() -> Self {
        Module {
            functions: Vec::new(),
            struct_defs: Vec::new(),
            static_array_types: HashMap::new(),
        }
    }

    fn make_array_type(&mut self, element: Type, dim: usize) -> Type {
        let sig = ArraySig {
            element: element.clone(),
            dim,
        };

        if self.static_array_types.contains_key(&sig) {
            self.static_array_types[&sig].clone()
        } else {
            let next_id = self.static_array_types.len();

            let mut array_members = HashMap::new();
            array_members.insert(FieldID(0), element.clone().sized_array(dim));

            let name = StructName::StaticArray(next_id);
            let array_struct = StructDef {
                decl: StructDecl { name: name.clone() },
                members: array_members,
            };

            self.struct_defs.push(array_struct);
            let array_struct_ty = Type::Struct(name);
            self.static_array_types.insert(sig, array_struct_ty.clone());

            array_struct_ty
        }
    }

    pub fn add_ir(&mut self, module: &ir::Module) {
        for (id, struct_def) in module.metadata.structs() {
            let c_struct = StructDef::translate(*id, struct_def, self);
            self.struct_defs.push(c_struct);
        }

        for (id, func) in &module.functions {
            let c_func = FunctionDef::translate(*id, func, self);
            self.functions.push(c_func);
        }

        let main_index = self
            .functions
            .iter()
            .position(|f| f.decl.name == FunctionName::Main);
        let mut main = match main_index {
            Some(index) => self.functions.remove(index),
            None => FunctionDef {
                decl: FunctionDecl {
                    name: FunctionName::Main,
                    params: Vec::new(),
                    return_ty: Type::Int,
                },
                body: Vec::new(),
            },
        };

        let init_stmts = translate_instructions(&module.init, self);
        main.body.extend(init_stmts);

        self.functions.push(main);
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", include_str!("prelude.h"))?;

        for struct_def in &self.struct_defs {
            writeln!(f, "{};", struct_def.decl)?;
            writeln!(f)?;
        }

        for struct_def in &self.struct_defs {
            writeln!(f, "{}", struct_def)?;
            writeln!(f)?;
        }

        for func in &self.functions {
            writeln!(f, "{};", func.decl)?;
            writeln!(f)?;
        }

        for func in &self.functions {
            writeln!(f, "{}", func)?;
            writeln!(f)?;
        }

        writeln!(f)
    }
}

#[derive(Eq, PartialEq, Hash)]
struct ArraySig {
    element: Type,
    dim: usize,
}