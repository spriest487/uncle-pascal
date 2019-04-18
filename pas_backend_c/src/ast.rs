use std::{collections::HashMap, fmt};

use pas_ir::{
    self as ir,
    metadata::{
        FunctionID,
        GlobalName,
        StringID,
    },
};
use crate::Options;

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
    classes: Vec<Class>,
    ifaces: Vec<Interface>,

    builtin_funcs: HashMap<FunctionID, FunctionName>,

    string_literals: HashMap<StringID, String>,

    opts: Options,
}

impl Module {
    pub fn new(metadata: &ir::metadata::Metadata, opts: Options) -> Self {
        let mut builtin_funcs = HashMap::new();

        fn lookup_sys_builtin(
            metadata: &ir::metadata::Metadata,
            results: &mut HashMap<FunctionID, FunctionName>,
            decl_name: &str,
            c_name: FunctionName,
        ) {
            let global_name = &GlobalName::new(decl_name, vec!["System"]);
            if let Some(inttostr_id) = metadata.find_function(global_name) {
                results.insert(inttostr_id, c_name);
            }
        }

        lookup_sys_builtin(metadata, &mut builtin_funcs, "IntToStr", FunctionName::IntToStr);
        lookup_sys_builtin(metadata, &mut builtin_funcs, "StrToInt", FunctionName::StrToInt);
        lookup_sys_builtin(metadata, &mut builtin_funcs, "GetMem", FunctionName::GetMem);
        lookup_sys_builtin(metadata, &mut builtin_funcs, "FreeMem", FunctionName::FreeMem);
        lookup_sys_builtin(metadata, &mut builtin_funcs, "WriteLn", FunctionName::WriteLn);

        let classes = metadata.structs()
            .iter()
            .map(|(struct_id, struct_def)| Class::translate(*struct_id, struct_def, metadata))
            .collect();

        let string_literals = metadata.strings()
            .map(|(id, str)| (id, str.to_string()))
            .collect();

        let mut module = Module {
            functions: Vec::new(),
            struct_defs: Vec::new(),
            static_array_types: HashMap::new(),

            string_literals,

            classes,
            ifaces: Vec::new(),

            builtin_funcs,

            opts,
        };

        module.ifaces = metadata.ifaces()
            .iter()
            .map(|(iface_id, iface_def)| Interface::translate(*iface_id, iface_def, &mut module))
            .collect();

        module
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
            array_members.insert(FieldName::StaticArrayElements, element.clone().sized_array(dim));

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

    pub fn function_name(&self, id: FunctionID) -> FunctionName {
        match self.builtin_funcs.get(&id) {
            Some(builtin) => *builtin,
            None => FunctionName::ID(id),
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

        let init_index = self
            .functions
            .iter()
            .position(|f| f.decl.name == FunctionName::Init);
        let mut init_func = match init_index {
            Some(index) => self.functions.remove(index),
            None => FunctionDef {
                decl: FunctionDecl {
                    name: FunctionName::Init,
                    params: Vec::new(),
                    return_ty: Type::Void,
                    comment: None,
                },
                body: Vec::new(),
            },
        };

        let mut init_builder = Builder::new(self);
        init_builder.translate_instructions(&module.init);

        init_func.body.extend(init_builder.stmts);

        self.functions.push(init_func);
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.opts.trace_heap {
            writeln!(f, "#define TRACE_HEAP 1")?;
        }

        if self.opts.trace_rc {
            writeln!(f, "#define TRACE_RC 1")?;
        }

        writeln!(f, "{}", include_str!("prelude.h"))?;

        for struct_def in &self.struct_defs {
            writeln!(f, "{};", struct_def.decl)?;
            writeln!(f)?;
        }

        for struct_def in &self.struct_defs {
            // special case for System.String: we expect it to already be defined in the prelude
            if struct_def.decl.name == StructName::Class(ir::metadata::STRING_ID) {
                continue;
            }

            writeln!(f, "{}", struct_def)?;
            writeln!(f)?;
        }

        for func in &self.functions {
            writeln!(f, "{};", func.decl)?;
            writeln!(f)?;
        }

        for iface in &self.ifaces {
            writeln!(f, "{}", iface.method_table_string())?;
            writeln!(f)?;
        }

        for class in &self.classes {
            writeln!(f, "{}", class.to_def_string())?;
            writeln!(f)?;
        }

        for (str_id, lit) in &self.string_literals {
            let string_name = StructName::Class(ir::metadata::STRING_ID);
            writeln!(f, "static struct {} String_{} = {{", string_name, str_id.0)?;
            writeln!(f, "  .field_0 = (unsigned char*) \"{}\",", lit)?;
            writeln!(f, "  .field_1 = {},", lit.len())?;
            writeln!(f, "}};")?;

            writeln!(f, "static struct {} StringRc_{} = {{", StructName::Rc, str_id.0)?;
            writeln!(f, "  .resource = &String_{},", str_id.0)?;
            writeln!(f, "  .class = &Class_{},", ir::metadata::STRING_ID.0)?;
            writeln!(f, "  .count = -1,")?;
            writeln!(f, "}};")?;
        }

        for func in &self.functions {
            writeln!(f, "{}", func)?;
            writeln!(f)?;
        }

        writeln!(f, "{}", include_str!("epilogue.h"))?;

        writeln!(f)
    }
}

#[derive(Eq, PartialEq, Hash)]
struct ArraySig {
    element: Type,
    dim: usize,
}