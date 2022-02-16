pub use self::{function::*, stmt::*, ty::*};
use crate::{
    ast::string_lit::StringLiteral,
    Options,
};
use pas_ir::{
    self as ir,
    metadata::{FunctionID, StringID, Symbol, STRING_CHARS_FIELD, STRING_LEN_FIELD}
};
use std::{
    borrow::Cow,
    collections::hash_map::{Entry, HashMap},
    fmt,
};
use topological_sort::TopologicalSort;

mod function;
mod stmt;
mod string_lit;
mod ty;

pub struct Module {
    functions: Vec<FunctionDef>,

    static_array_types: HashMap<ArraySig, Type>,

    type_defs: HashMap<StructName, TypeDef>,
    type_defs_order: TopologicalSort<StructName>,

    classes: Vec<Class>,
    ifaces: Vec<Interface>,

    builtin_funcs: HashMap<FunctionID, FunctionName>,

    string_literals: HashMap<StringID, StringLiteral>,

    opts: Options,

    type_names: HashMap<ir::Type, String>,
}

impl Module {
    pub fn new(metadata: &ir::metadata::Metadata, opts: Options) -> Self {
        let system_funcs = &[
            ("Int8ToStr", FunctionName::Int8ToStr),
            ("ByteToStr", FunctionName::ByteToStr),
            ("Int16ToStr", FunctionName::Int16ToStr),
            ("UInt16ToStr", FunctionName::UInt16ToStr),
            ("IntToStr", FunctionName::IntToStr),
            ("UInt32ToStr", FunctionName::UInt32ToStr),
            ("Int64ToStr", FunctionName::Int64ToStr),
            ("UInt64ToStr", FunctionName::UInt64ToStr),
            ("NativeIntToStr", FunctionName::NativeIntToStr),
            ("NativeUIntToStr", FunctionName::NativeUIntToStr),

            ("StrToInt", FunctionName::StrToInt),
            ("GetMem", FunctionName::GetMem),
            ("FreeMem", FunctionName::FreeMem),
            ("WriteLn", FunctionName::WriteLn),
            ("ReadLn", FunctionName::ReadLn),
            ("ArrayLengthInternal", FunctionName::ArrayLengthInternal),
            (
                "ArraySetLengthInternal",
                FunctionName::ArraySetLengthInternal,
            ),
        ];

        let mut builtin_funcs = HashMap::new();
        for (pas_name, c_name) in system_funcs {
            let global_name = &Symbol::new(*pas_name, vec!["System"]);

            // if a function isn't used then it won't be included in the metadata
            if let Some(func_id) = metadata.find_function(global_name) {
                builtin_funcs.insert(func_id, *c_name);
            }
        }

        let string_literals = metadata
            .strings()
            .map(|(id, str)| (id, StringLiteral::from(str.to_string())))
            .collect();

        let type_names = metadata
            .type_defs()
            .map(|(id, ty_def)| {
                let ty = match ty_def {
                    ir::metadata::TypeDef::Variant(..) => ir::Type::Variant(id),
                    ir::metadata::TypeDef::Struct(..) => ir::Type::Struct(id),
                };

                let name = metadata.pretty_ty_name(&ty);
                (ty, name.to_string())
            })
            .collect();

        let mut module = Module {
            functions: Vec::new(),
            type_defs: HashMap::new(),
            type_defs_order: TopologicalSort::new(),

            static_array_types: HashMap::new(),

            string_literals,

            classes: Vec::new(),
            ifaces: Vec::new(),

            builtin_funcs,

            opts,

            type_names,
        };

        let class_defs = metadata.type_defs().filter_map(|(id, def)| match def {
            ir::metadata::TypeDef::Struct(struct_def) => Some((id, struct_def)),
            _ => None,
        });

        for (class_id, class_def) in class_defs {
            let class = Class::translate(class_id, class_def, metadata, &mut module);
            module.classes.push(class);
        }

        for (iface_id, iface_def) in metadata.ifaces() {
            let iface = Interface::translate(iface_id, iface_def, &mut module);
            module.ifaces.push(iface);
        }

        module
    }

    pub fn pretty_type(&self, ir_ty: &ir::Type) -> Cow<str> {
        match self.type_names.get(ir_ty) {
            Some(name) => Cow::Borrowed(name),
            None => Cow::Owned(ir_ty.to_string()),
        }
    }

    pub fn pretty_name(&self, name_path: &ir::metadata::NamePath) -> String {
        name_path.to_pretty_string(|ty| self.pretty_type(ty))
    }

    fn make_array_type(&mut self, element: Type, dim: usize) -> Type {
        let sig = ArraySig {
            element: element.clone(),
            dim,
        };

        let next_id = self.static_array_types.len();

        match self.static_array_types.entry(sig) {
            Entry::Occupied(entry) => entry.get().clone(),

            Entry::Vacant(entry) => {
                let name = StructName::StaticArray(next_id);
                let array_struct = StructDef {
                    decl: StructDecl { name: name.clone() },
                    members: vec![StructMember {
                        name: FieldName::StaticArrayElements,
                        ty: element.clone().sized_array(dim),
                        comment: None,
                    }],
                    comment: Some(format!("array[{}] of {}", dim, element.typename())),
                };

                self.type_defs.insert(name.clone(), TypeDef::Struct(array_struct));

                let array_struct_ty = Type::Struct(name.clone());
                entry.insert(array_struct_ty.clone());

                self.type_defs_order.insert(name.clone());
                for element_dep in element.type_def_deps() {
                    self.type_defs_order.add_dependency(element_dep, name.clone());
                }

                array_struct_ty
            }
        }
    }

    pub fn function_name(&self, id: FunctionID) -> FunctionName {
        match self.builtin_funcs.get(&id) {
            Some(builtin) => *builtin,
            None => FunctionName::ID(id),
        }
    }

    pub fn add_ir(&mut self, module: &ir::Module) {
        for (id, type_def) in module.metadata.type_defs() {
            let mut member_deps = Vec::new();

            let c_type_def = match type_def {
                ir::metadata::TypeDef::Struct(struct_def) => {
                    let struct_def = StructDef::translate(id, struct_def, self);
                    for member in &struct_def.members {
                        member.ty.collect_type_def_deps(&mut member_deps);
                    }

                    TypeDef::Struct(struct_def)
                }

                ir::metadata::TypeDef::Variant(variant_def) => {
                    let variant_def = VariantDef::translate(id, variant_def, self);
                    for case in &variant_def.cases {
                        if let Some(case_ty) = &case.ty {
                            case_ty.collect_type_def_deps(&mut member_deps);
                        }
                    }

                    TypeDef::Variant(variant_def)
                }
            };

            let c_def_name = c_type_def.decl().name.clone();

            self.type_defs.insert(c_def_name.clone(), c_type_def);

            self.type_defs_order.insert(c_def_name.clone());
            for member_dep in member_deps {
                self.type_defs_order.add_dependency(member_dep, c_def_name.clone());
            }
        }

        for (id, func) in &module.functions {
            if let ir::Function::Local(func_def) = func {
                let c_func = FunctionDef::translate(*id, func_def, self);
                self.functions.push(c_func);
            }
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

        for def in self.type_defs.values() {
            writeln!(f, "{};", def.decl())?;
            writeln!(f)?;
        }

        let ordered_type_defs: Vec<_> = self.type_defs_order.clone().into_iter().collect();
        if ordered_type_defs.len() != self.type_defs_order.len() {
            eprintln!("ordered defs ({}):", ordered_type_defs.len());
            for def in ordered_type_defs {
                eprintln!(" - {}", def);
            }

            eprintln!("type order sort {}:", self.type_defs_order.len());
            for def in self.type_defs_order.clone().into_iter() {
                eprintln!(" - {}", def);
            }

            panic!("type metadata contained illegal circular references");
        }

        for def_name in ordered_type_defs.iter() {
            // special case for System.String: we expect it to already be defined in the prelude
            if *def_name == StructName::Struct(ir::metadata::STRING_ID) {
                continue;
            }

            let def = &self.type_defs[def_name];

            writeln!(f, "{}", def)?;
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
            writeln!(f, "{}", class.to_decl_string())?;
            writeln!(f, "{}", class.to_def_string())?;
            writeln!(f)?;
        }

        for (str_id, lit) in &self.string_literals {
            let chars_field = FieldName::ID(STRING_CHARS_FIELD);
            let len_field = FieldName::ID(STRING_LEN_FIELD);

            let string_name = StructName::Struct(ir::metadata::STRING_ID);
            writeln!(f, "static struct {} String_{} = {{", string_name, str_id.0)?;
            write!(f, "  .{} = {}", chars_field, lit)?;
            writeln!(f, ", ")?;
            writeln!(f, "  .{} = {},", len_field, lit.as_str().len())?;
            writeln!(f, "}};")?;

            writeln!(
                f,
                "static struct {} StringRc_{} = {{",
                StructName::Rc,
                str_id.0
            )?;
            writeln!(f, "  .{} = &String_{},", FieldName::RcResource, str_id.0)?;
            writeln!(
                f,
                "  .{} = &Class_{},",
                FieldName::RcClass,
                ir::metadata::STRING_ID.0
            )?;
            writeln!(f, "  .{} = -1,", FieldName::RcRefCount)?;
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
