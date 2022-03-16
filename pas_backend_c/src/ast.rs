mod function;
mod stmt;
mod string_lit;
mod ty;

pub use self::{function::*, stmt::*, ty::*};
use crate::{ast::string_lit::StringLiteral, Options};
use pas_ir::{
    metadata::STRING_ID,
    self as ir,
    metadata::{FunctionID, StringID, Symbol, STRING_CHARS_FIELD, STRING_LEN_FIELD},
    StaticClosure,
    BUILTIN_SRC
};
use std::{
    borrow::Cow,
    collections::hash_map::{Entry, HashMap},
    fmt,
};
use topological_sort::TopologicalSort;

pub struct Module {
    functions: Vec<FunctionDef>,
    ffi_funcs: Vec<FfiFunction>,
    builtin_funcs: HashMap<FunctionID, FunctionName>,

    static_array_types: HashMap<ArraySig, Type>,

    type_defs: HashMap<TypeDefName, TypeDef>,
    type_defs_order: TopologicalSort<TypeDefName>,

    classes: Vec<Class>,
    ifaces: Vec<Interface>,

    string_literals: HashMap<StringID, StringLiteral>,
    static_closures: Vec<StaticClosure>,

    opts: Options,

    type_names: HashMap<ir::Type, String>,
}

impl Module {
    pub fn new(metadata: &ir::metadata::Metadata, opts: Options) -> Self {
        let string_ty = Type::DefinedType(TypeDefName::Struct(STRING_ID)).ptr();

        let system_funcs = &[
            (
                "Int8ToStr",
                FunctionName::Int8ToStr,
                vec![Type::SChar],
                string_ty.clone(),
            ),
            (
                "ByteToStr",
                FunctionName::ByteToStr,
                vec![Type::UChar],
                string_ty.clone(),
            ),
            (
                "Int16ToStr",
                FunctionName::Int16ToStr,
                vec![Type::Int16],
                string_ty.clone(),
            ),
            (
                "UInt16ToStr",
                FunctionName::UInt16ToStr,
                vec![Type::UInt16],
                string_ty.clone(),
            ),
            (
                "IntToStr",
                FunctionName::IntToStr,
                vec![Type::Int32],
                string_ty.clone(),
            ),
            (
                "UInt32ToStr",
                FunctionName::UInt32ToStr,
                vec![Type::UInt32],
                string_ty.clone(),
            ),
            (
                "Int64ToStr",
                FunctionName::Int64ToStr,
                vec![Type::Int64],
                string_ty.clone(),
            ),
            (
                "UInt64ToStr",
                FunctionName::UInt64ToStr,
                vec![Type::UInt64],
                string_ty.clone(),
            ),
            (
                "NativeIntToStr",
                FunctionName::NativeIntToStr,
                vec![Type::PtrDiffType],
                string_ty.clone(),
            ),
            (
                "NativeUIntToStr",
                FunctionName::NativeUIntToStr,
                vec![Type::SizeType],
                string_ty.clone(),
            ),
            (
                "StrToInt",
                FunctionName::StrToInt,
                vec![string_ty.clone()],
                Type::Int32,
            ),
            (
                "GetMem",
                FunctionName::GetMem,
                vec![Type::Int32],
                Type::UChar.ptr(),
            ),
            (
                "FreeMem",
                FunctionName::FreeMem,
                vec![Type::UChar.ptr()],
                Type::Void,
            ),
            (
                "WriteLn",
                FunctionName::WriteLn,
                vec![string_ty.clone()],
                Type::Void,
            ),
            ("ReadLn", FunctionName::ReadLn, vec![], string_ty.clone()),
            (
                "ArrayLengthInternal",
                FunctionName::ArrayLengthInternal,
                vec![Type::Void.ptr()],
                Type::Int32,
            ),
            (
                "ArraySetLengthInternal",
                FunctionName::ArraySetLengthInternal,
                vec![Type::Void.ptr(), Type::Int32, Type::Void.ptr(), Type::Int32],
                Type::Void.ptr(),
            ),
        ];

        let mut builtin_funcs = HashMap::new();
        for (pas_name, c_name, _params, _return_ty) in system_funcs {
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
                    ir::metadata::TypeDef::Function(..) => ir::Type::Function(id),
                };

                let name = metadata.pretty_ty_name(&ty);
                (ty, name.to_string())
            })
            .collect();

        let mut module = Module {
            functions: Vec::new(),
            ffi_funcs: Vec::new(),
            builtin_funcs,

            type_defs: HashMap::new(),
            type_defs_order: TopologicalSort::new(),

            static_array_types: HashMap::new(),

            string_literals,
            static_closures: Vec::new(),

            classes: Vec::new(),
            ifaces: Vec::new(),

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
                let name = TypeDefName::StaticArray(next_id);
                let array_struct = StructDef {
                    decl: TypeDecl { name: name.clone() },
                    members: vec![StructMember {
                        name: FieldName::StaticArrayElements,
                        ty: element.clone().sized_array(dim),
                        comment: None,
                    }],
                    comment: Some(format!("array[{}] of {}", dim, element.typename())),
                };

                self.type_defs
                    .insert(name.clone(), TypeDef::Struct(array_struct));

                let array_struct_ty = Type::DefinedType(name.clone());
                entry.insert(array_struct_ty.clone());

                self.type_defs_order.insert(name.clone());
                for element_dep in element.type_def_deps() {
                    self.type_defs_order
                        .add_dependency(element_dep, name.clone());
                }

                array_struct_ty
            },
        }
    }

    pub fn function_name(&self, id: FunctionID) -> FunctionName {
        match self.builtin_funcs.get(&id) {
            Some(builtin) => *builtin,
            None => FunctionName::ID(id),
        }
    }

    pub fn add_ir(&mut self, module: &ir::Module) {
        let mut module_type_defs = Vec::new();

        for (id, type_def) in module.metadata.type_defs() {
            let mut member_deps = Vec::new();

            let c_type_def = match type_def {
                ir::metadata::TypeDef::Struct(struct_def) => {
                    let struct_def = StructDef::translate(id, struct_def, self);
                    for member in &struct_def.members {
                        member.ty.collect_type_def_deps(&mut member_deps);
                    }

                    TypeDef::Struct(struct_def)
                },

                ir::metadata::TypeDef::Variant(variant_def) => {
                    let variant_def = VariantDef::translate(id, variant_def, self);
                    for case in &variant_def.cases {
                        if let Some(case_ty) = &case.ty {
                            case_ty.collect_type_def_deps(&mut member_deps);
                        }
                    }

                    TypeDef::Variant(variant_def)
                },

                ir::metadata::TypeDef::Function(func_def) => {
                    let return_ty = Type::from_metadata(&func_def.return_ty, self);
                    return_ty.collect_type_def_deps(&mut member_deps);

                    let mut param_tys = Vec::new();

                    // any function-type object needs a closure param
                    param_tys.push(Type::Void.ptr());

                    for param_ty in &func_def.param_tys {
                        let param_ty = Type::from_metadata(param_ty, self);
                        param_ty.collect_type_def_deps(&mut member_deps);
                        param_tys.push(param_ty);
                    }

                    let func_alias_def = FuncAliasDef {
                        decl: TypeDecl {
                            name: TypeDefName::Alias(id),
                        },
                        param_tys,
                        return_ty,
                        comment: Some(func_def.to_string()),
                    };

                    TypeDef::FuncAlias(func_alias_def)
                },
            };

            let c_def_name = c_type_def.decl().name.clone();

            module_type_defs.push(c_type_def.clone());
            self.type_defs.insert(c_def_name.clone(), c_type_def);

            self.type_defs_order.insert(c_def_name.clone());
            for member_dep in member_deps {
                self.type_defs_order
                    .add_dependency(member_dep, c_def_name.clone());
            }
        }

        for (id, func) in &module.functions {
            match func {
                ir::Function::Local(func_def) => {
                    let c_func = FunctionDef::translate(*id, func_def, self);

                    self.functions.push(c_func);
                },

                ir::Function::External(func_ref) if func_ref.src == BUILTIN_SRC => {},

                ir::Function::External(func_ref) => {
                    let ffi_func = FfiFunction::translate(*id, func_ref, self);

                    self.ffi_funcs.push(ffi_func);
                },
            }
        }

        // now that real functions are defined, we can generate method vcall wrappers
        for class in self.classes.clone() {
            for wrapper_func_def in class.gen_vcall_wrappers(self) {
                self.functions.push(wrapper_func_def);
            }
        }

        for static_closure in module.static_closures() {
            self.static_closures.push(static_closure.clone());
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

        let ffi_init_stmts: Vec<_> = self
            .ffi_funcs
            .iter()
            .map(|ffi_func| ffi_func.init_statement())
            .collect();

        let mut init_builder = Builder::new(self);

        init_builder.stmts.extend(ffi_init_stmts);

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

        for def_name in &ordered_type_defs {
            if let Some(forward_decl) = self.type_defs[def_name].forward_decl() {
                writeln!(f, "{};", forward_decl)?;
                writeln!(f)?;
            }
        }

        for def_name in &ordered_type_defs {
            // special case for System.String: we expect it to already be defined in the prelude
            if *def_name == TypeDefName::Struct(ir::metadata::STRING_ID) {
                continue;
            }

            let def = &self.type_defs[def_name];

            writeln!(f, "{};", def)?;
            writeln!(f)?;
        }

        for func in &self.functions {
            writeln!(f, "{};", func.decl)?;
            writeln!(f)?;
        }

        for ffi_func in &self.ffi_funcs {
            writeln!(f, "{};", ffi_func.func_ptr_decl())?;
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

            let string_name = TypeDefName::Struct(ir::metadata::STRING_ID);
            let lit_name = GlobalName::StringLiteral(*str_id);
            writeln!(f, "static struct {} {} = {{", string_name, lit_name)?;

            // rc state
            writeln!(f, "  .{} = {{", FieldName::Rc)?;
            writeln!(
                f,
                "    .{} = &{},",
                FieldName::RcClass,
                GlobalName::ClassInstance(ir::metadata::STRING_ID),
            )?;
            writeln!(f, "    .{} = -1,", FieldName::RcRefCount)?;
            writeln!(f, "  }},")?;

            write!(f, "  .{} = {}", chars_field, lit)?;
            writeln!(f, ", ")?;
            writeln!(f, "  .{} = {},", len_field, lit.as_str().len())?;
            writeln!(f, "}};")?;
        }

        for static_closure in &self.static_closures {
            let global_name = GlobalName::StaticClosure(static_closure.id);
            let decl_string = Type::DefinedType(TypeDefName::Struct(static_closure.closure_id))
                .to_decl_string(&global_name);

            writeln!(f, "{};", decl_string)?;
        }
        writeln!(f)?;

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