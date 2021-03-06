use std::{collections::HashMap, fmt};

use pas_ir::metadata::{self, ClassID, FieldID, InterfaceID, MethodID, StructID};

use crate::ast::{FunctionDecl, FunctionName, Module};

#[allow(unused)]
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Void,
    Int,
    Int32,
    Struct(StructName),
    Pointer(Box<Type>),
    Bool,
    Float,
    UChar,
    SizedArray(Box<Type>, usize),
    FunctionPointer {
        return_ty: Box<Type>,
        params: Vec<Type>,
    },
}

impl Type {
    pub fn from_metadata(ty: &metadata::Type, module: &mut Module) -> Type {
        match ty {
            metadata::Type::Pointer(target) => Type::from_metadata(target.as_ref(), module).ptr(),
            metadata::Type::RcPointer(..) => Type::Struct(StructName::Rc).ptr(),
            metadata::Type::Struct(id) => Type::Struct(StructName::Class(*id)),
            metadata::Type::Variant(id) => Type::Struct(StructName::Variant(*id)),
            metadata::Type::Nothing => Type::Void,
            metadata::Type::I32 => Type::Int32,
            metadata::Type::Bool => Type::Bool,
            metadata::Type::F32 => Type::Float,
            metadata::Type::U8 => Type::UChar,
            metadata::Type::Array { element, dim } => {
                let element = Type::from_metadata(element, module);
                module.make_array_type(element, *dim)
            }
        }
    }

    pub fn ptr(self) -> Self {
        Type::Pointer(Box::new(self))
    }

    pub fn sized_array(self, size: usize) -> Self {
        Type::SizedArray(Box::new(self), size)
    }

    fn build_decl_string(&self, left: &mut String, right: &mut String) {
        match self {
            Type::Pointer(ty) => {
                ty.build_decl_string(left, right);
                left.push('*');
            }
            Type::Void => {
                left.push_str("void");
            }
            Type::Int => {
                left.push_str("int");
            }
            Type::Int32 => {
                left.push_str("int32_t");
            }
            Type::Bool => {
                left.push_str("bool");
            }
            Type::Float => {
                left.push_str("float");
            }
            Type::UChar => {
                left.push_str("unsigned char");
            }

            Type::Struct(name) => {
                left.push_str("struct ");
                left.push_str(&name.to_string());
            }

            Type::SizedArray(el, size) => {
                el.build_decl_string(left, right);
                right.push('[');
                right.push_str(&size.to_string());
                right.push(']');
            }

            Type::FunctionPointer { return_ty, params } => {
                left.push_str(&return_ty.typename());
                left.push_str("(*");

                right.push_str(")(");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        right.push_str(", ");
                    }
                    right.push_str(&param.typename());
                }
                right.push(')');
            }
        }
    }

    pub fn to_decl_string<Name>(&self, name: &Name) -> String
    where
        Name: ?Sized + fmt::Display,
    {
        let mut left = String::new();
        let mut right = String::new();

        self.build_decl_string(&mut left, &mut right);

        format!("{} {} {}", left, name, right).trim().to_string()
    }

    pub fn typename(&self) -> String {
        match self {
            Type::Void => "void".to_string(),
            Type::Int => "int".to_string(),
            Type::Int32 => "int32_t".to_string(),
            Type::Struct(name) => format!("struct {}", name),
            Type::SizedArray(ty, ..) | Type::Pointer(ty) => format!("{}*", ty.typename()),
            Type::Bool => "bool".to_string(),
            Type::Float => "float".to_string(),
            Type::UChar => "unsigned char".to_string(),
            Type::FunctionPointer { return_ty, params } => {
                let mut name = format!("{} (*)(", return_ty.typename());
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        name.push_str(", ");
                    }
                    name.push_str(&param.typename());
                }
                name.push(')');
                name
            }
        }
    }
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum FieldName {
    // ID from metadata
    ID(FieldID),

    // builtin name: ref count field of RC internal struct
    RcRefCount,
    // builtin name: resource pointer field of RC internal strut
    RcResource,
    // builtin name: class info pointer field of RC internal struct
    RcClass,

    // builtin name: static array inner array
    StaticArrayElements,

    VariantTag,
    VariantData,
    VariantDataCase(usize),
}

impl fmt::Display for FieldName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FieldName::ID(id) => write!(f, "field_{}", id.0),
            FieldName::RcRefCount => write!(f, "count"),
            FieldName::RcResource => write!(f, "resource"),
            FieldName::RcClass => write!(f, "class"),
            FieldName::StaticArrayElements => write!(f, "elements"),
            FieldName::VariantTag => write!(f, "tag"),
            FieldName::VariantData => write!(f, "data"),
            FieldName::VariantDataCase(case) => write!(f, "data_{}", case),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum StructName {
    // internal struct for implementation of RC pointers
    Rc,

    // struct from class def ID
    Class(StructID),

    // struct from variant def ID
    Variant(StructID),

    // struct for a fixed-size array with a generated unique ID
    StaticArray(usize),
}

pub struct StructDecl {
    pub name: StructName,
}

impl fmt::Display for StructDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "struct {}", self.name)
    }
}

pub struct StructMember {
    pub name: FieldName,
    pub ty: Type,

    pub comment: Option<String>,
}

pub struct StructDef {
    pub decl: StructDecl,
    pub members: Vec<StructMember>,

    pub comment: Option<String>,
}

impl fmt::Display for StructName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StructName::Rc => write!(f, "Rc"),
            StructName::Class(id) => write!(f, "Struct_{}", id.0),
            StructName::Variant(id) => write!(f, "Variant_{}", id.0),
            StructName::StaticArray(i) => write!(f, "StaticArray_{}", i),
        }
    }
}

impl StructDef {
    pub fn translate(
        id: metadata::StructID,
        ir_struct: &metadata::Struct,
        module: &mut Module,
    ) -> Self {
        let members = ir_struct
            .fields
            .iter()
            .map(|(id, field)| {
                let ty = Type::from_metadata(&field.ty, module);

                StructMember {
                    name: FieldName::ID(*id),
                    ty,
                    comment: None,
                }
            })
            .collect();

        let struct_ty = metadata::Type::Struct(id);
        let comment = module.pretty_type(&struct_ty).to_string();

        Self {
            decl: StructDecl {
                name: StructName::Class(id),
            },
            members,
            comment: Some(comment),
        }
    }
}

impl fmt::Display for StructDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(comment) = &self.comment {
            writeln!(f, "/** {} */", comment)?;
        }

        writeln!(f, "{} {{", self.decl)?;
        for member in self.members.iter() {
            if let Some(comment) = &member.comment {
                writeln!(f, "/** {} */", comment)?;
            }

            let name = format!("{}", member.name);
            writeln!(f, "{};", member.ty.to_decl_string(&name))?;
        }
        write!(f, "}};")
    }
}

pub struct VariantCaseDef {
    pub ty: Option<Type>,
    pub comment: Option<String>,
}

pub struct VariantDef {
    pub decl: StructDecl,
    pub cases: Vec<VariantCaseDef>,

    pub comment: Option<String>,
}

impl VariantDef {
    pub fn translate(id: StructID, variant: &metadata::Variant, module: &mut Module) -> Self {
        let cases = variant
            .cases
            .iter()
            .map(|case| {
                let ty = case
                    .ty
                    .as_ref()
                    .map(|data_ty| Type::from_metadata(data_ty, module));
                VariantCaseDef {
                    ty,
                    comment: Some(case.name.clone())
                }
            })
            .collect();

        let variant_ty = metadata::Type::Variant(id);
        let comment = module.pretty_type(&variant_ty).to_string();

        Self {
            decl: StructDecl {
                name: StructName::Variant(id),
            },
            cases,
            comment: Some(comment),
        }
    }
}

impl fmt::Display for VariantDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(comment) = &self.comment {
            writeln!(f, "/** {} */", comment)?;
        }

        writeln!(f, "{} {{", self.decl)?;

        writeln!(
            f,
            "  {};",
            Type::Int32.to_decl_string(&FieldName::VariantTag)
        )?;

        if self.cases.iter().any(|c| c.ty.is_some()) {
            writeln!(f, "  union {}_Data {{", self.decl.name)?;
            for (index, case) in self.cases.iter().enumerate() {
                let name = FieldName::VariantDataCase(index);

                if let Some(case_ty) = &case.ty {
                    if let Some(comment) = &case.comment {
                        writeln!(f, "  /** {} */", comment)?;
                    }

                    writeln!(f, "    {};", case_ty.to_decl_string(&name))?;
                }
            }

            writeln!(f, "  }} {};", FieldName::VariantData)?;
        }

        write!(f, "}};")
    }
}

pub enum TypeDef {
    Struct(StructDef),
    Variant(VariantDef),
}

impl TypeDef {
    pub fn decl(&self) -> &StructDecl {
        match self {
            TypeDef::Struct(s) => &s.decl,
            TypeDef::Variant(v) => &v.decl,
        }
    }
}

impl fmt::Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDef::Struct(s) => write!(f, "{}", s),
            TypeDef::Variant(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InterfaceImpl {
    method_impls: HashMap<MethodID, FunctionName>,
}

#[derive(Clone, Debug)]
pub struct Class {
    struct_id: StructID,
    impls: HashMap<InterfaceID, InterfaceImpl>,
    disposer: Option<FunctionName>,
    cleanup_func: FunctionName,
}

impl Class {
    pub fn translate(
        struct_id: StructID,
        _struct_def: &metadata::Struct,
        metadata: &metadata::Metadata,
    ) -> Self {
        let class_id = ClassID::Class(struct_id);
        let class_ty = metadata::Type::RcPointer(Some(class_id));
        let mut impls = HashMap::new();

        for (iface_id, iface) in metadata.ifaces() {
            let method_impls: HashMap<_, _> = iface
                .methods
                .iter()
                .enumerate()
                .filter_map(|(method_index, _method)| {
                    let method_id = MethodID(method_index);
                    let method_impl = metadata.find_impl(&class_ty, iface_id, method_id)?;

                    Some((method_id, FunctionName::ID(method_impl)))
                })
                .collect();

            if method_impls.is_empty() {
                continue;
            }

            impls.insert(iface_id, InterfaceImpl { method_impls });
        }

        let disposer = impls
            .get(&metadata::DISPOSABLE_ID)
            .and_then(|disposable_impl| {
                disposable_impl
                    .method_impls
                    .get(&metadata::DISPOSABLE_DISPOSE_INDEX)
                    .cloned()
            });

        let resource_ty = metadata::Type::Struct(struct_id);
        let cleanup_func = metadata.find_rc_boilerplate(&resource_ty)
            .map(|funcs| FunctionName::ID(funcs.release))
            .unwrap_or_else(|| panic!(
                "missing rc cleanup func for resource struct of IR class {}",
                metadata.pretty_ty_name(&resource_ty),
            ));

        Class {
            struct_id,
            impls,
            disposer,
            cleanup_func,
        }
    }

    pub fn to_def_string(&self) -> String {
        let mut def = String::new();

        // impl method tables
        let impls: Vec<_> = self.impls.iter().enumerate().collect();
        for (i, (iface_id, iface_impl)) in &impls {
            def.push_str(&format!(
                "struct MethodTable_{} ImplTable_{}_{} = {{\n",
                iface_id.0, self.struct_id.0, iface_id.0
            ));
            for (method_id, method_name) in &iface_impl.method_impls {
                def.push_str("  .base = {\n");

                def.push_str("    .iface = ");
                def.push_str(&iface_id.to_string());
                def.push_str(",\n");

                def.push_str("    .next = ");
                match impls.get(i + 1) {
                    Some((_, (next_impl_id, _))) => {
                        def.push_str(&format!("ImplTable_{}_{}", self.struct_id, next_impl_id));
                    }
                    None => def.push_str("NULL"),
                }

                def.push_str(",\n");
                def.push_str("  },\n");

                def.push_str("  .method_");
                def.push_str(&method_id.0.to_string());
                def.push_str(" = &");
                def.push_str(&method_name.to_string());
                def.push_str(",\n");
            }
            def.push_str("};\n\n");
        }

        // class struct

        def.push_str(&format!("struct Class Class_{} = {{\n", self.struct_id));

        def.push_str("  .size = sizeof(struct ");
        def.push_str(&StructName::Class(self.struct_id).to_string());
        def.push_str("),\n");

        def.push_str("  .disposer = ");
        if let Some(disposer_func) = self.disposer {
            def.push('&');
            def.push_str(&disposer_func.to_string());
        } else {
            def.push_str("NULL");
        };
        def.push_str(",\n");

        def.push_str("  .cleanup = (RcCleanupFunc)&");
        def.push_str(&self.cleanup_func.to_string());
        def.push_str(",\n");

        def.push_str("  .iface_methods = ");
        if let Some((_, (first_iface_id, _))) = impls.get(0) {
            def.push_str(&format!(
                "(struct MethodTable*) &ImplTable_{}_{}",
                self.struct_id, first_iface_id
            ));
        } else {
            def.push_str("NULL");
        }
        def.push_str(",\n");

        def.push_str("};\n\n");

        def
    }
}

pub struct Interface {
    id: InterfaceID,
    methods: Vec<FunctionDecl>,
}

impl Interface {
    pub fn translate(
        iface_id: InterfaceID,
        iface: &metadata::Interface,
        module: &mut Module,
    ) -> Self {
        let methods = iface
            .methods
            .iter()
            .enumerate()
            .map(|(method_index, method)| {
                let return_ty = Type::from_metadata(&method.return_ty, module);
                let method_id = MethodID(method_index);
                let params = method
                    .params
                    .iter()
                    .map(|param| Type::from_metadata(param, module))
                    .collect();

                let name = FunctionName::Method(iface_id, method_id);

                let comment = Some(format!(
                    "Method {} of interface {}",
                    method.name, iface.name
                ));

                FunctionDecl {
                    return_ty,
                    params,
                    name,
                    comment,
                }
            })
            .collect();

        Self {
            id: iface_id,
            methods,
        }
    }

    pub fn method_table_string(&self) -> String {
        let mut table = format!("struct MethodTable_{} {{\n", self.id.0);
        table.push_str("  struct MethodTable base;\n");

        for (method_index, method) in self.methods.iter().enumerate() {
            table.push_str("  ");
            let method_ptr_name = format!("method_{}", method_index);
            table.push_str(&method.ptr_type().to_decl_string(&method_ptr_name));
            table.push_str(";\n");
        }

        table.push_str("};\n\n");

        // vcall thunks for each method
        for (method_index, method) in self.methods.iter().enumerate() {
            table.push_str(&method.to_string());
            table.push_str(" {\n");

            let (self_arg_local, arg_offset) = match method.return_ty {
                Type::Void => ("L0", 0),
                _ => ("L1", 1),
            };

            // find the matching table
            table.push_str(&format!(
                "  struct MethodTable* table = {}->class->iface_methods;\n",
                self_arg_local
            ));
            table.push_str("  while (table) {\n");
            table.push_str(&format!("    if (table->iface == {}) {{\n", self.id.0));
            table.push_str(&format!(
                "      struct MethodTable_{}* my_table = (struct MethodTable_{}*) table;\n",
                self.id.0, self.id.0
            ));

            // get the pointer from the table
            table.push_str("      ");
            table.push_str(&method.ptr_type().to_decl_string("method_ptr"));
            table.push_str(&format!(" = my_table->method_{};\n", method_index));

            if method.return_ty == Type::Void {
                table.push_str("      ");
            } else {
                table.push_str("      return ");
            }

            // call the function
            table.push_str("method_ptr(");

            for (i, _param) in method.params.iter().enumerate() {
                if i > 0 {
                    table.push_str(", ");
                }
                table.push_str("L");
                table.push_str(&(i + arg_offset).to_string());
            }
            table.push_str(");\n");

            if method.return_ty == Type::Void {
                table.push_str("      return;\n");
            }

            table.push_str("    } else {\n");
            table.push_str("      table = table->next;\n");
            table.push_str("    }\n");

            table.push_str("  }\n");

            // missing vcall
            table.push_str("  abort();\n");

            table.push_str("}\n\n");
        }
        table
    }
}
