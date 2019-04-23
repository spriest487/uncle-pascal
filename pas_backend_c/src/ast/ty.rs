use std::{collections::HashMap, fmt};

use pas_ir::{
    metadata::{self, FieldID, StructID},
};

use crate::ast::Module;

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
}

impl Type {
    pub fn from_metadata(ty: &metadata::Type, module: &mut Module) -> Type {
        match ty {
            metadata::Type::Pointer(target) => Type::from_metadata(target.as_ref(), module).ptr(),
            metadata::Type::RcPointer(..) => Type::Struct(StructName::Rc).ptr(),
            metadata::Type::Struct(id) => Type::Struct(StructName::ID(*id)),
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
                left.push_str("char");
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
        }
    }

    pub fn to_decl_string(&self, name: &str) -> String {
        let mut left = String::new();
        let mut right = String::new();

        self.build_decl_string(&mut left, &mut right);

        format!("{} {} {}", left, name, right).trim().to_string()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum StructName {
    // internal struct for implementation of RC pointers
    Rc,

    // struct from ID
    ID(StructID),

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

pub struct StructDef {
    pub decl: StructDecl,
    pub members: HashMap<FieldID, Type>,
}

impl fmt::Display for StructName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StructName::Rc => write!(f, "Rc"),
            StructName::ID(id) => write!(f, "Struct{}", id),
            StructName::StaticArray(i) => write!(f, "StaticArray{}", i),
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
                (*id, ty)
            })
            .collect();

        Self {
            decl: StructDecl {
                name: StructName::ID(id),
            },
            members,
        }
    }
}

impl fmt::Display for StructDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{} {{", self.decl)?;
        for (id, member) in self.members.iter() {
            let name = format!("member_{}", id.0);
            writeln!(f, "{};", member.to_decl_string(&name))?;
        }
        write!(f, "}};")
    }
}