use crate::ast::{FieldName, Module, Type, TypeDecl, TypeDefName};
use pas_ir::metadata;
use std::{
    fmt,
    hash::{Hash, Hasher}
};

#[derive(Clone, Eq)]
pub struct StructMember {
    pub name: FieldName,
    pub ty: Type,

    pub comment: Option<String>,
}

impl PartialEq for StructMember {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.ty == other.ty
    }
}

impl Hash for StructMember {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.ty.hash(state);
    }
}

#[derive(Clone, Eq)]
pub struct StructDef {
    pub decl: TypeDecl,
    pub members: Vec<StructMember>,

    pub comment: Option<String>,
}

impl PartialEq for StructDef {
    fn eq(&self, other: &Self) -> bool {
        self.decl == other.decl && self.members == other.members
    }
}

impl Hash for StructDef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.decl.hash(state);
        self.members.hash(state);
    }
}

impl StructDef {
    pub fn translate(
        id: metadata::TypeDefID,
        ir_struct: &metadata::Struct,
        module: &mut Module,
    ) -> Self {
        let mut members = Vec::new();

        if ir_struct.identity.is_ref_type() {
            members.push(StructMember {
                name: FieldName::Rc,
                ty: Type::Rc,
                comment: None,
            });
        }

        // fields should be written in the numerical order of their field IDs in the IR
        let mut sorted_fields: Vec<_> = ir_struct.fields.iter().collect();
        sorted_fields.sort_by_key(|(id, _)| (id.0));

        for (id, field) in sorted_fields {
            let ty = Type::from_metadata(&field.ty, module);

            members.push(StructMember {
                name: FieldName::ID(*id),
                ty,
                comment: None,
            });
        }

        let struct_ty = metadata::Type::Struct(id);
        let comment = module.pretty_type(&struct_ty).to_string();

        Self {
            decl: TypeDecl {
                name: TypeDefName::Struct(id),
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
        write!(f, "}}")
    }
}
