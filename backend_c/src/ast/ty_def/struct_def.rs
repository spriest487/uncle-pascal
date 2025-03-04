use crate::ast::Unit;
use crate::ast::Type;
use crate::ast::TypeDecl;
use crate::ast::TypeDefName;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use crate::ir;

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

    pub packed: bool,

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
        id: ir::TypeDefID,
        ir_struct: &ir::Struct,
        module: &mut Unit,
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

        let struct_ty = ir::Type::Struct(id);
        let comment = module.pretty_type(&struct_ty).to_string();

        // user-defined types will have explicit padding, so they should be packed to avoid
        // the C frontend inserting any extra padding
        let packed = match &ir_struct.identity {
            ir::StructIdentity::Record(_) | ir::StructIdentity::Class(_) | ir::StructIdentity::Array(..) => true,
            _ => false,
        };

        Self {
            decl: TypeDecl {
                name: TypeDefName::Struct(id),
            },
            packed,
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

        if self.packed {
            write!(f, "PACKED_DECL(")?;
        }

        writeln!(f, "{} {{", self.decl)?;
        for member in self.members.iter() {
            if let Some(comment) = &member.comment {
                writeln!(f, "/** {} */", comment)?;
            }

            let name = format!("{}", member.name);
            writeln!(f, "{};", member.ty.to_decl_string(&name))?;
        }
        write!(f, "}}")?;

        if self.packed {
            write!(f, ")")?;
        }

        Ok(())
    }
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum FieldName {
    // ID from metadata
    ID(ir::FieldID),

    // rc state for rc types
    Rc,

    // builtin name: ref count field of RC internal struct
    RcStrongCount,
    RcWeakCount,

    // builtin name: class info pointer field of RC internal struct
    RcClass,
    
    // internal class property that stores the class's allocation function
    DynArrayAlloc,

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
            FieldName::Rc => write!(f, "rc"),
            FieldName::RcStrongCount => write!(f, "strong_count"),
            FieldName::RcWeakCount => write!(f, "weak_count"),
            FieldName::RcClass => write!(f, "class"),
            FieldName::StaticArrayElements => write!(f, "elements"),
            FieldName::DynArrayAlloc => write!(f, "alloc"),
            FieldName::VariantTag => write!(f, "tag"),
            FieldName::VariantData => write!(f, "data"),
            FieldName::VariantDataCase(case) => write!(f, "data_{}", case),
        }
    }
}
