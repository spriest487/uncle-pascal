use crate::{pas_ty, translate_name, Module, NamePath, Type, TypeDefID};
use linked_hash_map::LinkedHashMap;
use pas_common::span::{Span, Spanned};
use pas_syn::ast::StructKind;
use std::{collections::HashMap, fmt};
use pas_typecheck::layout::{StructLayoutMember};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StructFieldDef {
    pub name: Option<String>,
    pub ty: Type,
    pub rc: bool,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum StructIdentity {
    Record(NamePath),
    Class(NamePath),
    Array(Type, usize),
    DynArray(Type),
    Closure(ClosureIdentity),
}

impl StructIdentity {
    pub fn is_ref_type(&self) -> bool {
        match self {
            StructIdentity::Array(..) | StructIdentity::Record(..) => false,
            StructIdentity::Class(..) | StructIdentity::DynArray(..) | StructIdentity::Closure(..) => true,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ClosureIdentity {
    pub func_ty_id: TypeDefID,
    pub module: String,
    pub line: usize,
    pub col: usize,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Struct {
    pub identity: StructIdentity,
    pub fields: LinkedHashMap<FieldID, StructFieldDef>,

    pub src_span: Option<Span>,
}

impl Struct {
    pub fn find_field(&self, name: &str) -> Option<FieldID> {
        self.fields.iter().find_map(|(id, field)| {
            let field_name = field.name.as_ref()?;
            if field_name == name {
                Some(*id)
            } else {
                None
            }
        })
    }

    pub fn get_field(&self, id: FieldID) -> Option<&StructFieldDef> {
        self.fields.get(&id)
    }

    pub fn new(identity: StructIdentity, src_span: Option<Span>) -> Self {
        Self {
            identity,
            fields: LinkedHashMap::new(),
            src_span,
        }
    }

    pub fn name(&self) -> Option<&NamePath> {
        match &self.identity {
            StructIdentity::Class(name) | StructIdentity::Record(name) => Some(name),
            StructIdentity::Closure(..) | StructIdentity::Array(..) | StructIdentity::DynArray(..) => None,
        }
    }

    pub fn with_field(mut self, name: impl Into<String>, ty: Type, rc: bool) -> Self {
        let id = self
            .fields
            .keys()
            .max_by_key(|id| id.0)
            .map(|id| FieldID(id.0 + 1))
            .unwrap_or(FieldID(0));

        self.fields.insert(
            id,
            StructFieldDef {
                name: Some(name.into()),
                ty,
                rc,
            },
        );

        self
    }

    pub fn with_fields(mut self, fields: HashMap<FieldID, StructFieldDef>) -> Self {
        self.fields.extend(fields);
        self
    }
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.identity {
            StructIdentity::Array(ty, dim) => write!(f, "array[{}] of {}", dim, ty),
            StructIdentity::DynArray(ty) => write!(f, "array of {}", ty),
            StructIdentity::Class(name) | StructIdentity::Record(name) => write!(f, "{}", name),
            StructIdentity::Closure(identity) => write!(
                f,
                "closure of function type {} @ {}:{}:{}",
                identity.func_ty_id, identity.module, identity.line, identity.col
            ),
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd)]
pub struct FieldID(pub usize);

impl fmt::Display for FieldID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn translate_struct_def(
    struct_def: &pas_ty::ast::StructDef,
    type_args: Option<&pas_ty::TypeList>,
    module: &mut Module,
) -> Struct {
    let name_path = translate_name(&struct_def.name, type_args, module);

    let mut fields = HashMap::new();
    let mut pad_run = 0;
    let mut next_id = FieldID(0);
    for member in module.aligned_struct_members(struct_def) {
        match member {
            StructLayoutMember::Data { member, .. } => {
                if pad_run > 0 {
                    fields.insert(next_id, StructFieldDef {
                        name: None,
                        rc: false,
                        ty: Type::U8.array(pad_run),
                    });
                    pad_run = 0;
                    next_id.0 += 1;
                }

                let name = member.ident.to_string();
                let ty = module.translate_type(&member.ty, type_args);
                let rc = member.ty.is_rc_reference();
                fields.insert(next_id, StructFieldDef { name: Some(name), ty, rc });
                next_id.0 += 1;
            }

            StructLayoutMember::PaddingByte => {
                pad_run += 1;
            }
        }
    }

    if pad_run > 0 {
        let pad_ty = Type::U8.array(pad_run);
        fields.insert(next_id, StructFieldDef { name: None, rc: false, ty: pad_ty });
    }

    let src_span = struct_def.span().clone();

    let identity = match struct_def.kind {
        StructKind::Class => StructIdentity::Class(name_path),
        StructKind::Record | StructKind::PackedRecord => {
            StructIdentity::Record(name_path)
        },
    };

    Struct::new(identity, Some(src_span)).with_fields(fields)
}
