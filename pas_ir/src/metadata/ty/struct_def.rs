use crate::{Module, NamePath, pas_ty, translate_name, Type, TypeDefID};
use linked_hash_map::LinkedHashMap;
use pas_common::span::{Span, Spanned};
use std::collections::HashMap;
use std::fmt;
use pas_syn::ast::CompositeTypeKind;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StructFieldDef {
    pub name: String,
    pub ty: Type,
    pub rc: bool,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum StructIdentity {
    Record(NamePath),
    Class(NamePath),
    Closure(ClosureIdentity),
}

impl StructIdentity {
    pub fn is_ref_type(&self) -> bool {
        match self {
            StructIdentity::Record(..) => false,
            StructIdentity::Class(..) | StructIdentity::Closure(..) => true,
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
            if field.name.as_str() == name {
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
            StructIdentity::Closure(..) => None,
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
                name: name.into(),
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

pub fn translate_class(
    class_def: &pas_ty::ast::Composite,
    type_args: Option<&pas_ty::TypeList>,
    module: &mut Module,
) -> Struct {
    let name_path = translate_name(&class_def.name, type_args, module);

    let mut fields = HashMap::new();
    for (id, member) in class_def.members.iter().enumerate() {
        let name = member.ident.to_string();
        let ty = module.translate_type(&member.ty, type_args);
        let rc = member.ty.is_rc_reference();

        fields.insert(FieldID(id), StructFieldDef { name, ty, rc });
    }

    let src_span = class_def.span().clone();

    let identity = match class_def.kind {
        CompositeTypeKind::Class => StructIdentity::Class(name_path),
        CompositeTypeKind::Record => StructIdentity::Record(name_path),
    };

    Struct::new(identity, Some(src_span)).with_fields(fields)
}