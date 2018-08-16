use std::collections::hash_map::*;
use std::fmt;

use types::*;
use node::{self, Identifier, UnitReferenceKind};
use semantic::*;

#[derive(Clone, Debug)]
pub enum Named {
    Type(DeclaredType),
    Symbol(DeclaredType),
}

#[derive(Clone)]
pub struct Scope {
    local_name: Option<Identifier>,
    names: HashMap<Identifier, Named>,

    /* map of imported name => global name
     e.g. uses System.* adds String => System.String
     */
    imported_names: HashMap<Identifier, Identifier>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ScopedSymbol {
    /* symbol refers to a name that is in the current scope */
    Local {
        name: Identifier,
        decl_type: DeclaredType,
    },

    RecordMember {
        record_id: Identifier,
        record_type: DeclaredRecord,
        name: String,
    }
}

impl ScopedSymbol {
    #[allow(dead_code)]
    pub fn to_symbol(&self) -> Symbol {
        match self {
            &ScopedSymbol::Local { ref name, ref decl_type } => {
                Symbol::new(name.clone(), decl_type.clone())
            }

            &ScopedSymbol::RecordMember { ref record_id, ref name, ref record_type } => {
                let member_type = record_type.get_member(name)
                    .unwrap()
                    .decl_type
                    .clone();
                let member_id = record_id.child(name);

                Symbol::new(member_id, member_type)
            }
        }
    }

    pub fn decl_type(&self) -> DeclaredType {
        match self {
            &ScopedSymbol::Local { ref decl_type, .. } =>
                decl_type.clone(),

            &ScopedSymbol::RecordMember { ref record_type, ref name, .. } => {
                let member = record_type.members.iter()
                    .find(|m| m.name.to_string() == *name)
                    .unwrap();

                member.decl_type.clone()
            }
        }
    }
}

impl fmt::Display for ScopedSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ScopedSymbol::Local { ref name, ref decl_type } =>
                write!(f, "{} ({})", name, decl_type),

            &ScopedSymbol::RecordMember { ref record_id, ref name, ref record_type } => {
                write!(f, "{} ({} member of record {})",
                       record_id.child(name),
                       self.decl_type(),
                       record_type.name)
            }
        }
    }
}

impl node::Symbol for ScopedSymbol {
    type Type = DeclaredType;
}

impl node::ToSource for ScopedSymbol {
    fn to_source(&self) -> String {
        match self {
            ScopedSymbol::Local { name, .. } =>
                name.to_source(),
            ScopedSymbol::RecordMember { record_id, name, .. } =>
                format!("{}.{}", record_id.to_source(), name),
        }
    }
}

impl Default for Scope {
    fn default() -> Self {
        Scope::new().reference(Scope::system(), UnitReferenceKind::Namespaced)
    }
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Scope `{}` {{", match self.local_name {
            Some(ref ns) => ns.to_string(),
            None => "(root)".to_owned(),
        })?;

        let types: Vec<_> = self.names.iter()
            .flat_map(|(name, named)| match named {
                &Named::Type(ref dt) => Some((name, dt)),
                _ => None
            })
            .collect();

        let symbols: Vec<_> = self.names.iter()
            .flat_map(|(name, named)| match named {
                &Named::Symbol(ref dt) => Some((name, dt)),
                _ => None,
            })
            .collect();

        writeln!(f, "\ttypes: [")?;
        for (name, declared_type) in types {
            writeln!(f, "\t\t{}: {}", name, declared_type)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "\tsymbols: [")?;
        for (name, sym_type) in symbols {
            writeln!(f, "\t\t{}: {}", name, sym_type)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "\timported names: [")?;
        for (name, global_name) in self.imported_names.iter() {
            writeln!(f, "\t\t{}: {}", name, global_name)?;
        }
        writeln!(f, "\t]")?;

        writeln!(f, "}}")
    }
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            local_name: None,
            names: HashMap::new(),
            imported_names: HashMap::new(),
        }
    }

    pub fn local_name(&self) -> Option<&Identifier> {
        self.local_name.as_ref()
    }

    pub fn system() -> Self {
        let string_type = DeclaredType::Record(DeclaredRecord {
            name: Identifier::from("System.String"),
            kind: RecordKind::Class,
            members: vec![
                Symbol::new(Identifier::from("Chars"), DeclaredType::Byte.pointer()),
                Symbol::new(Identifier::from("Length"), DeclaredType::Integer),
            ],
        });

        Scope::new()
            .with_local_namespace("System")
            .with_type(Identifier::from("System.Byte"), DeclaredType::Byte)
            .with_type(Identifier::from("System.Integer"), DeclaredType::Integer)
            .with_type(Identifier::from("System.Pointer"), DeclaredType::RawPointer)
            .with_type(Identifier::from("System.Boolean"), DeclaredType::Boolean)

            /* these decls need to be built in to support string concatenation sugar */
            .with_type(Identifier::from("System.String"), string_type.clone())
            .with_symbol_absolute(Identifier::from("System.StringConcat"),
                                  DeclaredType::from(FunctionSignature {
                                      name: Identifier::from("StringConcat"),
                                      arg_types: vec![string_type.clone(), string_type.clone()],
                                      return_type: Some(string_type.clone()),
                                  }))
    }

    pub fn qualify_local_name(&self, name: &str) -> Identifier {
        match &self.local_name {
            &Some(ref local_name) => local_name.child(&name),
            &None => Identifier::from(name)
        }
    }

    pub fn with_type(mut self, name: Identifier, named_type: DeclaredType) -> Self {
        self.names.insert(name, Named::Type(named_type));
        self
    }

    pub fn with_symbol_local(mut self, name: &str, decl_type: DeclaredType) -> Self {
        let name_id = Identifier::from(name);
        assert_eq!(0, name_id.namespace.len(), "names passed to with_symbol_local must be unqualified, but got {}", name);
        self.names.insert(name_id, Named::Symbol(decl_type));
        self
    }

    pub fn with_symbol_absolute<TId>(mut self, name: TId, decl_type: DeclaredType) -> Self
        where TId: Into<Identifier>
    {
        self.names.insert(name.into(), Named::Symbol(decl_type));
        self
    }

    pub fn with_local_namespace(mut self, name: &str) -> Self {
        self.local_name = match self.local_name {
            Some(current_name) => Some(current_name.child(name)),
            None => Some(Identifier::from(name)),
        };

        self
    }

    pub fn with_vars_local<'a>(mut self, vars: impl IntoIterator<Item=&'a VarDecl>) -> Self {
        for var in vars {
            assert_eq!(var.name.namespace.len(), 0, "vars passed to with_vars_local should not be namespaced! was {}", var.name);

            self = self.with_symbol_local(&var.name.name, var.decl_type.clone());
        }
        self
    }

    pub fn with_vars_absolute<'a>(mut self,
                                  vars: impl IntoIterator<Item=&'a VarDecl>)
                                  -> Self {
        for var in vars {
            self = self.with_symbol_absolute(var.name.clone(), var.decl_type.clone());
        }
        self
    }

    pub fn reference(mut self,
                     other: Scope,
                     ref_kind: UnitReferenceKind) -> Self {
        for (name, named) in other.names {
            match ref_kind {
                UnitReferenceKind::All => {
                    let imported_name = Identifier::from(&name.name);
                    self.imported_names.insert(imported_name, name.clone());
                }

                UnitReferenceKind::Name(ref imported_name) => {
                    if name.name == *imported_name {
                        self.imported_names.insert(Identifier::from(imported_name),
                                                   name.clone());
                    }
                },

                UnitReferenceKind::Namespaced => {
                    //do nothing
                },
            }

            self.names.insert(name, named);
        }
        self
    }

    pub fn reference_all(mut self,
                         others: impl IntoIterator<Item=Scope>)
                         -> Self {
        for scope in others {
            self = self.reference(scope, UnitReferenceKind::Namespaced);
        }
        self
    }

    fn get_symbol_imported(&self, name: &Identifier) -> Option<ScopedSymbol> {
        self.imported_names.get(name)
            .and_then(|global_name| self.get_symbol_global(global_name))
    }

    fn get_symbol_global(&self, name: &Identifier) -> Option<ScopedSymbol> {
        match self.names.get(&name) {
            Some(&Named::Symbol(ref symbol_type)) => {
                let local_sym = ScopedSymbol::Local {
                    name: name.clone(),
                    decl_type: symbol_type.clone(),
                };

                Some(local_sym)
            }
            _ => None
        }
    }

    pub fn get_symbol(&self, name: &Identifier) -> Option<ScopedSymbol> {
        self.local_name.as_ref()
            .and_then(|local_name| {
                let name_in_local_ns = local_name.append(name);

                self.get_symbol_global(&name_in_local_ns)
            })
            .or_else(|| {
                // search local names which refer to records to see if
                // this is the field of a record
                name.parent().and_then(|record_id| {
                    self.find_record_member(&record_id, &name.name)
                })
            })
            .or_else(|| {
                self.get_symbol_imported(name)
            })
            .or_else(|| {
                self.get_symbol_global(name)
            })
    }

    fn get_type_global(&self, name: &Identifier) -> Option<DeclaredType> {
        match self.names.get(&name) {
            Some(Named::Type(result)) => Some(result.clone()),
            _ => None
        }
    }

    fn get_type_imported(&self, name: &Identifier) -> Option<DeclaredType> {
        self.imported_names.get(name)
            .and_then(|global_name| self.get_type_global(global_name))
    }

    pub fn get_type(&self, name: &Identifier) -> Option<DeclaredType> {
        self.local_name.as_ref()
            .and_then(|local_name| {
                let name_in_local_ns = local_name.append(name);

                self.get_type_global(&name_in_local_ns)
            })
            .or_else(|| {
                self.get_type_imported(name)
            })
            .or_else(|| {
                self.get_type_global(name)
            })
    }

    fn find_record_member(&self,
                          parent_id: &Identifier,
                          child_name: &str) -> Option<ScopedSymbol> {
        let parent_sym = self.get_symbol(&parent_id)?;

        let record_decl = match parent_sym.decl_type() {
            DeclaredType::Record(record_type) =>
                Some(record_type),

            DeclaredType::Pointer(ref ptr_to_record) if ptr_to_record.is_record() =>
                Some(ptr_to_record.clone().unwrap_record()),

            _ => None
        }?;

        Some(ScopedSymbol::RecordMember {
            record_id: parent_id.clone(),
            name: child_name.to_owned(),
            record_type: record_decl,
        })
    }
}
