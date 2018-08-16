use std::collections::hash_map::*;
use std::fmt;

use semantic::*;
use node;
use types::*;

#[derive(Clone, Debug)]
pub enum Named {
    Type(DeclaredType),
    Symbol(DeclaredType),
}

#[derive(Clone, Debug)]
pub struct Scope {
    local_name: Option<node::Identifier>,
    names: HashMap<String, Named>,

    children: HashMap<String, Scope>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ScopedSymbol {
    /* symbol refers to a name that is in the current scope */
    Local {
        name: String,
        decl_type: DeclaredType,
    },

    /* symbol refers to a member of a child scope (or record) */
    Child {
        scope: node::Identifier,
        name: String,
        decl_type: DeclaredType,
    },

    RecordMember {
        record_id: node::Identifier,
        record_type: DeclaredRecord,
        name: String,
    },
}

impl ScopedSymbol {
    #[allow(dead_code)]
    pub fn to_symbol(&self) -> Symbol {
        match self {
            &ScopedSymbol::Local { ref name, ref decl_type } => {
                let id = node::Identifier::from(name.as_str());
                Symbol::new(id, decl_type.clone())
            }

            &ScopedSymbol::Child { ref scope, ref name, ref decl_type } => {
                let id = scope.child(name);
                Symbol::new(id, decl_type.clone())
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
            &ScopedSymbol::Local { ref decl_type, .. } => decl_type.clone(),
            &ScopedSymbol::Child { ref decl_type, .. } => decl_type.clone(),
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
            &ScopedSymbol::Child { ref scope, ref name, ref decl_type } =>
                write!(f, "{} ({})", scope.child(name), decl_type),
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

impl Default for Scope {
    fn default() -> Self {
        Scope::new().with_child("System".to_owned(), Scope::system())
    }
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            local_name: None,
            names: HashMap::new(),
            children: HashMap::new(),
        }
    }

    pub fn system() -> Self {
        Scope::new()
            .with_local_namespace("System")
            .with_type("Byte".to_owned(), DeclaredType::Byte)
            .with_type("Integer".to_owned(), DeclaredType::Integer)
            .with_type("String".to_owned(), DeclaredType::String)
            .with_type("Pointer".to_owned(), DeclaredType::RawPointer)
            .with_type("Boolean".to_owned(), DeclaredType::Boolean)
            .with_symbol("WriteLn".to_owned(),
                         DeclaredType::from(FunctionSignature {
                             name: node::Identifier::from("WriteLn"),
                             arg_types: vec![DeclaredType::String],
                             return_type: None,
                         }))
            .with_symbol("GetMem".to_owned(),
                         DeclaredType::from(FunctionSignature {
                             name: node::Identifier::from("GetMem"),
                             arg_types: vec![DeclaredType::Integer],
                             return_type: Some(DeclaredType::Byte.pointer()),
                         }))
            .with_symbol("FreeMem".to_owned(),
                         DeclaredType::from(FunctionSignature {
                             name: node::Identifier::from("FreeMem"),
                             arg_types: vec![DeclaredType::Byte.pointer()],
                             return_type: None,
                         }))
    }

    pub fn qualify_local_name(&self, name: &str) -> String {
        match &self.local_name {
            &Some(ref local_name) => local_name.child(&name).to_string(),
            &None => name.to_owned()
        }
    }

    pub fn with_child(mut self, name: String, child: Scope) -> Self {
        let qualified_name = self.qualify_local_name(&name);
        self.children.insert(qualified_name, child);
        self
    }

    pub fn with_type(mut self, name: String, named_type: DeclaredType) -> Self {
        let qualified_name = self.qualify_local_name(&name);
        self.names.insert(qualified_name, Named::Type(named_type));
        self
    }

    pub fn with_symbol(mut self, name: String, decl_type: DeclaredType) -> Self {
        let qualified_name = self.qualify_local_name(&name);
        self.names.insert(qualified_name, Named::Symbol(decl_type));
        self
    }

    pub fn with_local_namespace(mut self, name: &str) -> Self {
        self.local_name = match self.local_name {
            Some(current_name) => Some(current_name.child(name)),
            None => Some(node::Identifier::from(name)),
        };

        self
    }

    pub fn with_vars<'a, TIter>(mut self, vars: TIter) -> Self
        where TIter: IntoIterator<Item=&'a VarDecl>
    {
        for var in vars {
            self = self.with_symbol(var.name.clone(), var.decl_type.clone());
        }
        self
    }

    pub fn get_symbol(&self, name: &node::Identifier) -> Option<ScopedSymbol> {
        match &self.local_name {
            &None => self.get_symbol_global(name),
            &Some(ref local_name) => {
                let name_in_local_ns = local_name.append(name);

                self.get_symbol_global(&name_in_local_ns)
                    .or_else(|| self.get_symbol_global(name))
            }
        }
    }

    pub fn get_type(&self, name: &node::Identifier) -> Option<DeclaredType> {
        match &self.local_name {
            &None => {
                self.get_type_global(name)
            },
            &Some(ref local_name) => {
                let name_in_local_ns = local_name.append(name);

                self.get_type_global(&name_in_local_ns)
                    .or_else(|| self.get_type_global(name))
            }
        }
    }

    fn get_symbol_global(&self, name: &node::Identifier) -> Option<ScopedSymbol> {
        match name.parent() {
            /* the identifier is qualified with a namespace, and exists either in a child scope,
            or as a member of a record in the local scope or a child scope */
            Some(parent_id) => {
                /* find a matching record in any scope - this is a recursive search which first
                 looks for the parent of the symbol name as a record name, then the parent of
                 that, etc */
                let record_member = self.find_record_member(&parent_id, &name.name);

                /* if it's not a record member, look for variables in all child scopes instead */
                record_member.or_else(|| -> Option<ScopedSymbol> {
                    //search for child ns, e.g. for name System.Foo.Bar, find System scope
                    self.children.get(&parent_id.head())
                        .and_then(|child_scope| {
                            //look for Foo.Bar in System
                            child_scope.get_symbol(&name.tail().unwrap())
                        })
                        .map(|child_sym| match child_sym {
                            ScopedSymbol::Local { ref name, ref decl_type } => {
                                ScopedSymbol::Child {
                                    scope: parent_id.clone(),
                                    name: name.clone(),
                                    decl_type: decl_type.clone(),
                                }
                            }
                            ScopedSymbol::Child { ref scope, ref name, ref decl_type } => {
                                ScopedSymbol::Child {
                                    scope: parent_id.append(scope),
                                    name: name.clone(),
                                    decl_type: decl_type.clone(),
                                }
                            }
                            ScopedSymbol::RecordMember { ref record_id, ref name, ref record_type } => {
                                ScopedSymbol::RecordMember {
                                    record_id: parent_id.append(record_id),
                                    name: name.clone(),
                                    record_type: record_type.clone(),
                                }
                            }
                        })
                })
            }
            None => match self.names.get(&name.name) {
                Some(&Named::Symbol(ref symbol_type)) => {
                    let local_sym = ScopedSymbol::Local {
                        name: name.name.clone(),
                        decl_type: symbol_type.clone(),
                    };

                    Some(local_sym)
                }
                _ => None
            }
        }
    }

    fn find_record_member(&self,
                          parent_id: &node::Identifier,
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

    fn get_type_global(&self, name: &node::Identifier) -> Option<DeclaredType> {
        fn type_named_in_child(child_ns: &str, child_type: &DeclaredType) -> DeclaredType {
            match child_type {
                &DeclaredType::Record(ref record_decl) => {
                    let mut qualified_decl = record_decl.clone();
                    qualified_decl.name = node::Identifier::from(child_ns)
                        .append(&record_decl.name);

                    DeclaredType::Record(qualified_decl)
                }

                &DeclaredType::Function(ref sig) => {
                    let mut qualified_sig = sig.clone();
                    qualified_sig.name = node::Identifier::from(child_ns)
                        .append(&sig.name);

                    DeclaredType::Function(qualified_sig)
                }

                &DeclaredType::Pointer(ref target) => {
                    type_named_in_child(child_ns, target.as_ref())
                        .pointer()
                }

                //type does not need qualifying
                _ => child_type.clone()
            }
        }

        if name.namespace.len() > 0 {
            let child_scope_name = &name.namespace[0];
            let name_in_child_scope = name.tail().unwrap();

            self.children.get(child_scope_name)
                .and_then(|child_scope| {
                    child_scope.get_type(&name_in_child_scope)
                })
                .map(|type_in_child| {
                    type_named_in_child(child_scope_name, &type_in_child)
                })
        } else {
            match self.names.get(&name.name) {
                Some(&Named::Type(ref result)) => Some(result.clone()),
                _ => None
            }
        }
    }
}
