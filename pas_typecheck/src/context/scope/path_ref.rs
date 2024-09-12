use pas_syn::{Ident, IdentPath};
use crate::{ScopeMember, ScopeMemberRef, Scope};

#[derive(Debug)]
pub struct ScopePathRef<'s> {
    pub(super) namespaces: Vec<&'s Scope>,
}

impl<'s> Clone for ScopePathRef<'s> {
    fn clone(&self) -> Self {
        ScopePathRef {
            namespaces: self.namespaces.clone(),
        }
    }
}

impl<'s> ScopePathRef<'s> {
    pub fn find(&self, key: &Ident) -> Option<ScopeMemberRef<'s>> {
        let mut current = self.namespaces.len() - 1;

        loop {
            let scope = &self.namespaces[current];
            let path = &self.namespaces[0..=current];

            if let Some((key, member)) = scope.get_member(key) {
                return match member {
                    ScopeMember::Scope(ns) => {
                        let mut path = path.to_vec();
                        path.push(ns);

                        Some(ScopeMemberRef::Scope {
                            path: ScopePathRef { namespaces: path },
                        })
                    }

                    ScopeMember::Decl(value) => {
                        let parent_path = ScopePathRef { namespaces: path.to_vec() };

                        Some(ScopeMemberRef::Decl {
                            key,
                            value,
                            parent_path,
                        })
                    },
                }
            }

            if scope.key() == Some(key) {
                return Some(ScopeMemberRef::Scope {
                    path: ScopePathRef { namespaces: path.to_vec() },
                });
            }

            if current == 0 {
                break;
            }

            current -= 1;
            // while current > 0 && !scope.can_access_members(self.namespaces[current]) {
            //     current -= 1;
            // }
        }

        None
    }

    pub fn as_slice(&self) -> &[&Scope] {
        self.namespaces.as_slice()
    }

    pub fn top(&self) -> &Scope {
        self.namespaces.last().unwrap()
    }

    pub fn keys(&self) -> impl Iterator<Item = &Ident> {
        self.namespaces.iter().cloned().filter_map(|s| s.key())
    }

    pub fn join(&self, sep: &str) -> String {
        self.namespaces
            .iter()
            .filter_map(|ns| ns.key())
            .map(|k| k.to_string())
            .collect::<Vec<_>>()
            .join(sep)
    }

    pub fn to_namespace(&self) -> IdentPath {
        let namespace = self.as_slice().iter().filter_map(|s| s.key().cloned());
        IdentPath::from_parts(namespace)
    }

    pub fn all_used_units(&self) -> Vec<&IdentPath> {
        let mut unit_paths = Vec::new();

        for scope in self.as_slice().iter().rev() {
            for used_unit in scope.use_units() {
                if !unit_paths.contains(&used_unit) {
                    unit_paths.push(used_unit);
                }
            }
        }

        unit_paths
    }

    pub fn is_used_unit(&self, unit_name: &IdentPath) -> bool {
        self.as_slice()
            .iter()
            .rev()
            .flat_map(|scope| scope.use_units.iter())
            .any(|used_unit| used_unit == unit_name)
    }

    pub fn is_parent_of(&self, other: &ScopePathRef) -> bool {
        if self.namespaces.len() >= other.namespaces.len() {
            return false;
        }

        other.namespaces
            .iter()
            .zip(self.namespaces.iter())
            .all(|(a, b)| a.id == b.id)
    }
}

#[derive(Debug)]
pub struct ScopePathRefMut<'s> {
    pub(super) namespaces: Vec<&'s mut Scope>,
}

impl<'s> ScopePathRefMut<'s> {
    pub fn top(&'s mut self) -> &'s mut Scope {
        self.namespaces.last_mut().unwrap()
    }

    pub fn as_slice(&mut self) -> &mut [&'s mut Scope] {
        self.namespaces.as_mut_slice()
    }
}
