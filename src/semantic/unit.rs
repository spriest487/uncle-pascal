use std::rc::Rc;
use node::{self, RecordKind};
use syntax;
use semantic::*;

pub type Unit = node::Unit<ScopedSymbol, SemanticContext>;
pub type UnitDeclaration = node::UnitDeclaration<ScopedSymbol, SemanticContext>;
pub type UnitReference = node::UnitReference<SemanticContext>;

impl Unit {
    pub fn annotate_decls<'a, TDecls>(decls: TDecls,
                                      mut scope: Scope)
                                      -> Result<(Vec<UnitDeclaration>, Scope), SemanticError>
        where TDecls: IntoIterator<Item=&'a syntax::UnitDeclaration>,
    {
        let mut result = Vec::new();

        for decl in decls {
            match decl {
                node::UnitDeclaration::Type(parsed_type_decl) => {
                    let type_decl = TypeDecl::annotate(parsed_type_decl, Rc::new(scope.clone()))?;
                    match &type_decl {
                        node::TypeDecl::Record(record_decl) => {
                            scope = match record_decl.kind {
                                RecordKind::Record => {
                                    scope.with_record(record_decl.clone())
                                }
                                RecordKind::Class => {
                                    scope.with_class(record_decl.clone())
                                }
                            }
                        }

                        node::TypeDecl::Enumeration(enum_decl) => {
                            scope = scope.with_enumeration(enum_decl.clone());
                        }

                        node::TypeDecl::Set(set_decl) => {
                            scope = scope.with_set(set_decl.clone());
                        }

                        node::TypeDecl::Alias { alias, of, .. } => {
                            let alias_name = node::Identifier::from(alias);

                            scope = scope.with_alias(alias_name, of.clone());
                        }
                    }

                    result.push(node::UnitDeclaration::Type(type_decl));
                }

                node::UnitDeclaration::Function(parsed_func) => {
                    let func_decl = FunctionDecl::annotate(parsed_func, Rc::new(scope.clone()))?;

                    scope = scope.with_function(func_decl.name.clone(), func_decl.clone());

                    func_decl.type_check()?;

                    result.push(node::UnitDeclaration::Function(func_decl));
                }

                node::UnitDeclaration::Vars(parsed_vars) => {
                    let vars = VarDecls::annotate(parsed_vars, Rc::new(scope.clone()))?;

                    scope = scope.with_vars_local(vars.decls.iter());

                    result.push(node::UnitDeclaration::Vars(vars));
                }

                node::UnitDeclaration::Consts(parsed_consts) => {
                    let mut consts = ConstDecls::default();

                    for parsed_const in parsed_consts.decls.iter() {
                        let (const_decl, new_scope) = ConstDecl::annotate(parsed_const, Rc::new(scope))?;
                        scope = new_scope;

                        consts.decls.push(const_decl);
                    }

                    result.push(node::UnitDeclaration::Consts(consts));
                }
            }
        }

        Ok((result, scope))
    }

    pub fn annotate_uses<'a>(uses: impl IntoIterator<Item=&'a syntax::UnitReference>,
                             scope: Rc<Scope>)
                             -> Vec<UnitReference> {
        uses.into_iter()
            .map(|unit_ref| {
                UnitReference {
                    name: unit_ref.name.clone(),
                    context: SemanticContext {
                        /* at the moment, the scope provided must already have all the symbols
                        imported from the uses section, so we don't need to do anything here */
                        scope: scope.clone(),
                        token: unit_ref.context.token().clone(),
                    },
                    kind: unit_ref.kind.clone(),
                }
            })
            .collect()
    }

    pub fn annotate(unit: &syntax::Unit, scope: Scope) -> Result<(Self, Scope), SemanticError> {
        let unit_scope = scope.with_local_namespace(&unit.name);

        let (interface_decls, interface_scope) = Unit::annotate_decls(
            unit.interface.iter(),
            unit_scope.clone())?;

        let (impl_decls, _) = Unit::annotate_decls(
            unit.implementation.iter(),
            interface_scope.clone())?;

        let unit = Unit {
            interface: interface_decls,
            implementation: impl_decls,
            name: unit.name.clone(),
            uses: Self::annotate_uses(unit.uses.iter(), Rc::new(unit_scope)),
        };

        Ok((unit, interface_scope))
    }
}