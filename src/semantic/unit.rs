use node;
use syntax;
use semantic::*;

pub type Unit = node::Unit<ScopedSymbol>;
pub type UnitDeclaration = node::UnitDeclaration<ScopedSymbol>;

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
                    let type_decl = TypeDecl::annotate(parsed_type_decl, &scope)?;
                    match &type_decl {
                        node::TypeDecl::Record(record_decl) => {
                            scope = scope.with_type(record_decl.name.clone(),
                                                    record_decl.record_type());
                        }

                        node::TypeDecl::Alias { alias, of, .. } => {
                            let alias_name = node::Identifier::from(alias);

                            scope = scope.with_type(alias_name, of.clone());
                        }
                    }

                    result.push(node::UnitDeclaration::Type(type_decl))
                }

                node::UnitDeclaration::Function(parsed_func) => {
                    let func_decl = Function::annotate(parsed_func, &scope)?;

                    scope = scope.with_symbol_absolute(func_decl.name.clone(),
                                                       func_decl.signature_type());

                    func_decl.type_check()?;

                    result.push(node::UnitDeclaration::Function(func_decl))
                }

                node::UnitDeclaration::Vars(parsed_vars) => {
                    let vars = Vars::annotate(parsed_vars, &scope, SemanticVarsKind::Namespaced)?;

                    scope = scope.with_vars_absolute(vars.decls.iter());

                    result.push(node::UnitDeclaration::Vars(vars))
                }
            }
        }

        Ok((result, scope))
    }

    pub fn annotate(unit: &syntax::Unit, scope: Scope) -> Result<(Self, Scope), SemanticError> {
        let unit_scope = scope.with_local_namespace(&unit.name);

        let (interface_decls, interface_scope) = Unit::annotate_decls(
            unit.interface.iter(),
            unit_scope)?;

        let (impl_decls, _) = Unit::annotate_decls(
            unit.implementation.iter(),
            interface_scope.clone())?;

        let unit = Unit {
            interface: interface_decls,
            implementation: impl_decls,
            name: unit.name.clone(),
            uses: unit.uses.clone(),
        };

        Ok((unit, interface_scope))
    }
}