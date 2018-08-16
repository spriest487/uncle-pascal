use std::rc::Rc;
use node;
use syntax;
use semantic::*;

pub type Unit = node::Unit<SemanticContext>;
pub type UnitDecl = node::UnitDecl<SemanticContext>;
pub type Implementation = node::Implementation<SemanticContext>;
pub type UnitReference = node::UnitReference<SemanticContext>;

impl Unit {
    fn annotate_decl(decl: &syntax::UnitDecl,
                     mut scope: Rc<Scope>) -> SemanticResult<(Option<UnitDecl>, Rc<Scope>)> {
        match decl {
            node::UnitDecl::Type(parsed_type_decl) => {
                let (type_decl, scope) = TypeDecl::annotate(parsed_type_decl, scope)?;
                let decl = type_decl.map(|decl| node::UnitDecl::Type(decl));

                Ok((decl, scope))
            }

            node::UnitDecl::Function(parsed_func) => {
                let (func_decl, scope) = FunctionDecl::annotate(parsed_func, scope)?;

                func_decl.type_check()?;

                Ok((Some(node::UnitDecl::Function(func_decl)), scope))
            }

            node::UnitDecl::Vars(parsed_vars) => {
                let (vars, scope) = VarDecls::annotate(parsed_vars, scope)?;

                Ok((Some(node::UnitDecl::Vars(vars)), scope))
            }

            node::UnitDecl::Consts(parsed_consts) => {
                /* consts exist in scope but we no longer need to store the declarations - they
                don't get emitted in the backend */
                for parsed_const in parsed_consts.decls.iter() {
                    scope = ConstDecl::annotate(parsed_const, scope)?;
                }

                Ok((None, scope))
            }
        }
    }

    pub fn annotate_impls<'a>(decls: impl IntoIterator<Item=&'a syntax::Implementation>,
                              mut scope: Rc<Scope>)
                              -> SemanticResult<(Vec<Implementation>, Rc<Scope>)> {
        let mut result = Vec::new();
        for decl in decls {
            match decl {
                node::Implementation::Decl(parsed_decl) => {
                    let (decl, new_scope) = Self::annotate_decl(parsed_decl, scope)?;
                    scope = new_scope;

                    if let Some(decl) = decl {
                        result.push(node::Implementation::Decl(decl));
                    }
                }

                node::Implementation::Function(parsed_func) => {
                    let (func, new_scope) = Function::annotate(parsed_func, scope)?;
                    scope = new_scope;

                    result.push(node::Implementation::Function(func));
                }
            }
        }
        Ok((result, scope))
    }

    pub fn annotate_decls<'a, TDecls>(decls: TDecls,
                                      mut scope: Rc<Scope>)
                                      -> SemanticResult<(Vec<UnitDecl>, Rc<Scope>)>
        where TDecls: IntoIterator<Item=&'a syntax::UnitDecl>,
    {
        let mut result = Vec::new();

        for parsed_decl in decls {
            let (decl, new_scope) = Self::annotate_decl(parsed_decl, scope)?;
            scope = new_scope;

            if let Some(decl) = decl {
                result.push(decl);
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

    pub fn annotate(unit: &syntax::Unit, scope: Rc<Scope>) -> Result<(Self, Rc<Scope>), SemanticError> {
        let unit_scope = Rc::new(scope.as_ref().clone().with_local_namespace(&unit.name));

        let (interface_decls, interface_scope) = Unit::annotate_decls(
            unit.interface.iter(),
            unit_scope.clone())?;

        let (impl_decls, impl_scope) = Unit::annotate_impls(
            unit.implementation.iter(),
            interface_scope.clone())?;

        let initialization = match unit.initialization.as_ref() {
            Some(block) => Some(Block::annotate(block, impl_scope.clone())?.0),
            None => None,
        };
        let finalization = match unit.finalization.as_ref() {
            Some(block) => Some(Block::annotate(block, impl_scope.clone())?.0),
            None => None,
        };

        let unit = Unit {
            interface: interface_decls,
            implementation: impl_decls,
            name: unit.name.clone(),
            uses: Self::annotate_uses(unit.uses.iter(), unit_scope),
            initialization,
            finalization,
        };

        Ok((unit, interface_scope))
    }
}