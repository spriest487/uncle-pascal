use std::{
    rc::Rc,
    collections::HashMap,
};
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

            node::UnitDecl::Var(parsed_var) => {
                let global_binding_kind = BindingKind::Mutable;
                let (var, new_scope) = VarDecl::annotate(parsed_var, scope, global_binding_kind)?;

                Ok((Some(node::UnitDecl::Var(var)), new_scope))
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

    pub fn reference_uses(mut scope: Scope,
                          uses: &[UnitReference],
                          available_units: &HashMap<String, impl AsRef<Scope>>)
                          -> SemanticResult<Scope> {
        for unit_ref in uses.iter() {
            let ref_name = unit_ref.name.to_string();

            match available_units.get(&ref_name) {
                Some(ref_scope) => {
                    scope = scope.reference(ref_scope.as_ref(), unit_ref.kind.clone());
                }
                None => return Err(SemanticError::unresolved_unit(
                    ref_name.clone(),
                    unit_ref.context.clone(),
                ))
            }
        }

        Ok(scope)
    }

    pub fn annotate(unit: &syntax::Unit,
                    available_units: &HashMap<String, impl AsRef<Scope>>)
                    -> Result<(Self, Rc<Scope>), SemanticError> {
        let unit_scope = Scope::new_unit(unit.name.as_str());
        let uses = Self::annotate_uses(unit.uses.iter(), Rc::new(unit_scope.clone()));

        let unit_scope = Self::reference_uses(unit_scope, &uses, available_units)?;

        let (interface_decls, interface_scope) = Unit::annotate_decls(
            unit.interface.iter(),
            Rc::new(unit_scope)
        )?;

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
            uses,
            initialization,
            finalization,
        };

        Ok((unit, interface_scope))
    }
}