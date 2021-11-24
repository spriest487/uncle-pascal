use std::rc::Rc;

use pas_syn::{Ident, IdentPath};

use crate::ast::FunctionDecl;
use crate::{Context, Decl, FunctionSig, MemberRef, NamingResult, Type};

#[derive(Clone, Debug)]
pub enum InstanceMethod {
    FreeFunction {
        func_name: IdentPath,
        sig: Rc<FunctionSig>,
    },
    Method {
        iface_ty: Type,
        decl: FunctionDecl,
    },
}

impl InstanceMethod {
    pub fn ident(&self) -> &Ident {
        match self {
            InstanceMethod::FreeFunction { func_name, .. } => func_name.last(),
            InstanceMethod::Method { decl, .. } => decl.ident.last(),
        }
    }
}

/// an instance method is an interface impl method for `ty` that takes Self as the first argument
/// OR a virtual method of the interface type referenced by `ty`
/// OR any free function taking `ty` as its first parameter (UFCS syntax)
/// OR any free function taking a generic type, or a parameterized type into which `ty` can
///    be substituted, as its first parameter (generic UFCS)
pub fn instance_methods_of(ty: &Type, ctx: &Context) -> NamingResult<Vec<InstanceMethod>> {
    match ty {
        Type::Interface(iface) => {
            let iface_def = ctx.find_iface_def(iface)?;
            let methods = iface_def
                .methods
                .iter()
                .map(|method_decl| InstanceMethod::Method {
                    iface_ty: ty.clone(),
                    decl: method_decl.clone(),
                })
                .collect();

            Ok(methods)
        }

        Type::GenericParam(generic_param_ty) => {
            match &generic_param_ty.is_iface {
                Some(is_iface) => instance_methods_of(is_iface, ctx),
                None => Ok(Vec::new()),
            }
        },

        _ => {
            let mut methods = Vec::new();

            methods.extend(find_ufcs_methods(ty, ctx));
            methods.extend(find_iface_impl_methods(ty, ctx)?);

            Ok(methods)
        }
    }
}

fn find_ufcs_methods(ty: &Type, ctx: &Context) -> Vec<InstanceMethod> {
    let mut members = Vec::new();

    let current_ns = ctx.namespace();
    ctx.scopes.visit_all(|path, _| {
        let path = IdentPath::from_parts(path.to_vec());
        if path.parent().as_ref() == Some(&current_ns) {
            members.push(ctx.find(path.last()).unwrap());
        }
    });

    let methods = members.into_iter()
        .filter_map(|member| {
            match member {
                MemberRef::Name {
                    value: Decl::Function { sig, visibility: _ },
                    ref parent_path,
                    key,
                } => {
                    let path =
                        IdentPath::from_parts(parent_path.keys().cloned()).child(key.clone());

                    if sig.params.is_empty() {
                        return None;
                    }

                    let self_param = &sig.params[0];

                    if self_param.ty == *ty
                        || (self_param.ty.contains_generic_params()
                            && self_param.ty.same_decl_type(ty))
                    {
                        Some(InstanceMethod::FreeFunction {
                            func_name: path,
                            sig: sig.clone(),
                        })
                    } else {
                        None
                    }
                }

                _ => {
                    // name is not a function
                    None
                }
            }
        })
        .collect();

    methods
}

fn find_iface_impl_methods(ty: &Type, ctx: &Context) -> NamingResult<Vec<InstanceMethod>> {
    let mut methods = Vec::new();

    for (iface_ident, iface_impls) in &ctx.iface_impls {
        let iface_decl = ctx
            .resolve(iface_ident)
            .and_then(|member| member.as_value())
            .unwrap();

        let (iface_ty, iface) = match iface_decl {
            Decl::Type {
                ty: iface_ty @ Type::Interface(..),
                ..
            } => match iface_ty {
                Type::Interface(iface) => (iface_ty, iface),
                _ => unreachable!(),
            },

            _ => panic!("invalid kind of decl referenced in iface impl"),
        };

        if iface_impls.contains_key(ty) {
            let iface_def = ctx.find_iface_def(iface)?;
            let iface_instance_methods = iface_def.methods.iter().filter(|method_decl| {
                method_decl
                    .params
                    .get(0)
                    .map(|arg_0| arg_0.ty == Type::MethodSelf)
                    .unwrap_or(false)
            });

            // add all the methods, we don't need to check if they're actually defined
            // or implemented - we should check that elsewhere
            for method in iface_instance_methods {
                methods.push(InstanceMethod::Method {
                    iface_ty: iface_ty.clone(),
                    decl: method.clone(),
                });
            }
        }
    }

    Ok(methods)
}

#[cfg(test)]
mod test {
    use crate::context::ufcs::find_ufcs_methods;
    use crate::test::{unit_from_src, units_from_src};
    use crate::Type;

    #[test]
    fn finds_ufcs_func() {
        let unit = unit_from_src(
            "test",
            r"type UFCSTarget = class
            end;

            export function TargetMethod(t: UFCSTarget)
            begin
            end;",
        );

        let target = Type::of_decl(&unit.unit.type_decls().next().unwrap());
        assert_eq!(target.full_path().unwrap().last().name, "UFCSTarget");

        let methods = find_ufcs_methods(&target, &unit.context);

        assert_eq!(methods.len(), 1);
        assert_eq!(methods[0].ident().name, "TargetMethod");
    }

    #[test]
    fn finds_exported_ufcs_func_from_other_unit() {
        let a_src = r"export type UFCSTarget = class
            end;";

        let b_src = r"export function TargetMethod(t: A.UFCSTarget)
            begin
            end";

        let c_src = "uses A;uses B;";

        let units = units_from_src(vec![("A", a_src), ("B", b_src), ("C", c_src)]);

        let a = &units[0];
        let c = &units[2];

        let target = Type::of_decl(&a.unit.type_decls().next().unwrap());
        let methods = find_ufcs_methods(&target, &c.context);

        assert_eq!(methods.len(), 1);
        assert_eq!(methods[0].ident().name, "TargetMethod");
    }
}
