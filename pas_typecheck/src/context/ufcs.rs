use std::rc::Rc;

use pas_syn::{Ident, IdentPath};

use crate::ast::FunctionDecl;
use crate::{Context, Decl, FunctionSig, Member, Namespace, NamingResult, Scope, Type};

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

        Type::GenericParam(generic_param_ty) => match &generic_param_ty.is_iface {
            Some(is_iface) => instance_methods_of(is_iface, ctx),
            None => Ok(Vec::new()),
        },

        _ => {
            let mut methods = Vec::new();

            methods.extend(find_ufcs_free_functions(ty, ctx));
            methods.extend(find_iface_impl_methods(ty, ctx)?);

            Ok(methods)
        }
    }
}

fn find_ufcs_free_functions(ty: &Type, ctx: &Context) -> Vec<InstanceMethod> {
    // the namespaces we look for UFCS methods in - the current NS and any used NSs
    let mut namespaces = vec![ctx.namespace()];
    namespaces.extend(ctx.all_used_units());

    let mut methods = Vec::new();

    ctx.scopes.visit_members(|name_parts, member| {
        // eprintln!("visiting {}", IdentPath::from_parts(name_parts.to_vec()));
        member_to_ufcs_free_functions(
            ty,
            name_parts,
            member,
            &namespaces,
            &mut methods,
        )
    });

    methods
}

fn member_to_ufcs_free_functions(
    ty: &Type,
    name_parts: &[Ident],
    member: &Member<Scope>,
    namespaces: &[IdentPath],
    methods: &mut Vec<InstanceMethod>,
) {
    match member {
        Member::Name(Decl::Function { sig, visibility: _ }) if !sig.params.is_empty() => {
            if name_parts.len() <= 1 {
                return;
            }

            let name_parent_ns = &name_parts[0..name_parts.len() - 1];
            if !namespaces.iter().any(|ns| ns.as_slice() == name_parent_ns) {
                return;
            }

            let self_param = &sig.params[0];

            if self_param.ty == *ty
                || (self_param.ty.contains_generic_params() && self_param.ty.same_decl_type(ty))
            {
                methods.push(InstanceMethod::FreeFunction {
                    func_name: IdentPath::from_parts(name_parts.to_vec()),
                    sig: sig.clone(),
                })
            }
        }

        Member::Namespace(ns) => {
            let nested_ns_keys = ns.keys();

            let nested_ns_members = nested_ns_keys.into_iter().filter_map(|k| ns.get_member(&k));

            for (key, member) in nested_ns_members {
                let mut member_name = name_parts.to_vec();
                member_name.push(key.clone());

                member_to_ufcs_free_functions(ty, &member_name, member, namespaces, methods);
            }
        }

        _ => {}
    }
}

fn find_iface_impl_methods(ty: &Type, ctx: &Context) -> NamingResult<Vec<InstanceMethod>> {
    let mut methods = Vec::new();

    for (iface_ident, iface_impls) in &ctx.iface_impls {
        let iface_def = ctx.find_iface_def(iface_ident)?;
        let iface_ty = Type::Interface(iface_ident.clone());

        if !(iface_impls.contains_key(ty)) {
            continue;
        }

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

    Ok(methods)
}

#[cfg(test)]
mod test {
    use crate::context::ufcs::find_ufcs_free_functions;
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

        let methods = find_ufcs_free_functions(&target, &unit.context);

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
        let methods = find_ufcs_free_functions(&target, &c.context);

        assert_eq!(methods.len(), 1);
        assert_eq!(methods[0].ident().name, "TargetMethod");
    }
}
