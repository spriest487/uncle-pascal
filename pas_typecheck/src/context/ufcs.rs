use std::rc::Rc;

use pas_syn::{Ident, IdentPath};

use crate::ast::FunctionDecl;
use crate::{Context, Decl, FunctionSig, NameResult, Type};

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
pub fn find_instance_methods_of(ty: &Type, ctx: &Context) -> NameResult<Vec<InstanceMethod>> {
    match ty {
        Type::Interface(iface) => {
            let iface_def = ctx.find_iface_def(iface)?;
            let methods = iface_def
                .methods
                .iter()
                .map(|method_decl| InstanceMethod::Method {
                    iface_ty: ty.clone(),
                    decl: method_decl.decl.clone(),
                })
                .collect();

            Ok(methods)
        }

        Type::GenericParam(generic_param_ty) => match &generic_param_ty.is_iface {
            Some(is_iface) => find_instance_methods_of(is_iface, ctx),
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
    namespaces.extend(ctx.scopes.current_path().all_used_units());

    let mut methods = Vec::new();

    ctx.scopes.visit_visible(|ns_path, key, decl| {
        let sig = match decl {
            Decl::Function { sig, .. } if sig.params.len() > 0 => sig.clone(),
            _ => return,
        };

        if !namespaces.iter().any(|ns| ns.as_slice() == ns_path) {
            return;
        }

        let self_param = &sig.params[0];

        if self_param.ty == *ty
            || (self_param.ty.contains_generic_params() && self_param.ty.same_decl_type(ty))
        {
            let func_name = IdentPath::new(key.clone(), ns_path.to_vec());

            methods.push(InstanceMethod::FreeFunction {
                func_name,
                sig: sig.clone(),
            })
        }
    });

    methods
}

fn find_iface_impl_methods(ty: &Type, ctx: &Context) -> NameResult<Vec<InstanceMethod>> {
    let mut methods = Vec::new();

    for (iface_ident, iface_impls) in &ctx.iface_impls {
        let iface_def = ctx.find_iface_def(iface_ident)?;
        let iface_ty = Type::Interface(Box::new(iface_ident.clone()));

        if !(iface_impls.contains_key(ty)) {
            continue;
        }

        let iface_instance_methods = iface_def.methods.iter().filter(|method_decl| {
            method_decl
                .decl
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
                decl: method.decl.clone(),
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
            r"interface

            type UFCSTarget = class
            end;

            function TargetMethod(t: UFCSTarget)
            begin
            end;

            end",
        );

        let target = Type::of_decl(&unit.unit.type_decls().next().unwrap());
        assert_eq!(target.full_path().unwrap().last().name.as_str(), "UFCSTarget");

        let methods = find_ufcs_free_functions(&target, &unit.context);

        assert_eq!(methods.len(), 1);
        assert_eq!(methods[0].ident().name.as_str(), "TargetMethod");
    }

    #[test]
    fn finds_exported_ufcs_func_from_other_unit() {
        let a_src = r"interface
            type UFCSTarget = class
            end
            end";

        let b_src = r"interface
            function TargetMethod(t: A.UFCSTarget)
            begin
            end
            end";

        let c_src = "uses A;uses B;";

        let units = units_from_src(vec![("A", a_src), ("B", b_src), ("C", c_src)]);

        let a = &units[0];
        let c = &units[2];

        let target = Type::of_decl(&a.unit.type_decls().next().unwrap());
        let methods = find_ufcs_free_functions(&target, &c.context);

        assert_eq!(methods.len(), 1);
        assert_eq!(methods[0].ident().name.as_str(), "TargetMethod");
    }

    #[test]
    fn doesnt_find_private_ufcs_func_from_other_unit() {
        let a_src = r"
            type UFCSTarget = class
            end";

        let b_src = r"
            implementation
            function TargetMethod(t: A.UFCSTarget)
            begin
            end;
            end.";

        let c_src = "uses A;uses B;";

        let units = units_from_src(vec![("A", a_src), ("B", b_src), ("C", c_src)]);

        let a = &units[0];
        let c = &units[2];

        let target = Type::of_decl(&a.unit.type_decls().next().unwrap());
        let methods = find_ufcs_free_functions(&target, &c.context);

        assert_eq!(methods.len(), 0);
    }
}
