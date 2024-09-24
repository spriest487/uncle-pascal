use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;

use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::typ::ast::FunctionDecl;
use crate::typ::Context;
use crate::typ::Decl;
use crate::typ::FunctionSig;
use crate::typ::NameResult;
use crate::typ::Type;

#[derive(Clone, Debug)]
pub enum InstanceMethod {
    FreeFunction {
        /// fully-qualified function name, starting with the namespace the function is declared in
        func_name: IdentPath,
        sig: Rc<FunctionSig>,
    },
    Method {
        owning_ty: Type,
        decl: FunctionDecl,
    },
}

impl InstanceMethod {
    pub fn ident(&self) -> &Ident {
        match self {
            InstanceMethod::FreeFunction { func_name, .. } => func_name.last(),
            InstanceMethod::Method { decl, .. } => &decl.name.ident,
        }
    }
}

impl fmt::Display for InstanceMethod {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            InstanceMethod::FreeFunction { func_name, .. } => write!(f, "function {}", func_name),
            InstanceMethod::Method { owning_ty, decl, .. } => write!(f, "method {}.{}", owning_ty, decl.name.ident ),
        }
        
    }
}

/// an instance method is an interface impl method for `ty_def` that takes Self as the first argument
/// OR a virtual method of the interface type referenced by `ty_def`
/// OR any free function taking `ty_def` as its first parameter (UFCS syntax)
/// OR any free function taking a generic type, or a parameterized type into which `ty_def` can
///    be substituted, as its first parameter (generic UFCS)
pub fn find_instance_methods_of(ty: &Type, ctx: &Context) -> NameResult<Vec<InstanceMethod>> {
    let mut methods = Vec::new();
    
    // eprintln!("gathering methods for {}", ty);

    // match ty {
    //     Type::Interface(iface) => {
    //         let iface_def = ctx.find_iface_def(iface)?;
    // 
    //         methods.extend(iface_def
    //             .methods
    //             .iter()
    //             .map(|method_decl| InstanceMethod::Method {
    //                 owning_ty: ty.clone(),
    //                 decl: method_decl.decl.clone(),
    //             }));
    //     }
    // 
    //     Type::GenericParam(generic_param_ty) => {
    //         if let Some(is_iface) = &generic_param_ty.is_iface {
    //             let mut iface_methods = find_instance_methods_of(is_iface, ctx)?;
    //             eprintln!("added {} iface methods", iface_methods.len());
    //             for method in &iface_methods {
    //                 eprintln!(" - {}", method);
    //             }
    //             methods.append(&mut iface_methods);
    //         }
    //     },
    // 
    //     _ => {}
    // }

    let mut free_functions = find_ufcs_free_functions(ty, ctx);
    // eprintln!("added {} free functions", free_functions.len());
    // for method in &free_functions {
    //     eprintln!(" - {}", method);
    // }
    methods.append(&mut free_functions);
    
    let mut ufcs_methods = find_ufcs_methods(ty, ctx)?;
    // eprintln!("added {} methods", ufcs_methods.len());
    // for method in &ufcs_methods {
    //     eprintln!(" - {}", method);
    // }
    methods.append(&mut ufcs_methods);
    
    // eprintln!("ok: {} total", methods.len());
    Ok(methods)
}

fn find_ufcs_free_functions(ty: &Type, ctx: &Context) -> Vec<InstanceMethod> {
    // the namespaces we look for UFCS methods in - the current NS and any used NSs
    let current_ns = ctx.namespace();
    let current_path = ctx.scopes.current_path();

    let mut search_namespaces = vec![&current_ns];
    search_namespaces.extend(current_path.all_used_units());

    let mut methods = Vec::new();

    ctx.scopes.visit_visible(|decl_path, decl| {
        // ignore decls that aren't members of one of the search namespaces
        if !search_namespaces.iter().any(|search_ns| search_ns.is_parent_of(decl_path)) {
            return;
        }

        let sig = match decl {
            Decl::Function { sig, .. } if sig.params.len() > 0 => sig.clone(),
            _ => return,
        };
        let self_param = &sig.params[0];

        if self_param.ty == *ty
            || (self_param.ty.contains_generic_params(ctx) && self_param.ty.same_decl_type(ty))
        {
            methods.push(InstanceMethod::FreeFunction {
                func_name: decl_path.clone(),
                sig: sig.clone(),
            })
        }
    });

    methods
}

fn find_ufcs_methods(ty: &Type, ctx: &Context) -> NameResult<Vec<InstanceMethod>> {
    let mut methods = Vec::new();

    // the type's own methods
    for method in ty.methods(ctx)? {
        // if is_callable_method(method, ty) {
            methods.push(InstanceMethod::Method {
                owning_ty: ty.clone(),
                decl: method.clone(),
            });
        // }
    }
    
    // methods from all interface types the type impleemnts
    // for iface_ty in ctx.implemented_ifaces(ty)? {
    //     let iface_methods = iface_ty.methods(ctx)?;
    // 
    //     // methods valid for UFCS are any which have either the generic self type or this
    //     // specific type as the first parmeter
    //     let callable_methods = iface_methods
    //         .iter()
    //         .filter(|method_decl| is_callable_method(method_decl, ty));
    // 
    //     // add all the methods, we don't need to check if they're actually defined
    //     // or implemented - we should check that elsewhere
    //     for method in callable_methods {
    //         methods.push(InstanceMethod::Method {
    //             owning_ty: iface_ty.clone(),
    //             decl: (**method).clone(),
    //         });
    //     }
    // }

    Ok(methods)
}

// fn is_callable_method(method_decl: &FunctionDecl, self_ty: &Type) -> bool {
//     method_decl
//         .params
//         .get(0)
//         .map(|self_param| {
//             if self_param.modifier.is_some() {
//                 return false;
//             }
// 
//             self_param.ty == Type::MethodSelf || self_param.ty == *self_ty
//         })
//         .unwrap_or(false)
// }

#[cfg(test)]
mod test {
    use crate::typ::context::ufcs::find_ufcs_free_functions;
    use crate::typ::test::{unit_from_src, units_from_src};
    use crate::typ::Type;

    #[test]
    fn finds_ufcs_func() {
        let unit = unit_from_src(
            "test",
            r"implementation

            type UFCSTarget = class
            end;

            function TargetMethod(t: UFCSTarget);
            begin
            end;
            
            end",
        );

        let (_, target_decl) = unit.unit.type_decl_items().next().unwrap();
        let target = Type::of_decl(target_decl);
        assert_eq!(target.full_path().unwrap().last().name.as_str(), "UFCSTarget");

        let methods = find_ufcs_free_functions(&target, &unit.context);

        assert_eq!(methods.len(), 1);
        assert_eq!(methods[0].ident().name.as_str(), "TargetMethod");
    }

    #[test]
    fn finds_exported_ufcs_func_from_other_unit() {
        let a_src = r"
            interface

            type UFCSTarget = class
            end;

            end";

        let b_src = r"
            interface

            function TargetMethod(t: A.UFCSTarget);
            
            implementation

            function TargetMethod(t: A.UFCSTarget);
            begin
            end;

            end";

        let c_src = r"
            implementation
            uses A;
            uses B;
            end";

        let units = units_from_src(vec![("A", a_src), ("B", b_src), ("C", c_src)]);

        let a = &units["A"];
        let c = &units["C"];

        let (_, target_decl) = a.unit.type_decl_items().next().unwrap();
        let target = Type::of_decl(target_decl);
        let methods = find_ufcs_free_functions(&target, &c.context);

        assert_eq!(methods.len(), 1);
        assert_eq!(methods[0].ident().name.as_str(), "TargetMethod");
    }

    #[test]
    fn doesnt_find_private_ufcs_func_from_other_unit() {
        let a_src = r"
            implementation
            
            type UFCSTarget = class
            end;
            
            end.";

        let b_src = r"
            implementation
            function TargetMethod(t: A.UFCSTarget);
            begin
            end;

            end.";

        let c_src = r"
            implementation
            uses A;
            uses B;
            end.";

        let units = units_from_src(vec![("A", a_src), ("B", b_src), ("C", c_src)]);

        let a = &units["A"];
        let c = &units["C"];

        let target = Type::of_decl(&a.unit.type_decl_items().next().unwrap().1);
        let methods = find_ufcs_free_functions(&target, &c.context);

        assert_eq!(methods.len(), 0);
    }
}
