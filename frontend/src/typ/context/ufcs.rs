#[cfg(test)]
mod test;

use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;

use crate::ast::Ident;
use crate::typ::ast::FunctionDecl;
use crate::typ::Decl;
use crate::typ::FunctionSig;
use crate::typ::NameResult;
use crate::typ::Type;
use crate::typ::{Context, Symbol};

#[derive(Clone, Debug)]
pub enum InstanceMethod {
    FreeFunction {
        /// fully-qualified function name, starting with the namespace the function is declared in
        func_name: Symbol,
        sig: Rc<FunctionSig>,
    },
    Method {
        owning_ty: Type,
        decl: Rc<FunctionDecl>,
    },
}

impl InstanceMethod {
    pub fn ident(&self) -> &Ident {
        match self {
            InstanceMethod::FreeFunction { func_name, .. } => func_name.ident(),
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

    let mut free_functions = find_ufcs_free_functions(ty, ctx);
    methods.append(&mut free_functions);
    
    let mut ufcs_methods = find_ufcs_methods(ty, ctx)?;
    methods.append(&mut ufcs_methods);
    
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

        // ignore decls that can't possible be called via UFCS since they have 0 params
        let func_decl = match decl {
            Decl::Function { decl, .. } if decl.params.len() > 0 => decl,
            _ => return,
        };
        let self_param = &func_decl.params[0];

        if self_param.ty == *ty
            || (self_param.ty.contains_generic_params(ctx) && self_param.ty.same_decl_type(ty))
        {
            let func_name = Symbol::from(decl_path.clone())
                .with_ty_params(func_decl.type_params.clone());
            
            methods.push(InstanceMethod::FreeFunction {
                func_name,
                sig: Rc::new(FunctionSig::of_decl(func_decl)),
            })
        }
    });

    methods
}

fn find_ufcs_methods(ty: &Type, ctx: &Context) -> NameResult<Vec<InstanceMethod>> {
    let ty_methods = ty.methods(ctx)?;

    // eprintln!("{} has methods:", ty);
    let instance_methods = ty_methods
        .into_iter()
        .map(|method| {
            // eprintln!(" - {}", method.name);
            InstanceMethod::Method {
                owning_ty: ty.clone(),
                decl: method,
            }
        })
        .collect();
    
    Ok(instance_methods)
}
