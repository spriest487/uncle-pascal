#[cfg(test)]
mod test;

use crate::metadata::Metadata;
use crate::ty::Struct;
use crate::ty::TypeDef;
use crate::ty::VariantDef;
use ir_lang::*;
use linked_hash_map::LinkedHashMap;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;

// sort list of type defs to resolve deep structural dependencies on other defs.
// struct, variant and static array fields count as structural dependencies (and are
// guaranteed from the pascal end to be non-recursive, because they must be declared
// in order).
// this will panic if any defs depend on themselves.
pub fn sort_defs<Defs>(defs: Defs, metadata: &Metadata) -> LinkedHashMap<TypeDefID, TypeDef>
where
    Defs: IntoIterator<Item = (TypeDefID, TypeDef)>,
{
    let defs: HashMap<TypeDefID, TypeDef> = defs.into_iter().collect();
    let mut ids: Vec<_> = defs.keys().map(|id| *id).collect();

    let mut def_deps = HashMap::new();

    for (id, def) in defs.iter() {
        let deps = find_deps(def, metadata);
        def_deps.insert(*id, deps);
    }

    ids.sort_unstable_by(|a, b| {
        if def_deps[a].contains(b) {
            Ordering::Greater
        } else if def_deps[b].contains(a) {
            Ordering::Less
        } else {
            Ordering::Equal
        }
    });

    let mut defs = defs;
    ids.into_iter()
        .map(|id| (id, defs.remove(&id).unwrap()))
        .collect()
}

fn find_deps(def: &TypeDef, metadata: &Metadata) -> HashSet<TypeDefID> {
    let mut deps = HashSet::new();

    match def {
        TypeDef::Struct(struct_def) => {
            add_struct_deps(struct_def, &mut deps, metadata);
        }

        TypeDef::Variant(variant_def) => {
            add_variant_deps(variant_def, &mut deps, metadata);
        }

        TypeDef::Function(func_ty_def) => {
            add_func_ty_deps(func_ty_def, &mut deps, metadata);
        }
    }

    deps
}

fn add_struct_deps(struct_def: &Struct, deps: &mut HashSet<TypeDefID>, metadata: &Metadata) {
    for (_, field) in &struct_def.fields {
        add_dep(&field.ty, deps, metadata);
    }
}

fn add_variant_deps(variant_def: &VariantDef, deps: &mut HashSet<TypeDefID>, metadata: &Metadata) {
    for case in &variant_def.cases {
        if let Some(case_ty) = &case.ty {
            add_dep(case_ty, deps, metadata);
        }
    }
}

fn add_func_ty_deps(func_ty_def: &FunctionSig, deps: &mut HashSet<TypeDefID>, metadata: &Metadata) {
    add_dep(&func_ty_def.return_ty, deps, metadata);
    for param_ty in &func_ty_def.param_tys {
        add_dep(param_ty, deps, metadata);
    }
}

fn add_dep(ty: &Type, deps: &mut HashSet<TypeDefID>, metadata: &Metadata) {
    match ty {
        Type::Variant(id) => {
            deps.insert(*id);

            let def = metadata.get_variant_def(*id).unwrap();
            add_variant_deps(def, deps, metadata);
        }

        Type::Struct(id) => {
            deps.insert(*id);

            let def = metadata.get_struct_def(*id).unwrap();
            add_struct_deps(def, deps, metadata);
        }

        Type::Array { element, .. } => {
            add_dep(element, deps, metadata);
        }

        _ => {
            // no structural deps
        }
    }
}