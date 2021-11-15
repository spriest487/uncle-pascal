use crate::metadata::{Metadata, StructID};
use linked_hash_map::LinkedHashMap;
use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
};
use crate::ty::{Struct, TypeDef, Variant};
use crate::Type;

// sort list of type defs to resolve deep structural dependencies on other defs.
// struct, variant and static array fields count as structural dependencies (and are
// guaranteed from the pascal end to be non-recursive, because they must be declared
// in order).
// this will panic if any defs depend on themselves.
pub fn sort_defs<Defs>(defs: Defs, metadata: &Metadata) -> LinkedHashMap<StructID, TypeDef>
where
    Defs: IntoIterator<Item = (StructID, TypeDef)>,
{
    let defs: HashMap<StructID, TypeDef> = defs.into_iter().collect();
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

fn find_deps(def: &TypeDef, metadata: &Metadata) -> HashSet<StructID> {
    let mut deps = HashSet::new();

    match def {
        TypeDef::Struct(struct_def) => {
            add_struct_deps(struct_def, &mut deps, metadata);
        }

        TypeDef::Variant(variant_def) => {
            add_variant_deps(variant_def, &mut deps, metadata);
        }
    }

    deps
}

fn add_struct_deps(struct_def: &Struct, deps: &mut HashSet<StructID>, metadata: &Metadata) {
    for (_, field) in &struct_def.fields {
        add_dep(&field.ty, deps, metadata);
    }
}

fn add_variant_deps(variant_def: &Variant, deps: &mut HashSet<StructID>, metadata: &Metadata) {
    for case in &variant_def.cases {
        if let Some(case_ty) = &case.ty {
            add_dep(case_ty, deps, metadata);
        }
    }
}

fn add_dep(ty: &Type, deps: &mut HashSet<StructID>, metadata: &Metadata) {
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

#[cfg(test)]
mod test {
    use crate::{
        metadata::{StructID, TypeDef},
        translate, IROptions,
    };
    use pas_common::{span::Span, BuildOptions};
    use pas_syn::{parse::TokenStream, Ident, TokenTree, ast, IdentPath};
    use pas_typecheck as ty;
    use std::collections::HashMap;
    use crate::{
        dep_sort::sort_defs,
        metadata::Metadata
    };
    use crate::metadata::NamePath;
    use crate::dep_sort::find_deps;

    fn defs_from_src(src: &str) -> (HashMap<StructID, TypeDef>, Metadata) {
        let tokens = TokenTree::tokenize("test", src, &BuildOptions::default()).unwrap();
        let mut stream = TokenStream::new(tokens, Span::zero("test"));

        let unit = ast::Unit::parse(&mut stream, IdentPath::from_parts(vec![Ident::new("test", Span::zero("test"))])).unwrap();

        let module = ty::Module::typecheck(&[unit], true).unwrap();
        let ir = translate(&module, IROptions::default());

        let defs = ir.metadata.type_defs()
            .map(|(id, def)| (id, def.clone()))
            .collect();

        (defs, ir.metadata)
    }

    fn get_id(metadata: &Metadata, name: &str) -> StructID {
        let name_path = NamePath::from_parts(vec!["test".to_string(), name.to_string()]);

        match metadata.find_struct_def(&name_path) {
            Some((id, _)) => { id },
            None => match metadata.find_variant_def(&name_path) {
                Some((id, _)) => { id },
                None => panic!("not found: {} in: {:#?}", name, metadata)
            }
        }
    }

    #[test]
    fn finds_record_deps() {
        let (defs, metadata) = defs_from_src(r"
            type A = record
                val: Integer;
            end;

            type B = record
                val: A;
            end;

            type C = record
                val: B;
            end;

            var a: A;
            var b: B;
            var c: C;
        ");

        let a = get_id(&metadata, "A");
        let a_def = &defs[&a];
        let a_deps = find_deps(&a_def, &metadata);
        assert_eq!(0, a_deps.len());

        let b = get_id(&metadata, "B");
        let b_def = &defs[&b];
        let b_deps = find_deps(&b_def, &metadata);
        assert_eq!(1, b_deps.len());
        assert!(b_deps.contains(&a));

        let c = get_id(&metadata, "C");
        let c_def = &defs[&c];
        let c_deps = find_deps(&c_def, &metadata);
        assert_eq!(2, c_deps.len());
        assert!(c_deps.contains(&a));
        assert!(c_deps.contains(&b));
    }

    #[test]
    fn finds_variant_deps() {
        let (defs, metadata) = defs_from_src(r"
            type A = record
                val: Integer;
            end;

            type B = variant
                First: A;
            end;

            var a: A;
            var b: B;
        ");

        let a = get_id(&metadata, "A");
        let a_def = &defs[&a];
        let a_deps = find_deps(&a_def, &metadata);
        assert_eq!(0, a_deps.len());

        let b = get_id(&metadata, "B");
        let b_def = &defs[&b];
        let b_deps = find_deps(&b_def, &metadata);
        assert_eq!(1, b_deps.len());
        assert!(b_deps.contains(&a));
    }

    #[test]
    fn sorts_record_deps() {
        let (mut defs, metadata) = defs_from_src(r"
            type A = record
                val: Integer;
            end;

            type B = record
                val: A;
            end;

            type C = record
                val: B;
            end;

            var a: A;
            var b: B;
            var c: C;
        ");

        let a_id = get_id(&metadata, "A");
        let b_id  = get_id(&metadata, "B");
        let c_id = get_id(&metadata, "C");

        // mix up the order
        let unsorted_defs = vec![
            (c_id, defs.remove(&c_id).unwrap()),
            (a_id, defs.remove(&a_id).unwrap()),
            (b_id, defs.remove(&b_id).unwrap()),
        ];

        let sorted = sort_defs(unsorted_defs, &metadata);

        let mut sorted_it = sorted.into_iter();
        assert_eq!("A", sorted_it.next().unwrap().1.name().path.last());
        assert_eq!("B", sorted_it.next().unwrap().1.name().path.last());
        assert_eq!("C", sorted_it.next().unwrap().1.name().path.last());
    }
}
