use crate::dep_sort::find_deps;
use crate::dep_sort::sort_defs;
use crate::metadata::Metadata;
use crate::metadata::NamePath;
use crate::metadata::TypeDef;
use crate::metadata::TypeDefID;
use crate::syn;
use crate::syn::Ident;
use crate::syn::IdentPath;
use crate::translate;
use crate::typ;
use crate::IROptions;
use common::span::Span;
use common::BuildOptions;
use frontend::parse::TokenStream;
use frontend::TokenTree;
use frontend::pp::Preprocessor;
use std::collections::HashMap;

fn defs_from_src(src: &str) -> (HashMap<TypeDefID, TypeDef>, Metadata) {
    let test_unit = Preprocessor::new("test", BuildOptions::default())
        .preprocess(src)
        .unwrap();
    let tokens = TokenTree::tokenize(test_unit).unwrap();
    let mut stream = TokenStream::new(tokens, Span::zero("test"));

    let unit = syn::Unit::parse(&mut stream, IdentPath::from_parts(vec![Ident::new("test", Span::zero("test"))])).unwrap();
    stream.finish().unwrap();

    let module = typ::Module::typecheck(&[unit]).unwrap();
    let ir = translate(&module, IROptions::default());

    let defs = ir.metadata.type_defs()
        .map(|(id, def)| (id, def.clone()))
        .collect();

    (defs, ir.metadata)
}

fn get_id(metadata: &Metadata, name: &str) -> TypeDefID {
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
        implementation
        type A = record
            val: Int32;
        end;

        type B = record
            val: A;
        end;

        type C = record
            val: B;
        end;
        
        initialization
            var a: A;
            var b: B;
            var c: C;
        end.
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
        implementation
        type A = record
            val: Int32;
        end;

        type B = variant
            First: A;
        end;
    
        initialization
            var a: A;
            var b: B;
        end.
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
        implementation
        type A = record
            val: Int32;
        end;

        type B = record
            val: A;
        end;

        type C = record
            val: B;
        end;

        initialization
            var a: A;
            var b: B;
            var c: C;
        end.
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
    assert_eq!("A", sorted_it.next().unwrap().1.name().unwrap().path.last());
    assert_eq!("B", sorted_it.next().unwrap().1.name().unwrap().path.last());
    assert_eq!("C", sorted_it.next().unwrap().1.name().unwrap().path.last());
}