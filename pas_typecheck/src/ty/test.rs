use crate::{
    ast::{typecheck_unit, Unit},
    context::*,
    ty::*,
};
use pas_common::BuildOptions;
use pas_syn::{ast, parse::prelude::*};
use std::rc::Rc;

const INT32: Type = Type::Primitive(Primitive::Int32);
const BYTE: Type = Type::Primitive(Primitive::Byte);

fn unit_from_src(src: &str) -> Unit {
    let tokens = TokenTree::tokenize("test", src, &BuildOptions::default()).unwrap();
    let mut stream = TokenStream::new(tokens, Span::zero("test"));

    let unit = ast::Unit::parse(&mut stream, Ident::new("test", Span::zero("test"))).unwrap();

    let mut ctx = Context::root(true);
    typecheck_unit(&unit, &mut ctx).unwrap()
}

fn types_from_src(src: &str) -> Vec<Type> {
    let unit = unit_from_src(src);
    let decls = unit.type_decls().map(Type::of_decl).collect();

    decls
}

#[test]
fn specialize_class_has_correct_member_types() {
    let tys = types_from_src(
        r"  type A of T = class
                t: T;
                x: Byte;
            end;
        ",
    );
    let class = tys[0].as_class().unwrap();

    let type_args = vec![INT32.clone()];
    let result = specialize_generic_class(&class, type_args, &builtin_span()).unwrap();

    assert_eq!(1, result.name.type_args.len());
    assert_eq!(INT32, result.name.type_args[0]);

    assert_eq!(INT32, result.members[0].ty);
    assert_eq!(BYTE, result.members[1].ty);
}

#[test]
fn specialize_class_has_multi_correct_member_types() {
    let tys = types_from_src(
        r"  type A of T1, T2 = class
                t1: T1;
                t2: T2;
            end;
        ",
    );
    let class = tys[0].as_class().unwrap();

    let type_args = vec![INT32.clone(), BYTE.clone()];
    let result = specialize_generic_class(&class, type_args, &builtin_span()).unwrap();

    assert_eq!(2, result.name.type_args.len());
    assert_eq!(INT32, result.name.type_args[0]);
    assert_eq!(BYTE, result.name.type_args[1]);

    assert_eq!(INT32, result.members[0].ty);
    assert_eq!(BYTE, result.members[1].ty);
}

#[test]
fn specialize_class_with_deep_params() {
    let tys = types_from_src(
        r"  type A of T1, T2 = record
                t1: T1;
                t2: T2;
            end;

            type B of T = class
                a: A of T, T;
                b: T;
            end;
        ",
    );

    let span = builtin_span();

    let a_class = tys[0].as_record().unwrap();
    let b_class = tys[1].as_class().unwrap();

    let expected_a_ty = specialize_generic_class(&a_class, vec![INT32, INT32], &span).unwrap();
    let result = specialize_generic_class(&b_class, vec![INT32], &span).unwrap();

    assert_eq!(Type::Record(Rc::new(expected_a_ty)), result.members[0].ty);
    assert_eq!(INT32, result.members[1].ty);
}

#[test]
fn specialized_fn_has_right_sig() {
    let span = builtin_span();
    let unit = unit_from_src(
        r"  function A of T(t: T): T
            begin
                t
            end;
        ");

    let a_func = unit.func_defs().next().unwrap();
    let a_sig = FunctionSig::of_decl(&a_func.decl);

    let a_int_sig = a_sig.specialize_generic(vec![INT32.clone()], &span).unwrap();

    let expect_sig = FunctionSig {
        type_params_len: 1,
        return_ty: INT32.clone(),
        params: vec![
            FunctionParamSig {
                ty: INT32.clone(),
                modifier: None,
            }
        ]
    };

    assert_eq!(expect_sig, a_int_sig);
}

#[test]
fn specialized_fn_with_specialized_params_has_right_params() {
    let span = builtin_span();
    let unit = unit_from_src(
        r"
            type A of AT = record
                a: AT;
            end;

            function B of BT(a: A of BT): A of BT
            begin
                a
            end;
        ");

    let int_params = vec![INT32.clone()];

    let a_ty = Type::of_decl(unit.type_decls().next().unwrap());
    let a_class = a_ty.as_record().unwrap();
    let a_int = specialize_generic_class(&a_class, int_params.clone(), &span)
        .map(|class| Type::Record(Rc::new(class)))
        .unwrap();

    let b_func = unit.func_defs().next().unwrap();
    let b_sig = FunctionSig::of_decl(&b_func.decl);

    let b_int_sig = b_sig.specialize_generic(int_params, &span).unwrap();

    let expect_sig = FunctionSig {
        type_params_len: 1,
        return_ty: a_int.clone(),
        params: vec![
            FunctionParamSig {
                ty: a_int,
                modifier: None,
            }
        ]
    };

    assert_eq!(expect_sig, b_int_sig);
}
