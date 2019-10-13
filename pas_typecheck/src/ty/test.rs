use crate::{
    ast::{Class, Unit},
    context::*,
    ty::*,
};
use pas_syn::{ast};
use std::rc::Rc;
use crate::test::module_from_src;

const INT32: Type = Type::Primitive(Primitive::Int32);
const BYTE: Type = Type::Primitive(Primitive::Byte);

fn unit_from_src(src: &'static str) -> Unit {
    let mut module = module_from_src("test", src);
    module.units.pop().unwrap().unit
}

fn classes_from_src(src: &'static str) -> Vec<Rc<Class>> {
    let unit = unit_from_src(src);
    let decls = unit.type_decls().cloned().map(|t| match t {
        ast::TypeDecl::Class(class) => class,
        _ => unreachable!(),
    });

    decls.collect()
}

#[test]
fn specialize_class_has_correct_member_types() {
    let tys = classes_from_src(
        r"  type A of T = class
                t: T;
                x: Byte;
            end;
        ",
    );

    let type_args = vec![INT32.clone()];
    let result = specialize_class_def(&tys[0], type_args, &builtin_span()).unwrap();

    assert_eq!(1, result.name.type_args.len());
    assert_eq!(INT32, result.name.type_args[0]);

    assert_eq!(INT32, result.members[0].ty);
    assert_eq!(BYTE, result.members[1].ty);
}

#[test]
fn specialize_class_has_multi_correct_member_types() {
    let tys = classes_from_src(
        r"  type A of T1, T2 = class
                t1: T1;
                t2: T2;
            end;
        ",
    );

    let type_args = vec![INT32.clone(), BYTE.clone()];
    let result = specialize_class_def(&tys[0], type_args, &builtin_span()).unwrap();

    assert_eq!(2, result.name.type_args.len());
    assert_eq!(INT32, result.name.type_args[0]);
    assert_eq!(BYTE, result.name.type_args[1]);

    assert_eq!(INT32, result.members[0].ty);
    assert_eq!(BYTE, result.members[1].ty);
}

#[test]
fn specialize_class_with_deep_params() {
    let tys = classes_from_src(
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

    let result = specialize_class_def(&tys[1], vec![INT32], &span).unwrap();

    let a_name = result.members[0].ty.as_record().unwrap();
    assert_eq!("test.A of Integer, Integer", a_name.to_string());
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
        ",
    );

    let a_func = unit.func_defs().next().unwrap();
    let a_sig = FunctionSig::of_decl(&a_func.decl);

    let a_int_sig = a_sig
        .specialize_generic(vec![INT32.clone()], &span)
        .unwrap();

    let expect_sig = FunctionSig {
        type_params_len: 1,
        return_ty: INT32.clone(),
        params: vec![FunctionParamSig {
            ty: INT32.clone(),
            modifier: None,
        }],
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
        ",
    );

    let int_params = vec![INT32.clone()];

    let a_class = unit
        .type_decls()
        .filter_map(|d| match d {
            ast::TypeDecl::Class(class) => Some(class.clone()),
            _ => unreachable!(),
        })
        .next()
        .unwrap();

    let a_int = specialize_class_def(&a_class, int_params.clone(), &span)
        .map(|class| Type::Record(Box::new(class.name)))
        .unwrap();

    let b_func = unit.func_defs().next().unwrap();
    let b_sig = FunctionSig::of_decl(&b_func.decl);

    let b_int_sig = b_sig.specialize_generic(int_params, &span).unwrap();

    let expect_sig = FunctionSig {
        type_params_len: 1,
        return_ty: a_int.clone(),
        params: vec![FunctionParamSig {
            ty: a_int,
            modifier: None,
        }],
    };

    assert_eq!(expect_sig, b_int_sig);
}
