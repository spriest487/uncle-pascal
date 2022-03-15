use crate::{
    ast::{Composite, Unit},
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

fn classes_from_src(src: &'static str) -> Vec<Rc<Composite>> {
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
        r"  type A[T] = class
                t: T;
                x: Byte;
            end;
        ",
    );

    let span = Span::zero("test");

    let type_args = TypeList::new(vec![INT32.clone()], span.clone());
    let result = specialize_composite_def(&tys[0], &type_args).unwrap();

    assert!(result.name.type_args.is_some());
    let actual_type_args = result.name.type_args.unwrap();

    assert_eq!(1, actual_type_args.len());
    assert_eq!(INT32, actual_type_args.items[0]);

    assert_eq!(INT32, result.members[0].ty);
    assert_eq!(BYTE, result.members[1].ty);
}

#[test]
fn specialize_class_has_multi_correct_member_types() {
    let tys = classes_from_src(
        r"  type A[T1, T2] = class
                t1: T1;
                t2: T2;
            end;
        ",
    );

    let span = Span::zero("test");

    let type_args = TypeList::new(vec![INT32.clone(), BYTE.clone()], span.clone());
    let result = specialize_composite_def(&tys[0], &type_args).unwrap();

    assert!(result.name.type_args.is_some());
    let actual_type_args = result.name.type_args.unwrap();

    assert_eq!(2, actual_type_args.len());
    assert_eq!(INT32, actual_type_args.items[0]);
    assert_eq!(BYTE, actual_type_args.items[1]);

    assert_eq!(INT32, result.members[0].ty);
    assert_eq!(BYTE, result.members[1].ty);
}

#[test]
fn specialize_class_with_deep_params() {
    let tys = classes_from_src(
        r"  type A[T1, T2] = record
                t1: T1;
                t2: T2;
            end;

            type B[T] = class
                a: A[T, T];
                b: T;
            end;
        ",
    );

    let span = Span::zero("test");

    let type_args = TypeList::new(vec![INT32], span.clone());

    let result = specialize_composite_def(&tys[1], &type_args).unwrap();

    let a_name = result.members[0].ty.as_record().unwrap();
    assert_eq!("test.A[Integer, Integer]", a_name.to_string());
    assert_eq!(INT32, result.members[1].ty);
}

#[test]
fn specialized_fn_has_right_sig() {
    let unit = unit_from_src(
        r"  function A[T](t: T): T
            begin
                t
            end;
        ",
    );

    let span = Span::zero("test");

    let ctx = Context::root(true, span.clone());

    let a_func = unit.func_defs().next().unwrap();
    let a_sig = FunctionSig::of_decl(&a_func.decl);

    let type_args = TypeList::new([INT32], span.clone());

    let a_int_sig = a_sig
        .specialize_generic(&type_args, &ctx)
        .unwrap();

    let expect_sig = FunctionSig {
        type_params: Some(ast::TypeList::new([FunctionSigTypeParam { is_ty: Type::Any }], span.clone())),
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
            type A[AT] = record
                a: AT;
            end;

            function B[BT](a: A[BT]): A[BT]
            begin
                a
            end;
        ",
    );

    let int_params = TypeList::new([INT32.clone()], span.clone());

    let a_class = unit
        .type_decls()
        .filter_map(|d| match d {
            ast::TypeDecl::Class(class) => Some(class.clone()),
            _ => unreachable!(),
        })
        .next()
        .unwrap();

    let a_int = specialize_composite_def(&a_class, &int_params)
        .map(|class| Type::Record(Box::new(class.name)))
        .unwrap();

    let b_func = unit.func_defs().next().unwrap();
    let b_sig = FunctionSig::of_decl(&b_func.decl);

    let ctx = Context::root(true, span.clone());

    let b_int_sig = b_sig.specialize_generic(&int_params, &ctx).unwrap();

    let expect_sig = FunctionSig {
        type_params: Some(ast::TypeList::new([FunctionSigTypeParam { is_ty: Type::Any }], span.clone())),
        return_ty: a_int.clone(),
        params: vec![FunctionParamSig {
            ty: a_int,
            modifier: None,
        }],
    };

    assert_eq!(expect_sig, b_int_sig);
}
