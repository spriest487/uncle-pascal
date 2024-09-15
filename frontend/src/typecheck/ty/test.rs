use crate::typecheck::ast::StructDef;
use crate::typecheck::ast::Unit;
use crate::typecheck::context::*;
use crate::typecheck::ty::*;
use crate::ast;
use std::rc::Rc;
use crate::ast::{Call, Stmt};
use crate::typecheck::test::module_from_src;

const INT32: Type = Type::Primitive(Primitive::Int32);
const BYTE: Type = Type::Primitive(Primitive::UInt8);

fn unit_from_src(unit_name: &'static str, src: &'static str) -> Unit {
    let mut module = module_from_src(unit_name, src);
    module.units.pop().unwrap().unit
}

fn classes_from_src(unit_name: &'static str, src: &'static str) -> Vec<Rc<StructDef>> {
    let unit = unit_from_src(unit_name, src);
    let decls = unit.type_decls()
        .flat_map(|(_vis, decl)| decl.items.iter())
        .map(|t| match t {
            ast::TypeDeclItem::Struct(c) => c.clone(),
            _ => unreachable!(),
        });

    decls.collect()
}

#[test]
fn specialize_class_has_correct_member_types() {
    let tys = classes_from_src(
        "specialize_class_has_correct_member_types",
        r"  
            implementation
            type A[T] = class
                t: T;
                x: UInt8;
            end;
            end
        ",
    );

    let span = Span::zero("test");

    let type_args = TypeList::new(vec![INT32.clone()], span.clone());
    let result = specialize_struct_def(&tys[0], &type_args).unwrap();

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
        "specialize_class_has_multi_correct_member_types",
        r"
            implementation
            type A[T1, T2] = class
                t1: T1;
                t2: T2;
            end;
            end
        ",
    );

    let span = Span::zero("test");

    let type_args = TypeList::new(vec![INT32.clone(), BYTE.clone()], span.clone());
    let result = specialize_struct_def(&tys[0], &type_args).unwrap();

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
    const UNIT_NAME: &str ="specialize_class_with_deep_params"; 
    let tys = classes_from_src(
        UNIT_NAME,
        r"  
            implementation
            type A[T1, T2] = record
                t1: T1;
                t2: T2;
            end;
            
            type B[T] = class
                a: A[T, T];
                b: T;
            end;
            end
        ",
    );

    let span = Span::zero(UNIT_NAME);

    let type_args = TypeList::new(vec![INT32], span.clone());

    let result = specialize_struct_def(&tys[1], &type_args).unwrap();

    let a_name = result.members[0].ty.as_record().unwrap();
    assert_eq!(format!("{UNIT_NAME}.A[Int32, Int32]"), a_name.to_string());
    assert_eq!(INT32, result.members[1].ty);
}

#[test]
fn specialized_fn_has_right_sig() {
    let unit = unit_from_src(
        "specialized_fn_has_right_sig",
        r"  
            implementation
            function A[T](t: T): T;
            begin
                t
            end;
            end
        ",
    );

    let span = Span::zero("test");

    let ctx = Context::root(span.clone());

    let (_, a_func) = unit.func_defs().next().unwrap();
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
        "specialized_fn_with_specialized_params_has_right_params",
        r"
            implementation

            type A[AT] = record
                a: AT;
            end;

            function B[BT](a: A[BT]): A[BT];
            begin
                a
            end;
            
            end
        ",
    );

    let int_params = TypeList::new([INT32.clone()], span.clone());

    let a_class = unit
        .type_decl_items()
        .filter_map(|(_vis, d)| match d {
            ast::TypeDeclItem::Struct(class) => Some(class.clone()),
            _ => unreachable!(),
        })
        .next()
        .unwrap();

    let a_int = specialize_struct_def(&a_class, &int_params)
        .map(|class| Type::Record(Box::new(class.name)))
        .unwrap();

    let (_, b_func) = unit.func_defs().next().unwrap();
    let b_sig = FunctionSig::of_decl(&b_func.decl);

    let ctx = Context::root(span.clone());

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

#[test]
fn can_infer_ty_arg_from_real_record_arg() {
    const UNIT_NAME: &str = "can_infer_ty_args_from_real_record_arg";
    let module = module_from_src(
        UNIT_NAME,
        r"
            implementation
        
            type R = record
                member: Int32;
            end;
            
            function Func[T](item: T);
            begin
            end;
            
            initialization
                Func(R());
            end
        "
    );
    
    let unit = &module.units.iter().last().unwrap();
    let test_span = Span::zero(UNIT_NAME);

    let (_, r_ty) = unit.context
        .find_type(&IdentPath::from_parts(vec![
            Ident::new(UNIT_NAME, test_span.clone()),
            Ident::new("R", test_span.clone()),
        ])
        ).unwrap();

    let func_call = unit.unit.init.get(0)
        .and_then(Stmt::as_call)
        .and_then(Call::as_func_call)
        .unwrap_or_else(|| {
            panic!("expected unit to have a single function call in its init block")
        });

    assert!(func_call.type_args.is_some());
    let type_args = func_call.type_args.as_ref().unwrap();

    assert_eq!(&type_args[0], r_ty);
}

#[test]
fn can_infer_ty_arg_from_real_int_arg() {
    let unit = unit_from_src(
        "can_infer_ty_args_from_real_args",
        r"
            implementation
            
            function Func[T](item: T);
            begin
            end;
            
            initialization
                Func(123);
            end
        "
    );
    
    let func_call = unit.init.get(0)
        .and_then(Stmt::as_call)
        .and_then(Call::as_func_call)
        .unwrap_or_else(|| {
            panic!("expected unit to have a single function call in its init block")
        });
    
    assert!(func_call.type_args.is_some());
    let type_args = func_call.type_args.as_ref().unwrap();
    
    assert_eq!(type_args[0], Type::Primitive(Primitive::Int32));
}
