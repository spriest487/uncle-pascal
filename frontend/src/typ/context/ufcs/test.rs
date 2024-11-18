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
    let target = Type::of_decl(target_decl, &unit.context).unwrap();
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
    let target = Type::of_decl(target_decl, &a.context).unwrap();
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

    let target = Type::of_decl(&a.unit.type_decl_items().next().unwrap().1, &c.context).unwrap();
    let methods = find_ufcs_free_functions(&target, &c.context);

    assert_eq!(methods.len(), 0);
}

#[test]
fn generic_class_method_has_correct_self_ty() {
    let src = r"
        implementation
        uses System;
        
        type MyClass[T] = class
            val: Integer;
            function MyMethod;
        end;
        
        function MyClass[T].MyMethod;
        begin
        end;

        initialization
            var b := MyClass[Integer](val: 1);
            b.MyMethod();
        end.
    ";
    
    // eprintln!("{:#?}", {
    //     let pp_unit = crate::preprocess("test", src, crate::BuildOptions::default())
    //         .unwrap();
    //     let tokens = crate::tokenize(pp_unit).unwrap();
    //     let parsed = crate::parse("test", tokens).unwrap();
    //     parsed
    // });
    
    unit_from_src("test", src);
}
