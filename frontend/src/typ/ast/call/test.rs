use crate::ast;
use crate::ast::{Access, IdentPath};
use crate::parse::TokenStream;
use crate::pp::Preprocessor;
use crate::typ::ast::call::overload::resolve_overload;
use crate::typ::ast::call::overload::OverloadCandidate;
use crate::typ::test::module_from_src;
use crate::typ::test::try_module_from_src;
use crate::typ::test::try_module_from_srcs;
use crate::typ::Context;
use crate::typ::FunctionSig;
use crate::typ::Module;
use crate::typ::Symbol;
use crate::typ::TypeError;
use crate::typ::Typed;
use crate::TokenTree;
use common::span::Span;
use common::BuildOptions;
use std::rc::Rc;

fn expr_from_str(src: &str) -> ast::Expr<Span> {
    let test_unit = Preprocessor::new("test", BuildOptions::default())
        .preprocess(src)
        .unwrap();
    let tokens = TokenTree::tokenize(test_unit).unwrap();

    let mut tokens = TokenStream::new(tokens, Span::zero("test"));

    ast::Expr::parse(&mut tokens)
        .and_then(|expr| {
            tokens.finish()?;
            Ok(expr)
        })
        .unwrap()
}

fn candidates_from_module(module: &Module, unit_name: &str) -> Vec<OverloadCandidate> {
    let unit = module.units
        .iter()
        .find(|unit| unit.unit.ident.last().name.as_str() == unit_name)
        .unwrap();

    let candidates = unit.unit
        .func_defs()
        .map(|(_vis, func)| {
            let sig = FunctionSig::of_decl(&func.decl);

            match &func.decl.name.owning_ty {
                Some(explicit_impl) => {
                    let ident = func.decl.name.ident.clone();
                    let method = explicit_impl
                        .get_method(&ident, &unit.context)
                        .unwrap()
                        .expect("method defs in unit must have a corresponding decl in the type");

                    OverloadCandidate::Method {
                        ident,
                        owning_ty: explicit_impl.clone(),
                        sig: Rc::new(sig),
                        access: method.access,
                    }
                },

                None => {
                    let decl_name = IdentPath::from(func.decl.name.ident.clone());

                    OverloadCandidate::Function {
                        decl_name: Symbol::from(decl_name),
                        sig: Rc::new(sig),
                    }
                },
            }
        });

    candidates.collect()
}

fn candidates_from_src(src: &'static str, unit_name: &str) -> (Vec<OverloadCandidate>, Context) {
    let module = module_from_src(unit_name, src);

    let candidates = candidates_from_module(&module, unit_name);

    (candidates, module.units
        .into_iter()
        .filter_map(|unit| if unit.unit.ident.last().name.as_str() == unit_name {
            Some(unit.context)
        } else {
            None
        })
        .next()
        .unwrap())
}

#[test]
fn resolves_overload_single() {
    let src = r"
        implementation
        uses System;
        
        function X(i: Int32);
        begin
        end;
        
        var i: Int32;

        initialization
            i := 1;
        end
    ";
    let (candidates, mut ctx) = candidates_from_src(src, "overload");

    let expr = expr_from_str("i");
    let span = expr.annotation().clone();

    let overload = resolve_overload(&candidates, &[expr], None, None, &span, &mut ctx).unwrap();

    assert_eq!(0, overload.selected_sig);
    assert_eq!(1, overload.args.len());
    assert_eq!("i", overload.args[0].to_string());
}

// explicit overloading isn't implemented!
#[test]
fn resolves_overload_by_arg_ty() {
    let src = r"
        implementation
        uses System;
            
        function X(i: Int32); overload;
        begin
        end;

        function X(b: Boolean); overload;
        begin
        end;
        
        var
            i: Int32 = 1;
            b: Boolean = true;
        end
    ";
    let (candidates, mut ctx) = candidates_from_src(src, "overload");

    let expr = expr_from_str("i");
    let span = expr.annotation().clone();

    let overload = resolve_overload(&candidates, &[expr], None, None, &span, &mut ctx).unwrap();

    assert_eq!(0, overload.selected_sig);
    assert_eq!(1, overload.args.len());
    assert_eq!("i", overload.args[0].to_string());
}

#[test]
fn method_call_validates_too_many_args() {
    let src = r"
        implementation
        uses System;
        
        type AClass = class
            function M1(a: Integer);
        end;
        
        function AClass.M1(a: Integer);
        begin
        end;
        
        initialization
            var instance := AClass();
            instance.M1(1, 2);
        end.
    ";
    
    match try_module_from_src("method_call_validates_too_many_args", src) {
        Err(TypeError::InvalidArgs { actual, expected, .. }) => {
            assert_eq!(3, actual.len());
            assert_eq!(2, expected.len());
        }
        
        other => panic!("expected invalid args error, got: {:?}", other),
    }
}

#[test]
fn method_call_validates_too_few_args() {
    let src = r"
        implementation
        uses System;
        
        type AClass = class
            function M1(a, b: Integer);
        end;
        
        function AClass.M1(a, b: Integer);
        begin
        end;
        
        initialization
            var instance := AClass();
            instance.M1(1);
        end.
    ";

    match try_module_from_src("method_call_validates_too_many_args", src) {
        Err(TypeError::InvalidArgs { actual, expected, .. }) => {
            assert_eq!(2, actual.len());
            assert_eq!(3, expected.len());
        }

        other => panic!("expected invalid args error, got: {:?}", other),
    }
}

#[test]
fn specializes_func_call_by_arg_ty() {
    let src = r"
        implementation
        
        type B = class
        end;
        
        function A[T](arg: T);
        begin
        end;
        
        initialization
            var arg := B();
            A(arg);
        end.
    ";
    
    let module = module_from_src("Test", src);
    let unit = module.units.iter().last().unwrap();
    let init = unit.unit.init.as_ref().unwrap();

    match init.body[1].as_call() {
        Some(ast::Call::Function(func_call)) => {
            assert_eq!("A", func_call.target.to_string());
            assert_eq!("Test.B", func_call.args[0].annotation().ty().to_string());
            
            match func_call.target.annotation() {
                Typed::Function(func) => {
                    assert_eq!("Test.B", func.name.type_args.as_ref().unwrap()[0].to_string());
                    assert_eq!("Test.A[Test.B]",  func.name.to_string());
                    
                    // this should reference the *declared* sig, not the called one
                    assert_eq!("T", func.sig.params[0].ty.to_string());
                }

                other => panic!("expected function, got {:?}", other),
            }
        }
        
        other => panic!("expected call to A, got {:?}", other),
    }
}

#[test]
fn specializes_method_call_by_arg_ty() {
    let src = r"
        implementation
        
        type C = class
            function A[T](arg: T);
        end;
        
        type B = class
        end;
        
        function C.A[T](arg: T);
        begin
        end;
        
        initialization
            var instance := C();
            var arg := B();
            instance.A(arg);
        end.
    ";

    let module = module_from_src("Test", src);
    let unit = module.units.iter().last().unwrap();
    let init = unit.unit.init.as_ref().unwrap();

    match init.body[2].as_call() {
        Some(ast::Call::Method(method_call)) => {
            assert_eq!("instance", method_call.args[0].to_string());
            assert_eq!("Test.C", method_call.args[0].annotation().ty().to_string());

            assert_eq!("arg", method_call.args[1].to_string());
            assert_eq!("Test.B", method_call.args[1].annotation().ty().to_string());
            
            assert_eq!("A", method_call.ident.to_string());
            assert_eq!("Test.C", method_call.owning_type.to_string());
            
            assert_eq!("Test.B", method_call.type_args.as_ref().unwrap().items[0].to_string());
        }

        other => panic!("expected call to A, got {:?}", other),
    }
}

#[test]
fn specializes_method_call_by_lambda_arg_ty() {
    let src = r"
        implementation
        
        type C = class
            function A[T](arg: function: T);
        end;
        
        type B = class
        end;
        
        function C.A[T](arg: function: T);
        begin
        end;
        
        initialization
            var instance := C();
            instance.A(lambda: B());
        end.
    ";

    let module = module_from_src("Test", src);
    let unit = module.units.iter().last().unwrap();
    let init = unit.unit.init.as_ref().unwrap();

    match init.body[1].as_call() {
        Some(ast::Call::Method(method_call)) => {
            assert_eq!("instance", method_call.args[0].to_string());
            assert_eq!("Test.C", method_call.args[0].annotation().ty().to_string());

            assert_eq!("function: Test.B", method_call.args[1].annotation().ty().to_string());

            assert_eq!("A", method_call.ident.to_string());
            assert_eq!("Test.C", method_call.owning_type.to_string());

            assert_eq!("Test.B", method_call.type_args.as_ref().unwrap().items[0].to_string());
        }

        other => panic!("expected call to A, got {:?}", other),
    }
}

#[test]
fn overload_with_accessible_method_is_ambiguous() {
    let a_src = r"
        interface
        type MyClass = class
        public
            function A;
        end;
        
        function NewMyClass: MyClass;
        function A(my: MyClass);
        
        implementation
        
        function NewMyClass: MyClass;
        begin
            MyClass()
        end;
        
        function MyClass.A;
        begin
        end;
        
        function A(my: MyClass);
        begin
        end;

        end
    ";
    let b_src = r"
        implementation
        uses UnitA;

        initialization
            var i := NewMyClass;
            i.A();
        end
    ";

    let result = try_module_from_srcs([
        ("UnitA", a_src),
        ("UnitB", b_src),
    ]);

    match result {
        Ok(..) => panic!("call to A should be ambiguous"),
        Err(TypeError::AmbiguousFunction { candidates, .. }) => {
            candidates
                .iter()
                .find(|candidate| match candidate {
                    OverloadCandidate::Method { owning_ty: iface_ty, ident, .. } => {
                        iface_ty.to_string() == "UnitA.MyClass" && ident.name.as_str() == "A"
                    }
                    _ => false,
                })
                .expect("must have a candidate for the public method");

            candidates
                .iter()
                .find(|candidate| match candidate {
                    OverloadCandidate::Function { decl_name, .. } => {
                        decl_name.to_string() == "UnitA.A"
                    }
                    _ => false,
                })
                .expect("must have a candidate for the free function");
        }
        
        Err(other) => panic!("expected ambiguous function error, got: {}", other)
    }
}

/// if a method and a free function both match a call, but the method is inaccessible from the
/// call's context, it should resolve to the function instead of being ambiguous 
#[test]
fn overload_with_inaccessible_method_is_not_ambiguous() {
    let a_src = r"
        interface
        type MyClass = class 
        private
            function A;
        end;
        
        function NewMyClass: MyClass;
        function A(my: MyClass);

        implementation
        
        function NewMyClass: MyClass;
        begin
            MyClass()
        end;
        
        function MyClass.A;
        begin
        end;
        
        function A(my: MyClass);
        begin
        end;

        end
    ";
    let b_src = r"
        implementation
        uses UnitA;

        initialization
            var i := NewMyClass();
            i.A();
        end
    ";
    
    let result = try_module_from_srcs([
        ("UnitA", a_src),
        ("UnitB", b_src),
    ]);
    
    result.expect("call to A should not be ambiguous");
}

/// if an inaccessible method and a function have the same name, but only the method matches, 
/// it should resolve the method and report an access error instead of being ambiguous
#[test]
fn overload_resolves_inaccessible_method_if_only_match() {
    let a_src = r"
        interface
        type MyClass = class 
        private
            function A;
        end;
        
        function NewMyClass: MyClass;
        function A(my, extra: MyClass);

        implementation
        
        function NewMyClass: MyClass;
        begin
            MyClass()
        end;
        
        function MyClass.A;
        begin
        end;
        
        function A(my, extra: MyClass);
        begin
        end;

        end
    ";
    let b_src = r"
        implementation
        uses UnitA;

        initialization
            var i := NewMyClass();
            i.A();
        end
    ";

    let result = try_module_from_srcs([
        ("UnitA", a_src),
        ("UnitB", b_src),
    ]);

    match result {
        Ok(..) => panic!("call to A should be inaccessible"),

        Err(TypeError::TypeMemberInaccessible { member, access, .. }) => {
            assert_eq!("A", member.name.as_str());
            assert_eq!(Access::Private, access);
        }

        Err(other) => panic!("expected access error, got: {}", other)
    }
}
