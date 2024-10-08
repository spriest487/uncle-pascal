use std::rc::Rc;

use crate::ast;
use crate::ast::IdentPath;
use crate::parse::TokenStream;
use crate::pp::Preprocessor;
use crate::typ::ast::call::overload::resolve_overload;
use crate::typ::ast::call::overload::OverloadCandidate;
use crate::typ::test::{module_from_src, try_module_from_src};
use crate::typ::{Context, Symbol, TypeError, Typed};
use crate::typ::FunctionSig;
use crate::TokenTree;
use common::span::Span;
use common::BuildOptions;

fn parse_expr(src: &str) -> ast::Expr<Span> {
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

fn candidates_from_src(src: &'static str) -> (Vec<OverloadCandidate>, Context) {
    let module = module_from_src("overload", src);

    let unit = module.units
        .into_iter()
        .find(|unit| unit.unit.ident.as_slice()[0].name.as_str() == "overload")
        .unwrap();

    let candidates = unit.unit
        .func_defs()
        .map(|(_vis, func)| {
            let sig = FunctionSig::of_decl(&func.decl);
    
            match &func.decl.name.owning_ty {
                Some(explicit_impl) => {
                    let ident = func.decl.name.ident.clone();

                    OverloadCandidate::Method {
                        ident,
                        iface_ty: explicit_impl.clone(),
                        sig: Rc::new(sig),
                        decl: func.decl.clone(),
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

    (candidates.collect(), unit.context)
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
    let (candidates, mut ctx) = candidates_from_src(src);

    let expr = parse_expr("i");
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

        initialization
            var i: Int32 := 1;
        end
    ";
    let (candidates, mut ctx) = candidates_from_src(src);

    let expr = parse_expr("i");
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
                    assert_eq!("Test.B", func.sig.params[0].ty.to_string());
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
            assert_eq!("Test.C", method_call.iface_type.to_string());
            
            assert_eq!("Test.B", method_call.type_args.as_ref().unwrap().items[0].to_string());
        }

        other => panic!("expected call to A, got {:?}", other),
    }
}
