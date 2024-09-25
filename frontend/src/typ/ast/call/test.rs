use std::rc::Rc;

use crate::ast;
use crate::ast::IdentPath;
use crate::parse::TokenStream;
use crate::pp::Preprocessor;
use crate::typ::ast::call::overload::resolve_overload;
use crate::typ::ast::call::overload::OverloadCandidate;
use crate::typ::test::module_from_src;
use crate::typ::Context;
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
                Some(explicit_impl) => OverloadCandidate::Method {
                    ident: func.decl.name.ident.clone(),
                    iface_ty: explicit_impl.clone(),
                    sig: Rc::new(sig),
                    decl: func.decl.clone(),
                },
    
                None => OverloadCandidate::Function {
                    decl_name: IdentPath::from(func.decl.name.ident.clone()),
                    sig: Rc::new(sig),
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

    let overload = resolve_overload(&candidates, &[expr], None, &span, &mut ctx).unwrap();

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

    let overload = resolve_overload(&candidates, &[expr], None, &span, &mut ctx).unwrap();

    assert_eq!(0, overload.selected_sig);
    assert_eq!(1, overload.args.len());
    assert_eq!("i", overload.args[0].to_string());
}
