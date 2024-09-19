use std::rc::Rc;

use crate::ast;
use crate::parse::TokenStream;
use crate::pp::Preprocessor;
use crate::typ::ast::call::overload::resolve_overload;
use crate::typ::ast::call::overload::OverloadCandidate;
use crate::typ::ast::typecheck_expr;
use crate::typ::test::module_from_src;
use crate::typ::Context;
use crate::typ::FunctionSig;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::TokenTree;
use common::span::Span;
use common::BuildOptions;
use std::cmp::Ordering;
use crate::ast::IdentPath;

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

    let unit = module.units.into_iter()
        .find(|unit| unit.unit.ident.as_slice()[0].name.as_str() == "overload")
        .unwrap();

    let candidates = unit.unit.func_defs().map(|(_vis, func)| {
        let sig = FunctionSig::of_decl(&func.decl);

        match &func.decl.impl_iface {
            Some(impl_iface) => OverloadCandidate::Method {
                ident: func.decl.ident.clone(),
                iface_ty: impl_iface.iface.clone(),
                sig: Rc::new(sig),
                decl: func.decl.clone(),
            },

            None => OverloadCandidate::Function {
                decl_name: IdentPath::from(func.decl.ident.clone()),
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

#[test]
fn resolves_method_by_args() {
    let src = r"
        implementation
        uses System;
        
        type I1 = interface
            function M(self: Self; i: Int32);
        end;

        type I2 = interface
            function M(self: Self; b: Boolean);
        end;

        function M of I1(self: String; i: Int32);
        begin
        end;

        function M of I2(self: String; b: Boolean);
        begin
        end;
        
        var 
            i: Int32 = 1;
            s: String = 'something'; 
            var b: Boolean = true;

        end
    ";

    let (mut candidates, mut ctx) = candidates_from_src(src);
    // make sure they're in the declared order, just to be sure
    candidates.sort_by(
        |a, b| match (&a.sig().params[1].ty, &b.sig().params[1].ty) {
            (Type::Primitive(Primitive::Int32), Type::Primitive(Primitive::Boolean)) => {
                Ordering::Less
            }

            (Type::Primitive(Primitive::Boolean), Type::Primitive(Primitive::Int32)) => {
                Ordering::Greater
            }

            _ => Ordering::Equal,
        },
    );

    let self_expr = typecheck_expr(&parse_expr("s"), &Type::Nothing, &mut ctx).unwrap();

    let i_expr = parse_expr("i");
    let i_span = i_expr.annotation().clone();
    let i_overload =
        resolve_overload(&candidates, &[i_expr], Some(&self_expr), &i_span, &mut ctx).unwrap();

    assert_eq!(i_overload.selected_sig, 0);

    let b_expr = parse_expr("b");
    let b_span = b_expr.annotation().clone();
    let b_overload =
        resolve_overload(&candidates, &[b_expr], Some(&self_expr), &b_span, &mut ctx).unwrap();

    assert_eq!(b_overload.selected_sig, 1);
}

#[test]
fn resolves_overload_by_arg_ty() {
    let src = r"
        implementation
        function X(i: Int32);
        begin
        end;

        function X(b: Boolean);
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
