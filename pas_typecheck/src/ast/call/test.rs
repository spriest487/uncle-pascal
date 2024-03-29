use std::rc::Rc;

use pas_common::span::Span;
use pas_common::BuildOptions;
use pas_syn::parse::TokenStream;
use pas_syn::{ast, TokenTree};

use crate::ast::{call::resolve_overload, typecheck_expr, OverloadCandidate};
use crate::test::module_from_src;
use crate::{Context, FunctionSig, Primitive, Type};
use std::cmp::Ordering;

fn parse_expr(src: &str) -> ast::Expression<Span> {
    let tokens = TokenTree::tokenize("test", src, &BuildOptions::default()).unwrap();

    let mut tokens = TokenStream::new(tokens, Span::zero("test"));

    ast::Expression::parse(&mut tokens)
        .and_then(|expr| {
            tokens.finish()?;
            Ok(expr)
        })
        .unwrap()
}

fn candidates_from_src(src: &'static str) -> (Vec<OverloadCandidate>, Context) {
    let mut module = module_from_src("overload", src);

    let unit = module.units.remove(0);

    let candidates = unit.unit.func_defs().map(|func| {
        let sig = FunctionSig::of_decl(&func.decl);

        match &func.decl.impl_iface {
            Some(impl_iface) => OverloadCandidate::Method {
                ident: func.decl.ident.last().clone(),
                iface_ty: impl_iface.iface.clone(),
                sig: Rc::new(sig),
                decl: func.decl.clone(),
            },

            None => OverloadCandidate::Function {
                decl_name: func.decl.ident.clone(),
                sig: Rc::new(sig),
            },
        }
    });

    (candidates.collect(), unit.context)
}

#[test]
fn resolves_overload_single() {
    let src = r"
        function X(i: Integer)
        begin
        end;

        let i: Integer := 1;
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
        type I1 = interface
            function M(self: Self; i: Integer);
        end;

        type I2 = interface
            function M(self: Self; b: Boolean);
        end;

        type C = class end;

        function M of I1(self: C; i: Integer)
        begin
        end;

        function M of I2(self: C; b: Boolean)
        begin
        end;

        let c := C();
        let i: Integer := 1;
        let b: Boolean := true;
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

    let self_expr = typecheck_expr(&parse_expr("c"), &Type::Nothing, &mut ctx).unwrap();

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
        function X(i: Integer)
        begin
        end;

        function X(b: Boolean)
        begin
        end;

        let i: Integer := 1;
    ";
    let (candidates, mut ctx) = candidates_from_src(src);

    let expr = parse_expr("i");
    let span = expr.annotation().clone();

    let overload = resolve_overload(&candidates, &[expr], None, &span, &mut ctx).unwrap();

    assert_eq!(0, overload.selected_sig);
    assert_eq!(1, overload.args.len());
    assert_eq!("i", overload.args[0].to_string());
}
