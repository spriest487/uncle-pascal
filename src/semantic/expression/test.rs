use std::{
    rc::Rc,
};

use tokenizer::*;
use semantic::*;
use syntax;
use node::{
    self,
    ExpressionValue,
    UnitReferenceKind,
};
use operators;
use types::Type;
use opts::CompileOptions;

pub fn parse_expr(src: &str, scope: Rc<Scope>) -> (Expression, Rc<Scope>) {
    let tokens = tokenize("test", src, &CompileOptions::default())
        .unwrap();

    let mut stream = syntax::TokenStream::from(tokens);

    let parsed: syntax::Expression = stream.parse()
        .unwrap();

    stream.finish().unwrap();

    Expression::annotate(&parsed, None, scope)
        .unwrap()
}

fn parse_func_decl(src: &str, scope: Rc<Scope>) -> FunctionDecl {
    let tokens = tokenize("test", src, &CompileOptions::default())
        .unwrap();

    let mut stream = syntax::TokenStream::from(tokens);
    let parsed: syntax::FunctionDecl = stream.parse()
        .unwrap();

    stream.finish().expect("expr must not contain trailing tokens");

    FunctionDecl::annotate(&parsed, scope)
        .unwrap()
        .0
}

#[test]
fn assignment_to_wrong_type_is_err() {
    let scope = Scope::new_root()
        .with_binding("x", Type::RawPointer, BindingKind::Uninitialized);

    let (expr, _) = parse_expr("x := 1", Rc::new(scope));

    match expr.type_check() {
        Err(SemanticError {
                kind: SemanticErrorKind::InvalidOperator { op, args },
                ..
            }) => {
            assert_eq!(operators::Assignment, op);
            assert_eq!(Some(Type::RawPointer), args[0]);
            assert_eq!(Some(Type::Int32), args[1]);
        }
        _ => panic!("expected invalid types in assignment")
    }
}

#[test]
fn func_call_on_obj_uses_ufcs_from_target_ns() {
    let system_scope = Scope::new_unit("System");
    let func = parse_func_decl("function TestAdd(x, y: Int64): Int64", Rc::new(system_scope.clone()));
    let system_scope = system_scope.with_function_decl(func).unwrap();

    let expr_scope = Scope::new_unit("FromNs")
        .with_binding("a", Type::Int64, BindingKind::Mutable)
        .reference(&system_scope, UnitReferenceKind::Namespaced);

    let (expr, _) = parse_expr(r"a.TestAdd(1)", Rc::new(expr_scope));

    match expr.value {
        ExpressionValue::FunctionCall(node::FunctionCall::Function { target, args }) => {
            assert!(target.is_identifier(&Identifier::from("System.TestAdd")),
                    "expected identifier, found {:?}", target.to_source());

            assert_eq!(2, args.len());

            assert!(args[0].is_identifier(&Identifier::from("a")));

            assert!(args[1].is_literal_integer(1));
        }

        _ => panic!("result should be a function call")
    }
}

#[test]
fn type_of_pointer_deref_is_pointer_minus_indirection() {
    let scope = Scope::new_root()
        .with_binding("x", Type::Byte.pointer(), BindingKind::Immutable);

    let (expr, _) = parse_expr("^x", Rc::new(scope));
    assert_eq!(Type::Byte, expr.expr_type().unwrap().unwrap());
}

#[test]
fn type_of_pointer_plus_offset_is_pointer() {
    let scope = Scope::new_root()
        .with_binding("x", Type::Byte.pointer(), BindingKind::Immutable);

    let (expr, _) = parse_expr("x + 1", Rc::new(scope));
    assert_eq!(Type::Byte.pointer(), expr.expr_type().unwrap().unwrap());
}

#[test]
fn out_param_initializes_value() {
    let mut scope = Scope::new_root()
        .with_binding("x", Type::Int32, BindingKind::Uninitialized);

    scope = scope.with_function_decl(parse_func_decl(
        "procedure SetX(out xout: System.Int32)",
        Rc::new(Scope::new_root()),
    )).unwrap();

    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    let (_, scope) = parse_expr("SetX(x)", Rc::new(scope));
    assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
}

#[test]
fn assignment_initializes_value() {
    let scope = Scope::new_root()
        .with_binding("x", Type::Int32, BindingKind::Uninitialized);

    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    let (_, scope) = parse_expr("x := 1", Rc::new(scope));
    assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
}

#[test]
fn initialization_in_both_branches_of_if_propagates() {
    let scope = Scope::new_root()
        .with_binding("x", Type::Int32, BindingKind::Uninitialized);

    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    let (_, scope) = parse_expr("if true then x := 1 else x := 2", Rc::new(scope));
    assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
}

#[test]
fn initialization_in_one_branches_of_if_propagates() {
    let scope = Scope::new_root()
        .with_binding("x", Type::Int32, BindingKind::Uninitialized);

    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    let (_, scope) = parse_expr("if true then x := 1 else begin end", Rc::new(scope));
    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
}

#[test]
fn initialization_in_if_without_else_doesnt_propagate() {
    let scope = Scope::new_root()
        .with_binding("x", Type::Int32, BindingKind::Uninitialized);

    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    let (_, scope) = parse_expr("if true then x := 1", Rc::new(scope));
    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
}

#[test]
fn binding_in_single_branch_of_if_doesnt_propagate() {
    let scope = Scope::new_root();

    let (_, scope) = parse_expr("if true then let y = 1", Rc::new(scope));
    assert_eq!(None, scope.get_symbol(&Identifier::from("y")));
}

#[test]
fn initialization_in_block_propagates() {
    let scope = Scope::new_root()
        .with_binding("x", Type::Int32, BindingKind::Uninitialized);

    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    let (_, scope) = parse_expr("begin x := 1 end", Rc::new(scope));
    assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
}

#[test]
fn binding_in_block_doesnt_propagate() {
    let scope = Scope::new_root();

    let (_, scope) = parse_expr("begin let y = 0 end", Rc::new(scope));
    assert_eq!(None, scope.get_symbol(&Identifier::from("y")));
}

#[test]
fn binding_in_while_body_doesnt_propagate() {
    let scope = Scope::new_root();

    let (_, scope) = parse_expr("while false do let y = 0", Rc::new(scope));
    assert_eq!(None, scope.get_symbol(&Identifier::from("y")));
}

#[test]
fn initialization_in_while_body_doesnt_propagate() {
    let scope = Scope::new_root()
        .with_binding("x", Type::Int32, BindingKind::Uninitialized);

    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    let (_, scope) = parse_expr("while false do x := 1", Rc::new(scope));
    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
}

#[test]
fn initialization_in_while_condition_propagates() {
    let scope = Scope::new_root()
        .with_function_decl(parse_func_decl(
            "function GetX(out xOut: System.Int32): System.Boolean",
            Rc::new(Scope::new_root()),
        ))
        .unwrap()
        .with_binding("x", Type::Int32, BindingKind::Uninitialized);

    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    let (_, scope) = parse_expr("while GetX(x) do begin end", Rc::new(scope));
    assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
}

#[test]
fn initialization_in_for_from_propagates() {
    let scope = Scope::new_root()
        .with_function_decl(parse_func_decl(
            "function GetX(out xOut: System.Int32): System.Int32",
            Rc::new(Scope::new_root()),
        ))
        .unwrap()
        .with_binding("x", Type::Int32, BindingKind::Uninitialized);

    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    let (_, scope) = parse_expr("for let i = GetX(x) to 10 do begin end", Rc::new(scope));
    assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
}

#[test]
fn initialization_in_for_to_propagates() {
    let scope = Scope::new_root()
        .with_function_decl(parse_func_decl(
            "function GetX(out xOut: System.Int32): System.Int32",
            Rc::new(Scope::new_root()),
        ))
        .unwrap()
        .with_binding("x", Type::Int32, BindingKind::Uninitialized);

    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    let (_, scope) = parse_expr("for let i = 0 to GetX(x) do begin end", Rc::new(scope));
    assert_eq!(true, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
}

#[test]
fn initialization_in_for_body_doesnt_propagate() {
    let scope = Scope::new_root()
        .with_binding("x", Type::Int32, BindingKind::Uninitialized);

    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
    let (_, scope) = parse_expr("for let i = 0 to 10 do x := 1", Rc::new(scope));
    assert_eq!(false, scope.get_symbol(&Identifier::from("x")).unwrap().initialized());
}

#[test]
fn binding_in_for_range_doesnt_propagate() {
    let scope = Scope::new_root();

    let (_, scope) = parse_expr("for let i = 0 to 3 do begin end", Rc::new(scope));
    assert_eq!(None, scope.get_symbol(&Identifier::from("i")));
}

#[test]
fn binding_in_for_body_doesnt_propagate() {
    let scope = Scope::new_root();

    let (_, scope) = parse_expr("for let i = 0 to 3 do let y = 1", Rc::new(scope));
    assert_eq!(None, scope.get_symbol(&Identifier::from("y")));
}
