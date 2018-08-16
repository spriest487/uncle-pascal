use super::*;
use tokenizer::*;
use source;
use operators;
use node::ToSource;

fn try_parse_expr(src: &str) -> ExpressionResult {
    let context = source::test::empty_context();

    let tokens = tokenize("test", src).unwrap();

    Expression::parse(tokens, &context)
}

fn parse_expr(src: &str) -> Expression {
    try_parse_expr(src).unwrap()
        .finish()
        .expect("expression must parse with no trailing tokens")
}

#[test]
fn brackets_groups_exprs() {
    let expr = parse_expr("(1 + 2) - 3");

    assert!(expr.is_binary_op(operators::Minus));
    let (lhs, _, rhs) = expr.unwrap_binary_op();

    assert!(rhs.is_literal_integer(3));
    assert!(lhs.is_binary_op(operators::Plus));

    let (nested_lhs, _, nested_rhs) = lhs.unwrap_binary_op();
    assert!(nested_lhs.is_literal_integer(1));
    assert!(nested_rhs.is_literal_integer(2));
}

#[test]
fn simple_binary_op() {
    let expr = parse_expr("1 + 2");

    assert!(expr.is_binary_op(operators::Plus));

    let (lhs, _, rhs) = expr.unwrap_binary_op();
    assert!(lhs.is_literal_integer(1));
    assert!(rhs.is_literal_integer(2));
}

#[test]
fn simple_binary_op_in_brackets() {
    let expr = parse_expr("(1 + 2)");

    assert!(expr.is_binary_op(operators::Plus));

    let (lhs, _, rhs) = expr.unwrap_binary_op();
    assert!(lhs.is_literal_integer(1));
    assert!(rhs.is_literal_integer(2));
}

#[test]
fn simple_prefix_op() {
    let expr = parse_expr("+1");

    assert!(expr.is_prefix_op(operators::Plus));
    let (_, rhs) = expr.unwrap_prefix_op();

    assert!(rhs.is_literal_integer(1));
}

#[test]
fn parses_multiple_line_compound_expr() {
    let expr = try_parse_expr(r"a.b := 1
        b.c := 2")
        .unwrap();
    assert!(expr.value.is_binary_op(operators::Assignment));

    let remaining: Vec<_> = expr.next_tokens.collect();
    assert_eq!(5, remaining.len(), "expected `b := 2` to be left over but got tokens {:?}", remaining);

    let second_ctx = remaining[0].clone();
    let second_expr = Expression::parse(remaining, &second_ctx)
        .unwrap()
        .finish()
        .unwrap();

    assert!(second_expr.is_binary_op(operators::Assignment));
}

#[test]
fn parses_binary_expr_followed_by_func_call() {
    let expr_result = try_parse_expr(r"
            ^a := ^(b + 1)

            System.FreeMem(self.Elements)")
        .unwrap();

    let expr: Expression = expr_result.value;
    match &expr.value {
        &node::ExpressionValue::BinaryOperator { .. } => (),
        _ => panic!("expected first expr in stream to be assignment, found {:?}", expr)
    }
}

#[test]
fn parses_multi_line_block_without_terminators() {
    let expr = parse_expr(r"begin
        a.b := 1
        b.c := 2
        end");

    assert!(expr.is_block());
}

#[test]
fn binary_op_precedence() {
    let binary_ops_precedence = operators::PRECEDENCE.iter()
        .filter_map(|&(op, pos)| if pos == operators::Position::Binary {
            Some(op)
        } else {
            None
        })
        .collect::<Vec<_>>();

    for (prec_a, op_a) in binary_ops_precedence.iter().enumerate() {
        let other_ops = binary_ops_precedence.iter().enumerate()
            .filter(|&(prec, _)| prec != prec_a);

        for (prec_b, op_b) in other_ops {
            let hi_op = *if prec_a < prec_b { op_a } else { op_b };
            let lo_op = *if prec_a < prec_b { op_b } else { op_a };

            /* should parse as ((1 hi_op 2) lo_op 3) */
            let hi_first = parse_expr(&format!("1 {} 2 {} 3", hi_op.to_source(), lo_op.to_source()));
            assert!(hi_first.is_binary_op(lo_op), "{} should have precedence over {} (understood this as {:?})", hi_op, lo_op, hi_first);

            let (hi_first_1_then_2, _, hi_first_3) = hi_first.unwrap_binary_op();
            assert!(hi_first_1_then_2.is_binary_op(hi_op));
            assert!(hi_first_3.is_literal_integer(3), "rhs should be 3, but was `{:?}`", hi_first_3);

            /* should parse as (1 lo_op (2 hi_op 3)) */
            let lo_first = parse_expr(&format!("1 {} 2 {} 3", lo_op, hi_op));
            assert!(lo_first.is_binary_op(lo_op), "{} should have precedence over {} (understood as {:?})", hi_op, lo_op, lo_first);

            let (lo_first_1, _, lo_first_2_then_3) = lo_first.unwrap_binary_op();
            assert!(lo_first_1.is_literal_integer(1), "lhs should be 1, but was `{:?}`", lo_first_1);
            assert!(lo_first_2_then_3.is_binary_op(hi_op));
        }
    }
}

#[test]
fn parses_deref_on_lhs() {
    let expr = parse_expr("^a + 1");

    assert!(expr.is_binary_op(operators::Plus), "result should be plus, was: {}", expr.to_source());
    let (lhs, _, rhs) = expr.unwrap_binary_op();

    assert!(lhs.is_prefix_op(operators::Deref));
    assert!(rhs.is_literal_integer(1));
}

#[test]
fn parses_assign_deref_to_deref() {
    let expr = parse_expr("^(a + 1) := ^(b + 1)");

    assert!(expr.is_binary_op(operators::Assignment), "result should be an assignment, was: {}", expr.to_source());
    let (lhs, _, rhs) = expr.unwrap_binary_op();

    assert!(lhs.is_prefix_op(operators::Deref));
    assert!(rhs.is_prefix_op(operators::Deref));
}

#[test]
fn parses_nested_function_calls() {
    let expr = parse_expr("test(hello('world'), goodbye(cruel('world')))");

    assert!(expr.is_function_call(), "result should be a function call expr");
    let (test_id, test_args) = expr.unwrap_function_call();

    assert_eq!(node::Identifier::from("test"), test_id);
    assert_eq!(2, test_args.len());

    let hello_func = test_args[0].clone();
    assert!(hello_func.is_function_call(), "first argument should be a function call expr");
    let (hello_id, hello_args) = hello_func.unwrap_function_call();
    assert_eq!(node::Identifier::from("hello"), hello_id);
    assert_eq!(1, hello_args.len());
    assert!(hello_args[0].is_any_literal_string());
    assert_eq!("world", hello_args[0].clone().unwrap_literal_string());

    let goodbye_func = test_args[1].clone();
    assert!(goodbye_func.is_function_call(), "second argument should be a function call expr");
    let (goodbye_id, goodbye_args) = goodbye_func.unwrap_function_call();
    assert_eq!(node::Identifier::from("goodbye"), goodbye_id);
    assert_eq!(1, goodbye_args.len());
    assert!(goodbye_args[0].is_function_call());

    let (cruel_id, cruel_args) = goodbye_args[0].clone().unwrap_function_call();
    assert_eq!(node::Identifier::from("cruel"), cruel_id);
    assert_eq!(1, cruel_args.len());
    assert!(cruel_args[0].is_any_literal_string());
    assert_eq!("world", cruel_args[0].clone().unwrap_literal_string());
}

#[test]
fn parses_single_prefix_op_in_brackets() {
    let expr = parse_expr("(^a)");

    assert!(expr.is_prefix_op(operators::Deref));
}

#[test]
fn parses_deref_struct_field() {
    let expr = parse_expr("(^a).b");

    assert!(expr.is_any_member());
    let (base, name) = expr.unwrap_member();
    assert!(base.is_prefix_op(operators::Deref));
    assert_eq!("b", name);
}

#[test]
fn parses_namespaced_fn_call_with_operator_in_args() {
    let expr = parse_expr("a.b(1+2)");
    assert!(expr.is_function_call());
}

#[test]
fn parses_fn_call_with_binary_operator_in_args() {
    let expr = parse_expr("a(1+2)");
    assert!(expr.is_function_call());
}

#[test]
fn parses_fn_call_with_prefix_operator_in_args() {
    let expr = parse_expr("a(^b)");
    assert!(expr.is_function_call());
}

#[test]
fn parses_namespaced_fn_call_with_prefix_operator_in_args() {
    let expr = parse_expr("a.b(^c)");
    assert!(expr.is_function_call());
}

#[test]
fn parses_assignment_followed_by_prefix_operator() {
    let expr = try_parse_expr(r"a := b
        ^a")
        .unwrap();

    assert!(expr.value.is_binary_op(operators::Assignment));

    let next_expr = Expression::parse(expr.next_tokens, &expr.last_token)
        .unwrap()
        .finish()
        .unwrap();

    assert!(next_expr.is_prefix_op(operators::Deref));
}

#[test]
fn parses_binay_plus_followed_by_unary_plus_in_brackets() {
    let expr = try_parse_expr(r"1 + 2
        (+a)")
        .unwrap();
}

#[test]
fn parses_binary_op_with_fn_call_on_rhs() {
    let expr = parse_expr("x := a.b()");
    assert!(expr.is_binary_op(operators::Assignment));
}

#[test]
fn parses_binary_op_with_fn_where_args_contain_prefix_op_and_binary_op() {
    let expr = parse_expr("a := b((+c) - d)");
    assert!(expr.is_binary_op(operators::Assignment));
}

#[test]
fn parses_fn_call_where_args_contain_binary_op_with_operand_in_brackets() {
    let expr = parse_expr("b((c) - d)");
    assert!(expr.is_function_call());
}

#[test]
fn parses_binary_op_with_operand_in_brackets() {
    let expr = parse_expr("(c) - d");
    assert!(expr.is_binary_op(operators::Minus));
}

#[test]
fn parses_fn_where_args_contains_member_access() {
    let expr = parse_expr("a((b).c)");
    assert!(expr.is_function_call());
}

#[test]
fn parses_fn_where_args_contains_member_access_of_deref() {
    let expr = parse_expr("a((^b).c)");
    assert!(expr.is_function_call());
}

#[test]
fn parses_assignment_to_member_of_deref() {
    let expr = parse_expr("(^self).Elements := newElements");
    assert!(expr.is_binary_op(operators::Assignment));

    let (lhs, _, rhs) = expr.unwrap_binary_op();
    assert!(rhs.is_identifier(&node::Identifier::from("newElements")));

    assert!(lhs.is_any_member());
    let (deref, member_name) = lhs.unwrap_member();
    assert!(deref.is_prefix_op(operators::Deref));
    assert_eq!("Elements", member_name);
}

#[test]
fn parses_fn_call_then_expr_on_next_line() {
    let expr = try_parse_expr(r"a.b('hello world')
    if x = 0 then begin end")
        .expect("expression should parse successfully");

    assert!(expr.value.is_function_call());

    let next_expr = Expression::parse(expr.next_tokens, &expr.last_token)
        .expect("second expression must parse successfully")
        .finish()
        .expect("second expression must parse with no trailing tokens");

    assert!(next_expr.is_if());
}
