use std::rc::Rc;
use super::*;
use source;
use tokens;
use keywords;
use consts::IntConstant;

fn fake_context() -> SemanticContext {
    let location = source::Location::new("test", 0, 0);
    let token = tokens::Keyword(keywords::Program);
    SemanticContext {
        token: source::Token::new(token, location),
        scope: Rc::new(Scope::new_root()),
    }
}

#[test]
fn resolves_consts_in_referenced_units() {
    let const_val = ConstantExpression::Integer(IntConstant::from(3));

    let imported = Scope::new_unit("NS1")
        .with_const("CONST1", const_val.clone());

    let scope = Scope::new_unit("NS2")
        .reference(&imported, UnitReferenceKind::Namespaced);

    let expected_id = Identifier::from("NS1.CONST1");
    match scope.get_const(&expected_id) {
        Some((result_id, result_val)) => {
            assert_eq!(expected_id, *result_id);
            assert_eq!(const_val, *result_val);
        }
        None => panic!("name {} must be found in scope {:?}", expected_id, scope)
    }
}

#[test]
fn add_record_adds_record_in_local_ns() {
    let record_decl = RecordDecl {
        context: fake_context(),
        members: vec![],
        name: "World".to_string(),
        kind: RecordKind::Record,
        variant_part: None,
    };

    let scope = Scope::new_unit("Hello")
        .with_record(record_decl);

    let (result_id, result) = scope.get_record(&Identifier::from("Hello.World"))
        .unwrap_or_else(|| {
            panic!("get_record should return a result in scope {:?}", scope)
        });
    assert_eq!("World", &result.name);
    assert_eq!(Identifier::from("Hello.World"), *result_id);
}
