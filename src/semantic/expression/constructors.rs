use std::rc::Rc;

use node::{
    self,
    Identifier,
    RecordKind,
};
use syntax;
use semantic::{
    Scope,
    SemanticResult,
    SemanticContext,
    SemanticError,
    RecordDecl,
    Expression,
};
use types::Type;

pub type ObjectConstructor = node::ObjectConstructor<SemanticContext>;
pub type ObjectConstructorMember = node::ObjectConstructorMember<SemanticContext>;

fn find_object_decl<'a>(ty: &Type, scope: &'a Scope) -> Option<(&'a RecordDecl, RecordKind)> {
    match ty {
        Type::Record(record_id) => {
            let (_, record) = scope.get_record(record_id)?;

            Some((record, RecordKind::Record))
        }
        Type::Class(class_id) => {
            let (_, class) = scope.get_class(class_id)?;

            Some((class, RecordKind::Class))
        }
        _ => None
    }
}

pub fn annotate_object(obj: &syntax::ObjectConstructor,
                   expected_type: Option<&Type>,
                   context: &SemanticContext)
                   -> SemanticResult<(ObjectConstructor, Rc<Scope>)> {
    /* the constructor itself contains no type information, so we have to use whatever
     context we have to figure out what we're constructing. if that isn't available,
     the constructor is invalid at this location */
    let object_type = expected_type.cloned()
        .ok_or_else(|| {
            let expr = syntax::Expression::object_constructor(
                obj.clone(),
                syntax::ParsedContext::from(context.token.clone()),
            );
            SemanticError::unable_to_infer_type(expr, context.clone())
        })?;

    /* must be constructing a class or record type */
    let (record, kind) = find_object_decl(&object_type, context.scope.as_ref())
        .ok_or_else(|| {
            SemanticError::not_constructable(object_type.clone(), context.clone())
        })?;
    let private = kind == RecordKind::Class;

    let mut members: Vec<ObjectConstructorMember> = Vec::new();
    let mut scope = context.scope.clone();

    for member in obj.members.iter() {
        /* check access (we can throw a nicer error if we do it here, on the member) */
        if private && context.scope.unit_namespace() != record.scope().unit_namespace() {
            return Err(SemanticError::private_member_access_forbidden(
                record.qualified_name(),
                context.scope.unit_namespace().cloned(),
                member.name.clone(),
                context.clone(),
            ));
        }

        /* check we haven't got any duplicates */
        let duplicate = members.iter()
            .any(|other| other.name == member.name);

        if duplicate {
            return Err(SemanticError::duplicate_constructor_member(
                object_type,
                member.name.clone(),
                context.clone(),
            ));
        }

        /* find the record member it corresponds to */
        let target_member = record.members.iter()
            .find(|rec_member| rec_member.name == member.name)
            .ok_or_else(|| {
                let name = Identifier::from(&member.name);
                SemanticError::unknown_symbol(name, context.clone())
            })?;
        let expected_type = Some(&target_member.decl_type);

        let (value, new_scope) = Expression::annotate(&member.value, expected_type, scope)?;

        scope = new_scope;
        members.push(ObjectConstructorMember {
            name: member.name.clone(),
            value,
        });
    }

    let obj = ObjectConstructor {
        members,
        object_type: Some(object_type),
    };

    Ok((obj, scope))
}