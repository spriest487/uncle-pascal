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
    expression::ops::expect_valid,
};
use types::{
    Type,
    ParameterizedName
};
use operators;

pub type ObjectConstructor = node::ObjectConstructor<SemanticContext>;
pub type ObjectConstructorMember = node::ObjectConstructorMember<SemanticContext>;

struct ConstructedObject {
    record: RecordDecl,
    type_args: Vec<Type>,
}

impl ConstructedObject {
    fn qualified_name(&self) -> ParameterizedName {
        let name = self.record.qualified_name();
        ParameterizedName::new_with_args(name, self.type_args.clone())
    }
}

fn find_object_decl(ty: &Type, scope: &Scope) -> Option<ConstructedObject> {
    match ty {
        Type::Record(record_id) => {
            let (_, record) = scope.get_record_specialized(record_id)?;

            Some(ConstructedObject {
                record,
                type_args: record_id.type_args.clone(),
            })
        }
        Type::Class(class_id) => {
            let (_, class) = scope.get_class_specialized(class_id)?;

            Some(ConstructedObject {
                record: class,
                type_args: class_id.type_args.clone(),
            })
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
    let constructed = find_object_decl(&object_type, context.scope.as_ref())
        .ok_or_else(|| {
            SemanticError::not_constructable(object_type.clone(), context.clone())
        })?;
    let private = constructed.record.kind == RecordKind::Class;

    let mut members: Vec<ObjectConstructorMember> = Vec::new();
    let mut scope = context.scope.clone();

    for member in &obj.members {
        /* check access (we can throw a nicer error if we do it here, on the member) */
        if private && context.scope.unit_namespace() != constructed.record.scope().unit_namespace() {
            return Err(SemanticError::private_member_access_forbidden(
                constructed.qualified_name(),
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
        let target_member = constructed.record.members.iter()
            .find(|rec_member| rec_member.name == member.name)
            .ok_or_else(|| {
                let name = Identifier::from(&member.name);
                SemanticError::unknown_symbol(name, context.clone())
            })?;
        let expected_type = Some(&target_member.decl_type);

        let (value, new_scope) = Expression::annotate(&member.value, expected_type, scope)?;

        expect_valid(operators::Assignment, expected_type, &value, context)?;

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