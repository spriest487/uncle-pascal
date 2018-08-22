use std::rc::Rc;

use node::{
    self,
    Identifier,
    RecordKind,
    ConstExpression,
};
use consts::IntConstant;
use syntax;
use semantic::{
    Scope,
    SemanticResult,
    SemanticContext,
    SemanticError,
    RecordDecl,
    Expression,
    expression::ops::expect_valid,
    IndexRange,
};
use types::{
    Type,
    ReferenceType,
    ArrayType,
    ParameterizedName,
};
use operators;

pub type ObjectConstructor = node::ObjectConstructor<SemanticContext>;
pub type ObjectConstructorMember = node::ObjectConstructorMember<SemanticContext>;
pub type CollectionMember = node::CollectionMember<SemanticContext>;
pub type CollectionConstructor = node::CollectionConstructor<SemanticContext>;

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

        | Type::Reference(ReferenceType::Class(class_id))
        | Type::WeakReference(ReferenceType::Class(class_id))
        => {
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
            let expr = syntax::Expression::object_ctor(
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

impl CollectionMember {
    pub fn first_val(&self) -> &Expression {
        match self {
            node::CollectionMember::Single(val) => val,
            node::CollectionMember::Range { from, .. } => from,
        }
    }
}

impl CollectionConstructor {
    /* after typechecking, we always know the type of an array ctor */
    pub fn element_type(&self) -> &Type {
        self.element_type.as_ref().unwrap()
    }

    fn annotate_member(member: &syntax::CollectionMember,
                       type_hint: Option<&Type>,
                       scope: Rc<Scope>)
                       -> SemanticResult<(CollectionMember, Rc<Scope>)> {
        match member {
            node::CollectionMember::Single(val) => {
                let (val, scope) = Expression::annotate(val, type_hint, scope)?;
                Ok((node::CollectionMember::Single(val), scope))
            }

            node::CollectionMember::Range { from, to } => {
                let (from, scope) = Expression::annotate(from, type_hint, scope)?;

                /* the hint from the "from" expression is stronger than the ambient
                type hint */
                let from_type = from.expr_type()?;
                let to_type_hint = from_type.as_ref().or(type_hint);

                let (to, scope) = Expression::annotate(to, to_type_hint, scope)?;

                /* the type of "to" must be identical to that of "from" */
                let from_type = from.expr_type()?;
                let to_type = to.expr_type()?;

                if from_type != to_type {
                    return Err(SemanticError::unexpected_type(from_type, to_type, to.context));
                }

                Ok((node::CollectionMember::Range { from, to }, scope))
            }
        }
    }

    pub fn annotate(parsed_ctor: &node::CollectionConstructor<syntax::ParsedContext>,
                    expr_context: &SemanticContext)
                    -> SemanticResult<(Self, Rc<Scope>)> {
        /* type hints other than arrays are unhelpful and we can ignore them. we don't
         care about the dimensions of the hinted array type, there's nothing useful we
         can do at this stage if the hint doesn't match */
        let expected_element = expr_context.type_hint()
            .and_then(|type_hint| match type_hint {
                Type::Array(array_type) => Some(array_type.element.as_ref()),
                _ => None,
            });

        let scope = Rc::new(expr_context.scope().clone());

        match parsed_ctor.members.len() {
            0 => {
                /* a type hint must be present if the array is empty, or we can't
                 deduce its element type */
                match expected_element {
                    Some(element_type) => {
                        let ctor = CollectionConstructor {
                            members: vec![],
                            element_type: Some(element_type.clone()),
                        };

                        Ok((ctor, scope))
                    }

                    None => Err(SemanticError::unable_to_infer_type(
                        syntax::Expression::collection_ctor(
                            parsed_ctor.clone(),
                            expr_context.clone(),
                        ),
                        expr_context.clone(),
                    ))
                }
            }

            _ => {
                let (first_val, mut scope) = Self::annotate_member(
                    &parsed_ctor.members[0],
                    expected_element,
                    scope,
                )?;

                let mut result_vals = Vec::new();
                result_vals.push(first_val);

                /* the type hint takes priority, but if we don't have a hint then figure
                 out the actual type from the first element */
                let array_type = result_vals[0].first_val().expr_type()?
                    .ok_or_else(|| {
                        SemanticError::invalid_array_type(None, expr_context.clone())
                    })?;

                for member in parsed_ctor.members.iter().skip(1) {
                    let (member_val, new_scope) = Self::annotate_member(
                        member,
                        Some(&array_type),
                        scope,
                    )?;
                    expect_valid(
                        operators::Assignment,
                        Some(&array_type),
                        &member_val.first_val(),
                        &expr_context,
                    )?;

                    scope = new_scope;
                    result_vals.push(member_val);
                }

                let ctor = CollectionConstructor {
                    members: result_vals,
                    element_type: Some(array_type),
                };

                Ok((ctor, scope))
            }
        }
    }

    pub fn collection_type(&self, context: &SemanticContext) -> SemanticResult<Type> {
        match context.type_hint() {
            Some(Type::Set(set_id)) => {
                Ok(Type::Set(set_id.clone()))
            }

            _ if self.contains_ranges() =>
                unimplemented!("anonymous set type"),

            /* assume array if there are no ranges and the type hint doesn't ask for
             a set */
            _ => {
                let max_dim_val = IntConstant::from((self.members.len() - 1) as u64);

                let max_dim = max_dim_val
                    .as_i64()
                    .ok_or_else(|| SemanticError::invalid_array_index(
                        Some(ConstExpression::from(max_dim_val).value_type(None)),
                        context.clone(),
                    ))?;

                // todo? array constructors always produce single-dimension arrays
                Ok(Type::Array(ArrayType {
                    element: Box::new(self.element_type().clone()),
                    first_dim: IndexRange {
                        from: 0,
                        to: max_dim,
                    },
                    rest_dims: vec![],
                }))
            }
        }
    }
}