use std::fmt;

use types::Type;
use semantic::{
    Scope,
    Expression,
    BindingKind,
    SemanticContext,
    Block,
};
use node::{
    Context,
    ExpressionValue,
    FunctionCall,
    Identifier,
};
use operators;

pub struct BoundName {
    pub name: String,
    pub bound_type: Type,
}

pub struct RcTemporary {
    /* the main value that holds the rc subvalues, not necessarily an rc value itself.
    e.g. a record, an array */
    pub base_value: Expression,

    /* all subvalues of the base_value which need reference counting. if the base value is
     itself an rc value, this may be a single item with the same expression as base_value */
    pub rc_subvalues: Vec<RcSubValuePath>,
}

impl fmt::Display for RcTemporary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "`{}` (rc subvalues: ", self.base_value)?;
        for (i, subval) in self.rc_subvalues.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "`{}`", subval)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone)]
pub enum RcSubValuePath {
    This {
        parent: Option<Box<RcSubValuePath>>
    },

    Member {
        parent: Option<Box<RcSubValuePath>>,
        name: String,
    },

    ArrayElement {
        parent: Option<Box<RcSubValuePath>>,
        index: i64,
    },
}

impl fmt::Display for RcSubValuePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RcSubValuePath::This { parent: None } =>
                write!(f, "<this>"),
            RcSubValuePath::This { parent: Some(parent) } =>
                write!(f, "{}", parent),

            RcSubValuePath::Member { parent: None, name } =>
                write!(f, "<this>.{}", name),
            RcSubValuePath::Member { parent: Some(parent), name } =>
                write!(f, "{}.{}", parent, name),

            RcSubValuePath::ArrayElement { parent: None, index } =>
                write!(f, "<this>[{}]", index),
            RcSubValuePath::ArrayElement { parent: Some(parent), index } =>
                write!(f, "{}[{}]", parent, index),
        }
    }
}

pub enum RcStatement {
    Block(Vec<RcStatement>),

    Statement {
        bound_name: Option<BoundName>,
        body: Box<Expression>,

        /*
            expressions which create new reference-counted values that need to be hoisted
            into temporary vars so they can be released after the statement
        */
        rc_bindings: Vec<RcTemporary>,

        /*
            expressions referring to rc-typed lvalues that are assigned to during this
            statement, which will need to be released before assignment and retained
            afterwards
        */
        rc_assignments: Vec<Expression>,
    },
}

impl fmt::Debug for RcStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RcStatement::Block(stmts) => {
                writeln!(f, "RcStatement [")?;
                for stmt in stmts {
                    writeln!(f, "\t{:?}", stmt)?;
                }
                write!(f, "]")
            }

            RcStatement::Statement { body, rc_bindings, rc_assignments, bound_name } => {
                writeln!(f, "RcStatement {{")?;
                writeln!(f, "\tbody: `{}`", body)?;

                if let Some(bound_name) = bound_name {
                    writeln!(f, "\tbound name: `{}`", bound_name.name)?;
                }

                write!(f, "\trc bindings: [")?;
                if !rc_bindings.is_empty() {
                    writeln!(f)?;
                    for (i, binding) in rc_bindings.iter().enumerate() {
                        writeln!(f, "\t\t{} = {}", i, binding)?;
                    }
                    writeln!(f, "\t]")?;
                } else {
                    write!(f, "]")?;
                }

                write!(f, "\treassigned rc values: [")?;
                if !rc_assignments.is_empty() {
                    writeln!(f)?;
                    for val_name in rc_assignments {
                        writeln!(f, "\t\t`{}`", val_name)?;
                    }
                    writeln!(f, "\t]")?;
                } else {
                    write!(f, "]")?;
                }

                write!(f, "}}")
            }
        }
    }
}

impl RcStatement {
    fn unwrap_value(self,
                    rc_bindings: &mut Vec<RcTemporary>,
                    rc_assignments: &mut Vec<Expression>)
                    -> Expression {
        match self {
            RcStatement::Statement {
                body,
                rc_bindings: val_rc_bindings,
                rc_assignments: val_rc_assignments,
                bound_name: None
            } => {
                rc_bindings.extend(val_rc_bindings);
                rc_assignments.extend(val_rc_assignments);

                *body
            }

            unexpected => panic!(
                "expected value to be a value expression with no name binding, got {:?}",
                unexpected
            ),
        }
    }
}

fn hoist_rc_value(bindings: &mut Vec<RcTemporary>,
                  temporary: RcTemporary,
                  context: &SemanticContext,
                  id_offset: usize)
                  -> Expression {
    let name = (bindings.len() + id_offset).to_string();

    /* the temp binding needs to be added to scope in case anything typechecks again after
        this point*/
    let binding_scope = {
        let decl_type = temporary.base_value.expr_type().unwrap().unwrap();

        context.scope().clone()
            .with_binding(&name, decl_type, BindingKind::Internal)
    };

    let binding_context = SemanticContext::new(
        context.token().clone(),
        binding_scope,
        None,
    );

    /* construct a new expression referring to the temp binding instead of the original
           expression by its name */
    let binding_expr = Expression::identifier(
        Identifier::from(&name),
        binding_context,
    );

    /* store the original expression value for later when we write out the temp bindings */
    bindings.push(temporary);

    binding_expr
}

fn extract_rc_subexpr(expr: Expression,
                      rc_bindings: &mut Vec<RcTemporary>,
                      rc_assignments: &mut Vec<Expression>,
                      id_offset: usize)
                      -> Expression {
    let rc_expr = extract_stmt_rc_bindings(expr, id_offset + rc_bindings.len());

    rc_expr.unwrap_value(rc_bindings, rc_assignments)
}

pub fn extract_block_rc_statements(block: Block) -> Vec<RcStatement> {
    block.statements.into_iter()
        .map(|block_stmt| extract_stmt_rc_bindings(block_stmt, 0))
        .collect()
}

pub fn extract_stmt_rc_bindings(stmt: Expression, id_offset: usize) -> RcStatement {
    /* for blocks, do this recursively for all statements in the block and return
    that instead */
    if stmt.is_block() {
        let block = stmt.unwrap_block();
        return RcStatement::Block(extract_block_rc_statements(block));
    }

    /* it's a single statement not a block, let's figure out if it has any rc bindings */
    let mut rc_bindings = Vec::new();
    let mut rc_assignments = Vec::new();

    let context = stmt.context.clone();

    /* bind the results of any function calls which return RC objects to temporary bindings so we
     can release them later. nothing but function calls returning RC objects can leak an RC,
     because local vars and func args are already managed elsewhere */
    let (bound_name, body) = match &stmt.value {
        ExpressionValue::FunctionCall(func_call) => {
            // extract rc bindings from arg expressions
            let mut args: Vec<_> = func_call.args().iter()
                .cloned()
                .map(|arg| extract_rc_subexpr(arg.clone(),
                                              &mut rc_bindings,
                                              &mut rc_assignments,
                                              id_offset,
                ))
                .collect();

            /* if the returned type contains rc values, we need to add them to the
             current statement's temporaries, and the function call itself needs to be
             hoisted */
            let return_type_rcvals = func_call.signature().return_type.as_ref()
                .map(|return_type| rc_subvalues(return_type, context.scope(), None))
                .unwrap_or_else(|| vec![]);

            let call_expr = match func_call {
                FunctionCall::Function { target, .. } => {
                    let target = extract_rc_subexpr(
                        *target.clone(),
                        &mut rc_bindings, &mut rc_assignments,
                        id_offset,
                    );

                    Expression::function_call(target, args)
                }

                FunctionCall::Method { interface_id, func_name, for_type, .. } => {
                    Expression::method_call(
                        interface_id.clone(),
                        func_name.clone(),
                        for_type.clone(),
                        args,
                        context.clone(),
                    )
                }
            };

            /* hoist the function itself if it returns rc vals */
            let rc_expr = if return_type_rcvals.is_empty() {
                // don't modify the expression
                call_expr
            } else {
                let temporary = RcTemporary {
                    base_value: call_expr,
                    rc_subvalues: return_type_rcvals,
                };
                hoist_rc_value(&mut rc_bindings, temporary, &context, id_offset)
            };

            (None, rc_expr)
        }

        ExpressionValue::ObjectConstructor(ctor) => {
            let rc_subvals = rc_subvalues(
                ctor.object_type.as_ref().unwrap(),
                stmt.scope(),
                None,
            );

            let ctor_expr = Expression::object_constructor(ctor.clone(), context.clone());

            let rc_expr = if rc_subvals.is_empty() {
                ctor_expr
            } else {
                let temporary = RcTemporary {
                    base_value: ctor_expr,
                    rc_subvalues: rc_subvals,
                };

                hoist_rc_value(&mut rc_bindings, temporary, &context, id_offset)
            };

            (None, rc_expr)
        }

        ExpressionValue::LetBinding(binding) => {
            let bound_name = BoundName {
                bound_type: binding.value_type().unwrap(),
                name: binding.name.clone(),
            };

            let value_expr = extract_stmt_rc_bindings(*binding.value.clone(), rc_bindings.len())
                .unwrap_value(&mut rc_bindings, &mut rc_assignments);

            (Some(bound_name), value_expr)
        }

        ExpressionValue::BinaryOperator { lhs, op, rhs } => {
            let lhs = extract_rc_subexpr(
                *lhs.clone(),
                &mut rc_bindings,
                &mut rc_assignments,
                id_offset
            );

            let rhs = extract_rc_subexpr(
                *rhs.clone(),
                &mut rc_bindings,
                &mut rc_assignments,
                id_offset
            );

            (None, Expression::binary_op(lhs, *op, rhs, context))
        }

        _ => (None, stmt.clone()),
    };

    if let ExpressionValue::BinaryOperator { lhs, op: operators::Assignment, .. } = &body.value {
        /* if assigning a new value to an rc variable, release the old value first */
        let lhs_type = lhs.expr_type().unwrap().unwrap();

        if lhs_type.is_ref_counted() {
            rc_assignments.push(*lhs.clone());
        }
    }

    RcStatement::Statement {
        body: Box::new(body),
        bound_name,
        rc_bindings,
        rc_assignments,
    }
}

/* for a given type, find all the subvalues which require reference
counting:
    * a non-rc scalar value has no rc subvalues
    * a rc scalar value has one rc subvalue (itself)
    * a class reference or dynamic array counts as an rc scalar value
    * a record has one subvalue for every member which is an rc type (recursing into any
    records it contains)
    * an array of non-rc values has no rc subvalues
    * an array of rc values has one rc subvalue for each element
*/
pub fn rc_subvalues(expr_type: &Type,
                scope: &Scope,
                parent: Option<RcSubValuePath>)
                -> Vec<RcSubValuePath> {
    match expr_type {
        Type::Array(array_type) if array_type.element.is_ref_counted() => {
            array_type.first_dim.iter()
                .flat_map(|index| {
                    let element_path = RcSubValuePath::ArrayElement {
                        parent: parent.clone().map(Box::new),
                        index,
                    };
                    rc_subvalues(array_type.element.as_ref(), scope, Some(element_path))
                })
                .collect()
        }

        Type::Record(record_id) => {
            let (_, record_decl) = scope.get_record_specialized(&record_id)
                .unwrap();

            let member_exprs = record_decl.all_members()
                .flat_map(|member| {
                    let member_path = RcSubValuePath::Member {
                        parent: parent.clone().map(Box::new),
                        name: member.name.clone(),
                    };

                    rc_subvalues(&member.decl_type, scope, Some(member_path))
                })
                .collect();

            member_exprs
        }

        scalar if scalar.is_ref_counted()
        => vec![RcSubValuePath::This { parent: parent.map(Box::new) }],

        _ => vec![]
    }
}