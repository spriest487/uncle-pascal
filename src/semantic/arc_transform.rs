use std::fmt;

use types::Type;
use semantic::{
    Scope,
    Expression,
    BindingKind,
    SemanticContext,
    CollectionConstructor,
    Block,
    ObjectConstructor,
    ObjectConstructorMember,
};
use node::{
    self,
    Context,
    ExpressionValue,
    FunctionCall,
    Identifier,
};
use operators;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum RefStrength {
    Strong,
    Weak,
}

pub struct BoundName {
    pub name: String,
    pub bound_type: Type,
}

#[derive(Debug)]
pub struct ArcTemporary {
    /* the main value that holds the rc subvalues, not necessarily an rc value itself.
    e.g. a record, an array */
    pub base_value: Expression,

    /* all subvalues of the base_value which need reference counting. if the base value is
     itself an rc value, this may be a single item with the same expression as base_value */
    pub rc_subvalues: Vec<ArcSubValuePath>,
}

impl fmt::Display for ArcTemporary {
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
pub enum ArcSubValuePath {
    This {
        parent: Option<Box<ArcSubValuePath>>,
        strength: RefStrength,
    },

    Member {
        parent: Option<Box<ArcSubValuePath>>,
        name: String,
    },

    ArrayElement {
        parent: Option<Box<ArcSubValuePath>>,
        index: usize,
    },
}

impl fmt::Display for ArcSubValuePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArcSubValuePath::This { parent, strength } => {
                match parent {
                    Some(parent) => write!(f, "{}", parent)?,
                    None => write!(f, "<this>")?,
                }

                match *strength {
                    RefStrength::Weak => write!(f, " (weak)"),
                    RefStrength::Strong => Ok(())
                }
            }

            ArcSubValuePath::Member { parent: None, name } =>
                write!(f, "<this>.{}", name),
            ArcSubValuePath::Member { parent: Some(parent), name } =>
                write!(f, "{}.{}", parent, name),

            ArcSubValuePath::ArrayElement { parent: None, index } =>
                write!(f, "<this>[{}]", index),
            ArcSubValuePath::ArrayElement { parent: Some(parent), index } =>
                write!(f, "{}[{}]", parent, index),
        }
    }
}

impl ArcSubValuePath {
    pub fn strength(&self) -> RefStrength {
        match self {
            ArcSubValuePath::This { strength, .. } => *strength,
            _ => RefStrength::Strong,
        }
    }
}

pub enum ArcStatement {
    Block(Vec<ArcStatement>),

    Statement {
        bound_name: Option<BoundName>,
        body: Box<Expression>,

        /*
            expressions which create new reference-counted values that need to be hoisted
            into temporary vars so they can be released after the statement
        */
        rc_bindings: Vec<ArcTemporary>,

        /*
            expressions referring to rc-typed lvalues that are assigned to during this
            statement, which will need to be released before assignment and retained
            afterwards
        */
        rc_assignments: Vec<ArcValue>,
    },
}

impl fmt::Debug for ArcStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ArcStatement::Block(stmts) => {
                writeln!(f, "RcStatement [")?;
                for stmt in stmts {
                    writeln!(f, "\t{:?}", stmt)?;
                }
                write!(f, "]")
            }

            ArcStatement::Statement { body, rc_bindings, rc_assignments, bound_name } => {
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
                    for arc_val in rc_assignments {
                        writeln!(f, "\t\t`{}`", arc_val.expr)?;
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

pub struct ArcValue {
    pub expr: Expression,
    pub path: ArcSubValuePath,
}

struct ArcContext {
    /* if this context is a child of another context, we need to offset the IDs we
    give to temporaries so they match the placeholder names at extraction sites when
    this context's bindings are merged into the parent's */
    id_offset: usize,

    temp_bindings: Vec<ArcTemporary>,
    assignments: Vec<ArcValue>,
}

impl ArcContext {
    fn new(id_offset: usize) -> Self {
        ArcContext {
            id_offset,

            temp_bindings: Vec::new(),
            assignments: Vec::new(),
        }
    }

    fn next_id(&self) -> usize {
        self.temp_bindings.len() + self.id_offset
    }

    fn hoist_rc_value(&mut self, temporary: ArcTemporary, context: &SemanticContext) -> Expression {
        let name = self.next_id().to_string();

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
        self.temp_bindings.push(temporary);

        binding_expr
    }

    fn extract_rc_subexpr(&mut self, expr: Expression) -> Expression {
        match extract_stmt_rc_bindings(expr, self.next_id()) {
            ArcStatement::Statement {
                body,
                rc_bindings: val_rc_bindings,
                rc_assignments: val_rc_assignments,
                bound_name: None
            } => {
                self.temp_bindings.extend(val_rc_bindings);
                self.assignments.extend(val_rc_assignments);

                *body
            }

            unexpected => panic!(
                "expected value to be a value expression with no name binding, got {:?}",
                unexpected
            ),
        }
    }

    fn assigned_val(&mut self, expr: Expression, path: ArcSubValuePath) {
        self.assignments.push(ArcValue {
            expr,
            path,
        })
    }
}

pub fn extract_block_rc_statements(block: Block) -> Vec<ArcStatement> {
    block.statements.into_iter()
        .map(|block_stmt| extract_stmt_rc_bindings(block_stmt, 0))
        .collect()
}

pub fn extract_stmt_rc_bindings(stmt: Expression, id_offset: usize) -> ArcStatement {
    /* for blocks, do this recursively for all statements in the block and return
    that instead */
    if stmt.is_block() {
        let block = stmt.unwrap_block();
        return ArcStatement::Block(extract_block_rc_statements(block));
    }

    /* it's a single statement not a block, let's figure out if it has any rc bindings */
    let mut arc_ctx = ArcContext::new(id_offset);

    let context = stmt.context.clone();

    /* bind the results of any function calls which return RC objects to temporary bindings so we
     can release them later. nothing but function calls returning RC objects can leak an RC,
     because local vars and func args are already managed elsewhere */
    let (bound_name, body) = match stmt.value {
        ExpressionValue::FunctionCall(func_call) => {
            // extract rc bindings from arg expressions
            let mut args: Vec<_> = func_call.args().iter()
                .cloned()
                .map(|arg| arc_ctx.extract_rc_subexpr(arg))
                .collect();

            /* if the returned type contains rc values, we need to add them to the
             current statement's temporaries, and the function call itself needs to be
             hoisted */
            let return_type_rcvals = func_call.signature().return_type.as_ref()
                .map(|return_type| rc_subvalues(return_type, context.scope(), None))
                .unwrap_or_else(|| vec![]);

            let call_expr = match func_call {
                FunctionCall::Function { target, .. } => {
                    let target = arc_ctx.extract_rc_subexpr(*target);
                    Expression::function_call(target, args)
                }

                FunctionCall::Extension { self_expr, func_name, for_type, .. } => {
                    let self_expr = arc_ctx.extract_rc_subexpr(*self_expr);
                    Expression::extension_call(self_expr, func_name, for_type, args)
                }

                FunctionCall::Method { interface_id, func_name, for_type, .. } => {
                    Expression::method_call(interface_id, func_name, for_type, args, context.clone())
                }
            };

            /* hoist the function itself if it returns rc vals */
            let rc_expr = if return_type_rcvals.is_empty() {
                // don't modify the expression
                call_expr
            } else {
                let temporary = ArcTemporary {
                    base_value: call_expr,
                    rc_subvalues: return_type_rcvals,
                };
                arc_ctx.hoist_rc_value(temporary, &context)
            };

            (None, rc_expr)
        }

        ExpressionValue::ObjectConstructor(ctor) => {
            /* hoist rc members constructed inline in the ctor expression */
            let members = ctor.members.iter()
                .cloned()
                .map(|member| {
                    let value = arc_ctx.extract_rc_subexpr(member.value);
                    ObjectConstructorMember {
                        name: member.name,
                        value,
                    }
                })
                .collect();

            let object_type = ctor.object_type;
            let ctor = ObjectConstructor {
                members,
                object_type: object_type.clone(),
            };

            let ctor_expr = Expression::object_ctor(ctor, context.clone());

            /* then hoist the whole expression if it's constructing an rc type */
            let result = if object_type.as_ref().unwrap().is_ref_counted() {
                let temporary = ArcTemporary {
                    base_value: ctor_expr,
                    rc_subvalues: vec![ArcSubValuePath::This {
                        parent: None,
                        strength: RefStrength::Strong,
                    }],
                };

                arc_ctx.hoist_rc_value(temporary, &context)
            } else {
                ctor_expr
            };

            (None, result)
        }

        ExpressionValue::CollectionConstructor(ctor) => {
            let members = ctor.members.iter()
                .map(|member| match member {
                    node::CollectionMember::Single(val) => {
                        node::CollectionMember::Single(arc_ctx.extract_rc_subexpr(val.clone()))
                    }

                    node::CollectionMember::Range { from, to } => {
                        node::CollectionMember::Range {
                            from: arc_ctx.extract_rc_subexpr(from.clone()),
                            to: arc_ctx.extract_rc_subexpr(to.clone()),
                        }
                    }
                })
                .collect();

            let ctor = CollectionConstructor {
                members,
                element_type: ctor.element_type.clone(),
            };

            (None, Expression::collection_ctor(ctor, context))
        }

        ExpressionValue::LetBinding(binding) => {
            let bound_name = BoundName {
                bound_type: binding.value_type().unwrap(),
                name: binding.name.clone(),
            };

            let value_expr = arc_ctx.extract_rc_subexpr(*binding.value.clone());

            (Some(bound_name), value_expr)
        }

        ExpressionValue::BinaryOperator { lhs, op, rhs } => {
            let lhs = arc_ctx.extract_rc_subexpr(*lhs.clone());
            let rhs = arc_ctx.extract_rc_subexpr(*rhs.clone());

            /* there are no binary operators that result in a RC type, so we don't
             have to check if the value needs hoisting (string concatenation is sugar
             for a function call so we can ignore it) */
            (None, Expression::binary_op(lhs, op, rhs, context))
        }

        ExpressionValue::PrefixOperator { op, rhs } => {
            let rhs = arc_ctx.extract_rc_subexpr(*rhs.clone());
            (None, Expression::prefix_op(op, rhs, context))
        }

        _ => {
            (None, stmt.clone())
        }
    };

    if let ExpressionValue::BinaryOperator { lhs, op: operators::Assignment, .. } = &body.value {
        /* if assigning a new value to something containing rc values,
        track which values got reassigned so we can release them (before assigning)
        and retain them (after assigning) */
        let lhs_type = lhs.expr_type().unwrap().unwrap();

        for reassigned_val in rc_subvalues(&lhs_type, body.scope(), None) {
            arc_ctx.assigned_val(*lhs.clone(), reassigned_val);
        }
    }

    ArcStatement::Statement {
        body: Box::new(body),
        bound_name,
        rc_bindings: arc_ctx.temp_bindings,
        rc_assignments: arc_ctx.assignments,
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
                    parent: Option<ArcSubValuePath>)
                    -> Vec<ArcSubValuePath> {
    match expr_type {
        Type::Array(array_type) if array_type.element.is_ref_counted() => {
            array_type.first_dim.offsets()
                .flat_map(|index| {
                    let element_path = ArcSubValuePath::ArrayElement {
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
                    let member_path = ArcSubValuePath::Member {
                        parent: parent.clone().map(Box::new),
                        name: member.name.clone(),
                    };

                    let mut vals = rc_subvalues(&member.decl_type, scope, Some(member_path));

                    if member.decl_type.is_weak() {
                        assert_eq!(1, vals.len(), "weak members cannot have multiple rc subvalues (should be a single RC pointer, got: {:#?})", vals);
                        match &mut vals[0] {
                            ArcSubValuePath::This { strength, .. } => {
                                *strength = RefStrength::Weak;
                            }
                            _ => unreachable!(),
                        }
                    }
                    vals
                });

            member_exprs.collect()
        }

        scalar if scalar.is_ref_counted()
        => vec![
            ArcSubValuePath::This {
                parent: parent.map(Box::new),
                strength: if scalar.is_weak() {
                    RefStrength::Weak
                } else {
                    RefStrength::Strong
                }
            }
        ],

        _ => vec![]
    }
}