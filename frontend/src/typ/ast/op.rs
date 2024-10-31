#[cfg(test)]
mod test;

mod variant_case;

use crate::ast;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Operator;
use crate::ast::INTERFACE_METHOD_ACCESS;
use crate::typ::ast::member_annotation;
use crate::typ::ast::overload_to_no_args_call;
use crate::typ::ast::try_resolve_overload;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_object_ctor;
use crate::typ::ast::Call;
use crate::typ::ast::Expr;
use crate::typ::ast::MethodCall;
use crate::typ::ast::OverloadCandidate;
use crate::typ::ast::check_overload_visibility;
use crate::typ::ast::const_eval_integer;
use crate::typ::ast::implicit_conversion;
use crate::typ::ast::MethodDecl;
use crate::typ::builtin_displayable_name;
use crate::typ::string_type;
use crate::typ::Context;
use crate::typ::FunctionTyped;
use crate::typ::InstanceMember;
use crate::typ::MethodTyped;
use crate::typ::NameContainer;
use crate::typ::NameError;
use crate::typ::OverloadTyped;
use crate::typ::Primitive;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeMember;
use crate::typ::TypeResult;
use crate::typ::Typed;
use crate::typ::TypedValue;
use crate::typ::UfcsTyped;
use crate::typ::ValueKind;
use crate::typ::DISPLAYABLE_TOSTRING_METHOD;
use crate::typ::STRING_CONCAT_FUNC_NAME;
use crate::typ::SYSTEM_UNIT_NAME;
use crate::IntConstant;
use common::span::Span;
use common::span::Spanned;
use std::rc::Rc;
use variant_case::try_expr_into_noargs_variant_ctor;
use variant_case::typecheck_variant_case;

pub type BinOp = ast::BinOp<Typed>;

fn invalid_bin_op(bin_op: &ast::BinOp<Span>, lhs: &Expr, rhs: &Expr) -> TypeError {
    TypeError::InvalidBinOp {
        lhs: lhs.annotation().ty().into_owned(),
        rhs: rhs.annotation().ty().into_owned(),
        op: bin_op.op,
        span: bin_op.annotation.span().clone(),
    }
}

pub fn typecheck_bin_op(
    bin_op: &ast::BinOp<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    let span = bin_op.annotation.clone();

    match &bin_op.op {
        Operator::Period => typecheck_member_of(&bin_op.lhs, &bin_op.rhs, span, expect_ty, ctx),

        Operator::Index => typecheck_indexer(&bin_op.lhs, &bin_op.rhs, &span, ctx),

        Operator::And | Operator::Or => {
            let bin_op = typecheck_logical_op(bin_op, span, ctx)?;
            Ok(Expr::from(bin_op))
        },

        Operator::Equals | Operator::NotEquals => {
            let bin_op = typecheck_equality(bin_op, span, ctx)?;
            Ok(Expr::from(bin_op))
        },

        Operator::Gt | Operator::Gte | Operator::Lt | Operator::Lte => {
            let bin_op = typecheck_comparison(bin_op, span, ctx)?;
            Ok(Expr::from(bin_op))
        },

        Operator::BitAnd | Operator::BitOr | Operator::Shr | Operator::Shl | Operator::Caret => {
            let bin_op = typecheck_bitwise_op(bin_op, expect_ty, ctx)?;
            Ok(Expr::from(bin_op))
        },
        
        Operator::Assignment => {
            unreachable!("typecheck_bin_op: assignment should never be checked as expression, only a statement")
        }

        _ => {
            let mut lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
            let mut rhs = typecheck_expr(&bin_op.rhs, &lhs.annotation().ty(), ctx)?;

            // string concat sugar isn't available if the String class isn't loaded
            if let Ok(string_ty) = string_type(ctx) {
                let mut lhs_string = *lhs.annotation().ty() == string_ty;
                let mut rhs_string = *rhs.annotation().ty() == string_ty;

                if lhs_string && !rhs_string {
                    if let Some(rhs_to_string) = desugar_displayable_to_string(&rhs, &span, ctx) {
                        rhs = rhs_to_string;
                        rhs_string = true;
                    }
                } else if !lhs_string && lhs_string {
                    if let Some(lhs_to_string) = desugar_displayable_to_string(&lhs, &span, ctx) {
                        lhs = lhs_to_string;
                        lhs_string = true;
                    }
                }

                let is_string_concat = bin_op.op == Operator::Add && lhs_string && rhs_string;

                if is_string_concat {
                    return desugar_string_concat(lhs, rhs, &string_ty, ctx);
                }
            }

            let valid_math = lhs
                .annotation()
                .ty()
                .valid_math_op(bin_op.op, &rhs.annotation().ty());

            if !valid_math {
                return Err(invalid_bin_op(&bin_op, &lhs, &rhs));
            }

            // check valid ops etc, result type etc
            let result_ty = lhs.annotation().ty().into_owned();

            let annotation = match result_ty {
                Type::Nothing => Typed::Untyped(span.clone()),
                ty => TypedValue {
                    ty,
                    value_kind: ValueKind::Temporary,
                    span,
                    decl: None,
                }
                .into(),
            };

            Ok(Expr::from(ast::BinOp {
                lhs,
                op: bin_op.op,
                rhs,
                annotation,
            }))
        },
    }
}

fn typecheck_logical_op(
    bin_op: &ast::BinOp<Span>,
    span: Span,
    ctx: &mut Context,
) -> TypeResult<BinOp> {
    let bool_ty = Type::Primitive(Primitive::Boolean);

    let lhs = typecheck_expr(&bin_op.lhs, &bool_ty, ctx)?;
    lhs.annotation().expect_value(&bool_ty)?;

    let rhs = typecheck_expr(&bin_op.rhs, &bool_ty, ctx)?;
    rhs.annotation().expect_value(&bool_ty)?;

    let annotation = TypedValue {
        ty: bool_ty,
        value_kind: ValueKind::Temporary,
        span,
        decl: None,
    }
    .into();

    Ok(BinOp {
        lhs,
        rhs,
        op: bin_op.op,
        annotation,
    })
}

fn typecheck_equality(
    bin_op: &ast::BinOp<Span>,
    span: Span,
    ctx: &mut Context,
) -> TypeResult<BinOp> {
    let lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
    lhs.annotation().expect_any_value()?;

    let rhs = typecheck_expr(&bin_op.rhs, &lhs.annotation().ty(), ctx)?;
    rhs.annotation().expect_any_value()?;

    if !lhs.annotation().ty().equatable(&rhs.annotation().ty()) {
        return Err(invalid_bin_op(bin_op, &lhs, &rhs));
    }

    let annotation = TypedValue {
        ty: Type::Primitive(Primitive::Boolean),
        value_kind: ValueKind::Temporary,
        span,
        decl: None,
    }
    .into();

    Ok(BinOp {
        lhs,
        rhs,
        op: bin_op.op,
        annotation,
    })
}

fn typecheck_comparison(
    bin_op: &ast::BinOp<Span>,
    span: Span,
    ctx: &mut Context,
) -> TypeResult<BinOp> {
    let lhs = typecheck_expr(&bin_op.lhs, &Type::Nothing, ctx)?;
    lhs.annotation().expect_any_value()?;
    
    let rhs = typecheck_expr(&bin_op.rhs, &lhs.annotation().ty(), ctx)?;
    rhs.annotation().expect_value(&lhs.annotation().ty())?;

    if !lhs.annotation().ty().self_orderable() {
        return Err(invalid_bin_op(&bin_op, &lhs, &rhs));
    }

    let annotation = TypedValue {
        ty: Type::Primitive(Primitive::Boolean),
        value_kind: ValueKind::Temporary,
        span,
        decl: None,
    }
    .into();

    Ok(BinOp {
        lhs,
        rhs,
        op: bin_op.op,
        annotation,
    })
}

fn typecheck_bitwise_op(
    bin_op: &ast::BinOp<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<BinOp> {
    // if there's no expected type for a bitwise op, expect UInt32
    let expect_ty = match expect_ty {
        Type::Nothing => &Type::Primitive(Primitive::UInt32),
        _ => expect_ty, 
    };

    let lhs = typecheck_expr(&bin_op.lhs, expect_ty, ctx)?;
    lhs.annotation().expect_any_value()?;

    // for bitwise operations to make sense the lhs and rhs must be the exact same type so insert a
    // conversion here as necessary
    let rhs = implicit_conversion(
        typecheck_expr(&bin_op.rhs, &lhs.annotation().ty(), ctx)?,
        &lhs.annotation().ty(),
        ctx,
    )?;
    rhs.annotation().expect_any_value()?;

    let lhs_ty = lhs.annotation().ty();
    let rhs_ty = rhs.annotation().ty();

    if !lhs_ty.valid_math_op(bin_op.op, &rhs_ty) {
        return Err(invalid_bin_op(&bin_op, &lhs, &rhs));
    }

    Ok(BinOp {
        annotation: TypedValue {
            ty: lhs.annotation().ty().into_owned(),
            span: bin_op.span().clone(),
            value_kind: ValueKind::Temporary,
            decl: None,
        }
        .into(),
        lhs,
        rhs,
        op: bin_op.op,
    })
}

// turn value `x` that implements System.Displayable into a call to `System.Displayable.ToString(x)`
// when `x` is evaluated in a context where a string is expected e.g. string concat
fn desugar_displayable_to_string(expr: &Expr, span: &Span, ctx: &Context) -> Option<Expr> {
    let src_ty = expr.annotation().ty();

    let to_string_ident = Ident::new(DISPLAYABLE_TOSTRING_METHOD, span.clone());

    let displayable_path = builtin_displayable_name().full_path;

    let displayable_ty = Type::interface(builtin_displayable_name().full_path);
    let is_impl = ctx
        .is_implementation(src_ty.as_ref(), &displayable_ty)
        .ok()
        .unwrap_or(false);
    
    if !is_impl {
        return None;
    }

    let displayable_iface = match ctx.find_iface_def(&displayable_path) {
        Ok(iface_def) => iface_def,
        Err(..) => return None,
    };

    let (to_string_index, to_string_method) = match displayable_iface
        .methods
        .iter()
        .position(|m| *m.decl.ident() == to_string_ident) 
    {
        None => return None,
        Some(index) => {
            let method_decl = MethodDecl {
                func_decl: displayable_iface.methods[index].decl.clone(),
                access: INTERFACE_METHOD_ACCESS,
            };

            (index, method_decl)
        }
    };

    let to_string_sig = Rc::new(to_string_method.func_decl.sig());

    // make a call
    let displayable_call = Call::Method(MethodCall {
        iface_type: displayable_ty.clone(),
        self_type: src_ty.into_owned(),
        iface_method_index: to_string_index,
        ident: to_string_ident.clone(),
        args: vec![expr.clone()],
        type_args: None,
        args_span: span.clone(),
        func_type: Type::Function(to_string_sig.clone()),
        annotation: MethodTyped::new(
            displayable_ty,
            to_string_index,
            &to_string_method,
            span.clone()
        ).into(),
    });

    Some(Expr::from(displayable_call))
}

// desugar a binary + operation on two strings into a call to System.StringConcat
fn desugar_string_concat(
    lhs: Expr,
    rhs: Expr,
    string_ty: &Type,
    ctx: &Context,
) -> TypeResult<Expr> {
    let span = lhs.annotation().span().to(rhs.annotation().span());
    let annotation = TypedValue {
        ty: string_ty.clone(),
        span: span.clone(),
        value_kind: ValueKind::Temporary,
        decl: None,
    }
    .into();

    // if LHS and RHS are both string literals, we can concat them ahead of time
    match (&lhs, &rhs) {
        (
            ast::Expr::Literal(ast::Literal::String(a), _),
            ast::Expr::Literal(ast::Literal::String(b), _),
        ) => Ok(ast::Expr::Literal(
            ast::Literal::String(Rc::new((**a).clone() + b.as_str())),
            annotation,
        )),

        _ => {
            let system_path = IdentPath::from(Ident::new(SYSTEM_UNIT_NAME, span.clone()));
            let concat_path = system_path.child(Ident::new(STRING_CONCAT_FUNC_NAME, span.clone()));
            let (concat_path, concat_decls) = ctx
                .find_function(&concat_path)
                .map_err(|err| TypeError::from_name_err(err, span.clone()))?;
            
            assert_eq!(1, concat_decls.len());

            let concat_sym = Symbol::from(concat_path.clone());

            let concat_typed = FunctionTyped {
                name: concat_sym.clone(),
                span: span.clone(),
                sig: concat_decls[0].sig().clone(),
                visibility: concat_decls[0].visiblity(),
            };

            let concat_func = ast::Expr::Ident(concat_path.last().clone(), concat_typed.into());

            let concat_call = ast::Call::Function(ast::FunctionCall {
                annotation: annotation.clone(),
                args: vec![lhs, rhs],
                type_args: None,
                target: concat_func,
                args_span: span.clone(),
            });

            Ok(ast::Expr::from(concat_call))
        },
    }
}

fn typecheck_member_of(
    lhs: &ast::Expr<Span>,
    rhs: &ast::Expr<Span>,
    span: Span,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    let lhs = typecheck_expr(lhs, &Type::Nothing, ctx)?;

    match rhs {
        // x.y
        ast::Expr::Ident(member_ident, _) => {
            let member_ident = member_ident.clone();

            let annotation = match lhs.annotation() {
                // x is the name of a variant type - we are constructing that variant
                Typed::Type(Type::Variant(variant_name), ..) => {
                    let case = typecheck_variant_case(variant_name, &member_ident, &span, ctx)?;
                    if let Some(ctor) = try_expr_into_noargs_variant_ctor(&case, expect_ty, &span, ctx)? {
                        return Ok(ctor);
                    }
                    
                    Typed::from(case)
                },

                // x is a non-variant typename - we are accessing a member of that type
                // e.g. calling an interface method by its type-qualified name
                Typed::Type(ty, _) => {
                    typecheck_type_member(ty, &member_ident, expect_ty, span.clone(), ctx)?
                },

                // x is a value - we are accessing a member of that value
                Typed::TypedValue(base_val) => {
                    typecheck_member_value(
                        &lhs,
                        &base_val.ty,
                        base_val.value_kind,
                        &member_ident,
                        span.clone(),
                        ctx,
                    )?
                },

                Typed::Namespace(path, _) => {
                    let mut full_path = path.clone();
                    full_path.push(member_ident.clone());

                    match ctx.find_path(&full_path) {
                        Some(member) => member_annotation(&member, span.clone(), ctx),
                        None => {
                            let err = NameError::MemberNotFound {
                                member: member_ident,
                                base: NameContainer::for_annotated(lhs.annotation()),
                            };

                            return Err(TypeError::from_name_err(err, span));
                        },
                    }
                },

                _ => {
                    let err = NameError::MemberNotFound {
                        member: member_ident,
                        base: NameContainer::for_annotated(lhs.annotation()),
                    };

                    return Err(TypeError::from_name_err(err, span));
                },
            };

            let rhs_value = Typed::Untyped(member_ident.span.clone());
            let rhs = Expr::Ident(member_ident, rhs_value);

            let member_op = BinOp {
                lhs,
                op: Operator::Period,
                rhs,
                annotation,
            };
            
            let lhs_ty = member_op.lhs.annotation().ty();

            // member operations that reference function values with no params automatically turn into a
            // no-args call to that function, except in contexts where we expect a matching function value
            match &member_op.annotation {
                Typed::Function(func_val) => {
                    let no_args_call = if !func_val.should_call_noargs_in_expr(expect_ty) {
                        None 
                    } else {
                        let overload_candidate = &[
                            OverloadCandidate::Function {
                                sig: func_val.sig.clone(),
                                decl_name: func_val.name.clone(),
                                visibility: func_val.visibility,
                            }
                        ];

                        op_to_no_args_call(overload_candidate, &member_op, &span, ctx)?
                    };
                    
                    match no_args_call {
                        Some(call_expr) => Ok(call_expr),
                        None => Ok(Expr::from(member_op)),
                    }
                }

                Typed::UfcsFunction(ufcs_val) => {
                    let no_args_call = if !ufcs_val.should_call_noargs_in_expr(expect_ty) {
                        None
                    } else {
                        let overload_candidate = &[
                            OverloadCandidate::Function {
                                sig: ufcs_val.sig.clone(),
                                decl_name: ufcs_val.function_name.clone(),
                                visibility: ufcs_val.visibility,
                            }
                        ];

                        op_to_no_args_call(overload_candidate, &member_op, &span, ctx)?
                    };

                    match no_args_call {
                        Some(call_expr) => Ok(call_expr),
                        None => Ok(Expr::from(member_op)),
                    }
                }

                Typed::Method(method) => {
                    let no_args_call = if !method.should_call_noargs_in_expr(expect_ty, lhs_ty.as_ref()) {
                        None
                    } else {
                        let overload_candidate = &[
                            OverloadCandidate::Method {
                                iface_ty: method.self_ty.clone(),
                                self_ty: method.self_ty.clone(),
                                index: method.index,

                                ident: method.name.clone(),
                                access: method.access,
                                sig: method.decl_sig.clone(),
                            }
                        ];
                        
                        op_to_no_args_call(overload_candidate, &member_op, &span, ctx)?
                    };

                    match no_args_call {
                        Some(call_expr) => Ok(call_expr),
                        None => Ok(Expr::from(member_op)),
                    }
                }

                Typed::Overload(overloaded) => {
                    // if any of the possible overloads can be called with no args, assume this expression
                    // is a no-args call - if arguments are applied later we'll re-resolve the overload
                    let self_arg = overloaded.self_arg
                        .as_ref()
                        .map(|arg_box| arg_box.as_ref());
                    
                    // eprintln!("resolving overload for {member_op} @ {span}, self arg={}", self_arg.is_some());

                    match try_resolve_overload(&overloaded.candidates, &[], None, self_arg, &span, ctx) {
                        Some(overload) => {
                            check_overload_visibility(&overload, &overloaded.candidates, &span, ctx)?;
                            
                            let call = overload_to_no_args_call(
                                &overloaded.candidates,
                                overload,
                                Expr::from(member_op.clone()),
                                self_arg.cloned(),
                                &span
                            );
                            Ok(call)
                        }
                        None => Ok(Expr::from(member_op)),
                    }
                }

                _ => Ok(Expr::from(member_op)),
            }
        },

        // a.B(x: x)
        ast::Expr::ObjectCtor(ctor) => {
            match lhs.annotation() {
                // a must be a namespace qualifier before the constructed object name
                Typed::Namespace(ns_path, ..) => {
                    assert_eq!(
                        1,
                        ctor.ident.as_ref().unwrap().as_slice().len(),
                        "parsed ctor should only have a single ident in its path until this point, but found {}",
                        ctor.ident.as_ref().unwrap()
                    );

                    let qualified_ident = ns_path
                        .clone()
                        .child(ctor.ident.as_ref().unwrap().last().clone());
                    let qualified_ctor = ast::ObjectCtor {
                        ident: Some(qualified_ident),
                        ..(**ctor).clone()
                    };

                    let span = lhs.annotation().span().to(qualified_ctor.annotation.span());

                    let ctor = typecheck_object_ctor(&qualified_ctor, span, expect_ty, ctx)?;
                    Ok(Expr::from(ctor))
                },

                _ => Err(TypeError::InvalidCtorType {
                    ty: lhs.annotation().ty().into_owned(),
                    span,
                }),
            }
        },

        _ => {
            let rhs = typecheck_expr(rhs, &Type::Nothing, ctx)?;

            Err(TypeError::InvalidBinOp {
                lhs: lhs.annotation().ty().into_owned(),
                rhs: rhs.annotation().ty().into_owned(),
                span,
                op: Operator::Period,
            })
        },
    }
}

fn op_to_no_args_call(
    candidates: &[OverloadCandidate],
    target: &BinOp,
    span: &Span,
    ctx: &mut Context
) -> TypeResult<Option<Expr>> {
    // if the LHS is an untyped item (eg the type part of a class method call),
    // don't pass it as a self-arg
    let self_arg = match target.lhs.annotation().ty().as_ref() {
        Type::Nothing => None,
        _ => Some(target.lhs.clone()),
    };

    let overload_result = try_resolve_overload(
        candidates,
        &[],
        None,
        self_arg.as_ref(),
        span,
        ctx
    );
    
    let overload = match overload_result {
        Some(overload) => overload,
        None => return Ok(None),
    };

    check_overload_visibility(&overload, candidates, span, ctx)?;
    
    let target = Expr::from(target.clone());
    let call = overload_to_no_args_call(candidates, overload, target, self_arg, span);
    
    Ok(Some(call))
}

fn typecheck_type_member(
    ty: &Type,
    member_ident: &Ident,
    expect_return_ty: &Type,
    span: Span,
    ctx: &mut Context,
) -> TypeResult<Typed> {
    let type_member = ctx
        .find_type_member(ty, member_ident, expect_return_ty, &span, ctx)?;
    
    let member_access = type_member.access(); 
    if ty.get_current_access(ctx) < member_access {
        return Err(TypeError::TypeMemberInaccessible {
            member: member_ident.clone(),
            ty: ty.clone(),
            access: member_access,
            span,
        });
    }

    let annotation = match type_member {
        TypeMember::Method(candidate) => {
            if candidate.iface_ty.get_current_access(ctx) < candidate.method.access {
                return Err(TypeError::TypeMemberInaccessible {
                    ty: candidate.iface_ty.clone(),
                    member: member_ident.clone(),
                    access: candidate.method.access,
                    span,
                });
            }

            // this is a reference to the method itself, args list to follow presumably
            MethodTyped::new(candidate.iface_ty, candidate.index, &candidate.method, span).into()
        },
        
        TypeMember::MethodGroup(group) => {
            let candidates = group
                .into_iter()
                .map(|method_group_item| {
                    OverloadCandidate::Method { 
                        iface_ty: method_group_item.iface_ty.clone(),
                        self_ty: method_group_item.iface_ty,
                        index: method_group_item.index,

                        ident: member_ident.clone(),
                        sig: Rc::new(method_group_item.method.func_decl.sig()),
                        access: method_group_item.method.access,
                    }
                })
                .collect();

            OverloadTyped::new(candidates, None, span).into()
        }
    };

    Ok(annotation)
}

pub fn typecheck_member_value(
    lhs: &Expr,
    base_ty: &Type,
    value_kind: ValueKind,
    member_ident: &Ident,
    span: Span,
    ctx: &mut Context,
) -> TypeResult<Typed> {
    let member = ctx
        .find_instance_member(&lhs.annotation().ty(), &member_ident)
        .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

    let annotation = match member {
        InstanceMember::Method { iface_ty, self_ty, method, sig } => {
            let (method_index, iface_method) = iface_ty
                .find_method(&method, sig.as_ref(), ctx)
                .ok()
                .flatten()
                .unwrap_or_else(|| {
                    panic!(
                        "find_instance_member should only return methods that exist - no match for {}.{} : {}",
                        iface_ty,
                        method,
                        sig,
                    )
                });

            let method = OverloadTyped::method(
                self_ty,
                iface_ty,
                method_index,
                lhs.clone(),
                iface_method,
                span.clone(),
            );

            Typed::from(method)
        },

        InstanceMember::UFCSCall { func_name, sig, visibility } => {
            // to be resolved later, presumably in a subsequent call
            UfcsTyped {
                function_name: func_name,
                visibility,
                sig,
                span: span.clone(),
                self_arg: Box::new(lhs.clone()),
            }.into()
        },

        InstanceMember::Overloaded { candidates } => {
            OverloadTyped::new(
                candidates,
                Some(Box::new(lhs.clone())),
                span.clone(),
            ).into()
        }

        InstanceMember::Field { ty: member_ty, access: field_access } => {
            if base_ty.get_current_access(ctx) < field_access {
                return Err(TypeError::TypeMemberInaccessible {
                    member: member_ident.clone(),
                    ty: base_ty.clone(),
                    access: field_access,
                    span,
                });
            }
            
            /* class members are always mutable because a mutable class ref is only
            a mutable *reference*. record and variant members are accessed by readonly value */
            let value_kind = match base_ty {
                Type::Class(..) => ValueKind::Mutable,
                _ => value_kind,
            };

            TypedValue {
                ty: member_ty.clone(),
                span: span.clone(),
                value_kind,
                decl: None,
            }
            .into()
        },
    };

    Ok(annotation)
}

pub type UnaryOp = ast::UnaryOp<Typed>;

pub fn typecheck_unary_op(
    unary_op: &ast::UnaryOp<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<UnaryOp> {
    let operand_expect_ty = match unary_op.op {
        Operator::Add | Operator::Sub | Operator::Not => expect_ty.clone(),
        Operator::AddressOf => match expect_ty {
            // value of the operator expr is the pointer to the deref type, so the operand
            // is of the deref type
            Type::Pointer(deref_ty) => (**deref_ty).clone(),
            _ => Type::Nothing,
        },

        Operator::Caret => expect_ty.clone().ptr(),
        _ => Type::Nothing,
    };

    let span = unary_op.span().clone();
    let operand = typecheck_expr(&unary_op.operand, &operand_expect_ty, ctx)?;
    let operand_ty = operand.annotation().ty();

    let annotation = match unary_op.op {
        Operator::AddressOf => {
            let ty = operand.annotation().ty();
            let value_kind = operand.annotation().value_kind();

            let kind_addressable = match operand.annotation().value_kind() {
                None
                | Some(ValueKind::Temporary | ValueKind::Immutable | ValueKind::Uninitialized) => {
                    false
                },

                Some(ValueKind::Mutable) => true,
            };

            match (kind_addressable, ty.as_ref()) {
                (false, _) | (true, Type::Nothing | Type::Nil | Type::Function(..)) => {
                    Err(TypeError::NotAddressable {
                        ty: ty.into_owned(),
                        value_kind,
                        span,
                    })
                },

                (
                    true,
                    Type::Class(..)
                    | Type::Interface(..)
                    | Type::DynArray { .. }
                    | Type::Array { .. }
                    | Type::MethodSelf { .. }
                    | Type::Variant(..)
                    | Type::GenericParam(..),
                ) if !ctx.allow_unsafe() => Err(TypeError::UnsafeAddressoOfNotAllowed {
                    ty: ty.into_owned(),
                    span,
                }),

                _ => Ok(TypedValue {
                    ty: ty.into_owned().ptr(),
                    value_kind: ValueKind::Temporary,
                    span,
                    decl: None,
                }
                .into()),
            }?
        },

        // unary +, is this always a no-op?
        Operator::Add if operand_ty.valid_math_op(Operator::Add, &operand_ty) => {
            TypedValue {
                ty: operand_ty.into_owned(),
                value_kind: ValueKind::Temporary,
                span,
                decl: None,
            }
            .into()
        },

        // unary negation - should this be disallowed for unsigned types?
        Operator::Sub if operand_ty.valid_math_op(Operator::Sub, &operand_ty) => {
            TypedValue {
                ty: operand_ty.into_owned(),
                value_kind: ValueKind::Temporary,
                span,
                decl: None,
            }
            .into()
        },

        Operator::Caret => {
            let deref_ty = operand
                .annotation()
                .ty()
                .deref_ty()
                .cloned()
                .ok_or_else(|| TypeError::NotDerefable {
                    ty: operand.annotation().ty().into_owned(),
                    span: span.clone(),
                })?;

            let value_kind = ValueKind::Mutable;

            TypedValue {
                ty: deref_ty,
                value_kind,
                span,
                decl: operand.annotation().decl().cloned(),
            }
            .into()
        },

        Operator::Not => {
            operand
                .annotation()
                .expect_value(&Type::Primitive(Primitive::Boolean))?;

            TypedValue {
                ty: Type::Primitive(Primitive::Boolean),
                value_kind: ValueKind::Temporary,
                span,
                decl: operand.annotation().decl().cloned(),
            }
            .into()
        },

        Operator::BitNot => {
            let valid_ty = match operand.annotation().ty().as_ref() {
                Type::Primitive(p) => p.is_integer() && !p.is_signed(),
                _ => false,
            };

            if !valid_ty {
                return Err(TypeError::InvalidUnaryOp {
                    operand: operand.annotation().ty().into_owned(),
                    op: unary_op.op,
                    span,
                });
            }

            TypedValue {
                ty: operand.annotation().ty().into_owned(),
                value_kind: ValueKind::Temporary,
                span,
                decl: None,
            }
            .into()
        },

        _ => {
            return Err(TypeError::InvalidUnaryOp {
                op: unary_op.op,
                operand: operand.annotation().ty().into_owned(),
                span: unary_op.annotation.clone(),
            });
        },
    };

    Ok(UnaryOp {
        operand,
        op: unary_op.op,
        annotation,
    })
}

pub fn typecheck_indexer(
    base: &ast::Expr<Span>,
    index: &ast::Expr<Span>,
    span: &Span,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    // todo: other index types
    let index_ty = Type::Primitive(Primitive::Int32);
    let index = typecheck_expr(&index, &index_ty, ctx)?;

    index.annotation().expect_value(&index_ty)?;

    let base = typecheck_expr(&base, &Type::Nothing, ctx)?;

    check_array_bound_static(&base, &index, ctx)?;

    let base_ty = base.annotation().ty();

    let (el_ty, value_kind) = match (base_ty.element_ty(), base.annotation().value_kind()) {
        (Some(el_ty), Some(base_value_kind)) => {
            let base_value_kind = if base.annotation().ty().is_by_ref() {
                // on heap e.g. dynamic array, always mutable
                ValueKind::Mutable
            } else {
                // inherit mutability from owning variable
                base_value_kind
            };

            (el_ty.clone(), base_value_kind)
        },

        // not indexable
        _ => {
            return Err(TypeError::InvalidIndexer {
                index_ty: index_ty.clone(),
                base: Box::new(base.clone()),
                span: span.clone(),
            })
        },
    };

    let annotation = TypedValue {
        value_kind,
        ty: el_ty,
        span: span.clone(),
        decl: None,
    }
    .into();

    Ok(Expr::from(BinOp {
        lhs: base,
        rhs: index,
        op: Operator::Index,
        annotation,
    }))
}

fn check_array_bound_static(base: &Expr, index: &Expr, ctx: &mut Context) -> TypeResult<()> {
    fn out_of_range(dim: usize, index: IntConstant) -> bool {
        index.as_i128() < 0 || index.as_i128() >= dim as i128
    }

    match (
        base.annotation().ty().as_ref(),
        const_eval_integer(index, ctx),
    ) {
        (Type::Array(array_ty), Ok(index_const)) if out_of_range(array_ty.dim, index_const) => {
            Err(TypeError::IndexOutOfBounds {
                index: index_const,
                base_ty: Box::new(base.annotation().ty().into_owned()),
                span: index.span().clone(),
            })
        },

        _ => Ok(()),
    }
}
