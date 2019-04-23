use crate::{prelude::*, translate_stmt};
use pas_common::span::*;
use pas_syn::{self as syn, ast};
use pas_typecheck::{self as pas_ty, TypeAnnotation, ValueKind};

pub fn translate_expr(expr: &pas_ty::ast::ExpressionNode, builder: &mut Builder) -> Ref {
    let out_val = match expr.expr.as_ref() {
        ast::Expression::Literal(lit) => translate_literal(lit, expr.annotation.ty(), builder),

        ast::Expression::BinOp(bin_op) => translate_bin_op(bin_op, expr.annotation.ty(), builder),

        ast::Expression::UnaryOp(unary_op) => {
            translate_unary_op(unary_op, expr.annotation.ty(), builder)
        }

        ast::Expression::Ident(ident) => {
            match &expr.annotation {
                TypeAnnotation::TypedValue {
                    value_kind: ValueKind::Temporary,
                    ..
                } => panic!("temporaries cannot be referenced by ident"),

                TypeAnnotation::Function { name, ns, .. } => {
                    let global_name =
                        GlobalName::new(name.to_string(), ns.iter().map(|i| i.to_string()));
                    let func_id = builder
                        .metadata
                        .find_function(&global_name)
                        .unwrap_or_else(|| panic!("missing metadata decl for function {}", ident));
                    let func_ref = GlobalRef::Function(func_id);

                    Ref::Global(func_ref)
                }

                // ident lvalues are evaluated as pointers to the original values. they don't need
                // to be refcounted separately, because if they have a name, they must exist
                // in a scope at least as wide as the current one
                TypeAnnotation::TypedValue {
                    value_kind: ValueKind::Immutable,
                    ..
                }
                | TypeAnnotation::TypedValue {
                    value_kind: ValueKind::Mutable,
                    ..
                } => {
                    let local_ref = builder
                        .find_local(&ident.to_string())
                        .map(|local| Ref::Local(local.id()))
                        .unwrap_or_else(|| {
                            panic!(
                                "identifier not found in local scope @ {}: {}",
                                expr.annotation.span(),
                                ident
                            )
                        });

                    let ref_ty = builder
                        .metadata
                        .translate_type(expr.annotation.ty())
                        .clone();
                    let ref_temp = builder.local_temp(ref_ty.clone().ptr());

                    builder.append(Instruction::AddrOf {
                        out: ref_temp.clone(),
                        a: local_ref,
                    });
                    ref_temp.deref()
                }

                _ => panic!("wrong kind of node annotation for ident: {:?}",),
            }
        }

        ast::Expression::Call(call) => {
            let return_ref = translate_call(call, builder)
                .expect("call used in expression must have a return value");
            return_ref
        }

        ast::Expression::ObjectCtor(ctor) => {
            let obj_ref = translate_object_ctor(ctor, builder);
            obj_ref
        }

        ast::Expression::CollectionCtor(ctor) => {
            let coll_ref = translate_collection_ctor(ctor, builder);
            coll_ref
        }

        ast::Expression::IfCond(if_cond) => {
            let result_ref = translate_if_cond(if_cond, builder)
                .expect("conditional used in expression must have a type");
            result_ref
        }

        ast::Expression::Block(block) => {
            let out_ref =
                translate_block(block, builder).expect("block used in expression must have a type");

            out_ref
        }

        ast::Expression::Indexer(indexer) => translate_indexer(indexer, builder),
    };

    out_val
}

fn translate_indexer(indexer: &pas_ty::ast::Indexer, builder: &mut Builder) -> Ref {
    let ty = builder.metadata.translate_type(&indexer.annotation.ty());
    let ptr_into = builder.local_temp(ty.clone().ptr());

    builder.begin_scope();

    let index_ref = translate_expr(&indexer.index, builder);
    let base_ref = translate_expr(&indexer.base, builder);

    match indexer.base.annotation.ty() {
        pas_ty::Type::Array { .. } => {
            builder.append(Instruction::Element {
                out: ptr_into.clone(),
                a: base_ref,
                index: Value::Ref(index_ref),
                element: ty,
            });
        }

        pas_ty::Type::Pointer(_) => {
            builder.append(Instruction::Add {
                out: ptr_into.clone(),
                a: base_ref.into(),
                b: index_ref.into(),
            });
        }

        _ => unimplemented!("IR for indexing into {}", indexer.base.annotation.ty()),
    }

    builder.end_scope();

    ptr_into.deref()
}

pub fn translate_if_cond(if_cond: &pas_ty::ast::IfCond, builder: &mut Builder) -> Option<Ref> {
    let (out_val, out_ty) = match if_cond.annotation.ty() {
        pas_ty::Type::Nothing => (None, Type::Nothing),
        out_ty => {
            let out_ty = builder.metadata.translate_type(out_ty);
            let out_val = builder.local_new(out_ty.clone(), None);
            (Some(out_val), out_ty)
        }
    };

    builder.begin_scope();

    let then_label = builder.alloc_label();
    let end_label = builder.alloc_label();
    let else_label = if_cond.else_branch.as_ref().map(|_| builder.alloc_label());

    let test_val = translate_expr(&if_cond.cond, builder);
    builder.append(Instruction::JumpIf {
        test: test_val.into(),
        dest: then_label,
    });

    if let Some(else_label) = else_label {
        builder.append(Instruction::Jump { dest: else_label });
    } else {
        builder.append(Instruction::Jump { dest: end_label });
    }

    builder.append(Instruction::Label(then_label));

    builder.begin_scope();
    let then_val = translate_expr(&if_cond.then_branch, builder);

    if let Some(out_val) = out_val.clone() {
        builder.append(Instruction::Move {
            out: out_val.clone(),
            new_val: then_val.into(),
        });
        builder.retain(out_val.clone(), &out_ty);
    }
    builder.end_scope();
    builder.append(Instruction::Jump { dest: end_label });

    if let Some(else_branch) = &if_cond.else_branch {
        builder.append(Instruction::Label(else_label.unwrap()));

        builder.begin_scope();
        let else_val = translate_expr(else_branch, builder);

        if let Some(out_val) = out_val.clone() {
            builder.append(Instruction::Move {
                out: out_val.clone(),
                new_val: else_val.into(),
            });
            builder.retain(out_val.clone(), &out_ty);
        }
        builder.end_scope();
    }

    builder.append(Instruction::Label(end_label));
    builder.end_scope();

    out_val
}

fn translate_call_with_args(
    target: impl Into<Value>,
    args: &[pas_ty::ast::ExpressionNode],
    return_ty: &pas_ty::Type,
    builder: &mut Builder,
) -> Option<Ref> {
    let out_val = match return_ty {
        pas_ty::Type::Nothing => None,
        return_ty => {
            let out_ty = builder.metadata.translate_type(return_ty).clone();
            let out_val = builder.local_new(out_ty, None);
            Some(out_val)
        }
    };

    builder.begin_scope();

    let mut arg_vals = Vec::new();
    for arg in args {
        let arg_expr = translate_expr(arg, builder);
        arg_vals.push(Value::from(arg_expr));
    }

    builder.append(Instruction::Call {
        function: target.into(),
        args: arg_vals.clone(),
        out: out_val.clone(),
    });

    // no need to retain, the result of a function must be retained as part of its body

    builder.end_scope();

    out_val
}

pub fn translate_call(call: &pas_ty::ast::Call, builder: &mut Builder) -> Option<Ref> {
    match call {
        ast::Call::Function(func_call) => {
            let sig = match func_call.target.annotation.ty() {
                pas_ty::Type::Function(sig) => sig,
                _ => panic!("type of function target expr must be a function"),
            };

            let target_val = translate_expr(&func_call.target, builder);

            translate_call_with_args(target_val, &func_call.args, &sig.return_ty, builder)
        }

        ast::Call::Method(method_call) => {
            let of_ty: &pas_ty::Type = &method_call.of_type;

            let method_decl = of_ty
                .get_method(&method_call.ident)
                .expect("method referenced in method call must exist");

            let (impl_func, sig) = match builder.metadata.translate_type(of_ty) {
                Type::InterfaceRef(iface_id) => {
                    let method_name = method_call.ident.to_string();
                    let self_ty = builder.metadata.translate_type(&method_call.self_type);
                    let method_sig = pas_ty::FunctionSig::of_decl(method_decl);

                    let impl_func = builder
                        .metadata
                        .find_impl(&self_ty, iface_id, &method_name)
                        .expect("referenced impl func must be declared");

                    (impl_func, method_sig)
                }
                _ => unimplemented!("non-interface method call"),
            };

            let target_val = Ref::Global(GlobalRef::Function(impl_func));

            translate_call_with_args(target_val, &method_call.args, &sig.return_ty, builder)
        }
    }
}

fn is_string_class(class: &pas_ty::ast::Class) -> bool {
    let str_path = syn::Path::from_parts(vec!["System".to_string(), "String".to_string()]);
    class.ident == str_path
}

fn translate_literal(lit: &ast::Literal, ty: &pas_ty::Type, builder: &mut Builder) -> Ref {
    let out_ty = builder.metadata.translate_type(ty);
    let out = builder.local_temp(out_ty);

    match lit {
        ast::Literal::Nil => {
            builder.mov(out.clone(), Value::LiteralNull);
        }

        ast::Literal::Boolean(b) => {
            builder.mov(out.clone(), Value::LiteralBool(*b));
        }

        ast::Literal::Integer(i) => match ty {
            pas_ty::Type::Primitive(pas_ty::Primitive::Int32) => {
                let val = i
                    .as_i32()
                    .expect("Int32-typed constant must be within range of i32");
                builder.mov(out.clone(), Value::LiteralI32(val));
            }
            _ => panic!("bad type for integer literal: {}", ty),
        },

        ast::Literal::Real(r) => match ty {
            pas_ty::Type::Primitive(pas_ty::Primitive::Real32) => {
                let val = r
                    .as_f32()
                    .expect("Real32-typed constant must be within range of f32");
                builder.mov(out.clone(), Value::LiteralF32(val));
            }
            _ => panic!("bad type for real literal: {}", ty),
        },

        ast::Literal::String(s) => match ty {
            pas_ty::Type::Class(class) if is_string_class(class) => {
                let lit_id = builder.metadata.find_or_insert_string(s);
                let lit_ref = GlobalRef::StringLiteral(lit_id);

                builder.mov(out.clone(), Value::Ref(Ref::Global(lit_ref)));
            }
            _ => panic!("bad type for string literal: {}", ty),
        },
    }

    out
}

fn translate_bin_op(
    bin_op: &pas_ty::ast::BinOp,
    out_ty: &pas_ty::Type,
    builder: &mut Builder,
) -> Ref {
    if bin_op.lhs.annotation.is_namespace() {
        // there's nothing to actually translate on the lhs, it's just for name resolution
        return translate_expr(&bin_op.rhs, builder);
    }

    let out_ty = builder.metadata.translate_type(out_ty);

    let (out_val, by_ref) = if bin_op.op == syn::Operator::Member {
        (builder.local_new(out_ty.clone().ptr(), None), true)
    } else {
        (builder.local_new(out_ty.clone(), None), false)
    };

    builder.begin_scope();
    let lhs_val = translate_expr(&bin_op.lhs, builder);

    match &bin_op.op {
        syn::Operator::Member => {
            // auto-deref for rc types

            let of_ty = builder.metadata.translate_type(bin_op.lhs.annotation.ty());

            let struct_name = NamePath::from_ident({
                let path = bin_op.lhs.annotation.ty().full_path();
                path.expect("member access must be of a named type")
            });
            let member_name = bin_op
                .rhs
                .expr
                .as_ident()
                .map(|i| i.to_string())
                .expect("rhs of member binop must be an ident");

            let (_struct_id, struct_def) = builder
                .metadata
                .find_struct(&struct_name)
                .expect("referenced struct must exist");
            let field = struct_def
                .find_field(&member_name)
                .expect("referenced field must exist");

            builder.append(Instruction::Field {
                out: out_val.clone(),
                a: lhs_val,
                of_ty,
                field,
            });
        }

        syn::Operator::NotEquals => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Eq {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
            builder.append(Instruction::Not {
                out: out_val.clone(),
                a: Value::Ref(out_val.clone()),
            });
        }

        syn::Operator::Equals => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Eq {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        }

        syn::Operator::Plus => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Add {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        }

        syn::Operator::Gt => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Gt {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        }

        syn::Operator::Gte => {
            let b = translate_expr(&bin_op.rhs, builder);

            let gt = builder.local_temp(Type::Bool);
            builder.append(Instruction::Gt {
                out: gt.clone(),
                a: lhs_val.clone().into(),
                b: b.clone().into(),
            });

            let eq = builder.local_temp(Type::Bool);
            builder.append(Instruction::Eq {
                out: eq.clone(),
                a: lhs_val.clone().into(),
                b: b.clone().into(),
            });

            builder.append(Instruction::Or {
                out: out_val.clone(),
                a: gt.into(),
                b: eq.into(),
            });
        }

        syn::Operator::Lt => {
            let b = translate_expr(&bin_op.rhs, builder);

            let gt = builder.local_temp(Type::Bool);
            builder.append(Instruction::Gt {
                out: gt.clone(),
                a: lhs_val.clone().into(),
                b: b.clone().into(),
            });

            let eq = builder.local_temp(Type::Bool);
            builder.append(Instruction::Eq {
                out: eq.clone(),
                a: lhs_val.clone().into(),
                b: b.clone().into(),
            });

            let gte = builder.local_temp(Type::Bool);
            builder.append(Instruction::Or {
                out: gte.clone(),
                a: gt.into(),
                b: eq.into(),
            });

            builder.append(Instruction::Not {
                out: out_val.clone(),
                a: gte.into(),
            });
        }

        syn::Operator::Lte => {
            let b = translate_expr(&bin_op.rhs, builder);

            let gt = builder.local_temp(Type::Bool);
            builder.append(Instruction::Gt {
                out: gt.clone(),
                a: lhs_val.clone().into(),
                b: b.clone().into(),
            });

            builder.append(Instruction::Not {
                out: out_val.clone(),
                a: gt.into(),
            });
        }

        syn::Operator::Minus => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Sub {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        }

        syn::Operator::And => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::And {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        }

        syn::Operator::Or => {
            let b = translate_expr(&bin_op.rhs, builder);
            builder.append(Instruction::Or {
                out: out_val.clone(),
                a: lhs_val.into(),
                b: b.into(),
            });
        }

        _ => unimplemented!("IR for op {}", bin_op.op),
    };

    builder.retain(out_val.clone(), &out_ty);

    builder.end_scope();

    if by_ref {
        out_val.deref()
    } else {
        out_val
    }
}

fn translate_unary_op(
    unary_op: &pas_ty::ast::UnaryOp,
    out_ty: &pas_ty::Type,
    builder: &mut Builder,
) -> Ref {
    let operand_ref = translate_expr(&unary_op.operand, builder);

    match unary_op.op {
        syn::Operator::AddressOf => {
            let out_ty = builder.metadata.translate_type(out_ty);
            let out_val = builder.local_new(out_ty.clone(), None);

            builder.append(Instruction::AddrOf {
                out: out_val.clone(),
                a: operand_ref,
            });
            builder.retain(out_val.clone(), &out_ty);

            out_val
        }

        syn::Operator::Deref => operand_ref.deref(),

        op => unimplemented!("unary operator {}", op),
    }
}

fn translate_object_ctor(ctor: &pas_ty::ast::ObjectCtor, builder: &mut Builder) -> Ref {
    let obj_name = NamePath::from_ident(ctor.ident.clone());

    let (_struct_id, struct_def) = builder
        .metadata
        .find_struct(&obj_name)
        .map(|(id, def)| (id, def.clone()))
        .unwrap_or_else(|| panic!("struct {} referenced in object ctor must exist", ctor.ident));

    let struct_ty = builder.metadata.translate_type(ctor.annotation.ty());

    let out_ptr = builder.local_new(struct_ty.clone(), None);

    match &struct_ty {
        Type::Rc(struct_ty) => {
            let struct_id = match struct_ty.as_ref() {
                Type::Struct(id) => *id,
                _ => panic!(
                    "bad object ctor: class type {:?} doesn't have struct layout",
                    struct_ty
                ),
            };

            // allocate class struct at out pointer
            builder.append(Instruction::RcNew {
                out: out_ptr.clone(),
                struct_id,
            });
        }

        Type::Struct(_) => {
            /* no init needed, a local struct is initialized on allocation */
        }

        ty => panic!("bad object ctor: type {:?} is not a record or class", ty),
    };

    builder.begin_scope();

    // todo: lookup members by id, don't assume they're in order
    for member in &ctor.args.members {
        let member_val = translate_expr(&member.value, builder);
        let field = struct_def
            .find_field(&member.ident.to_string())
            .unwrap_or_else(|| {
                panic!(
                    "field {} referenced in object ctor must exist",
                    member.ident
                )
            });

        let member_ty = builder
            .metadata
            .translate_type(member.value.annotation.ty());
        builder.comment(&format!("{}: {}", member.ident, member.value));

        builder.retain(member_val.clone(), &member_ty);

        let field_ptr = builder.local_temp(member_ty.ptr());
        builder.append(Instruction::Field {
            out: field_ptr.clone(),
            a: out_ptr.clone(),
            of_ty: struct_ty.clone(),
            field,
        });

        builder.mov(field_ptr.deref(), member_val);
    }

    builder.end_scope();
    out_ptr
}

fn translate_collection_ctor(ctor: &pas_ty::ast::CollectionCtor, builder: &mut Builder) -> Ref {
    match &ctor.annotation.ty() {
        pas_ty::Type::Array { element, dim } => {
            let el_ty = builder.metadata.translate_type(element.as_ref());

            let array_ty = el_ty.clone().array(*dim);
            let arr = builder.local_temp(array_ty.clone());

            builder.begin_scope();

            let el_ptr = builder.local_temp(el_ty.clone());

            for (i, el) in ctor.elements.iter().enumerate() {
                builder.begin_scope();

                builder.append(Instruction::Element {
                    out: el_ptr.clone(),
                    a: arr.clone(),
                    index: Value::LiteralI32(i as i32),
                    element: el_ty.clone(),
                });

                let el_init = translate_expr(el, builder);
                builder.mov(el_ptr.clone().deref(), el_init);

                builder.end_scope();
            }

            builder.end_scope();

            arr
        }

        unimpl => unimplemented!("IR for array constructor {} of type {}", ctor, unimpl),
    }
}

pub fn translate_block(block: &pas_ty::ast::Block, builder: &mut Builder) -> Option<Ref> {
    let (out_val, out_ty) = match &block.output {
        Some(out_expr) => {
            let out_ty = builder.metadata.translate_type(out_expr.annotation.ty());
            let out_val = builder.local_new(out_ty.clone(), None);

            (Some(out_val), out_ty)
        }
        None => (None, Type::Nothing),
    };

    builder.comment("begin");
    builder.begin_scope();

    for stmt in &block.statements {
        translate_stmt(stmt, builder);
    }

    if let Some(out) = &block.output {
        let result_val = translate_expr(out, builder);
        builder.mov(out_val.clone().unwrap(), result_val.clone());
        builder.retain(result_val, &out_ty);
    }

    builder.end_scope();
    builder.comment("end");

    out_val
}
