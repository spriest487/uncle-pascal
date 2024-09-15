use crate::builder::Builder;
use crate::expr;
use crate::Instruction;
use crate::Ref;
use crate::Type;
use crate::Value;
use crate::metadata::VirtualTypeID;
use crate::metadata::DYNARRAY_LEN_FIELD;
use crate::metadata::DYNARRAY_PTR_FIELD;
use compiler::typecheck as typ;

pub fn translate_object_ctor(ctor: &typ::ast::ObjectCtor, builder: &mut Builder) -> Ref {
    let object_ty = builder.translate_type(&ctor.annotation.ty());

    let struct_id = match &object_ty {
        Type::RcPointer(VirtualTypeID::Class(struct_id)) => *struct_id,
        Type::Struct(struct_id) => *struct_id,
        _ => panic!(
            "type of object ctor expr `{}` must be a record or class",
            ctor
        ),
    };

    let struct_def = builder
        .get_struct(struct_id)
        .unwrap_or_else(|| panic!("struct {} referenced in object ctor must exist", ctor.ident.as_ref().unwrap()))
        .clone();

    // either local struct of the correct type for value types, or a rc pointer to the struct
    // type for rc class types
    let out_val = builder.local_new(object_ty.clone(), None);

    if object_ty.is_rc() {
        // allocate class struct at out pointer
        builder.append(Instruction::RcNew {
            out: out_val.clone(),
            struct_id,
        });
    }

    builder.scope(|builder| {
        for member in &ctor.args.members {
            let member_val = expr::translate_expr(&member.value, builder);
            let field_id = struct_def
                .find_field(&member.ident.name)
                .unwrap_or_else(|| {
                    panic!(
                        "field {} referenced in object ctor must exist",
                        member.ident
                    )
                });

            let field_def = struct_def.get_field(field_id).unwrap();

            builder.comment(&format!(
                "{}: {} ({})",
                member.ident, member.value, field_def.ty
            ));

            let field_ptr = builder.local_temp(field_def.ty.clone().ptr());
            builder.append(Instruction::Field {
                out: field_ptr.clone(),
                a: out_val.clone(),
                of_ty: object_ty.clone(),
                field: field_id,
            });

            builder.mov(field_ptr.clone().to_deref(), member_val);
            builder.retain(field_ptr.to_deref(), &field_def.ty);
        }
    });

    out_val
}

pub fn translate_collection_ctor(ctor: &typ::ast::CollectionCtor, builder: &mut Builder) -> Ref {
    let ctor_ty = ctor.annotation.ty();
    match ctor_ty.as_ref() {
        typ::Type::Array(array_ty) => {
            translate_static_array_ctor(ctor, &array_ty.element_ty, array_ty.dim, builder)
        },

        typ::Type::DynArray { element } => translate_dyn_array_ctor(ctor, element, builder),

        unimpl => unimplemented!("IR for array constructor {} of type {}", ctor, unimpl),
    }
}

fn translate_static_array_ctor(
    ctor: &typ::ast::CollectionCtor,
    element: &typ::Type,
    dim: usize,
    builder: &mut Builder,
) -> Ref {
    let el_ty = builder.translate_type(element);

    let array_ty = el_ty.clone().array(dim);
    let arr = builder.local_temp(array_ty.clone());

    builder.begin_scope();

    let el_ptr = builder.local_temp(el_ty.clone().ptr());

    for (i, el) in ctor.elements.iter().enumerate() {
        builder.begin_scope();

        let index = i32::try_from(i).expect("invalid array index in array ctor");

        builder.append(Instruction::Element {
            out: el_ptr.clone(),
            a: arr.clone(),
            index: Value::LiteralI32(index),
            element: el_ty.clone(),
        });

        let el_init = expr::translate_expr(&el.value, builder);

        builder.mov(el_ptr.clone().to_deref(), el_init);
        builder.end_scope();
    }

    builder.end_scope();

    arr
}

fn translate_dyn_array_ctor(
    ctor: &typ::ast::CollectionCtor,
    element: &typ::Type,
    builder: &mut Builder,
) -> Ref {
    let elem_ty = builder.translate_type(element);

    // should be a class rc-ptr to the unique class for this dyn array element type
    let array_ty = builder.translate_type(&ctor.annotation.ty());
    let struct_id = match &array_ty {
        Type::RcPointer(VirtualTypeID::Class(struct_id)) => *struct_id,
        _ => unreachable!("dynamic array must have an rc class type"),
    };

    let arr = builder.local_new(array_ty.clone(), None);

    // allocate the array object itself
    builder.scope(|builder| {
        builder.append(Instruction::RcNew {
            out: arr.clone(),
            struct_id,
        });

        // get pointer to the length
        let len_ref = builder.local_temp(Type::I32.ptr());
        builder.append(Instruction::Field {
            out: len_ref.clone(),
            of_ty: array_ty.clone(),
            field: DYNARRAY_LEN_FIELD,
            a: arr.clone(),
        });

        // set length
        let len = i32::try_from(ctor.elements.len()).expect("invalid dynamic array ctor length");
        builder.mov(len_ref.clone().to_deref(), Value::LiteralI32(len));

        // get pointer to storage pointer
        let arr_ptr = builder.local_temp(elem_ty.clone().ptr().ptr());
        builder.append(Instruction::Field {
            out: arr_ptr.clone(),
            of_ty: array_ty,
            field: DYNARRAY_PTR_FIELD,
            a: arr.clone(),
        });

        // allocate array storage
        if len > 0 {
            let alloc_size = builder.local_temp(Type::I32);
            builder.mul(alloc_size.clone(), Value::SizeOf(elem_ty.clone()), Value::LiteralI32(len));

            let elements_mem = builder.local_temp(Type::U8.ptr());
            builder.get_mem(alloc_size, elements_mem.clone());
            builder.cast(arr_ptr.clone().to_deref(), elements_mem, elem_ty.clone().ptr());

            let el_ptr = builder.local_temp(elem_ty.clone().ptr());

            for (i, el) in ctor.elements.iter().enumerate() {
                builder.scope(|builder| {
                    // we know this cast is OK because we check the length is in range of i32 previously
                    let index = Value::LiteralI32(i as i32);

                    // el_ptr := arr_ptr^ + i
                    builder.append(Instruction::Add {
                        a: Value::Ref(arr_ptr.clone().to_deref()),
                        b: index,
                        out: el_ptr.clone(),
                    });

                    // el_ptr^ := el
                    let el = expr::translate_expr(&el.value, builder);
                    builder.mov(el_ptr.clone().to_deref(), el);

                    // retain each element. we don't do this for static arrays because retaining
                    // a static array retains all its elements - for dynamic arrays, retaining
                    // the array object itself does not retain the elements
                    builder.retain(el_ptr.clone().to_deref(), &elem_ty);
                });
            }
        } else {
            builder.mov(arr_ptr.to_deref(), Value::LiteralNull);
        }
    });

    arr
}
