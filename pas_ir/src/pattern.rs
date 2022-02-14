use pas_typecheck::TypePattern;
use crate::{Type, Value, Ref, pas_ty, Builder, Instruction};

pub struct PatternMatchBinding {
    pub name: String,
    pub ty: Type,
    pub binding_ref: Ref,
}

impl PatternMatchBinding {
    // allocate a new local in the builder with the name and type of this binding and copy + retain
    // the binding value into it
    pub fn bind_local(&self, builder: &mut Builder) {
        builder.comment(&format!(
            "pattern binding {}: {}",
            self.name,
            builder.pretty_ty_name(&self.ty)
        ));

        let local = builder.local_new(self.ty.clone(), Some(self.name.clone()));
        builder.mov(local.clone(), self.binding_ref.clone());
        builder.retain(local, &self.ty);
    }
}

pub struct PatternMatchOutput {
    pub is_match: Value,
    pub bindings: Vec<PatternMatchBinding>,
}

pub fn translate_pattern_match(pattern: &pas_ty::TypePattern, target_val: &Ref, target_ty: &Type, builder: &mut Builder) -> PatternMatchOutput {
    match pattern {
        TypePattern::Type { binding, ty, .. } => {
            let is_ty = builder.translate_type(ty);
            let is = translate_is_ty(target_val.clone(), &target_ty, &is_ty, builder);

            let bindings = match binding {
                Some(binding) => {
                    let binding_name = binding.name.to_string();
                    let binding_ref = target_val.clone();

                    vec![PatternMatchBinding {
                        name: binding_name,
                        ty: is_ty,
                        binding_ref
                    }]
                },
                None => Vec::new(),
            };

            PatternMatchOutput {
                is_match: is,
                bindings,
            }
        },

        TypePattern::NegatedType { ty, .. } => {
            let is_not_ty = builder.translate_type(ty);
            let is = translate_is_ty(target_val.clone(), &target_ty, &is_not_ty, builder);

            let is_not = builder.not_to_val(is);

            PatternMatchOutput {
                is_match: is_not,
                bindings: Vec::new(),
            }
        },

        TypePattern::VariantCase {
             variant,
             case,
             data_binding,
             ..
         } => {
            let (struct_id, case_index, case_ty) =
                builder.translate_variant_case(variant, case);
            let variant_ty = Type::Variant(struct_id);

            let bindings = match data_binding {
                Some(binding) => {
                    let binding_name = binding.name.to_string();

                    let case_ty = case_ty
                        .cloned()
                        .expect("variant pattern with binding must refer to a case with data");

                    let data_ptr = builder.local_temp(case_ty.clone().ptr());

                    builder.append(Instruction::VariantData {
                        out: data_ptr.clone(),
                        a: target_val.clone(),
                        of_ty: variant_ty.clone(),
                        tag: case_index,
                    });

                    vec![PatternMatchBinding {
                        name: binding_name,
                        binding_ref: data_ptr.to_deref(),
                        ty: case_ty,
                    }]
                },

                None => Vec::new(),
            };

            let is = translate_is_variant(target_val.clone(), variant_ty, case_index, builder);

            PatternMatchOutput {
                is_match: Value::Ref(is),
                bindings,
            }
        },

        TypePattern::NegatedVariantCase { variant, case, .. } => {
            let (struct_id, case_index, _case_ty) =
                builder.translate_variant_case(variant, case);

            let variant_ty = Type::Variant(struct_id);
            let is = translate_is_variant(target_val.clone(), variant_ty, case_index, builder);

            let is_not = builder.not_to_val(Value::Ref(is));

            PatternMatchOutput {
                is_match: is_not,
                bindings: Vec::new(),
            }
        },
    }
}

pub fn translate_is_ty(val: Ref, val_ty: &Type, ty: &Type, builder: &mut Builder) -> Value {
    if val_ty.is_rc() {
        match ty {
            Type::RcPointer(Some(class_id)) => {
                // checking if one RC type (probably an interface) is an instance of another RC
                // type (probably a class): this is a runtime check
                let result = builder.local_temp(Type::Bool);

                builder.append(Instruction::ClassIs {
                    out: result.clone(),
                    a: Value::Ref(val),
                    class_id: *class_id,
                });

                Value::Ref(result)
            },

            // value is RC and we are testing if it's Any: it always is
            Type::RcPointer(None) => Value::LiteralBool(true),

            // value is a value type, we wanted an RC type
            _ => Value::LiteralBool(false),
        }
    } else {
        // value types must match exactly
        let same_ty = *val_ty == *ty;
        Value::LiteralBool(same_ty)
    }
}

fn translate_is_variant(
    val: Ref,
    variant_ty: Type,
    case_index: usize,
    builder: &mut Builder,
) -> Ref {
    let tag_ptr = builder.local_temp(Type::I32.ptr());
    builder.append(Instruction::VariantTag {
        out: tag_ptr.clone(),
        a: val,
        of_ty: variant_ty,
    });

    let is = builder.local_temp(Type::Bool);
    builder.append(Instruction::Eq {
        out: is.clone(),
        a: Value::Ref(tag_ptr.to_deref()),
        b: Value::LiteralI32(case_index as i32), //todo: proper size type,
    });

    is
}