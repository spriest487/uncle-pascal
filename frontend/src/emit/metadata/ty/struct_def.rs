use crate::emit::module_builder::ModuleBuilder;
use crate::emit::syn;
use crate::emit::translate_name;
use crate::emit::typ;
use common::span::Spanned;
use ir_lang::*;
use std::collections::HashMap;
use syn::StructKind;
use typ::layout::StructLayoutMember;
use crate::emit::builder::GenericContext;

pub fn translate_struct_def(
    struct_def: &typ::ast::StructDef,
    generic_ctx: &GenericContext,
    module: &mut ModuleBuilder,
) -> Struct {
    let name_path = translate_name(&struct_def.name, generic_ctx, module);

    let mut fields = HashMap::new();
    let mut pad_run = 0;
    let mut next_id = FieldID(0);
    for member in module.aligned_struct_members(struct_def) {
        match member {
            StructLayoutMember::Data { member, .. } => {
                if pad_run > 0 {
                    fields.insert(next_id, StructFieldDef {
                        name: None,
                        rc: false,
                        ty: Type::U8.array(pad_run),
                    });
                    pad_run = 0;
                    next_id.0 += 1;
                }

                let name = member.ident.to_string();
                let ty = module.translate_type(&member.ty, generic_ctx);
                let rc = member.ty.is_rc_reference();
                fields.insert(next_id, StructFieldDef { name: Some(name), ty, rc });
                next_id.0 += 1;
            }

            StructLayoutMember::PaddingByte => {
                pad_run += 1;
            }
        }
    }

    if pad_run > 0 {
        let pad_ty = Type::U8.array(pad_run);
        fields.insert(next_id, StructFieldDef { name: None, rc: false, ty: pad_ty });
    }

    let src_span = struct_def.span().clone();

    let identity = match struct_def.kind {
        StructKind::Class => StructIdentity::Class(name_path),
        StructKind::Record | StructKind::PackedRecord => {
            StructIdentity::Record(name_path)
        },
    };

    Struct::new(identity, Some(src_span)).with_fields(fields)
}
