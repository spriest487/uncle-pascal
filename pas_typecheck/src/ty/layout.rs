use crate::ast::{StructDef, VARIANT_TAG_TYPE};
use crate::{Context, GenericError, NameError, NameResult, Primitive, Type, TypeAnnotation};
use pas_syn::ast::{StructKind, StructMember};
use std::mem::size_of;

const WORD_SIZE: usize = size_of::<usize>();

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum StructLayout {
    Auto,
    Packed,
}

impl StructLayout {
    pub fn align_of(self, ty: &Type, ctx: &Context) -> NameResult<usize> {
        let align = match self {
            StructLayout::Packed => 1,

            StructLayout::Auto => match ty {
                Type::Nothing
                | Type::Nil
                | Type::Pointer(..)
                | Type::Function(..)
                | Type::DynArray { .. }
                | Type::Class(..)
                | Type::Interface(..)
                | Type::Any
                | Type::Enum(..)
                | Type::Primitive(..) => self.size_of(ty, ctx)?,

                Type::Array(array_ty) => self.align_of(&array_ty.element_ty, ctx)?,

                Type::Record(record_sym) => {
                    let struct_def = ctx.instantiate_struct_def(&record_sym)?;

                    let mut max_member_align = 1;
                    for def_member in &struct_def.members {
                        let member_align = self.align_of(&def_member.ty, ctx)?;
                        max_member_align = usize::max(max_member_align, member_align);
                    }

                    max_member_align
                },

                Type::Variant(variant_sym) => {
                    let variant_def = ctx.instantiate_variant_def(&variant_sym)?;

                    let tag_align = self.align_of(&VARIANT_TAG_TYPE, ctx)?;
                    let mut max_data_align = 1;

                    for case in &variant_def.cases {
                        if let Some(data_ty) = &case.data_ty {
                            max_data_align =
                                usize::max(max_data_align, self.align_of(data_ty, ctx)?);
                        }
                    }

                    usize::max(tag_align, max_data_align)
                },

                Type::MethodSelf | Type::GenericParam(..) => {
                    return Err(NameError::GenericError(
                        GenericError::IllegalUnspecialized { ty: ty.clone() },
                    ))
                },
            },
        };

        Ok(align)
    }

    pub fn size_of(self, ty: &Type, ctx: &Context) -> NameResult<usize> {
        let size = match ty {
            Type::Nothing
            | Type::Nil
            | Type::Pointer(..)
            | Type::Function(..)
            | Type::DynArray { .. }
            | Type::Interface(..)
            | Type::Any
            | Type::Enum(..) // TODO: enums may be variable size later depending on their range?
            | Type::Class(..) => WORD_SIZE,

            Type::Array(array_ty) => self.size_of(&array_ty.element_ty, ctx)? * array_ty.dim,

            Type::Primitive(p) => match p {
                Primitive::Boolean | Primitive::UInt8 | Primitive::Int8 => 1,
                Primitive::Int16 | Primitive::UInt16 => 2,
                Primitive::Int32 | Primitive::UInt32 | Primitive::Real32 => 4,
                Primitive::Int64 | Primitive::UInt64 => 8,
                Primitive::NativeInt | Primitive::NativeUInt | Primitive::Pointer => WORD_SIZE,
            },

            Type::Variant(variant_name) => {
                let variant_def = ctx.instantiate_variant_def(&variant_name)?;

                let mut max_data_size = 0;
                let mut max_data_align = 1;
                for case in &variant_def.cases {
                    if let Some(data_ty) = &case.data_ty {
                        max_data_size = usize::max(max_data_size, self.size_of(data_ty, ctx)?);
                        max_data_align = usize::max(max_data_align, self.align_of(data_ty, ctx)?);
                    }
                }

                let tag_align = self.align_of(&VARIANT_TAG_TYPE, ctx)?;
                let tag_size = self.size_of(&VARIANT_TAG_TYPE, ctx)?;
                let tag_pad = Self::padding(tag_size, max_data_align);
                let end_pad = Self::padding(tag_size + tag_pad + max_data_size, tag_align);
                tag_size + tag_pad + max_data_size + end_pad
            },
            Type::Record(struct_sym) => {
                let struct_def = ctx.instantiate_struct_def(&struct_sym)?;

                let mut total_size = 0;
                for member in self.members_of(&struct_def, ctx)? {
                    total_size += match member {
                        StructLayoutMember::Data { size, .. } => size,
                        StructLayoutMember::PaddingByte => 1,
                    }
                }
                total_size
            },

            Type::MethodSelf | Type::GenericParam(..) => {
                return Err(NameError::GenericError(
                    GenericError::IllegalUnspecialized { ty: ty.clone() },
                ))
            },
        };

        Ok(size)
    }

    pub fn members_of<'a>(
        self,
        def: &'a StructDef,
        ctx: &Context,
    ) -> NameResult<Vec<StructLayoutMember<'a>>> {
        let mut members = Vec::with_capacity(def.members.len());

        let (mut offset, mut max_align) = match def.kind {
            StructKind::Class => {
                // class structs start with the RC state object which consists of the class pointer and
                // the two ref count values (both I32), which affects their initial offset and alignment requirement
                let max_align = WORD_SIZE;
                let offset = WORD_SIZE + 8;
                (offset, max_align)
            },
            _ => (0, 1),
        };

        for def_member in &def.members {
            let member_size = self.size_of(&def_member.ty, ctx)?;
            let member_align = self.align_of(&def_member.ty, ctx)?;

            max_align = usize::max(max_align, member_align);

            let pad_before = Self::padding(offset, member_align);
            for _ in 0..pad_before {
                members.push(StructLayoutMember::PaddingByte);
            }
            members.push(StructLayoutMember::Data {
                member: def_member,
                size: member_size,
            });

            offset += pad_before + member_size;
        }

        // class instance structs don't need end padding because they can never appear
        // consecutively in arrays
        if def.kind != StructKind::Class {
            let pad_end = Self::padding(offset, max_align);
            for _ in 0..pad_end {
                members.push(StructLayoutMember::PaddingByte);
            }
        }

        Ok(members)
    }

    fn padding(offset: usize, align: usize) -> usize {
        if offset > 0 && align > 1 {
            match offset % align {
                0 => 0,
                diff => align - diff,
            }
        } else {
            0
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum StructLayoutMember<'a> {
    Data {
        member: &'a StructMember<TypeAnnotation>,
        size: usize,
    },
    PaddingByte,
}
