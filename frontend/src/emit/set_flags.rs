use crate::emit::builder::Builder;
use crate::emit::module_builder::ModuleBuilder;
use crate::ir;
use crate::typ::builtin_span;

pub const WORD_TYPE: ir::Type = ir::Type::U64;
const WORD_BITS: usize = u64::BITS as usize;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct SetFlagsType {
    pub struct_id: ir::TypeDefID,

    // procedure Include(self: ^Self; bit: UInt8);
    pub include_func: ir::FunctionID,

    // procedure Exclude(self: ^Self; bit: UInt8);
    pub exclude_func: ir::FunctionID,
    
    // function Contains(self: ^Self, bit: UInt8): Boolean;
    pub contains_func: ir::FunctionID,
}

pub fn set_word_count(bit_count: usize) -> usize {
    usize::div_ceil(bit_count, WORD_BITS)
}

impl SetFlagsType {
    // full-size 256-bit flag struct, the max number of values supported by
    // delphi/FPC sets
    pub fn define_new(module: &mut ModuleBuilder, bit_count: usize) -> Self {
        let word_count = set_word_count(bit_count);

        let set_flags_struct = ir::Struct {
            identity: ir::StructIdentity::SetFlags { bits: bit_count },
            src_span: None,
            fields: (0..word_count)
                .map(|id| (ir::FieldID(id), ir::StructFieldDef {
                    name: None,
                    ty: WORD_TYPE,
                    rc: false,
                }))
                .collect()
        };

        let struct_id = module.metadata_mut().reserve_new_struct();
        module.metadata_mut().define_struct(struct_id, set_flags_struct);
        
        let include_func = Self::define_include(struct_id, word_count, module);
        let exclude_func = Self::define_exclude(struct_id, word_count, module);
        let contains_func = Self::define_contains(struct_id, word_count, module);
        
        Self {
            struct_id,
            include_func,
            exclude_func,
            contains_func,
        }
    }
    
    fn define_include(
        struct_id: ir::TypeDefID,
        word_count: usize,
        module: &mut ModuleBuilder
    ) -> ir::FunctionID {
        let struct_ty = ir::Type::Struct(struct_id);

        let mut builder = Builder::new(module);
        builder.bind_param(ir::LocalID(0), struct_ty.clone().ptr(), "flags", false);
        builder.bind_param(ir::LocalID(1), ir::Type::U8, "bit", false);

        let (word_ref, word_bit) = Self::find_word_bit(&mut builder, 0, word_count, struct_ty.clone());

        let word_mask = builder.local_temp(WORD_TYPE);

        // word_ref := word_ref | (1 shl (word_bit as u64))  
        builder.cast(word_mask.clone(), word_bit, WORD_TYPE);
        builder.shl(word_mask.clone(), ir::Value::LiteralU64(1), word_mask.clone());
        builder.bit_or(word_ref.clone(), word_mask.clone(), word_ref);
        
        let func = ir::Function::Local(ir::FunctionDef {
            src_span: builtin_span(),
            sig: ir::FunctionSig {
                return_ty: ir::Type::Nothing,
                param_tys: vec![
                    ir::Type::Struct(struct_id).ptr(),
                    ir::Type::U8,
                ]
            },
            debug_name: format!("Include ({}-bit flags)", word_count * WORD_BITS),
            body: builder.finish(),
        });
        
        let func_id = module.metadata_mut().insert_func(None);
        module.insert_func(func_id, func);
        
        func_id
    }

    fn define_exclude(struct_id: ir::TypeDefID, word_count: usize, module: &mut ModuleBuilder) -> ir::FunctionID {
        let struct_ty = ir::Type::Struct(struct_id);

        let mut builder = Builder::new(module);
        builder.bind_param(ir::LocalID(0), struct_ty.clone().ptr(), "flags", false);
        builder.bind_param(ir::LocalID(1), ir::Type::U8, "bit", false);

        let (word_ref, word_bit) = Self::find_word_bit(&mut builder, 0, word_count, struct_ty.clone());

        let word_mask = builder.local_temp(WORD_TYPE);

        // word_ref := word_ref & ~(1 shl (word_bit as u64))  
        builder.cast(word_mask.clone(), word_bit, WORD_TYPE);
        builder.shl(word_mask.clone(), ir::Value::LiteralU64(1), word_mask.clone());
        builder.bit_not(word_mask.clone(), word_mask.clone());
        builder.bit_and(word_ref.clone(), word_mask.clone(), word_ref);

        let func = ir::Function::Local(ir::FunctionDef {
            src_span: builtin_span(),
            sig: ir::FunctionSig {
                return_ty: ir::Type::Nothing,
                param_tys: vec![
                    ir::Type::Struct(struct_id).ptr(),
                    ir::Type::U8,
                ]
            },
            debug_name: format!("Exclude ({}-bit flags)", word_count * WORD_BITS),
            body: builder.finish(),
        });

        let func_id = module.metadata_mut().insert_func(None);
        module.insert_func(func_id, func);

        func_id
    }

    fn define_contains(struct_id: ir::TypeDefID, word_count: usize, module: &mut ModuleBuilder) -> ir::FunctionID {
        let struct_ty = ir::Type::Struct(struct_id);

        let mut builder = Builder::new(module);
        builder.bind_return();
        builder.bind_param(ir::LocalID(1), struct_ty.clone().ptr(), "flags", false);
        builder.bind_param(ir::LocalID(2), ir::Type::U8, "bit", false);
        
        let (word_ref, word_bit) = Self::find_word_bit(&mut builder, 1, word_count, struct_ty.clone());

        let word_mask = builder.local_temp(WORD_TYPE);

        // word_mask := 1 shl (word_bit as u64)
        builder.cast(word_mask.clone(), word_bit, WORD_TYPE);
        builder.shl(word_mask.clone(), ir::Value::LiteralU64(1), word_mask.clone());
        
        // result := (word_mask & word_ref) = word_mask
        let val_and_mask = builder.local_temp(WORD_TYPE);
        builder.bit_and(val_and_mask.clone(), word_mask.clone(), word_ref);
        builder.eq(ir::RETURN_REF, word_mask, val_and_mask.clone());

        let func = ir::Function::Local(ir::FunctionDef {
            src_span: builtin_span(),
            sig: ir::FunctionSig {
                return_ty: ir::Type::Bool,
                param_tys: vec![
                    ir::Type::Struct(struct_id).ptr(),
                    ir::Type::U8,
                ]
            },
            debug_name: format!("Contains ({}-bit flags)", word_count * WORD_BITS),
            body: builder.finish(),
        });

        let func_id = module.metadata_mut().insert_func(None);
        module.insert_func(func_id, func);

        func_id
    }
    
    // expected locals:
    // %0: pointer to a struct with fields of consecutive 64-bit words
    // %1: U8 bit number in the 0-255 range 
    // returns (ref to 64-bit word, bit in the 0-63 range) 
    fn find_word_bit(
        builder: &mut Builder,
        first_arg: usize,
        word_count: usize,
        struct_ty: ir::Type
    ) -> (ir::Ref, ir::Value) {
        let result = builder.local_temp(WORD_TYPE.ptr());
        let skip_flag = builder.local_temp(ir::Type::Bool);
        
        let self_ptr_arg = ir::Ref::Local(ir::LocalID(first_arg));
        let bit_arg = ir::Ref::Local(ir::LocalID(first_arg + 1));
        
        let break_label = builder.alloc_label();
        
        let word_bit = builder.local_temp(ir::Type::U8);

        // this doesn't have to be super smart for now
        for word in 0..word_count {
            let skip_label = if word < word_count - 1 {
                let skip_label = builder.alloc_label();
                let next_word_start = ir::Value::LiteralU8(((word + 1) * WORD_BITS) as u8);;

                builder.gte(skip_flag.clone(), bit_arg.clone(), next_word_start.clone());
                builder.jmp_if(skip_label, skip_flag.clone());
                Some(skip_label)
            } else {
                None
            };
            
            let self_ref = self_ptr_arg.clone().to_deref(); 
            let field_id = ir::FieldID(word);

            builder.field(result.clone(), self_ref, struct_ty.clone(), field_id);
            
            if word > 0 {
                let word_start = ir::Value::LiteralU8((word * WORD_BITS) as u8);
                builder.sub(word_bit.clone(), bit_arg.clone(), word_start);
            } else {
                builder.mov(word_bit.clone(), bit_arg.clone());
            }

            builder.jmp(break_label);

            if let Some(skip_label) = skip_label {
                builder.label(skip_label);
            }
        }
        
        builder.label(break_label);

        (result.to_deref(), ir::Value::from(word_bit))
    }
}
