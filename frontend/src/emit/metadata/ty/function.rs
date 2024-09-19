use crate::emit::ir;
use crate::emit::module_builder::ModuleBuilder;
use crate::emit::syn;
use crate::emit::typ;
use crate::emit::FunctionInstance;
use linked_hash_map::LinkedHashMap;
use std::fmt;
use ir_lang::FunctionSig;
use syn::Ident;

#[derive(Debug, Clone)]
pub struct ClosureInstance {
    // the function containing the closure's body
    pub func_instance: FunctionInstance,
    
    // ID of the function type (not the closure function but the target type)
    pub func_ty_id: ir::TypeDefID,
    
    // ID of the implementation struct type of this closure
    pub closure_id: ir::TypeDefID,
}

impl ClosureInstance {
    pub fn closure_ptr_ty(&self) -> ir::Type {
        ir::Type::RcPointer(ir::VirtualTypeID::Closure(self.func_ty_id))
    }
}

impl fmt::Display for ClosureInstance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "closure {} of {} ({})",
            self.closure_id, self.func_ty_id, self.func_instance.sig
        )
    }
}

pub fn translate_closure_struct(
    identity: ir::ClosureIdentity,
    captures: &LinkedHashMap<Ident, typ::Type>,
    type_args: Option<&typ::TypeList>,
    module: &mut ModuleBuilder,
) -> ir::TypeDefID {
    let id = module.metadata_mut().reserve_new_struct();
    
    let src_span = identity.src_span();

    let mut fields = LinkedHashMap::new();
    fields.insert(
        ir::CLOSURE_PTR_FIELD,
        ir::StructFieldDef {
            name: None,
            rc: false,
            ty: ir::Type::Function(identity.virt_func_ty),
        },
    );

    let mut field_id = ir::FieldID(ir::CLOSURE_PTR_FIELD.0 + 1);

    for (capture_name, capture_ty) in captures {
        let ty = module.translate_type(capture_ty, type_args);

        fields.insert(
            field_id,
            ir::StructFieldDef {
                name: Some((*capture_name.name).clone()),
                ty,
                rc: capture_ty.is_rc_reference(),
            },
        );

        field_id.0 += 1;
    }

    module.metadata_mut().define_closure_ty(
        id,
        ir::Struct {
            identity: ir::StructIdentity::Closure(identity),
            src_span: Some(src_span),
            fields,
        },
    );

    id
}

pub fn translate_sig(
    sig: &typ::FunctionSig,
    type_args: Option<&typ::TypeList>,
    module: &mut ModuleBuilder,
) -> FunctionSig {
    assert!(
        sig.type_params.is_none(),
        "cannot create type for a generic function pointer"
    );

    let return_ty = module.translate_type(&sig.return_ty, type_args);
    let mut param_tys = Vec::new();
    for param in &sig.params {
        let mut ty = module.translate_type(&param.ty, type_args);
        if param.is_by_ref() {
            ty = ty.ptr();
        }

        param_tys.push(ty);
    }

    FunctionSig {
        return_ty,
        param_tys,
    }
}