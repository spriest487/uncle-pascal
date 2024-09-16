use crate::ir;
use crate::metadata::Metadata;
use crate::module_builder::ModuleBuilder;
use crate::syn;
use crate::typ;
use crate::ClosureIdentity;
use crate::FunctionInstance;
use crate::Struct;
use crate::StructFieldDef;
use crate::StructIdentity;
use linked_hash_map::LinkedHashMap;
use std::fmt;
use syn::Ident;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FunctionSig {
    pub return_ty: ir::Type,
    pub param_tys: Vec<ir::Type>,
}

impl fmt::Display for FunctionSig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "function(")?;
        for (i, param_ty) in self.param_tys.iter().enumerate() {
            if i > 0 {
                write!(f, "; ")?;
            }
            write!(f, "{}", param_ty)?;
        }
        write!(f, "): {}", self.return_ty)?;

        Ok(())
    }
}

impl FunctionSig {
    pub fn translate(
        sig: &typ::FunctionSig,
        type_args: Option<&typ::TypeList>,
        module: &mut ModuleBuilder,
    ) -> Self {
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
    
    pub fn to_pretty(&self, metadata: &Metadata) -> String {
        let mut pretty = String::new();

        pretty.push_str("function(");

        for (i, param_ty) in self.param_tys.iter().enumerate() {
            if i > 0 {
                pretty.push_str("; ");
            }
            
            pretty.push_str(metadata.pretty_ty_name(param_ty).as_ref());
        }
        
        pretty.push_str("): ");
        pretty.push_str(metadata.pretty_ty_name(&self.return_ty).as_ref());

        pretty
    }
}

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
    identity: ClosureIdentity,
    captures: &LinkedHashMap<Ident, typ::Type>,
    type_args: Option<&typ::TypeList>,
    module: &mut ModuleBuilder,
) -> ir::TypeDefID {
    let id = module.metadata_mut().reserve_new_struct();
    
    let src_span = identity.src_span();

    let mut fields = LinkedHashMap::new();
    fields.insert(
        ir::CLOSURE_PTR_FIELD,
        StructFieldDef {
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
            StructFieldDef {
                name: Some((*capture_name.name).clone()),
                ty,
                rc: capture_ty.is_rc_reference(),
            },
        );

        field_id.0 += 1;
    }

    module.metadata_mut().define_closure_ty(
        id,
        Struct {
            identity: StructIdentity::Closure(identity),
            src_span: Some(src_span),
            fields,
        },
    );

    id
}
