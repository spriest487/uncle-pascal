use crate::jmp_exists;
use crate::metadata::FunctionSig;
use crate::syn;
use crate::syn::FunctionParamMod;
use crate::syn::Ident;
use crate::syn::IdentPath;
use crate::translate_block;
use crate::translate_literal;
use crate::typ;
use crate::Builder;
use crate::ClosureInstance;
use crate::FunctionID;
use crate::GlobalRef;
use crate::Instruction;
use crate::LocalID;
use crate::Module;
use crate::Ref;
use crate::Type;
use crate::TypeDefID;
use crate::Value;
use crate::CLOSURE_PTR_FIELD;
use crate::EXIT_LABEL;
use crate::RETURN_REF;
use common::span::Span;
use std::fmt;
use std::iter;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct ExternalFunctionRef {
    pub symbol: String,
    pub src: String,

    pub sig: FunctionSig,

    pub src_span: Span,
}

pub const BUILTIN_SRC: &str = "rt";

#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub debug_name: String,

    pub body: Vec<Instruction>,

    pub sig: FunctionSig,

    pub src_span: Span,
}

#[derive(Clone, Debug)]
pub enum Function {
    External(ExternalFunctionRef),
    Local(FunctionDef),
}

impl Function {
    pub fn debug_name(&self) -> &str {
        match self {
            Function::External(ExternalFunctionRef { symbol, .. }) => symbol.as_str(),
            Function::Local(FunctionDef { debug_name, .. }) => debug_name.as_str(),
        }
    }

    pub fn sig(&self) -> &FunctionSig {
        match self {
            Function::External(external_func) => &external_func.sig,
            Function::Local(local_func) => &local_func.sig,
        }
    }

    pub fn src_span(&self) -> &Span {
        match self {
            Function::External(external_func) => &external_func.src_span,
            Function::Local(local_func) => &local_func.src_span,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionInstance {
    pub id: FunctionID,
    pub sig: Rc<typ::FunctionSig>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum FunctionDeclKey {
    Function { name: IdentPath },
    Method(MethodDeclKey),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDeclKey {
    pub iface: IdentPath,
    pub self_ty: typ::Type,
    pub method: Ident,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct FunctionDefKey {
    pub decl_key: FunctionDeclKey,
    pub type_args: Option<typ::TypeList>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct StaticClosureID(pub usize);

impl fmt::Display for StaticClosureID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "static closure #{}", self.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StaticClosure {
    pub id: StaticClosureID,
    pub init_func: FunctionID,

    pub closure_id: TypeDefID,
    pub func_ty_id: TypeDefID,
}

fn create_function_body_builder<'m>(
    module: &'m mut Module,
    type_params: Option<&typ::TypeParamList>,
    type_args: Option<typ::TypeList>,
) -> Builder<'m> {
    match type_args {
        Some(type_args) => {
            let type_params = match type_params {
                Some(params) if params.len() == type_args.len() => params,
                Some(params) => panic!(
                    "type args in function body don't match params! expected {}, got {}",
                    params, type_args
                ),
                _ => panic!(
                    "type args in function body don't match params! expected nothing, got {}",
                    type_args
                ),
            };

            let mut builder = Builder::new(module).with_type_args(type_args.clone());
            builder.comment("function def body with type args:");
            for (type_param, type_arg) in type_params.iter().zip(type_args.iter()) {
                builder.comment(&format!("{} = {}", type_param, type_arg));
            }
            builder
        },
        None => Builder::new(module),
    }
}

pub fn build_func_def(
    module: &mut Module,
    def_params: &[typ::ast::FunctionParam],
    def_type_params: Option<&typ::TypeParamList>,
    def_type_args: Option<typ::TypeList>,
    def_return_ty: Option<&typ::Type>,
    def_locals: &[typ::ast::FunctionLocalDecl],
    def_body: &typ::ast::Block,
    src_span: Span,
    debug_name: String,
) -> FunctionDef {
    let mut body_builder = create_function_body_builder(module, def_type_params, def_type_args);

    let return_ty = bind_function_return(def_return_ty, &mut body_builder);

    let bound_params = bind_function_params(def_params, &mut body_builder);

    init_function_locals(def_locals, &mut body_builder);

    let body = build_func_body(def_body, &return_ty, body_builder);

    FunctionDef {
        body,
        sig: FunctionSig {
            param_tys: bound_params.into_iter().map(|(_id, ty)| ty).collect(),
            return_ty,
        },
        debug_name,
        src_span,
    }
}

pub fn build_func_static_closure_def(
    module: &mut Module,
    target_func: &FunctionInstance,
    target_ir_func: &Function,
) -> FunctionDef {
    let src_span = target_ir_func.src_span().clone();

    let params = target_func
        .sig
        .params
        .iter()
        .enumerate()
        .map(|(index, sig_param)| syn::FunctionParam {
            span: src_span.clone(),
            modifier: sig_param.modifier,
            ty: sig_param.ty.clone(),
            ident: Ident::new(&format!("P{index}"), src_span.clone()),
        })
        .collect::<Vec<_>>();

    let mut body_builder = create_function_body_builder(module, None, None);

    let bind_return_ty = match &target_func.sig.return_ty {
        typ::Type::Nothing => None,
        ty => Some(ty),
    };

    let return_ty = bind_function_return(bind_return_ty, &mut body_builder);

    let closure_ptr_local_id = body_builder.bind_closure_ptr();

    // this method needs to be compatible with the type-erased function pointer stored in a
    // closure struct, which has the sig "(Pointer, ...actual params)"
    let mut bound_params = bind_function_params(&params, &mut body_builder);
    bound_params.insert(0, (closure_ptr_local_id, Type::Nothing.ptr()));

    let func_global = Ref::Global(GlobalRef::Function(target_func.id));
    let func_args = bound_params
        .iter()
        .skip(1)
        .map(|(local_id, _)| Value::Ref(Ref::Local(*local_id)))
        .collect::<Vec<_>>();

    let return_ref = match return_ty {
        Type::Nothing => None,
        _ => Some(RETURN_REF),
    };

    body_builder.call(func_global, func_args, return_ref);

    FunctionDef {
        sig: FunctionSig {
            param_tys: bound_params
                .into_iter()
                .map(|(_, param_ty)| param_ty)
                .collect(),
            return_ty,
        },
        src_span,
        debug_name: format!("{} static closure", target_func.id),
        body: body_builder.finish(),
    }
}

pub fn build_closure_function_def(
    module: &mut Module,
    func_def: &typ::ast::AnonymousFunctionDef,
    closure_id: TypeDefID,
    src_span: Span,
    debug_name: String,
) -> FunctionDef {
    let closure_def = module.metadata.get_struct_def(closure_id).cloned().unwrap();

    let mut body_builder = create_function_body_builder(module, None, None);

    let return_ty = bind_function_return(func_def.return_ty.as_ref(), &mut body_builder);

    // the type-erased pointer to the closure struct is included as the 0th param but
    // *not* bound like a normal param since it can't be named from code, so bind it in the scope
    // of this function body now
    let closure_ptr_param_local_id = body_builder.bind_closure_ptr();
    let closure_ptr_param_ref = Ref::Local(closure_ptr_param_local_id);

    let bound_params = bind_function_params(&func_def.params, &mut body_builder);

    // cast the closure pointer param from the erased pointer passed in to its actual internal type
    let closure_struct_ty = Type::Struct(closure_id);
    let closure_struct_ptr_ty = closure_struct_ty.clone().ptr();
    let closure_ptr_ref = body_builder.local_temp(closure_struct_ptr_ty.clone());
    body_builder.cast(
        closure_ptr_ref.clone(),
        closure_ptr_param_ref,
        closure_struct_ptr_ty,
    );

    // copy closure members into the body scope
    // the order doesn't need to match the closure struct field order (although it probably
    // will), we just need to ensure all captures are bound to unique locals before we
    // start letting the body code allocate its own locals
    for (field_id, field_def) in closure_def.fields.iter() {
        if *field_id == CLOSURE_PTR_FIELD {
            continue;
        }

        let field_name = match field_def.name.as_ref() {
            None => continue,
            Some(name) => name,
        };

        let capture_val_ptr_ty = field_def.ty.clone().ptr();
        let capture_val_ptr_ref =
            body_builder.local_closure_capture(capture_val_ptr_ty, field_name.clone());

        body_builder.field(
            capture_val_ptr_ref.clone(),
            closure_ptr_ref.clone().to_deref(),
            closure_struct_ty.clone(),
            *field_id,
        );
    }

    let body = build_func_body(&func_def.body, &return_ty, body_builder);

    // the 0th parameter of the function is always a type-erased pointer, which we must
    // cast to the actual closure struct type in the body
    let actual_params = iter::once(Type::Nothing.ptr())
        .chain(bound_params.into_iter().map(|(_, param_ty)| param_ty))
        .collect();

    FunctionDef {
        body,
        sig: FunctionSig {
            param_tys: actual_params,
            return_ty,
        },
        debug_name,
        src_span,
    }
}

fn bind_function_return(return_ty: Option<&typ::Type>, builder: &mut Builder) -> Type {
    match return_ty {
        None | Some(typ::Type::Nothing) => Type::Nothing,
        Some(return_ty) => {
            let return_ty = builder.translate_type(return_ty);

            // anonymous return binding at %0
            builder.comment(&format!(
                "{} = {} (return slot)",
                LocalID(0),
                builder.pretty_ty_name(&return_ty),
            ));

            builder.bind_return();
            return_ty
        },
    }
}

fn bind_function_params(
    params: &[typ::ast::FunctionParam],
    builder: &mut Builder,
) -> Vec<(LocalID, Type)> {
    let mut bound_params = Vec::with_capacity(params.len());

    for param in params.iter() {
        let id = builder.next_local_id();

        let (param_ty, by_ref) = match &param.modifier {
            Some(FunctionParamMod::Var) | Some(FunctionParamMod::Out) => {
                (builder.translate_type(&param.ty).ptr(), true)
            },

            None => (builder.translate_type(&param.ty), false),
        };

        builder.comment(&format!("{} = {}", id, builder.pretty_ty_name(&param_ty)));
        builder.bind_param(id, param_ty.clone(), param.ident.to_string(), by_ref);

        bound_params.push((id, param_ty));
    }

    for (id, ty) in &bound_params {
        builder.retain(Ref::Local(*id), ty);
    }

    bound_params
}

fn init_function_locals(locals: &[typ::ast::FunctionLocalDecl], builder: &mut Builder) {
    for local in locals {
        if local.kind == syn::BindingDeclKind::Var {
            let ty = builder.translate_type(&local.ty);

            let local_ref = builder.local_new(ty, Some(local.ident.name.to_string()));

            if let Some(initial_val) = &local.initial_val {
                let init_val = translate_literal(initial_val, &local.ty, builder);
                builder.mov(local_ref, init_val);
            }
        }
    }
}

fn build_func_body(
    body: &typ::ast::Block,
    return_ty: &Type,
    mut builder: Builder,
) -> Vec<Instruction> {
    let body_block_out_ref = match return_ty {
        Type::Nothing => Ref::Discard,
        _ => RETURN_REF.clone(),
    };

    translate_block(&body, body_block_out_ref, &mut builder);

    let mut instructions = builder.finish();

    // all functions should finish with the reserved EXIT label but to
    // avoid writing unused label instructions, if none of the other instructions in the body
    // are jumps to the exit label, we can elide it
    if jmp_exists(&instructions, EXIT_LABEL) {
        instructions.push(Instruction::Label(EXIT_LABEL));
    }

    instructions
}

pub fn translate_func_params(sig: &typ::FunctionSig, module: &mut Module) -> Vec<Type> {
    sig.params
        .iter()
        .map(|sig_param| {
            let param_ty = module.translate_type(&sig_param.ty, None);
            match sig_param.modifier {
                None => param_ty,
                Some(FunctionParamMod::Var | FunctionParamMod::Out) => param_ty.ptr(),
            }
        })
        .collect()
}

pub fn build_static_closure_impl(
    closure: ClosureInstance,
    id: StaticClosureID,
    module: &mut Module,
) -> StaticClosure {
    let mut init_builder = Builder::new(module);

    let static_closure_ptr_ref = Ref::Global(GlobalRef::StaticClosure(id));

    let closure_ref = init_builder.build_closure_instance(closure.clone());
    init_builder.retain(closure_ref.clone(), &closure.closure_ptr_ty());
    init_builder.mov(static_closure_ptr_ref, closure_ref);

    let init_body = init_builder.finish();

    let init_func_id = module.metadata.insert_func(None);
    module.insert_func(
        init_func_id,
        Function::Local(FunctionDef {
            body: init_body,
            debug_name: format!("static closure init for {}", closure),
            sig: FunctionSig {
                param_tys: Vec::new(),
                return_ty: Type::Nothing,
            },
            src_span: Span::zero(""),
        }),
    );

    StaticClosure {
        id,
        init_func: init_func_id,
        closure_id: closure.closure_id,
        func_ty_id: closure.func_ty_id,
    }
}
