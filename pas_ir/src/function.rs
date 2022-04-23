use std::{
    fmt,
    rc::Rc
};
use pas_common::span::Span;
use pas_syn::{ast, Ident, IdentPath};
use pas_syn::ast::FunctionParamMod;
use crate::{Builder, CLOSURE_PTR_FIELD, EXIT_LABEL, FunctionID, Instruction, jmp_exists, LocalID, Module, pas_ty, Ref, RETURN_REF, translate_block, translate_literal, Type, TypeDefID, VirtualTypeID};

#[derive(Clone, Debug)]
pub struct ExternalFunctionRef {
    pub symbol: String,
    pub src: String,

    pub return_ty: Type,
    pub params: Vec<Type>,

    pub src_span: Span,
}

pub const BUILTIN_SRC: &str = "rt";

#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub debug_name: String,

    pub body: Vec<Instruction>,
    pub return_ty: Type,
    pub params: Vec<Type>,

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
}

#[derive(Clone, Debug)]
pub struct FunctionInstance {
    pub id: FunctionID,
    pub sig: Rc<pas_ty::FunctionSig>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum FunctionDeclKey {
    Function {
        name: IdentPath,
    },
    Method(MethodDeclKey),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct MethodDeclKey {
    pub iface: IdentPath,
    pub self_ty: pas_ty::Type,
    pub method: Ident,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct FunctionDefKey {
    pub decl_key: FunctionDeclKey,
    pub type_args: Option<pas_ty::TypeList>,
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
    type_params: Option<&pas_ty::TypeParamList>,
    type_args: Option<pas_ty::TypeList>,
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
    def_params: &[pas_ty::ast::FunctionParam],
    def_type_params: Option<&pas_ty::TypeParamList>,
    def_type_args: Option<pas_ty::TypeList>,
    def_return_ty: Option<&pas_ty::Type>,
    def_locals: &[pas_ty::ast::FunctionLocalDecl],
    def_body: &pas_ty::ast::Block,
    src_span: Span,
    debug_name: String,
) -> FunctionDef {
    let mut body_builder = create_function_body_builder(module, def_type_params, def_type_args);

    let return_ty = bind_function_return(def_return_ty, &mut body_builder);

    let bound_params = bind_function_params(def_params, &mut body_builder);

    init_function_locals(def_locals, &mut body_builder);

    build_func_body(def_body, &return_ty, &mut body_builder);

    let body = body_builder.finish();

    FunctionDef {
        body,
        params: bound_params.into_iter().map(|(_id, ty)| ty).collect(),
        return_ty,
        debug_name,
        src_span,
    }
}

pub fn build_closure_function_def(
    module: &mut Module,
    func_def: &pas_ty::ast::AnonymousFunctionDef,
    closure_id: TypeDefID,
    src_span: Span,
    debug_name: String,
) -> FunctionDef {
    let closure_def = module.metadata.get_struct_def(closure_id).cloned().unwrap();
    let closure_ptr_ty = Type::RcPointer(VirtualTypeID::Class(closure_id));

    let mut body_builder = create_function_body_builder(module, None, None);

    let return_ty = bind_function_return(func_def.return_ty.as_ref(), &mut body_builder);

    // the pointer to the closure is included as a param but *not* bound since it can't be
    // accessed directly from code, so allocate its id before any params
    let closure_ptr_local_id = body_builder.bind_closure_ptr(closure_ptr_ty.clone());

    let mut bound_params = bind_function_params(&func_def.params, &mut body_builder);

    bound_params.insert(0, (closure_ptr_local_id, closure_ptr_ty.clone()));

    // copy closure members into the body scope
    // the order doesn't need to match the closure struct field order (although it probably
    // will), we just need to ensure all captures are bound to unique locals before we
    // start letting the body code allocate its own locals
    for (field_id, field_def) in closure_def.fields.iter() {
        if *field_id == CLOSURE_PTR_FIELD {
            continue;
        }

        let capture_val_ptr_ty = field_def.ty.clone().ptr();
        let capture_val_ptr_ref =
            body_builder.local_closure_capture(capture_val_ptr_ty, field_def.name.clone());

        body_builder.field(
            capture_val_ptr_ref.clone(),
            Ref::Local(closure_ptr_local_id),
            closure_ptr_ty.clone(),
            *field_id,
        );
    }

    build_func_body(&func_def.body, &return_ty, &mut body_builder);

    let body = body_builder.finish();

    FunctionDef {
        body,
        params: bound_params.into_iter().map(|(_, ty)| ty).collect(),
        return_ty,
        debug_name,
        src_span,
    }
}

fn bind_function_return(return_ty: Option<&pas_ty::Type>, builder: &mut Builder) -> Type {
    match return_ty {
        None | Some(pas_ty::Type::Nothing) => Type::Nothing,
        Some(return_ty) => {
            let return_ty = builder.translate_type(return_ty);

            // anonymous return binding at %0
            builder.comment(&format!(
                "{} = {} (return slot)",
                LocalID(0),
                builder.pretty_ty_name(&return_ty),
            ));

            builder.bind_return(return_ty.clone());
            return_ty
        },
    }
}

fn bind_function_params(
    params: &[pas_ty::ast::FunctionParam],
    builder: &mut Builder,
) -> Vec<(LocalID, Type)> {
    let mut bound_params = Vec::with_capacity(params.len());

    for param in params.iter() {
        let id = builder.next_local_id();

        let (param_ty, by_ref) = match &param.modifier {
            Some(ast::FunctionParamMod::Var) | Some(ast::FunctionParamMod::Out) => {
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

fn init_function_locals(
    locals: &[pas_ty::ast::FunctionLocalDecl],
    builder: &mut Builder,
) {
    for local in locals {
        if local.kind == pas_syn::ast::FunctionLocalDeclKind::Var {
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
    body: &pas_ty::ast::Block,
    return_ty: &Type,
    builder: &mut Builder,
) {
    let body_block_out_ref = match return_ty {
        Type::Nothing => Ref::Discard,
        _ => RETURN_REF.clone(),
    };

    translate_block(&body, body_block_out_ref, builder);

    // all functions should finish with the reserved EXIT label but to
    // avoid writing unused label instructions, if none of the other instructions in the body
    // are jumps to the exit label, we can elide it
    if jmp_exists(builder.instructions(), EXIT_LABEL) {
        builder.label(EXIT_LABEL);
    }
}

pub fn translate_func_params(sig: &pas_ty::FunctionSig, module: &mut Module) -> Vec<Type> {
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