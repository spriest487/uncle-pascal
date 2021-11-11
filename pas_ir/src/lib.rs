use std::{collections::hash_map::HashMap, fmt};

pub use self::{formatter::*, instruction::*, metadata::ty::Type};
use crate::ty::{ClassID, FieldID, TypeDef};
use crate::{builder::Builder, expr::*, metadata::*, stmt::*};
use linked_hash_map::LinkedHashMap;
use pas_common::span::*;
use pas_syn::{ast, IdentPath};
use pas_ty::ast::specialize_func_decl;
use pas_typecheck as pas_ty;

mod builder;
mod dep_sort;
mod expr;
mod formatter;
mod stmt;

pub mod prelude {
    pub use crate::{
        instruction::*, metadata::ty::*, metadata::*, GlobalRef, Instruction, Label, Ref, Value,
        RETURN_REF,
    };
}

pub mod metadata;

pub const RETURN_REF: Ref = Ref::Local(LocalID(0));
pub const EXIT_LABEL: Label = Label(0);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IROptions {
    pub annotate_scopes: bool,
    pub annotate_stmts: bool,
    pub annotate_rc: bool,
}

impl Default for IROptions {
    fn default() -> Self {
        Self {
            annotate_scopes: false,
            annotate_stmts: false,
            annotate_rc: false,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct LocalID(pub usize);

impl fmt::Display for LocalID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum GlobalRef {
    Function(FunctionID),
    StringLiteral(StringID),
}

impl fmt::Display for GlobalRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GlobalRef::Function(func_id) => write!(f, "{}", func_id),
            GlobalRef::StringLiteral(id) => write!(f, "string `{}`", id),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ref {
    Discard, // write-only ref that doesn't result in mov instructions when written to
    Local(LocalID),
    Global(GlobalRef),
    Deref(Box<Value>),
}

impl Ref {
    pub fn to_deref(self) -> Self {
        Ref::Deref(Box::new(Value::Ref(self)))
    }
}

impl fmt::Display for Ref {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ref::Discard => write!(f, "_"),
            Ref::Local(id) => write!(f, "{}", id),
            Ref::Global(name) => write!(f, "{}", name),
            Ref::Deref(at) => write!(f, "{}^", at),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Ref(Ref),
    LiteralNull,
    LiteralBool(bool),
    LiteralI32(i32),
    LiteralF32(f32),
    LiteralByte(u8),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Ref(r) => write!(f, "{}", r),
            Value::LiteralByte(i) => write!(f, "{}u8", i),
            Value::LiteralI32(i) => write!(f, "{}i32", i),
            Value::LiteralBool(b) => write!(f, "{}", b),
            Value::LiteralF32(x) => write!(f, "{:.6}", x),
            Value::LiteralNull => write!(f, "NULL"),
        }
    }
}

impl From<Ref> for Value {
    fn from(r: Ref) -> Self {
        Value::Ref(r)
    }
}

impl Value {
    pub fn deref(self) -> Ref {
        Ref::Deref(Box::new(self))
    }
}

pub mod instruction;

#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub debug_name: String,

    pub body: Vec<Instruction>,
    pub return_ty: Type,
    pub params: Vec<Type>,
}

#[derive(Clone, Debug)]
pub enum Function {
    External { symbol: String, src: String },
    Local(FunctionDef),
}

impl Function {
    pub fn debug_name(&self) -> &str {
        match self {
            Function::External { symbol, .. } => symbol.as_str(),
            Function::Local(FunctionDef { debug_name, .. }) => debug_name.as_str(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionInstance {
    pub id: FunctionID,
    pub sig: pas_ty::FunctionSig,
}

#[derive(Clone, Debug)]
pub struct Module {
    src_metadata: pas_ty::Context,

    type_cache: LinkedHashMap<pas_ty::Type, Type>,

    pub opts: IROptions,

    pub metadata: Metadata,

    pub functions: HashMap<FunctionID, Function>,
    translated_funcs: HashMap<FunctionCacheKey, FunctionInstance>,

    pub init: Vec<Instruction>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum FunctionDeclKey {
    Function {
        name: IdentPath,
    },
    Method {
        iface: IdentPath,
        self_ty: pas_ty::Type,
        method: pas_syn::Ident,
    },
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct FunctionCacheKey {
    decl_key: FunctionDeclKey,
    type_args: Option<pas_ty::TypeList>,
}

impl Module {
    pub fn new(src_metadata: pas_ty::Context, metadata: Metadata, opts: IROptions) -> Self {
        Self {
            src_metadata,
            type_cache: Default::default(),

            init: Vec::new(),

            opts,

            functions: HashMap::new(),
            translated_funcs: HashMap::new(),

            metadata,
        }
    }

    pub fn translate_unit(&mut self, unit: &pas_ty::ast::Unit) {
        let mut init_builder = Builder::new(self);
        for stmt in &unit.init {
            translate_stmt(stmt, &mut init_builder);
        }
        let unit_init = init_builder.finish();

        self.init.extend(unit_init);
    }

    pub fn class_types(&self) -> impl Iterator<Item = &Type> {
        self.type_cache.iter().filter_map(|(src_ty, ir_ty)| {
            if src_ty.as_class().is_ok() {
                Some(ir_ty)
            } else {
                None
            }
        })
    }

    pub fn insert_func(&mut self, id: FunctionID, function: Function) {
        assert!(
            self.metadata.get_function(id).is_some(),
            "function passed to insert_func must have been previously registered in metadata"
        );

        self.functions.insert(id, function);
    }

    fn instantiate_func(&mut self, key: FunctionCacheKey) -> FunctionInstance {
        if let Some(cached_func) = self.translated_funcs.get(&key) {
            return cached_func.clone();
        }

        match &key.decl_key {
            FunctionDeclKey::Function { name } => {
                match self.src_metadata.find_def(&name).cloned() {
                    Some(pas_ty::Def::Function(func_def)) => {
                        let specialized_decl = match key.type_args.as_ref() {
                            Some(key_type_args) => specialize_func_decl(
                                &func_def.decl,
                                key_type_args,
                                func_def.span(),
                                &self.src_metadata,
                            )
                            .expect("function specialization must be valid after typechecking"),
                            None => func_def.decl.clone(),
                        };

                        let sig = pas_ty::FunctionSig::of_decl(&specialized_decl);

                        let id = self
                            .metadata
                            .declare_func(&func_def.decl, key.type_args.as_ref());

                        // cache the function before translating the instantiation, because
                        // it may recurse and instantiate itself in its own body
                        let cached_func = FunctionInstance { id, sig };
                        self.translated_funcs
                            .insert(key.clone(), cached_func.clone());

                        let debug_name = specialized_decl.to_string();
                        let ir_func =
                            self.translate_func_def(&func_def, key.type_args.clone(), debug_name);

                        self.functions.insert(id, Function::Local(ir_func));

                        cached_func
                    }

                    Some(pas_ty::Def::External(extern_decl)) => {
                        assert!(
                            key.type_args.is_none(),
                            "external function must not be generic"
                        );

                        let id = self
                            .metadata
                            .declare_func(&extern_decl, key.type_args.as_ref());
                        let sig = pas_ty::FunctionSig::of_decl(&extern_decl);
                        let cached_func = FunctionInstance { id, sig };

                        let extern_src = extern_decl
                            .external_src()
                            .expect("function with external def must have an extern src");

                        self.functions.insert(
                            id,
                            Function::External {
                                src: extern_src.to_string(),
                                symbol: extern_decl.ident.last().to_string(),
                            },
                        );
                        self.translated_funcs.insert(key, cached_func.clone());

                        cached_func
                    }

                    _ => panic!("missing source def for function {}", name),
                }
            }

            FunctionDeclKey::Method {
                iface,
                self_ty,
                method,
            } => {
                let method_name = method.to_string();

                let iface_def = match self.src_metadata.find_iface_def(iface) {
                    Ok(def) => def,
                    Err(..) => panic!("missing interface def {}", iface),
                };

                let iface_id = match self.metadata.find_iface_decl(&iface_def.name.qualified) {
                    Some(iface_id) => iface_id,
                    None => {
                        let mut builder = Builder::new(self);
                        let iface_meta = builder.translate_iface(&iface_def);
                        self.metadata.define_iface(iface_meta)
                    }
                };

                let method_def = self
                    .src_metadata
                    .find_method_impl_def(&iface_def.name.qualified, self_ty, method)
                    .cloned()
                    .unwrap_or_else(|| {
                        panic!(
                            "missing method def: {}.{} for {}",
                            iface_def.name.qualified, method, self_ty,
                        )
                    });

                let specialized_decl = match &key.type_args {
                    Some(key_type_args) => specialize_func_decl(
                        &method_def.decl,
                        &key_type_args,
                        &method_def.span(),
                        &self.src_metadata,
                    )
                    .expect("method specialization failed in codegen"),
                    None => method_def.decl.clone(),
                };

                let id = self
                    .metadata
                    .declare_func(&specialized_decl, key.type_args.as_ref());

                let self_ty = self.metadata.find_type(self_ty);

                self.metadata
                    .impl_method(iface_id, self_ty, method_name, id);

                // cache the function before translating the instantiation, because
                // it may recurse and instantiate itself in its own body
                let cached_func = FunctionInstance {
                    id,
                    sig: pas_ty::FunctionSig::of_decl(&specialized_decl),
                };
                self.translated_funcs
                    .insert(key.clone(), cached_func.clone());

                let debug_name = specialized_decl.to_string();
                let ir_func =
                    self.translate_func_def(&method_def, key.type_args.clone(), debug_name);
                self.functions.insert(id, Function::Local(ir_func));

                cached_func
            }
        }
    }

    // statically reference a method and get a function ID. interface methods are all translated
    // at the end of compilation for a module anyway, but for methods that are referenced statically
    // this call reserves us a function ID
    pub fn translate_method_impl(
        &mut self,
        iface: IdentPath,
        method: pas_syn::Ident,
        self_ty: pas_ty::Type,
    ) -> FunctionInstance {
        let key = FunctionCacheKey {
            decl_key: FunctionDeclKey::Method {
                iface,
                method,
                self_ty,
            },

            // dynamic method calls can't have type args
            type_args: None,
        };

        // methods must always be present so make sure they're immediately instantiated
        self.instantiate_func(key)
    }

    pub fn translate_func(
        &mut self,
        func_name: IdentPath,
        type_args: Option<pas_ty::TypeList>,
    ) -> FunctionInstance {
        let key = FunctionCacheKey {
            type_args,
            decl_key: FunctionDeclKey::Function { name: func_name },
        };

        self.instantiate_func(key)
    }

    fn translate_func_def(
        &mut self,
        func: &pas_ty::ast::FunctionDef,
        type_args: Option<pas_ty::TypeList>,
        debug_name: String,
    ) -> FunctionDef {
        let mut body_builder = match type_args {
            Some(type_args) => {
                let type_params = match &func.decl.type_params {
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

                let mut builder = Builder::new(self).with_type_args(type_args.clone());
                builder.comment("function def body with type args:");
                for (type_param, type_arg) in type_params.iter().zip(type_args.iter()) {
                    builder.comment(&format!("{} = {}", type_param, type_arg));
                }
                builder
            }
            None => Builder::new(self),
        };

        let return_ty = match func.decl.return_ty.as_ref() {
            None | Some(pas_ty::Type::Nothing) => Type::Nothing,
            Some(ty) => {
                let return_ty = body_builder.translate_type(ty);

                // anonymous return binding at %0
                body_builder.comment(&format!(
                    "{} = {} (return slot)",
                    LocalID(0),
                    body_builder.pretty_ty_name(&return_ty),
                ));

                body_builder.bind_return(return_ty.clone());
                return_ty
            }
        };

        let param_id_offset = match return_ty {
            Type::Nothing => 0,
            _ => {
                assert!(func.decl.return_ty.is_some());
                1
            }
        };

        let mut bound_params = Vec::with_capacity(func.decl.params.len());

        for (i, param) in func.decl.params.iter().enumerate() {
            // if the function returns a value, $0 is the return pointer, and args start at $1
            let id = LocalID(i + param_id_offset);

            let (param_ty, by_ref) = match &param.modifier {
                Some(ast::FunctionParamMod::Var) | Some(ast::FunctionParamMod::Out) => {
                    (body_builder.translate_type(&param.ty).ptr(), true)
                }

                None => (body_builder.translate_type(&param.ty), false),
            };

            body_builder.comment(&format!(
                "{} = {}",
                id,
                body_builder.pretty_ty_name(&param_ty)
            ));
            body_builder.bind_param(id, param_ty.clone(), param.ident.to_string(), by_ref);

            bound_params.push((id, param_ty));
        }

        for (id, ty) in &bound_params {
            body_builder.retain(Ref::Local(*id), ty);
        }

        let block_output = translate_block(&func.body, &mut body_builder);

        if let Some(return_val) = block_output {
            let return_at = Ref::Local(LocalID(0));
            body_builder.append(Instruction::Move {
                out: return_at.clone(),
                new_val: Value::Ref(return_val),
            });

            // the value we just moved in came from the block output in this function's scope,
            // so that ref is about to be released at the end of the function - we need to retain
            // the return value so it outlives the function
            body_builder.retain(return_at, &return_ty);
        }

        let mut body = body_builder.finish();

        // all functions should finish with the reserved EXIT label but to
        // avoid writing unused label instructions, if none of the other instructions in the body
        // are jumps to the exit label, we can elide it
        if jmp_exists(&body, EXIT_LABEL) {
            body.push(Instruction::Label(EXIT_LABEL));
        }

        FunctionDef {
            body,
            params: bound_params.into_iter().map(|(_id, ty)| ty).collect(),
            return_ty,
            debug_name,
        }
    }

    pub fn find_dyn_array_struct(&self, elem_ty: &Type) -> Option<StructID> {
        self.metadata.find_dyn_array_struct(elem_ty)
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "* Structures")?;
        let mut defs: Vec<_> = self.metadata.type_defs().collect();
        defs.sort_by_key(|(id, _)| *id);

        for (id, def) in &defs {
            match def {
                TypeDef::Struct(s) => {
                    write!(f, "{}: ", id.0)?;
                    self.metadata.format_name(&s.name, f)?;
                    writeln!(f)?;

                    let max_field_id = s.fields.keys().max().cloned().unwrap_or(FieldID(0));
                    let fields = (0..=max_field_id.0).filter_map(|id| {
                        let field = s.fields.get(&FieldID(id))?;
                        Some((id, field))
                    });

                    for (id, field) in fields {
                        write!(f, "{:8>} {}: ", format!("  .{}", id), field.name,)?;
                        self.metadata.format_type(&field.ty, f)?;
                        writeln!(f)?;
                    }

                    let ty_as_struct = Type::Struct(*id);
                    let ty_as_class = Type::RcPointer(Some(ClassID::Class(*id)));
                    let mut iface_impls = self.metadata.impls(&ty_as_struct);
                    iface_impls.extend(self.metadata.impls(&ty_as_class));

                    if !iface_impls.is_empty() {
                        writeln!(f, "interface impls:")?;
                        for iface_id in iface_impls {
                            let iface_ty = Type::RcPointer(Some(ClassID::Interface(iface_id)));
                            write!(f, "  * ")?;
                            self.metadata.format_type(&iface_ty, f)?;
                            writeln!(f)?;
                        }
                    }
                }

                TypeDef::Variant(v) => {
                    writeln!(f, "{}: {}", id, v.name)?;
                    for (i, case) in v.cases.iter().enumerate() {
                        write!(f, "{:8>} ({})", format!("  .{}", i), case.name,)?;

                        if let Some(ty) = &case.ty {
                            write!(f, ": {}", ty)?;
                        }
                        writeln!(f)?;
                    }
                }
            }

            writeln!(f)?;
        }

        writeln!(f, "* Interfaces: ")?;
        let mut ifaces: Vec<_> = self.metadata.ifaces().collect();
        ifaces.sort_by_key(|(id, _)| *id);

        for (id, iface) in &ifaces {
            writeln!(f, "{}: {}", id, iface.name)?;

            for (i, method) in iface.methods.iter().enumerate() {
                let sig_params: Vec<_> = method
                    .params
                    .iter()
                    .map(|param| self.metadata.pretty_ty_name(param))
                    .collect();
                let return_ty = self.metadata.pretty_ty_name(&method.return_ty);

                let sig = format!("({}) -> {}", sig_params.join(", "), return_ty);

                let index = format!("  .{}", i);
                write!(f, "{:8>} ({}): {}", index, method.name, sig)?;
            }
            writeln!(f)?;
        }
        writeln!(f)?;

        writeln!(f, "* String literals")?;
        for (id, lit) in self.metadata.strings() {
            writeln!(f, "{}: '{}'", id.0, lit)?;
        }
        writeln!(f)?;

        let mut funcs: Vec<_> = self.functions.iter().collect();
        funcs.sort_by_key(|(id, _)| **id);

        writeln!(f, "* Functions")?;
        for (id, func) in funcs {
            write!(f, "{}: ", id.0)?;
            match self.metadata.func_desc(*id) {
                Some(desc_name) => {
                    writeln!(f, "{}", desc_name)?;
                }
                None => {
                    writeln!(f, " /* {} */", func.debug_name())?;
                }
            }

            match func {
                Function::Local(FunctionDef { body, .. }) => {
                    write_instruction_list(f, &self.metadata, body)?;
                }

                Function::External { src, symbol } => {
                    writeln!(f, "<external function '{}' in module '{}'>", symbol, src)?;
                }
            }
            writeln!(f)?;
        }

        writeln!(f, "* Init:")?;
        write_instruction_list(f, &self.metadata, &self.init)?;
        Ok(())
    }
}

fn write_instruction_list(
    f: &mut fmt::Formatter,
    metadata: &Metadata,
    instructions: &[Instruction],
) -> fmt::Result {
    let num_len = instructions.len().to_string().len();

    let formatter = StatefulIndentedFormatter::new(metadata, 4);

    for (i, instruction) in instructions.iter().enumerate() {
        write!(f, "{:>width$}|", i, width = num_len)?;
        formatter.format_instruction(instruction, f)?;
        writeln!(f)?;
    }

    Ok(())
}

fn gen_dyn_array_rc_boilerplate(module: &mut Module, elem_ty: &Type, struct_id: StructID) {
    let array_class = ClassID::Class(struct_id);
    let array_ref_ty = Type::RcPointer(Some(array_class));

    // the thing we're cleaning up is the internal struct, not the rc object itself,
    // so the parameter type of the releaser will be a pointer to that struct
    let array_struct_ty = Type::Struct(struct_id);

    let rc_boilerplate = module
        .metadata
        .find_rc_boilerplate(&array_struct_ty)
        .expect("rc boilerplate function ids for dynarray inner struct must exist");

    let mut releaser_builder = Builder::new(module);

    // %0 is the self arg, the pointer to the inner struct
    releaser_builder.bind_param(LocalID(0), array_struct_ty.clone().ptr(), "self", true);
    let self_arg = Ref::Local(LocalID(0)).to_deref();

    let len_field_ptr = releaser_builder.local_temp(Type::I32.ptr());
    releaser_builder.append(Instruction::Field {
        out: len_field_ptr.clone(),
        of_ty: array_struct_ty.clone(),
        field: DYNARRAY_LEN_FIELD,
        a: self_arg.clone(),
    });

    let arr_field_ptr = releaser_builder.local_temp(elem_ty.clone().ptr().ptr());
    releaser_builder.append(Instruction::Field {
        out: arr_field_ptr.clone(),
        of_ty: array_struct_ty.clone(),
        field: DYNARRAY_PTR_FIELD,
        a: self_arg,
    });

    // release every element
    let counter = releaser_builder.local_temp(Type::I32);
    releaser_builder.mov(counter.clone(), Value::LiteralI32(0));

    // jump to loop end if counter == array len
    let start_loop_label = releaser_builder.alloc_label();
    let end_loop_label = releaser_builder.alloc_label();

    releaser_builder.append(Instruction::Label(start_loop_label));

    // at_end := counter eq array.length
    let at_end = releaser_builder.local_temp(Type::Bool);
    releaser_builder.eq(
        at_end.clone(),
        counter.clone(),
        len_field_ptr.clone().to_deref(),
    );

    // if at_end then break
    releaser_builder.append(Instruction::JumpIf {
        dest: end_loop_label,
        test: Value::Ref(at_end),
    });

    // release arr[counter]
    let el_ptr = releaser_builder.local_temp(elem_ty.clone().ptr());
    releaser_builder.append(Instruction::Add {
        out: el_ptr.clone(),
        a: Value::Ref(arr_field_ptr.clone().to_deref()),
        b: Value::Ref(counter.clone()),
    });
    releaser_builder.release(el_ptr.to_deref(), &elem_ty);

    // counter := counter + 1
    releaser_builder.append(Instruction::Add {
        out: counter.clone(),
        a: Value::Ref(counter),
        b: Value::LiteralI32(1),
    });

    releaser_builder.append(Instruction::Jump {
        dest: start_loop_label,
    });
    releaser_builder.append(Instruction::Label(end_loop_label));

    // free the dynamic-allocated buffer - if len > 0
    let after_free = releaser_builder.alloc_label();

    let zero_elements = releaser_builder.local_temp(Type::Bool);
    releaser_builder.append(Instruction::Eq {
        a: Value::Ref(len_field_ptr.clone().to_deref()),
        b: Value::LiteralI32(0),
        out: zero_elements.clone(),
    });
    releaser_builder.append(Instruction::JumpIf {
        dest: after_free,
        test: Value::Ref(zero_elements),
    });

    releaser_builder.append(Instruction::DynFree {
        at: arr_field_ptr.clone().to_deref(),
    });

    releaser_builder.append(Instruction::Label(after_free));

    releaser_builder.mov(len_field_ptr.to_deref(), Value::LiteralI32(0));
    releaser_builder.mov(arr_field_ptr.to_deref(), Value::LiteralNull);

    let releaser_body = releaser_builder.finish();

    let array_ref_ty_name = module.metadata.pretty_ty_name(&array_ref_ty).into_owned();

    module.insert_func(
        rc_boilerplate.release,
        Function::Local(FunctionDef {
            debug_name: format!("<generated dynarray releaser for {}>", array_ref_ty_name),
            return_ty: Type::Nothing,
            params: vec![array_struct_ty.clone().ptr()],
            body: releaser_body,
        }),
    );

    // no custom retain behaviour (dynarrays can't be retained!)
    module.insert_func(
        rc_boilerplate.retain,
        Function::Local(FunctionDef {
            debug_name: format!("<generated empty retainer for {}>", array_ref_ty_name),
            return_ty: Type::Nothing,
            params: vec![array_struct_ty.clone().ptr()],
            body: Vec::new(),
        }),
    );
}

// interface methods may not be statically referenced for every type that implements them due to
// dynamic dispatch, so we need to cover all possible combinations and generate function bodies for
// every interface method implemented by a class at the end of codegen
fn gen_iface_impls(module: &mut Module) {
    let mut last_instance_count = module.type_cache.len();

    // generating an impl might actually reference new types in the body of the
    // function, so just keep doing this until the type cache is a stable size
    loop {
        for real_ty in module.type_cache.keys().cloned().collect::<Vec<_>>() {
            let ifaces = module.src_metadata.implemented_ifaces(&real_ty);

            for iface in &ifaces {
                let iface_def = module.src_metadata.find_iface_def(iface).unwrap();

                for method in &iface_def.methods {
                    let cache_key = FunctionCacheKey {
                        decl_key: FunctionDeclKey::Method {
                            self_ty: real_ty.clone(),
                            method: method.ident.single().clone(),
                            iface: iface.clone(),
                        },
                        type_args: None,
                    };

                    module.instantiate_func(cache_key);
                }
            }
        }

        if module.type_cache.len() == last_instance_count {
            break;
        } else {
            last_instance_count = module.type_cache.len();
        }
    }
}

// class types must generate cleanup code for their inner struct which isn't
// explicitly called in IR but must be called dynamically by the target to
// clean up the inner structs of class RC cells.
// for example, a class instance maybe be stored behind an `Any` reference,
// at which point rc instructions must discover the actual class type
// dynamically from the rc cell's class pointer/class ID
fn gen_class_rc_boilerplate(module: &mut Module, class_ty: &Type) {
    let resource_struct = class_ty
        .rc_resource_class_id()
        .and_then(|class_id| class_id.as_class())
        .expect("resource class of translated class type was not a struct");

    let mut builder = Builder::new(module);
    builder.gen_rc_boilerplate(&Type::Struct(resource_struct));
}

pub fn translate(module: &pas_ty::Module, opts: IROptions) -> Module {
    let metadata = Metadata::new();
    let mut ir_module = Module::new(module.root_ctx.clone(), metadata, opts);

    let builtin_disposable = pas_ty::builtin_disposable_iface();

    // make sure compiler builtin types are defined e.g. dynamic array types add implementations
    // to Disposable so need that interface to be defined
    let disposable_iface = {
        let mut builder = Builder::new(&mut ir_module);
        builder.translate_iface(&builtin_disposable)
    };

    ir_module.metadata.define_iface(disposable_iface);

    // if String is defined it needs to be defined in the metadata even if it isn't used,
    // for the benefit of the stdlib (it's not defined in the type context with --no-stdlib)
    let string_name = pas_ty::builtin_string_name();
    if let Ok(string_class) = module.root_ctx.find_class_def(&string_name.qualified) {
        let name = {
            let mut builder = Builder::new(&mut ir_module);
            builder.translate_name(&string_name)
        };

        ir_module.metadata.reserve_struct(STRING_ID);
        ir_module.metadata.declare_struct(STRING_ID, &name);

        let string_def = {
            let mut builder = Builder::new(&mut ir_module);
            builder.translate_class(&string_class)
        };

        ir_module.metadata.define_struct(STRING_ID, string_def);

        Builder::new(&mut ir_module).gen_rc_boilerplate(&Type::Struct(STRING_ID));
    }

    for unit in &module.units {
        ir_module.translate_unit(&unit.unit);
    }

    gen_iface_impls(&mut ir_module);
    for (elem_ty, struct_id) in ir_module.metadata.dyn_array_structs().clone() {
        gen_dyn_array_rc_boilerplate(&mut ir_module, &elem_ty, struct_id);
    }
    for class_ty in ir_module.class_types().cloned().collect::<Vec<_>>() {
        gen_class_rc_boilerplate(&mut ir_module, &class_ty);
    }

    ir_module.metadata.sort_type_defs_by_deps();

    ir_module
}
