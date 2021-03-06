use std::{collections::hash_map::HashMap, fmt};

use pas_syn::{ast, IdentPath};
use pas_ty::ast::specialize_func_decl;
use pas_typecheck as pas_ty;

use crate::{builder::Builder, expr::*, metadata::*, stmt::*};

pub use self::{
    formatter::*,
    interpret::{Interpreter, InterpreterOpts},
    metadata::Type,
};
use linked_hash_map::LinkedHashMap;

mod builder;

mod dep_sort;
mod expr;
mod formatter;
mod stmt;

pub mod prelude {
    pub use crate::{metadata::*, GlobalRef, Instruction, Interpreter, Label, Ref, Value};
}

pub mod interpret;
pub mod metadata;

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
    Local(LocalID),
    Global(GlobalRef),
    Deref(Box<Value>),
}

impl Ref {
    pub fn deref(self) -> Self {
        Ref::Deref(Box::new(Value::Ref(self)))
    }
}

impl fmt::Display for Ref {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Label(pub usize);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Comment(String),

    LocalAlloc(LocalID, Type),
    LocalBegin,
    LocalEnd,

    Move {
        out: Ref,
        new_val: Value,
    },
    Add {
        out: Ref,
        a: Value,
        b: Value,
    },
    Sub {
        out: Ref,
        a: Value,
        b: Value,
    },
    Mul {
        out: Ref,
        a: Value,
        b: Value,
    },
    IDiv {
        out: Ref,
        a: Value,
        b: Value,
    },

    Eq {
        out: Ref,
        a: Value,
        b: Value,
    },
    Gt {
        out: Ref,
        a: Value,
        b: Value,
    },
    Not {
        out: Ref,
        a: Value,
    },
    And {
        out: Ref,
        a: Value,
        b: Value,
    },
    Or {
        out: Ref,
        a: Value,
        b: Value,
    },

    /// Stores a pointer to `a` into `out`
    AddrOf {
        out: Ref,
        a: Ref,
    },

    /// Stores the address of an array element from array at `a` into `out`
    Element {
        out: Ref,
        a: Ref,
        index: Value,
        element: Type,
    },

    /// stores a pointer to the tag of a variant at `a` into `out`
    VariantTag {
        out: Ref,
        a: Ref,
        of_ty: Type,
    },
    /// stores a pointer to the data for a variant case of index `tag` at `a` into `out`
    VariantData {
        out: Ref,
        a: Ref,
        tag: usize,
        of_ty: Type,
    },

    /// Stores the address of an object field from object of type `of_ty` at location `a` into `out`.
    /// `of_ty` must match the type of the value stored at `a` and must also be a structured type
    /// i.e. one that has fields (struct or RC-pointer to struct).
    Field {
        out: Ref,
        a: Ref,
        of_ty: Type,
        field: FieldID,
    },

    Call {
        out: Option<Ref>,
        function: Value,
        args: Vec<Value>,
    },
    VirtualCall {
        out: Option<Ref>,
        iface_id: InterfaceID,
        method: MethodID,
        self_arg: Value,
        rest_args: Vec<Value>,
    },
    ClassIs {
        out: Ref,
        a: Value,
        class_id: ClassID,
    },

    Label(Label),
    Jump {
        dest: Label,
    },
    JumpIf {
        dest: Label,
        test: Value,
    },

    RcNew {
        out: Ref,
        struct_id: StructID,
    },

    Release {
        at: Ref,
    },
    Retain {
        at: Ref,
    },

    DynAlloc {
        out: Ref,
        element_ty: Type,
        len: Value,
    },
    DynFree {
        at: Ref,
    },
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = String::new();
        RawInstructionFormatter
            .format_instruction(self, &mut buf)
            .map_err(|_| fmt::Error)?;

        f.write_str(&buf)
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub debug_name: String,

    pub body: Vec<Instruction>,
    pub return_ty: Type,
    pub params: Vec<Type>,
}

#[derive(Clone, Debug)]
pub enum Function {
    External {
        symbol: String,
        src: String,
    },
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
pub struct CachedFunction {
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
    translated_funcs: HashMap<FunctionCacheKey, CachedFunction>,

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
    type_args: Vec<pas_ty::Type>,
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

    pub fn insert_func(&mut self, id: FunctionID, function: Function) {
        assert!(
            self.metadata.get_function(id).is_some(),
            "function passed to insert_func must have been previously registered in metadata"
        );

        self.functions.insert(id, function);
    }

    fn instantiate_func(&mut self, key: FunctionCacheKey) -> CachedFunction {
        if let Some(cached_func) = self.translated_funcs.get(&key) {
            return cached_func.clone();
        }

        match &key.decl_key {
            FunctionDeclKey::Function { name } => {
                match self.src_metadata.find_def(&name).cloned() {
                    Some(pas_ty::Def::Function(func_def)) => {
                        let specialized_decl = specialize_func_decl(&func_def.decl, &key.type_args)
                            .expect("function specialization must be valid after typechecking");

                        let sig = pas_ty::FunctionSig::of_decl(&specialized_decl);

                        let id = self
                            .metadata
                            .declare_func(&func_def.decl, key.type_args.clone());

                        let debug_name = specialized_decl.to_string();
                        let ir_func =
                            self.translate_func_def(&func_def, key.type_args.clone(), debug_name);

                        self.functions.insert(id, Function::Local(ir_func));

                        let cached_func = CachedFunction { id, sig };
                        self.translated_funcs.insert(key, cached_func.clone());

                        cached_func
                    }

                    Some(pas_ty::Def::External(extern_decl)) => {
                        assert!(
                            key.type_args.is_empty(),
                            "external function must not be generic"
                        );

                        let id = self
                            .metadata
                            .declare_func(&extern_decl, key.type_args.clone());
                        let sig = pas_ty::FunctionSig::of_decl(&extern_decl);
                        let cached_func = CachedFunction { id, sig };

                        let extern_src = extern_decl.external_src()
                            .expect("function with external def must have an extern src");

                        self.functions.insert(id, Function::External {
                            src: extern_src.to_string(),
                            symbol: extern_decl.ident.last().to_string(),
                        });
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
                    .unwrap_or_else(|| panic!(
                        "missing method def: {}.{} for {}",
                        iface_def.name.qualified,
                        method,
                        self_ty,
                    ));

                let specialized_decl = specialize_func_decl(&method_def.decl, &key.type_args)
                    .expect("method specialization failed in codegen");

                let id = self
                    .metadata
                    .declare_func(&specialized_decl, key.type_args.clone());

                let self_ty = self.metadata.find_type(self_ty);

                self.metadata
                    .impl_method(iface_id, self_ty, method_name, id);

                let debug_name = specialized_decl.to_string();
                let ir_func =
                    self.translate_func_def(&method_def, key.type_args.clone(), debug_name);
                self.functions.insert(id, Function::Local(ir_func));

                let cached_func = CachedFunction {
                    id,
                    sig: pas_ty::FunctionSig::of_decl(&specialized_decl),
                };
                self.translated_funcs.insert(key, cached_func.clone());

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
    ) -> CachedFunction {
        let key = FunctionCacheKey {
            decl_key: FunctionDeclKey::Method {
                iface,
                method,
                self_ty,
            },

            // dynamic method calls can't have type args
            type_args: Vec::new(),
        };

        // methods must always be present so make sure they're immediately instantiated
        self.instantiate_func(key)
    }

    pub fn translate_func(
        &mut self,
        func_name: IdentPath,
        type_args: Vec<pas_ty::Type>,
    ) -> CachedFunction {
        let key = FunctionCacheKey {
            type_args,
            decl_key: FunctionDeclKey::Function { name: func_name },
        };

        self.instantiate_func(key)
    }

    fn translate_func_def(
        &mut self,
        func: &pas_ty::ast::FunctionDef,
        type_args: Vec<pas_ty::Type>,
        debug_name: String,
    ) -> FunctionDef {
        let mut body_builder = Builder::new(self).with_type_args(type_args);

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
            body_builder.bind_local(id, param_ty.clone(), param.ident.to_string(), by_ref);

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

        let body = body_builder.finish();

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

// dynamic arrays are given an ID for their dispose impl when the type is
// added to the metadata. here's where we generate the code for those disposers
fn gen_dyn_array_disposers(module: &mut Module) {
    for (elem_ty, struct_id) in module.metadata.dyn_array_structs().clone() {
        let array_class = ClassID::Class(struct_id);
        let array_ref_ty = Type::RcPointer(Some(array_class));

        let disposer_id = module
            .metadata
            .find_impl(&array_ref_ty, DISPOSABLE_ID, DISPOSABLE_DISPOSE_INDEX)
            .expect("dynamic array class must have disposer impl registered");

        // build a disposer func for it
        let mut body_builder = Builder::new(module);

        // %0 is first arg, and the builder expects us to retain it
        body_builder.bind_local(LocalID(0), array_ref_ty.clone(), "self", false);
        let self_arg = Ref::Local(LocalID(0));
        body_builder.retain(self_arg.clone(), &array_ref_ty);

        let len_field_ptr = body_builder.local_temp(Type::I32.ptr());
        body_builder.append(Instruction::Field {
            out: len_field_ptr.clone(),
            of_ty: array_ref_ty.clone(),
            field: DYNARRAY_LEN_FIELD,
            a: self_arg.clone(),
        });

        let arr_field_ptr = body_builder.local_temp(elem_ty.clone().ptr().ptr());
        body_builder.append(Instruction::Field {
            out: arr_field_ptr.clone(),
            of_ty: array_ref_ty.clone(),
            field: DYNARRAY_PTR_FIELD,
            a: self_arg,
        });

        // release every element
        let counter = body_builder.local_temp(Type::I32);
        body_builder.mov(counter.clone(), Value::LiteralI32(0));

        // jump to loop end if counter == array len
        let start_loop_label = body_builder.alloc_label();
        let end_loop_label = body_builder.alloc_label();

        body_builder.append(Instruction::Label(start_loop_label));

        let at_end = body_builder.local_temp(Type::Bool);
        body_builder.append(Instruction::Eq {
            out: at_end.clone(),
            a: Value::Ref(len_field_ptr.clone().deref()),
            b: Value::Ref(counter.clone()),
        });
        body_builder.append(Instruction::JumpIf {
            dest: end_loop_label,
            test: Value::Ref(at_end),
        });

        // release arr[counter]
        let el_ptr = body_builder.local_temp(elem_ty.clone().ptr());
        body_builder.append(Instruction::Add {
            out: el_ptr.clone(),
            a: Value::Ref(arr_field_ptr.clone().deref()),
            b: Value::Ref(counter.clone()),
        });
        body_builder.release(el_ptr.deref(), &elem_ty);

        // counter := counter + 1
        body_builder.append(Instruction::Add {
            out: counter.clone(),
            a: Value::Ref(counter),
            b: Value::LiteralI32(1),
        });

        body_builder.append(Instruction::Jump {
            dest: start_loop_label,
        });
        body_builder.append(Instruction::Label(end_loop_label));

        // free the dynamic-allocated buffer
        body_builder.append(Instruction::DynFree {
            at: arr_field_ptr.clone().deref(),
        });

        body_builder.mov(len_field_ptr.deref(), Value::LiteralI32(0));
        body_builder.mov(arr_field_ptr.deref(), Value::LiteralNull);

        let body = body_builder.finish();

        module.insert_func(
            disposer_id,
            Function::Local(FunctionDef {
                debug_name: format!("<generated disposer for {}>", array_ref_ty),
                return_ty: Type::Nothing,
                params: vec![array_ref_ty.clone()],
                body,
            }),
        );
    }
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
                        type_args: Vec::new(),
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
fn gen_class_rc_boilerplate(module: &mut Module) {
    let translated_classes: Vec<_> = module
        .type_cache
        .iter()
        .filter_map(|(src_ty, ir_ty)| {
            if src_ty.as_class().is_ok() {
                Some(ir_ty.clone())
            } else {
                None
            }
        })
        .collect();

    for class_ty in &translated_classes {
        let resource_struct = class_ty
            .rc_resource_class_id()
            .and_then(|class_id| class_id.as_class())
            .expect("resource class of translated class type was not a struct");

        let mut builder = Builder::new(module);
        builder.translate_rc_boilerplate(&Type::Struct(resource_struct));
    }
}

pub fn translate(module: &pas_ty::Module, opts: IROptions) -> Module {
    let metadata = Metadata::new();
    let mut ir_module = Module::new(module.root_ctx.clone(), metadata, opts);

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

        Builder::new(&mut ir_module).translate_rc_boilerplate(&Type::Struct(STRING_ID));
    }

    for unit in &module.units {
        ir_module.translate_unit(&unit.unit);
    }

    // the disposable interface may never be referenced statically but needs to be instantiated
    // for automatic cleanup calls
    let builtin_disposable = pas_ty::builtin_disposable_iface();
    if ir_module
        .metadata
        .find_iface_decl(&builtin_disposable.name.qualified)
        .is_none()
    {
        let disposable_iface = {
            let mut builder = Builder::new(&mut ir_module);
            builder.translate_iface(&builtin_disposable)
        };

        ir_module.metadata.define_iface(disposable_iface);
    }

    gen_iface_impls(&mut ir_module);
    gen_dyn_array_disposers(&mut ir_module);
    gen_class_rc_boilerplate(&mut ir_module);

    ir_module.metadata.sort_type_defs_by_deps();

    ir_module
}
