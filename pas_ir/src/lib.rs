use std::{
    collections::hash_map::{HashMap, Entry},
    fmt,
};

use pas_syn::{ast, IdentPath};
use pas_typecheck as pas_ty;
use pas_ty::{
    ast::{
        specialize_func_decl,
        specialize_func_def,
    },
    Specializable,
};

use crate::{
    expr::*,
    metadata::*,
    stmt::*,
};

pub use self::{
    formatter::*,
    interpret::{
        Interpreter,
        InterpreterOpts,
    },
};

mod expr;
mod formatter;
mod stmt;

#[cfg(test)]
mod test;

pub mod prelude {
    pub use crate::{
        metadata::*,
        Builder,
        GlobalRef,
        Instruction,
        Interpreter,
        Label,
        Ref,
        Value,
    };
}

pub mod interpret;
pub mod metadata;

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
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Ref(r) => write!(f, "{}", r),
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
        method: usize,
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
enum Local {
    // the builder created this local allocation and must track its lifetime to drop it
    New {
        id: LocalID,
        name: Option<String>,
        ty: Type,
    },

    // the builder created this local allocation but we don't want to track its lifetime
    Temp {
        id: LocalID,
        ty: Type,
    },

    // function parameter slots as established by the calling convention - %1.. if the function
    // has a return value, otherwise $0..

    // by-value parameter. value is copied into the local by the caller
    Param {
        id: LocalID,
        name: String,
        ty: Type,

        // by-ref parameter?
        // if so pointer to the value is copied into the local by the caller, and must
        // be dereferenced every time it is used
        by_ref: bool,
    },

    // return value: always occupies local %0 if present.
    // the return value is not named and is not cleaned up on scope exit (if it's a
    // rc type, the reference is owned by the caller after the function exits)
    Return {
        ty: Type,
    },
}

impl Local {
    fn id(&self) -> LocalID {
        match self {
            Local::New { id, .. } | Local::Temp { id, .. } | Local::Param { id, .. } => *id,
            Local::Return { .. } => LocalID(0),
        }
    }

    fn name(&self) -> Option<&String> {
        match self {
            Local::New { name, .. } => name.as_ref(),
            Local::Param { name, .. } => Some(&name),
            Local::Temp { .. } | Local::Return { .. } => None,
        }
    }

    /// if a local is by-ref, it's treated in pascal syntax like a value of this type but in the IR
    /// it's actually a pointer. if this returns true, it's necessary to wrap Ref::Local values
    /// that reference this local in a Ref::Deref to achieve the same effect as the pascal syntax
    fn by_ref(&self) -> bool {
        match self {
            Local::Param { by_ref, .. } => *by_ref,
            _ => false,
        }
    }
}

#[derive(Debug)]
struct Scope {
    locals: Vec<Local>,
}

#[derive(Debug)]
pub struct LoopBlock {
    pub continue_label: Label,
    pub break_label: Label,

    pub block_level: usize,
}

#[derive(Debug)]
pub struct Builder<'m> {
    module: &'m mut Module,

    instructions: Vec<Instruction>,
    scopes: Vec<Scope>,
    next_label: Label,

    loop_stack: Vec<LoopBlock>,
}

impl<'m> Builder<'m> {
    pub fn new(module: &'m mut Module) -> Self {
        Self {
            module,
            instructions: vec![Instruction::LocalBegin],
            next_label: Label(0),

            scopes: vec![Scope { locals: Vec::new() }],

            loop_stack: Vec::new(),
        }
    }

    pub fn finish(&mut self) {
        while !self.scopes.is_empty() {
            self.end_scope();
        }

        // cleanup: remove empty begin/end pairs, which we seem to create a lot of
        let empty_blocks_at: Vec<_> = self.instructions.windows(2)
            .enumerate()
            .filter_map(|(pos, pair)| {
                match (&pair[0], &pair[1]) {
                    (Instruction::LocalBegin, Instruction::LocalEnd) => Some(pos),
                    _ => None,
                }
            })
            .rev()
            .collect();

        for pos in empty_blocks_at {
            self.instructions.remove(pos);
            self.instructions.remove(pos);
        }
    }

    pub fn append(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn comment(&mut self, content: &(impl fmt::Display + ?Sized)) {
        self.append(Instruction::Comment(content.to_string()));
    }

    pub fn mov(&mut self, out: impl Into<Ref>, val: impl Into<Value>) {
        self.append(Instruction::Move {
            out: out.into(),
            new_val: val.into(),
        });
    }

    fn bind_local(&mut self, id: LocalID, ty: Type, name: String, by_ref: bool) {
        let slot_free = !self
            .current_scope_mut()
            .locals
            .iter()
            .any(|local| local.id() == id);
        assert!(
            slot_free,
            "scope must not already have a binding for {}: {:?}",
            Ref::Local(id),
            self.current_scope_mut()
        );
        assert!(!self
            .current_scope_mut()
            .locals
            .iter()
            .any(|l| l.name() == Some(&name) || l.id() == id));

        if by_ref {
            let is_ptr = match &ty {
                Type::Pointer(..) => true,
                _ => false,
            };
            assert!(is_ptr, "by-ref parameters must have pointer type");
        }

        self.current_scope_mut().locals.push(Local::Param {
            id,
            name,
            ty,
            by_ref,
        });
    }

    // binds a return local in %0 with the indicated type
    fn bind_return(&mut self, ty: Type) {
        let scope = self.current_scope_mut();

        let slot_free = !scope.locals.iter().any(|l| l.id() == LocalID(0));
        assert!(slot_free, "%0 must not already be bound in bind_return");

        scope.locals.push(Local::Return { ty });
    }

    fn find_next_local_id(&self) -> LocalID {
        self.scopes
            .iter()
            .flat_map(|scope| scope.locals.iter())
            .map(Local::id)
            .max_by_key(|id| id.0)
            .map(|id| LocalID(id.0 + 1))
            .unwrap_or(LocalID(0))
    }

    pub fn alloc_label(&mut self) -> Label {
        let label = self.next_label;
        self.next_label = Label(self.next_label.0 + 1);
        label
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes
            .iter_mut()
            .rev()
            .next()
            .expect("scope must be active")
    }

    pub fn local_temp(&mut self, ty: Type) -> Ref {
        assert_ne!(Type::Nothing, ty);

        let id = self.find_next_local_id();

        self.current_scope_mut()
            .locals
            .push(Local::Temp { id, ty: ty.clone() });

        self.instructions.push(Instruction::LocalAlloc(id, ty));
        Ref::Local(id)
    }

    pub fn local_new(&mut self, ty: Type, name: Option<String>) -> Ref {
        assert_ne!(Type::Nothing, ty);

        let id = self.find_next_local_id();

        self.current_scope_mut().locals.push(Local::New {
            id,
            name,
            ty: ty.clone(),
        });

        self.instructions.push(Instruction::LocalAlloc(id, ty));
        Ref::Local(id)
    }

    fn find_local(&self, name: &str) -> Option<&Local> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|scope| {
                scope.locals.iter().find(|local| {
                    local
                        .name()
                        .map(|local_name| local_name == name)
                        .unwrap_or(false)
                })
            })
            .next()
    }

    fn visit_deep<Visitor>(&mut self, at: Ref, ty: &Type, f: Visitor)
        where
            Visitor: Fn(&mut Builder, &Type, Ref) + Copy,
    {
        match ty {
            Type::Struct(struct_id) => {
                let fields: Vec<_> = self
                    .module
                    .metadata
                    .get_struct(*struct_id)
                    .unwrap()
                    .fields
                    .iter()
                    .map(|(field_id, field)| (*field_id, field.ty.clone(), field.rc))
                    .collect();

                for (field, field_ty, field_rc) in fields {
                    if field_rc || field_ty.as_struct().is_some() {
                        // store the field pointer in a temp slot
                        let field_val = self.local_temp(field_ty.clone().ptr());
                        self.append(Instruction::Field {
                            out: field_val.clone(),
                            a: at.clone(),
                            of_ty: Type::Struct(*struct_id),
                            field,
                        });

                        self.visit_deep(field_val.deref(), &field_ty, f);
                    }
                }
            }

            Type::Variant(id) => {
                let cases = &self.module.metadata.get_variant(*id).unwrap().cases.to_vec();

                // get the tag
                let tag_ptr = self.local_temp(Type::I32.ptr());
                self.append(Instruction::VariantTag {
                    out: tag_ptr.clone(),
                    a: at.clone(),
                    of_ty: Type::Variant(*id),
                });

                // jump out of the search loop if we find the matching case
                let break_label = self.alloc_label();

                // for each case, check if the tag matches and jump past it if not
                let is_not_case = self.local_temp(Type::Bool);
                for (tag, case) in cases.iter().enumerate() {
                    if let Some(data_ty) = &case.ty {
                        let skip_case_label = self.alloc_label();

                        // is_not_case := tag_ptr^ != tag
                        self.append(Instruction::Eq {
                            out: is_not_case.clone(),
                            a: Value::Ref(tag_ptr.clone().deref()),
                            b: Value::LiteralI32(tag as i32), // todo proper size type
                        });
                        self.append(Instruction::Not {
                            out: is_not_case.clone(),
                            a: Value::Ref(is_not_case.clone()),
                        });

                        self.append(Instruction::JumpIf {
                            dest: skip_case_label,
                            test: Value::Ref(is_not_case.clone()),
                        });

                        // get ptr into case data and visit it
                        let data_ptr = self.local_temp(data_ty.clone().ptr());
                        self.append(Instruction::VariantData {
                            out: data_ptr.clone(),
                            a: at.clone(),
                            of_ty: Type::Variant(*id),
                            tag,
                        });

                        self.visit_deep(data_ptr.deref(), &data_ty, f);

                        // break
                        self.append(Instruction::Jump { dest: break_label });

                        // jump to here if this case isn't active
                        self.append(Instruction::Label(skip_case_label));
                    }
                }

                self.append(Instruction::Label(break_label));
            }

            Type::Array { element, dim } => {
                let element_ptr = self.local_temp(element.clone().ptr());
                for i in 0..*dim {
                    self.append(Instruction::Element {
                        out: element_ptr.clone(),
                        a: at.clone(),
                        element: *element.clone(),
                        index: Value::LiteralI32(i as i32), // todo: real usize type,
                    });

                    self.visit_deep(element_ptr.clone().deref(), element, f);
                }
            }

            // field or element
            _ => f(self, ty, at),
        };
    }

    pub fn retain(&mut self, at: Ref, ty: &Type) {
        self.scope(|builder| {
            builder.visit_deep(at, ty, |builder, element_ty, element_ref| {
                if let Type::RcPointer(..) = element_ty {
                    builder.append(Instruction::Retain { at: element_ref });
                }
            });
        });
    }

    pub fn release(&mut self, at: Ref, ty: &Type) {
        self.scope(|builder| {
            builder.visit_deep(at, ty, |builder, element_ty, element_ref| {
                if let Type::RcPointer(..) = element_ty {
                    builder.append(Instruction::Release {
                        at: element_ref.clone(),
                    });

                    // a local might be reused within the same scope, for example in a second
                    // loop iteration and we rely on RC pointers being null when they're uninitialized
                    builder.mov(element_ref, Value::LiteralNull);
                }
            });
        });
    }

    pub fn begin_loop_body_scope(&mut self, continue_label: Label, break_label: Label) {
        self.loop_stack.push(LoopBlock {
            continue_label,
            break_label,
            block_level: self.scopes.len(),
        });

        self.begin_scope();
    }

    pub fn end_loop_body_scope(&mut self) {
        self.end_scope();

        self.loop_stack
            .pop()
            .expect("end_loop called without an active loop");
    }

    pub fn current_loop(&self) -> Option<&LoopBlock> {
        self.loop_stack.last()
    }

    pub fn begin_scope(&mut self) {
        self.instructions.push(Instruction::LocalBegin);
        self.scopes.push(Scope { locals: Vec::new() });
    }

    pub fn scope<F>(&mut self, f: F)
        where
            F: FnOnce(&mut Self),
    {
        self.begin_scope();
        f(self);
        self.end_scope();
    }

    /// release locals in all scopes after the position indicated by
    /// `to_scope` in the scope stack
    /// this should be used when jumping out a scope, or before popping one
    fn cleanup_scope(&mut self, to_scope: usize) {
        assert!(
            self.scopes.len() > to_scope,
            "reset_scope index out of range: {}",
            to_scope
        );

        // locals from all scopes up to the target scope, in order of deepest->shallowest,
        // then in reverse allocation order
        let locals: Vec<_> = self.scopes[to_scope..]
            .iter()
            .rev()
            .flat_map(|scope| {
                let mut locals = scope.locals.to_vec();
                locals.reverse();
                locals
            })
            .collect();

        // release local bindings that will be lost when the current scope is popped.
        // of course, running release code may create new locals, but we just assume
        // that none of those will need cleanup themselves, because they should never
        for local in locals {
            match local {
                Local::Param { id, ty, .. } | Local::New { id, ty, .. } => {
                    self.release(Ref::Local(id), &ty);
                }

                Local::Return { .. } | Local::Temp { .. } => {
                    // no cleanup required
                }
            }
        }
    }

    pub fn end_scope(&mut self) {
        self.cleanup_scope(self.scopes.len() - 1);

        self.scopes.pop().unwrap();
        self.instructions.push(Instruction::LocalEnd);
    }

    pub fn break_loop(&mut self) {
        let (break_label, break_scope) = {
            let current_loop = self
                .current_loop()
                .expect("break statement must appear in a loop");

            (current_loop.break_label, current_loop.block_level)
        };

        // write cleanup code for the broken scope and its children
        self.cleanup_scope(break_scope);

        // jump to the label (presumably somewhere outside the broken scope!)
        self.append(Instruction::Jump { dest: break_label })
    }

    pub fn continue_loop(&mut self) {
        let (continue_label, continue_scope) = {
            let current_loop = self
                .current_loop()
                .expect("continue statement must appear in a loop");

            (current_loop.continue_label, current_loop.block_level)
        };

        self.cleanup_scope(continue_scope);
        self.append(Instruction::Jump {
            dest: continue_label,
        });
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub debug_name: String,

    pub body: Vec<Instruction>,
    pub return_ty: Type,
    pub params: Vec<Type>,
}

#[derive(Clone, Debug)]
pub enum FunctionSrc {
    Defined(pas_ty::ast::FunctionDef),
    External {
        decl: pas_ty::ast::FunctionDecl,
        src: String,
    }
}

#[derive(Clone, Debug)]
pub struct CachedFunction {
    pub id: FunctionID,
    pub sig: pas_ty::FunctionSig,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub metadata: Metadata,

    pub functions: HashMap<FunctionID, Function>,
    func_src: HashMap<FunctionDeclKey, FunctionSrc>,
    translated_funcs: HashMap<FunctionCacheKey, CachedFunction>,

    pub init: Vec<Instruction>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum FunctionDeclKey {
    Function {
        name: IdentPath,
    },
    Method {
        iface: pas_ty::QualifiedDeclName,
        self_ty: pas_ty::Type,
        method: pas_syn::Ident,
    },
}

impl FunctionDeclKey {
    pub fn new(decl: &pas_ty::ast::FunctionDecl) -> Self {
        match &decl.impl_iface {
            None => FunctionDeclKey::Function {
                name: decl.ident.clone(),
            },

            Some(impl_iface) => FunctionDeclKey::Method {
                iface: match &impl_iface.iface {
                    pas_ty::Type::Interface(iface_decl) => iface_decl.name.clone(),
                    _ => unreachable!("method iface impl type is always an interface"),
                },
                method: decl.ident.single().clone(),
                self_ty: impl_iface.for_ty.clone(),
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct FunctionCacheKey {
    decl_key: FunctionDeclKey,
    type_args: Vec<pas_ty::Type>,
}

impl Module {
    pub fn new(metadata: Metadata) -> Self {
        Self {
            init: Vec::new(),

            functions: HashMap::new(),
            translated_funcs: HashMap::new(),
            func_src: HashMap::new(),

            metadata,
        }
    }

    pub fn translate_unit(&mut self, unit: &pas_ty::ast::Unit) {
        for func_def in unit.func_defs() {
            let func_key = FunctionDeclKey::new(&func_def.decl);

            self.func_src.insert(func_key, FunctionSrc::Defined(func_def.clone()));
        }

        let externals = unit.func_decls()
            .filter_map(|decl| {
                let src = decl.external_src()?;
                Some((decl, src))
            });

        for (external_decl, external_src) in externals {
            let func_key = FunctionDeclKey::new(&external_decl);
            assert!(external_decl.type_params.is_empty(), "external functions cannot be generic");

            self.func_src.insert(func_key, FunctionSrc::External {
                decl: external_decl.clone(),
                src: external_src.to_string(),
            });
        }

        let mut init_builder = Builder::new(self);
        for stmt in &unit.init {
            translate_stmt(stmt, &mut init_builder);
        }
        init_builder.finish();

        let unit_init = init_builder.instructions;
        self.init.extend(unit_init);
    }

    fn translate_func_usage(
        &mut self,
        key: FunctionCacheKey,
        debug_name: String,
    ) -> CachedFunction {
        match self.translated_funcs.entry(key.clone()) {
            Entry::Occupied(entry) => {
                entry.get().clone()
            }

            Entry::Vacant(entry) => {
                let src = self.func_src.get(&entry.key().decl_key)
                    .cloned()
                    .unwrap_or_else(|| panic!("source for function {} must exist - key: {:#?}", debug_name, entry.key().decl_key));

                match src {
                    FunctionSrc::Defined(def) => {
                        // declare func in metadata
                        let def = specialize_func_def(&def, &key.type_args).unwrap();
                        let sig = pas_ty::FunctionSig::of_decl(&def.decl);

                        let id = self.metadata.declare_func(&def.decl, key.type_args.clone());

                        // declare impl in metadata
                        if let Some(impl_iface) = &def.decl.impl_iface {
                            let method_name = def.decl.ident.single().to_string();

                            let iface_id = self.metadata
                                .translate_type(&impl_iface.iface)
                                .as_iface()
                                .expect("implemented type must be an interface");

                            let for_ty = self.metadata.translate_type(&impl_iface.for_ty);

                            self.metadata.impl_method(iface_id, for_ty, method_name, id);
                        }

                        // add translation to module functions
                        let type_args = entry.key().type_args.clone();
                        let cached_func = CachedFunction { id, sig };
                        entry.insert(cached_func.clone());

                        assert!(def.decl.params.iter().all(|p| !p.ty.is_generic()));
                        assert!(def.decl.return_ty.as_ref().map(|p| !p.is_generic()).unwrap_or(true));

                        let func = self.translate_func_def(&def, &type_args, debug_name);
                        self.functions.insert(id, func);

                        cached_func
                    }

                    FunctionSrc::External { decl, .. } => {
                        let specialized = specialize_func_decl(&decl, &key.type_args).unwrap();
                        let sig = pas_ty::FunctionSig::of_decl(&specialized);

                        let id = self.metadata.declare_func(&specialized, key.type_args);
                        let cached_func = CachedFunction { id, sig };
                        entry.insert(cached_func.clone());

                        cached_func
                    }
                }
            }
        }
    }

    pub fn translate_method(
        &mut self,
        iface_ty: &pas_ty::Type,
        self_ty: &pas_ty::Type,
        method: pas_syn::Ident,
        type_args: Vec<pas_ty::Type>,
    ) -> CachedFunction {
        let iface_ty = iface_ty.as_iface().expect("iface ty must always be an interface");

        let iface_name = NamePath::from_decl(iface_ty.name.clone(), &mut self.metadata);
        let method_name = NamePath {
            path: pas_syn::Path::from(method.to_string()),
            type_args: type_args.iter().map(|arg| self.metadata.translate_type(arg)).collect()
        };
        let debug_name = format!("{}::{}", iface_name, method_name);

        let key = FunctionCacheKey {
            decl_key: FunctionDeclKey::Method {
                iface: iface_ty.name.clone(),
                self_ty: self_ty.clone(),
                method,
            },
            type_args,
        };

        self.translate_func_usage(key, debug_name)
    }

    pub fn translate_func(
        &mut self,
        func_name: IdentPath,
        type_args: Vec<pas_ty::Type>,
    ) -> CachedFunction {
        let debug_name = NamePath::from_ident_path(&func_name, &type_args, &mut self.metadata)
            .to_string();

        let key = FunctionCacheKey {
            type_args,
            decl_key: FunctionDeclKey::Function {
                name: func_name,
            },
        };

        self.translate_func_usage(key, debug_name)
    }

    fn translate_func_def(
        &mut self,
        func: &pas_ty::ast::FunctionDef,
        type_args: &[pas_ty::Type],
        debug_name: String,
    ) -> Function {
        let mut body_builder = Builder::new(self);

        let type_args = type_args.iter()
            .map(|arg| body_builder.module.metadata.translate_type(arg))
            .collect();

        body_builder.module.metadata.set_type_args(type_args);

        let return_ty = match func.decl.return_ty.as_ref() {
            None | Some(pas_ty::Type::Nothing) => Type::Nothing,
            Some(ty) => {
                let return_ty = body_builder.module.metadata.translate_type(ty);

                // anonymous return binding at %0
                body_builder.comment(&format!(
                    "{} = {} (return slot)",
                    LocalID(0),
                    body_builder.module.metadata.pretty_ty_name(&return_ty),
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
                    (body_builder.module.metadata.translate_type(&param.ty).ptr(), true)
                }

                None => (body_builder.module.metadata.translate_type(&param.ty), false),
            };

            body_builder.comment(&format!(
                "{} = {}",
                id,
                body_builder.module.metadata.pretty_ty_name(&param_ty)
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
            // the return value so it outlives the functino
            body_builder.retain(return_at, &return_ty);
        }

        body_builder.module.metadata.set_type_args(Vec::new());

        body_builder.finish();

        Function {
            body: body_builder.instructions,
            params: bound_params.into_iter().map(|(_id, ty)| ty).collect(),
            return_ty,
            debug_name,
        }
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "* Structures")?;
        let mut defs: Vec<_> = self.metadata.type_defs().iter().collect();
        defs.sort_by_key(|(id, _)| **id);

        for (id, def) in defs {
            match def {
                TypeDef::Struct(s) => {
                    write!(f, "{}: ", id.0)?;
                    self.metadata.format_name(&s.name, f)?;
                    writeln!(f)?;

                    for (id, field) in &s.fields {
                        write!(f, "{:8>} {}: ", format!("  .{}", id), field.name, )?;
                        self.metadata.format_type(&field.ty, f)?;
                        writeln!(f)?;
                    }
                }

                TypeDef::Variant(v) => {
                    writeln!(f, "{}: {}", id, v.name)?;
                    for (i, case) in v.cases.iter().enumerate() {
                        write!(f, "{:8>} ({})", format!("  .{}", i), case.name, )?;

                        if let Some(ty) = &case.ty {
                            write!(f, ": {}", ty)?;
                        }
                        writeln!(f)?;
                    }
                }
            }

            writeln!(f)?;
        }

        writeln!(f, "* String literals")?;
        for (id, lit) in self.metadata.strings() {
            writeln!(f, "{}: '{}'", id.0, lit)?;
        }
        writeln!(f)?;

        let mut funcs: Vec<_> = self.functions.iter().collect();
        funcs.sort_by_key(|(id, _)| **id);

        writeln!(f, "* Functions")?;
        for (id, func) in funcs {
            let formatter = StatefulIndentedFormatter::new(&self.metadata, 8);

            writeln!(f, "{}: {}", id.0, self.metadata.func_desc(*id))?;

            for instruction in &func.body {
                formatter.format_instruction(instruction, f)?;
                writeln!(f)?;
            }
            writeln!(f)?;
        }

        let formatter = StatefulIndentedFormatter::new(&self.metadata, 8);
        writeln!(f, "* Init:")?;
        for instruction in &self.init {
            formatter.format_instruction(instruction, f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

pub fn translate(module: &pas_ty::Module) -> Module {
    let mut metadata = Metadata::new();
    metadata.translate_type(&pas_ty::Type::Interface(module.disposable_iface.clone()));
    metadata.translate_type(&pas_ty::Type::Class(module.string_class.clone()));

    let mut ir_module = Module::new(metadata);

    for unit in &module.units {
        ir_module.translate_unit(unit);
    }

    ir_module
}
