pub use self::interpret::{Interpreter, InterpreterOpts};
use crate::{expr::*, metadata::*, stmt::*};
use pas_syn::ast;
use pas_typecheck as pas_ty;
use std::{collections::HashMap, fmt};

mod expr;
mod stmt;

pub mod prelude {
    pub use crate::{
        metadata::{Metadata, StringId, GlobalName, NamePath, Type, STRING_ID},
        Builder, GlobalRef, Instruction, Interpreter, Label, Ref, Value,
    };
}

pub mod interpret;
pub mod metadata;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct LocalID(usize);

impl fmt::Display for LocalID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum GlobalRef {
    Function(FunctionID),
    StringLiteral(StringId),
}

impl fmt::Display for GlobalRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GlobalRef::Function(func_id) => write!(f, "{}", func_id),
            GlobalRef::StringLiteral(id) => write!(f, "string `{}`", id),
        }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
pub struct Label(usize);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Comment(String),

    LocalAlloc(LocalID, Type),
    LocalDelete(LocalID),

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

    /// Stores the address of an object field from object of type `of_ty` at location `a` into `out`
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

    Release(Ref),
    Retain(Ref),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const IX_WIDTH: usize = 8;
        match self {
            Instruction::Comment(comment) => write!(f, "// {}", comment),

            Instruction::LocalAlloc(id, ty) => write!(
                f,
                "{:>width$} {} of {}",
                "local",
                Ref::Local(*id),
                ty,
                width = IX_WIDTH
            ),
            Instruction::LocalDelete(id) => write!(
                f,
                "{:>width$} {}",
                "drop",
                Ref::Local(*id),
                width = IX_WIDTH
            ),
            Instruction::Move { out, new_val } => write!(
                f,
                "{:>width$} {} := {}",
                "mov",
                out,
                new_val,
                width = IX_WIDTH
            ),
            Instruction::Add { out, a, b } => write!(
                f,
                "{:>width$} {} := {} + {}",
                "add",
                out,
                a,
                b,
                width = IX_WIDTH
            ),
            Instruction::Sub { out, a, b } => write!(
                f,
                "{:>width$} {} := {} - {}",
                "sub",
                out,
                a,
                b,
                width = IX_WIDTH
            ),

            Instruction::Eq { out, a, b } => write!(
                f,
                "{:>width$} {} := {} = {}",
                "eq",
                out,
                a,
                b,
                width = IX_WIDTH
            ),
            Instruction::Gt { out, a, b } => write!(
                f,
                "{:>width$} {} := {} > {}",
                "gt",
                out,
                a,
                b,
                width = IX_WIDTH
            ),
            Instruction::Not { out, a } => {
                write!(f, "{:>width$} {} := ~{}", "not", out, a, width = IX_WIDTH)
            }
            Instruction::And { out, a, b } => write!(
                f,
                "{:>width$} {} := {} and {}",
                "and",
                out,
                a,
                b,
                width = IX_WIDTH
            ),
            Instruction::Or { out, a, b } => write!(
                f,
                "{:>width$} {} := {} or {}",
                "or",
                out,
                a,
                b,
                width = IX_WIDTH
            ),

            Instruction::Call {
                out,
                function,
                args,
            } => {
                write!(f, "{:>width$} ", "call", width = IX_WIDTH)?;
                if let Some(out_val) = out {
                    write!(f, "{} := ", out_val)?;
                }
                write!(f, "{}(", function)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }

            Instruction::AddrOf { out, a } => write!(
                f,
                "{:>width$} {} := @{}",
                "addrof",
                out,
                a,
                width = IX_WIDTH
            ),

            Instruction::Element { out, a, element, index } => write!(
                f,
                "{:>width$} {} := @({} as array of {})[{}]",
                "el",
                out,
                a,
                element,
                index,
                width = IX_WIDTH
            ),

            Instruction::Field { out, a, of_ty, field } => write!(
                f,
                "{:>width$} {} := @({} as {}).{}",
                "fld",
                out,
                a,
                of_ty,
                field,
                width = IX_WIDTH
            ),

            Instruction::Label(label) => {
                write!(f, "{:>width$} {}", "label", label, width = IX_WIDTH)
            }

            Instruction::Jump { dest } => write!(f, "{:>width$} {}", "jmp", dest, width = IX_WIDTH),

            Instruction::JumpIf { dest, test } => write!(
                f,
                "{:>width$} {} if {}",
                "jmpif",
                dest,
                test,
                width = IX_WIDTH
            ),

            Instruction::RcNew { out, struct_id } => write!(
                f,
                "{:>width$} {} at {}^",
                "rcnew",
                Type::Struct(*struct_id),
                out,
                width = IX_WIDTH
            ),

            Instruction::Release(at) => write!(f, "{:>width$} {}", "release", at, width = IX_WIDTH),

            Instruction::Retain(at) => write!(f, "{:>width$} {}", "retain", at, width = IX_WIDTH),
        }
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

    // the builder didn't create this local allocation but we know it exists e.g. we know where
    // function params are found due to the calling convention. we never need to drop these
    Bound {
        id: LocalID,
        name: Option<String>,
    },
}

impl Local {
    fn id(&self) -> LocalID {
        match self {
            Local::New { id, .. } => *id,
            Local::Temp { id, .. } => *id,
            Local::Bound { id, .. } => *id,
        }
    }

    fn name(&self) -> Option<&String> {
        match self {
            Local::New { name, .. } => name.as_ref(),
            Local::Temp { .. } => None,
            Local::Bound { name, .. } => name.as_ref(),
        }
    }
}

#[derive(Debug)]
struct Scope {
    locals: Vec<Local>,
}

#[derive(Debug)]
pub struct Builder<'metadata> {
    metadata: &'metadata mut Metadata,

    instructions: Vec<Instruction>,
    scopes: Vec<Scope>,
    next_label: Label,
}

impl<'metadata> Builder<'metadata> {
    pub fn new(metadata: &'metadata mut Metadata) -> Self {
        Self {
            metadata,
            instructions: Vec::new(),
            next_label: Label(0),

            scopes: vec![Scope { locals: Vec::new() }],
        }
    }

    pub fn finish(&mut self) {
        while !self.scopes.is_empty() {
            self.end_scope();
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

    pub fn bind_local(&mut self, id: LocalID, name: Option<String>) {
        assert!(
            !self
                .current_scope_mut()
                .locals
                .iter()
                .any(|local| local.id() == id),
            "scope must not already have a binding for {}: {:?}",
            Ref::Local(id),
            self.current_scope_mut()
        );

        if let Some(name) = name.as_ref() {
            assert!(!self
                .current_scope_mut()
                .locals
                .iter()
                .any(|l| l.name() == Some(&name) || l.id() == id));
        }

        self.current_scope_mut()
            .locals
            .push(Local::Bound { id, name });
    }

    fn find_next_local_id(&self) -> LocalID {
        self.scopes
            .iter()
            .flat_map(|scope| scope.locals.iter())
            .map(|l| l.id())
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

    fn visit_struct_deep(&mut self, at: Ref, struct_id: StructID, f: &impl Fn(&mut Builder, Ref)) {
        let fields: Vec<_> = self.metadata.structs()[&struct_id]
            .fields
            .iter()
            .map(|(field_id, field)| (*field_id, field.ty.clone(), field.rc))
            .collect();

        for (field, field_ty, field_rc) in fields {
            // todo: would be more efficient if we didn't have to copy each member
            // of the struct every time and instead knew which members contained deep
            // rc refs. also if we could address the fields by offsets instead of copying
            if field_rc || field_ty.as_struct().is_some() {
                let field_val = self.local_temp(field_ty.clone().ptr());
                self.append(Instruction::Field {
                    out: field_val.clone(),
                    a: at.clone(),
                    of_ty: field_ty.clone(),
                    field,
                });

                if let Type::Struct(field_struct_id) = &field_ty {
                    self.visit_struct_deep(field_val.deref(), *field_struct_id, f);
                } else {
                    f(self, field_val.deref());
                }
            }
        }
    }

    pub fn retain(&mut self, at: Ref, ty: &Type) {
        match ty {
            Type::Rc(_) => {
                self.append(Instruction::Retain(at.clone()));
            }

            Type::Struct(id) => {
                self.begin_scope();
                self.visit_struct_deep(at, *id, &mut |builder, field_ref| {
                    builder.append(Instruction::Retain(field_ref));
                });
                self.end_scope();
            }

            _ => {
                // nothing to retain
            }
        }
    }

    pub fn release(&mut self, at: Ref, ty: &Type) {
        match ty {
            Type::Rc(_) => {
                self.append(Instruction::Release(at.clone()));
            }

            Type::Struct(id) => {
                self.begin_scope();
                self.visit_struct_deep(at, *id, &mut |builder, field_ref| {
                    builder.append(Instruction::Release(field_ref));
                });
                self.end_scope();
            }

            _ => {
                // nothing to release
            }
        }
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(Scope { locals: Vec::new() })
    }

    pub fn end_scope(&mut self) {
        let popped_locals = self
            .scopes
            .last()
            .expect("called end_scope with no active scope")
            .locals
            .clone();

        for local in popped_locals {
            match local {
                Local::New { id, ty, .. } => {
                    self.release(Ref::Local(id), &ty);
                    self.instructions.push(Instruction::LocalDelete(id));
                }

                Local::Temp { id, .. } => {
                    self.instructions.push(Instruction::LocalDelete(id));
                }

                _ => {}
            }
        }

        self.scopes.pop().unwrap();
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    body: Vec<Instruction>,
    return_ty: Type,
}

pub fn translate_func(func: &pas_ty::ast::FunctionDef, metadata: &mut Metadata) -> Function {
    let mut body_builder = Builder::new(metadata);

    let return_ty = match func.decl.return_ty.as_ref() {
        None | Some(pas_ty::Type::Nothing) => Type::Nothing,
        Some(ty) => {
            // anonymous return binding at %0
            body_builder.bind_local(LocalID(0), None);

            body_builder.metadata.translate_type(ty)
        }
    };

    for (i, param) in func.decl.params.iter().enumerate() {
        // if the function returns a value, $0 is the return pointer, and args start at $1
        let id = LocalID(if return_ty != Type::Nothing {
            assert!(func.decl.return_ty.is_some());
            i + 1
        } else {
            i
        });

        body_builder.comment(&format!("{} = {}", id, param));
        body_builder.bind_local(id, Some(param.ident.to_string()));
    }

    let block_output = translate_block(&func.body, &mut body_builder);

    if let Some(return_val) = block_output {
        let return_at = Ref::Local(LocalID(0));
        body_builder.append(Instruction::Move {
            out: return_at.clone(),
            new_val: Value::Ref(return_val),
        });
        body_builder.retain(return_at, &return_ty);
    }

    body_builder.finish();

    Function {
        body: body_builder.instructions,
        return_ty,
    }
}

#[derive(Clone, Debug)]
pub struct Module {
    metadata: Metadata,
    functions: HashMap<FunctionID, Function>,
    init: Vec<Instruction>,
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "* Structures")?;
        for (id, struct_def) in self.metadata.structs() {
            writeln!(f, "{{{}}} ({}):", id, struct_def.name)?;
            for (id, field) in &struct_def.fields {
                writeln!(
                    f,
                    "  {:8>} ({}): {}",
                    format!(".{}", id),
                    field.name,
                    field.ty
                )?;
            }
            writeln!(f)?;
        }

        writeln!(f, "* String literals:")?;
        for (id, lit) in self.metadata.strings() {
            writeln!(f, "`{}`: '{}'", id, lit)?;
        }
        writeln!(f)?;

        writeln!(f, "* Functions")?;
        for (id, func) in &self.functions {
            writeln!(f, "{} ({}):", id, self.metadata.func_desc(*id))?;

            for instruction in &func.body {
                writeln!(f, "{}", instruction)?;
            }
            writeln!(f)?;
        }

        writeln!(f, "* Init:")?;
        for instruction in &self.init {
            writeln!(f, "{}", instruction)?;
        }
        Ok(())
    }
}

pub fn translate_units(units: &[pas_ty::ast::Unit]) -> Module {
    let mut metadata = Metadata::new();
    // add refs
    metadata.extend(&Metadata::system());

    let mut functions = HashMap::new();

    for unit in units.iter() {
        let unit_ns = vec![unit.ident.to_string()];

        for ty_decl in unit.type_decls() {
            match ty_decl {
                ast::TypeDecl::Class(class) => {
                    metadata.define_struct(class);
                }
                ast::TypeDecl::Interface(iface) => {
                    metadata.define_iface(iface);
                }
            }
        }

        // func decls need to be all processed before we translate any code because the code may
        // need to look up their IDs
        for func_decl in unit.func_decls() {
            let id = metadata.declare_func(&[unit.ident.clone()], func_decl);

            if let Some(impl_iface) = &func_decl.impl_iface {
                let iface_id = metadata.find_iface(&impl_iface.iface).unwrap_or_else(|| {
                    panic!(
                        "interface {} referenced in method impl of {} must already be defined",
                        impl_iface.iface, func_decl.ident
                    );
                });

                let for_ty = metadata.translate_type(&impl_iface.for_ty);
                metadata.impl_method(iface_id, for_ty, func_decl.ident.to_string(), id);
            }
        }

        for func_def in unit.func_defs() {
            let func = translate_func(func_def, &mut metadata);
            let func_id = match &func_def.decl.impl_iface {
                None => {
                    let global_name =
                        GlobalName::new(func_def.decl.ident.to_string(), unit_ns.clone());
                    metadata
                        .find_function(&global_name)
                        .expect("all defined functions must be declared first")
                }

                Some(impl_iface) => {
                    let method_name = func_def.decl.ident.to_string();

                    let iface_id = metadata.find_iface(&impl_iface.iface).unwrap();
                    let self_ty = metadata.translate_type(&impl_iface.for_ty);

                    metadata
                        .find_impl(&self_ty, iface_id, &method_name)
                        .expect("all defined method impls must be declared first")
                }
            };

            functions.insert(func_id, func);
        }
    }

    let mut init_builder = Builder::new(&mut metadata);
    for unit in units.iter() {
        for stmt in &unit.init {
            translate_stmt(stmt, &mut init_builder);
        }
    }
    init_builder.finish();

    Module {
        init: init_builder.instructions,
        functions,
        metadata,
    }
}
