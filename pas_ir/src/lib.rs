use {
    crate::{
        expr::*,
        metadata::*,
        stmt::*,
    },
    pas_syn::ast,
    pas_typecheck as pas_ty,
    std::{
        collections::HashMap,
        fmt,
    },
};
pub use {
    self::interpret::{
        Interpreter,
        InterpreterOpts,
    },
};

mod stmt;
mod expr;

pub mod prelude {
    pub use crate::{
        Builder,
        GlobalRef,
        Instruction,
        Interpreter,
        Label,
        metadata::{
            Metadata,
            STRING_ID,
            StringId,
            Type,
        },
        Ref,
        Value,
    };
}

pub mod metadata;
pub mod interpret;

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
    Local(usize),
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
            Ref::Local(id) => write!(f, "%{}", id),
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

    LocalAlloc(usize, Type),
    LocalDelete(usize),

    Move { out: Ref, new_val: Value },
    Add { out: Ref, a: Value, b: Value },
    Sub { out: Ref, a: Value, b: Value },

    Eq { out: Ref, a: Value, b: Value },
    Gt { out: Ref, a: Value, b: Value },
    Not { out: Ref, a: Value },
    And { out: Ref, a: Value, b: Value },
    Or { out: Ref, a: Value, b: Value },

    AddrOf { out: Ref, a: Ref },

    Call { out: Option<Ref>, function: Value, args: Vec<Value> },

    GetField { out: Ref, of: Ref, struct_id: StructID, field_id: usize },
    SetField { of: Ref, new_val: Value, struct_id: StructID, field_id: usize },

    Label(Label),
    Jump { dest: Label },
    JumpIf { dest: Label, test: Value },

    RcNew { out: Ref, struct_id: StructID },

    Release(Ref),
    Retain(Ref),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const IX_WIDTH: usize = 8;
        match self {
            Instruction::Comment(comment) => write!(f, "// {}", comment),

            Instruction::LocalAlloc(id, ty) => write!(f, "{:>width$} {} of {}", "local", Ref::Local(*id), ty, width = IX_WIDTH),
            Instruction::LocalDelete(id) => write!(f, "{:>width$} {}", "drop", Ref::Local(*id), width = IX_WIDTH),
            Instruction::Move { out, new_val } => write!(f, "{:>width$} {} := {}", "mov", out, new_val, width = IX_WIDTH),
            Instruction::Add { out, a, b } => write!(f, "{:>width$} {} := {} + {}", "add", out, a, b, width = IX_WIDTH),
            Instruction::Sub { out, a, b } => write!(f, "{:>width$} {} := {} - {}", "sub", out, a, b, width = IX_WIDTH),

            Instruction::Eq { out, a, b } => write!(f, "{:>width$} {} := {} = {}", "eq", out, a, b, width = IX_WIDTH),
            Instruction::Gt { out, a, b } => write!(f, "{:>width$} {} := {} > {}", "gt", out, a, b, width = IX_WIDTH),
            Instruction::Not { out, a } => write!(f, "{:>width$} {} := ~{}", "not", out, a, width = IX_WIDTH),
            Instruction::And { out, a, b } => write!(f, "{:>width$} {} := {} and {}", "and", out, a, b, width = IX_WIDTH),
            Instruction::Or { out, a, b } => write!(f, "{:>width$} {} := {} or {}", "or", out, a, b, width = IX_WIDTH),

            Instruction::Call { out, function, args } => {
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

            Instruction::AddrOf { out, a } => {
                write!(f, "{:>width$} {} := @{}", "addrof", out, a, width = IX_WIDTH)
            }

            Instruction::GetField { out, of, struct_id, field_id } => {
                write!(f, "{:>width$} ", "getfld", width = IX_WIDTH)?;
                write!(f, "{} := ({} as {}).{}", out, of, Type::Struct(*struct_id), field_id)
            }

            Instruction::SetField { of, new_val, struct_id, field_id } => {
                write!(f, "{:>width$} ", "setfld", width = IX_WIDTH)?;
                write!(f, "({} as {}).{} := {}", of, Type::Struct(*struct_id), field_id, new_val)
            }

            Instruction::Label(label) => {
                write!(f, "{:>width$} {}", "label", label, width = IX_WIDTH)
            }

            Instruction::Jump { dest } => {
                write!(f, "{:>width$} {}", "jmp", dest, width = IX_WIDTH)
            }

            Instruction::JumpIf { dest, test } => {
                write!(f, "{:>width$} {} if {}", "jmpif", dest, test, width = IX_WIDTH)
            }

            Instruction::RcNew { out, struct_id } => {
                write!(f, "{:>width$} {} at {}^", "rcnew", struct_id, out, width = IX_WIDTH)
            }

            Instruction::Release(at) => {
                write!(f, "{:>width$} {}", "release", at, width = IX_WIDTH)
            }

            Instruction::Retain(at) => {
                write!(f, "{:>width$} {}", "retain", at, width = IX_WIDTH)
            }
        }
    }
}

#[derive(Debug)]
enum Local {
    // the builder created this local allocation and must track its lifetime to drop it
    New {
        id: usize,
        name: Option<String>,
        ty: Type,
    },

    // the builder created this local allocation but we don't want to track its lifetime
    Temp {
        id: usize,
        ty: Type,
    },

    // the builder didn't create this local allocation but we know it exists e.g. we know where
    // function params are found due to the calling convention. we never need to drop these
    Bound {
        id: usize,
        name: Option<String>,
    },
}

impl Local {
    fn id(&self) -> usize {
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
    next_id: usize,
}

impl<'metadata> Builder<'metadata> {
    pub fn new(metadata: &'metadata mut Metadata) -> Self {
        Self {
            metadata,
            instructions: Vec::new(),
            next_id: 0,

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
        self.append(Instruction::Move { out: out.into(), new_val: val.into() });
    }

    pub fn bind_local(&mut self, id: usize, name: Option<String>) {
        assert!(!self.current_scope_mut().locals.iter()
            .any(|local| local.id() == id),
            "scope must not already have a binding for {}: {:?}", Ref::Local(id), self.current_scope_mut());

        if let Some(name) = name.as_ref() {
            assert!(!self.current_scope_mut().locals.iter()
                .any(|l| l.name() == Some(&name) || l.id() == id));
        }

        self.current_scope_mut().locals.push(Local::Bound { id, name });

        self.next_id = usize::max(id + 1, self.next_id + 1);
    }

    pub fn alloc_label(&mut self) -> Label {
        let id = self.next_id;
        self.next_id += 1;
        Label(id)
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.iter_mut().rev().next()
            .expect("scope must be active")
    }

    pub fn local_temp(&mut self, ty: Type) -> Ref {
        assert_ne!(Type::Nothing, ty);

        let id = self.next_id;

        self.current_scope_mut().locals.push(Local::Temp {
            id,
            ty: ty.clone(),
        });

        self.next_id += 1;

        self.instructions.push(Instruction::LocalAlloc(id, ty));
        Ref::Local(id)
    }

    pub fn local_new(&mut self, ty: Type, name: Option<String>) -> Ref {
        assert_ne!(Type::Nothing, ty);

        let id = self.next_id;

        self.current_scope_mut().locals.push(Local::New {
            id,
            name,
            ty: ty.clone(),
        });

        self.next_id += 1;

        self.instructions.push(Instruction::LocalAlloc(id, ty));
        Ref::Local(id)
    }

    fn find_local(&self, name: &str) -> Option<&Local> {
        self.scopes.iter().rev()
            .filter_map(|scope| {
                scope.locals.iter()
                    .find(|local| {
                        local.name()
                            .map(|local_name| local_name == name)
                            .unwrap_or(false)
                    })
            })
            .next()
    }

    fn visit_struct_deep(
        &mut self,
        at: Ref,
        struct_id: StructID,
        f: &impl Fn(&mut Builder, Ref))
    {
        let fields: Vec<_> = self.metadata.structs()[&struct_id].fields.iter()
            .map(|(field_id, field)| (*field_id, field.ty.clone(), field.rc))
            .collect();

        for (field_id, field_ty, field_rc) in fields {
            // todo: would be more efficient if we didn't have to copy each member
            // of the struct every time and instead knew which members contained deep
            // rc refs. also if we could address the fields by offsets instead of copying
            if field_rc || field_ty.is_any_struct() {
                let field_val = self.local_temp(field_ty.clone());
                self.append(Instruction::GetField {
                    out: field_val.clone(),
                    of: at.clone(),
                    struct_id,
                    field_id,
                });

                if let Type::Struct(field_struct_id) = &field_ty {
                    self.visit_struct_deep(field_val, *field_struct_id, f);
                } else {
                    f(self, field_val);
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
            },
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
            },
        }
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(Scope {
            locals: Vec::new(),
        })
    }

    pub fn end_scope(&mut self) {
        let top_scope = self.scopes.pop()
            .expect("called end_scope with no active scope");

        for local in top_scope.locals.into_iter().rev() {
            match local {
                Local::New { id, ty, .. } => {
                    self.release(Ref::Local(id), &ty);
                    self.instructions.push(Instruction::LocalDelete(id));
                }

                Local::Temp { id, .. } => {
                    self.instructions.push(Instruction::LocalDelete(id));
                }

                _ => {},
            }
        }
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
            body_builder.bind_local(0, None);

            body_builder.metadata.translate_type(ty)
        }
    };

    for (i, param) in func.decl.params.iter().enumerate() {
        // if the function returns a value, $0 is the return pointer, and args start at $1
        let id = if return_ty != Type::Nothing {
            assert!(func.decl.return_ty.is_some());
            i + 1
        } else {
            i
        };

        body_builder.bind_local(id, Some(param.ident.to_string()));
    }

    let block_output = translate_block(&func.body, &mut body_builder);

    if let Some(return_val) = block_output {
        let return_at = Ref::Local(0);
        body_builder.append(Instruction::Move { out: return_at.clone(), new_val: Value::Ref(return_val) });
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
                writeln!(f, "  {:8>} ({}): {}", format!(".{}", id), field.name, field.ty)?;
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
            writeln!(f, "{} ({}):", id, self.metadata.func_desc(*id))?;            writeln!(f, ":")?;

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

pub fn translate_unit(unit: &pas_ty::ast::Unit) -> Module {
    let mut metadata = Metadata::new();
    // add refs
    metadata.extend(&Metadata::system());

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

    let mut functions = HashMap::new();

    // func decls need to be all processed before we translate any code because the code may
    // need to look up their IDs
    for func_decl in unit.func_decls() {
        let id = metadata.declare_func(func_decl);

        if let Some(impl_iface) = &func_decl.impl_iface {
            let iface_id = metadata.find_iface(&impl_iface.iface)
                .expect("interface referenced in method implemention must already be defined");

            let for_ty = metadata.translate_type(&impl_iface.for_ty);
            metadata.impl_method(iface_id, for_ty, func_decl.ident.to_string(), id);
        }
    }

    for func_def in unit.func_defs() {
        let func = translate_func(func_def, &mut metadata);
        let func_id = match &func_def.decl.impl_iface {
            None => metadata.find_function(&func_def.decl.ident.to_string())
                .expect("all defined functions must be declared first"),

            Some(impl_iface) => {
                let method_name = func_def.decl.ident.to_string();

                let iface_id = metadata.find_iface(&impl_iface.iface).unwrap();
                let self_ty = metadata.translate_type(&impl_iface.for_ty);

                metadata.find_impl(&self_ty, iface_id, &method_name)
                    .expect("all defined method impls must be declared first")
            }
        };

        functions.insert(func_id, func);
    }

    let mut init_builder = Builder::new(&mut metadata);

    for stmt in &unit.init {
        translate_stmt(stmt, &mut init_builder);
    }

    init_builder.finish();

    Module {
        init: init_builder.instructions,
        functions,
        metadata,
    }
}
