mod stmt;
mod expr;

use {
    crate::{
        metadata::*,
        stmt::*,
        expr::*,
    },
    pas_syn::{
        ast,
    },
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

pub mod prelude {
    pub use crate::{
        GlobalRef,
        Ref,
        Value,
        Interpreter,
        Instruction,
        Label,
        Builder,
        metadata::{
            Type,
            STRING_ID,
            StringId,
            Metadata,
        },
    };
}

pub mod metadata;
pub mod interpret;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum GlobalRef {
    Function(String),
    StringLiteral(StringId),
}

impl fmt::Display for GlobalRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GlobalRef::Function(name) => write!(f, "function `{}`", name),
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
            Ref::Local(id) => write!(f, "${}", id),
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

impl Value {
    fn as_ref_val(&self) -> Option<&Ref> {
        match self {
            Value::Ref(ref_val) => Some(ref_val),
            _ => None,
        }
    }

    fn into_ref_val(self) -> Option<Ref> {
        match self {
            Value::Ref(ref_val) => Some(ref_val),
            _ => None,
        }
    }

    fn deref(self) -> Value {
        Value::Ref(Ref::Deref(Box::new(self)))
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
    LocalAlloc(usize, Type),
    LocalDelete(usize),

    Set { out: Ref, new_val: Value },
    Add { out: Ref, a: Value, b: Value },

    Eq { out: Ref, a: Value, b: Value },
    Gt { out: Ref, a: Value, b: Value },
    Not { out: Ref, a: Value, },
    And { out: Ref, a: Value, b: Value,},
    Or { out: Ref, a: Value, b: Value, },

    AddrOf { out: Ref, a: Ref },

    Call { out: Option<Ref>, function: Value, args: Vec<Value> },

    GetField { out: Ref, of: Ref, struct_id: StructId, field_id: usize },
    SetField { of: Ref, new_val: Value, struct_id: StructId, field_id: usize },

    Label(Label),
    Jump { dest: Label },
    JumpIf { dest: Label, test: Value },

    MemAlloc { out: Ref, ty: Type, count: usize, },

    Release(Ref),
    Retain(Ref),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const IX_WIDTH: usize = 8;
        match self {
            Instruction::LocalAlloc(id, ty) => write!(f, "{:>width$} {} of {}", "local", id, ty, width = IX_WIDTH),
            Instruction::LocalDelete(id) => write!(f, "{:>width$} {}", "drop", id, width = IX_WIDTH),
            Instruction::Set { out, new_val } => write!(f, "{:>width$} {} := {}", "set", out, new_val, width = IX_WIDTH),
            Instruction::Add { out, a, b } => write!(f, "{:>width$} {} := {} + {}", "add", out, a, b, width = IX_WIDTH),

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

            Instruction::MemAlloc { out, ty, count } => {
                write!(f, "{:>width$} {}^ of [{}; {}]", "malloc", out, ty, count, width = IX_WIDTH)
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
    Allocated {
        id: usize,
        name: Option<String>,
        rc: bool,
    },
    // the builder didn't create this local allocation but we know it exists e.g. we know where
    // function params are found due to the calling convention. we need to know its name but we
    // don't need to drop it
    Bound {
        id: usize,
        name: String,
    },
}

impl Local {
    fn id(&self) -> usize {
        match self {
            Local::Allocated { id, .. } => *id,
            Local::Bound { id, .. } => *id,
        }
    }

    fn name(&self) -> Option<&String> {
        match self {
            Local::Allocated { name, .. } => name.as_ref(),
            Local::Bound { name, .. } => Some(&name),
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

    pub fn bind_local(&mut self, id: usize, name: String) {
        assert!(id >= self.next_id);
        assert!(!self.current_scope_mut().locals.iter()
            .any(|l| l.name() == Some(&name) || l.id() == id));

        self.current_scope_mut().locals.push(Local::Bound { id, name: name.to_string() });

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

    pub fn new_local(&mut self, ty: Type, rc: bool, name: Option<String>) -> Ref {
        assert_ne!(Type::Nothing, ty);

        let id = self.next_id;

        self.current_scope_mut().locals.push(Local::Allocated {
            id,
            name,
            rc,
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

    pub fn begin_scope(&mut self) {
        self.scopes.push(Scope {
            locals: Vec::new(),
        })
    }

    pub fn end_scope(&mut self) {
        let top_scope = self.scopes.pop()
            .expect("called end_scope with no active scope");

        for local in top_scope.locals.iter().rev() {
            if let Local::Allocated { id, rc, .. } = local {
                if *rc {
                    self.instructions.push(Instruction::Release(Ref::Local(*id)));
                }
                self.instructions.push(Instruction::LocalDelete(*id));
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    body: Vec<Instruction>,
    return_ty: Type,
}

pub fn translate_func(func: &pas_ty::ast::FunctionDecl, metadata: &mut Metadata) -> Function {
    let mut body_builder = Builder::new(metadata);

    let return_ty = match func.return_ty.as_ref() {
        Some(ty) => body_builder.metadata.translate_type(ty),
        None => Type::Nothing,
    };

    for (i, param) in func.params.iter().enumerate() {
        // if the function returns a value, $0 is the return pointer, and args start at $1
        let id = if return_ty != Type::Nothing {
            i + 1
        } else {
            i
        };

        body_builder.bind_local(id, param.ident.to_string());
    }

    let block_output = translate_block(&func.body, &mut body_builder);

    if let Some(return_val) = block_output {
        let return_at = Ref::Local(0);
        body_builder.append(Instruction::Set { out: return_at, new_val: return_val });
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
    functions: HashMap<String, Function>,
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
        for (name, func) in &self.functions {
            writeln!(f, "{}:", name)?;
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

    let mut functions = HashMap::new();

    for ty_decl in unit.type_decls() {
        match ty_decl {
            ast::TypeDecl::Class(class) => {
                metadata.translate_struct(class);
            }
        }
    }

    for func_decl in unit.func_decls() {
        let func = translate_func(func_decl, &mut metadata);
        functions.insert(func_decl.ident.to_string(), func);
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
