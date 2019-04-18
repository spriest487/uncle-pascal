pub use self::{
    formatter::*,
    interpret::{Interpreter, InterpreterOpts},
};
use crate::{expr::*, metadata::*, stmt::*};
use pas_syn::{ast, IdentPath};
use pas_typecheck as pas_ty;
use std::{collections::HashMap, fmt};

mod expr;
mod formatter;
mod stmt;

pub mod prelude {
    pub use crate::{
        metadata::*,
        Builder, GlobalRef, Instruction, Interpreter, Label, Ref, Value,
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
pub struct Label(pub usize);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.0)
    }
}

#[derive(Debug, Clone)]
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
        RawInstructionFormatter.format_instruction(self, f)
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
    // function params are found due to the calling convention. we never need to drop these,
    // but we do need to release them at the end of scope
    Bound {
        id: LocalID,
        name: Option<String>,
        ty: Type,

        // values bound in one scope can be references to an outer scope, in which case they
        // must always be dereferenced to access the value
        by_ref: bool,
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

    /// if a lcoal is by-ref, it's treated in pascal syntax like a value of this type but in the IR
    /// it's actually a pointer. if this returns true, it's necessary to wrap Ref::Local values
    /// that reference this local in a Ref::Deref to achieve the same effect as the pascal syntax
    fn by_ref(&self) -> bool {
        match self {
            Local::New { .. } => false,
            Local::Temp { .. } => false,
            Local::Bound { by_ref, .. } => *by_ref,
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
            instructions: vec![Instruction::LocalBegin],
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

    pub fn bind_local(&mut self, id: LocalID, ty: Type, name: Option<String>, by_ref: bool) {
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

        if by_ref {
            assert!(
                match &ty {
                    Type::Pointer(..) => true,
                    _ => false,
                },
                "by-ref parameters must have pointer type"
            )
        }

        if let Some(name) = name.as_ref() {
            assert!(!self
                .current_scope_mut()
                .locals
                .iter()
                .any(|l| l.name() == Some(&name) || l.id() == id));
        }

        self.current_scope_mut().locals.push(Local::Bound {
            id,
            name,
            ty,
            by_ref,
        });
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

    fn visit_deep<Visitor>(&mut self, at: Ref, ty: &Type, f: &Visitor)
    where
        Visitor: Fn(&mut Builder, &Type, Ref),
    {
        match ty {
            Type::Struct(struct_id) => {
                let fields: Vec<_> = self.metadata.get_struct(*struct_id).unwrap()
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
                let cases = &self.metadata.get_variant(*id).unwrap()
                    .cases.to_vec();

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
                            b: Value::LiteralI32(tag as i32), //todo proper size type
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
                        index: Value::LiteralI32(i as i32), //todo: real usize type,
                    });

                    self.visit_deep(element_ptr.clone().deref(), element, f);
                }
            }

            // field or element
            _ => f(self, ty, at),
        };
    }

    pub fn retain(&mut self, at: Ref, ty: &Type) {
        self.begin_scope();
        self.visit_deep(at, ty, &mut |builder, element_ty, element_ref| {
            if let Type::RcPointer(..) = element_ty {
                builder.append(Instruction::Retain { at: element_ref });
            }
        });
        self.end_scope();
    }

    pub fn release(&mut self, at: Ref, ty: &Type) {
        self.begin_scope();
        self.visit_deep(at, ty, &mut |builder, element_ty, element_ref| {
            if let Type::RcPointer(..) = element_ty {
                builder.append(Instruction::Release { at: element_ref });
            }
        });
        self.end_scope();
    }

    pub fn begin_scope(&mut self) {
        self.instructions.push(Instruction::LocalBegin);
        self.scopes.push(Scope { locals: Vec::new() })
    }

    pub fn end_scope(&mut self) {
        let popped_locals = self
            .scopes
            .last()
            .expect("called end_scope with no active scope")
            .locals
            .clone();

        /* release local bindings that will be lost when the current scope is popped.
        of course, running release code may create new locals, but we just assume
        that none of those will need cleanup themselves, because they should never
        */
        for local in popped_locals.into_iter().rev() {
            match local {
                Local::Bound { id, ty, .. } | Local::New { id, ty, .. } => {
                    self.release(Ref::Local(id), &ty);
                }

                Local::Temp { .. } => {
                    // no cleanup required
                }
            }
        }

        self.scopes.pop().unwrap();
        self.instructions.push(Instruction::LocalEnd);
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: NamePath,

    pub body: Vec<Instruction>,
    pub return_ty: Type,
    pub params: Vec<Type>,
}

pub fn translate_func(func: &pas_ty::ast::FunctionDef, metadata: &mut Metadata) -> Function {
    // todo: this should be a name path already
    let name = NamePath::from_ident(IdentPath::from(func.decl.ident.clone()));

    let mut body_builder = Builder::new(metadata);

    let return_ty = match func.decl.return_ty.as_ref() {
        None | Some(pas_ty::Type::Nothing) => Type::Nothing,
        Some(ty) => {
            let return_ty = body_builder.metadata.translate_type(ty);

            // anonymous return binding at %0
            body_builder.comment(&format!(
                "{} = {} (return slot)",
                LocalID(0),
                body_builder.metadata.pretty_ty_name(&return_ty),
            ));
            body_builder.bind_local(LocalID(0), return_ty.clone(), None, false);

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
                (body_builder.metadata.translate_type(&param.ty).ptr(), true)
            }

            None => (body_builder.metadata.translate_type(&param.ty), false),
        };

        body_builder.comment(&format!(
            "{} = {}",
            id,
            body_builder.metadata.pretty_ty_name(&param_ty)
        ));
        body_builder.bind_local(id, param_ty.clone(), Some(param.ident.to_string()), by_ref);

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
        body_builder.retain(return_at.clone(), &return_ty);

        // the return val needs to end the function with +1 reference - if we only retain it once
        // it'll be released at the end of the function's scope
        body_builder.retain(return_at, &return_ty);
    }

    body_builder.finish();

    Function {
        body: body_builder.instructions,
        params: bound_params.into_iter()
            .map(|(_id, ty)| ty)
            .collect(),
        return_ty,
        name,
    }
}

#[derive(Clone, Debug)]
pub struct Module {
    pub metadata: Metadata,
    pub functions: HashMap<FunctionID, Function>,
    pub init: Vec<Instruction>,
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "* Structures")?;
        let mut defs: Vec<_> = self.metadata.type_defs().iter().collect();
        defs.sort_by_key(|(id, _)| **id);

        for (id, def) in defs {
            match def {
                TypeDef::Struct(s) => {
                writeln!(f, "{{{}}} ({}):", id, s.name)?;
                    for (id, field) in &s.fields {
                        writeln!(
                            f,
                            "  {:8>} ({}): {}",
                            format!(".{}", id),
                            field.name,
                            field.ty
                        )?;
                    }
                },

                TypeDef::Variant(v) => {
                    writeln!(f, "{{{}}} ({}):", id, v.name)?;
                    for (i, case) in v.cases.iter().enumerate() {
                        write!(
                            f,
                            "  {:8>} ({})",
                            format!(".{}", i),
                            case.name,
                        )?;

                        if let Some(ty) = &case.ty {
                            write!(f, ": {}", ty)?;
                        }
                        writeln!(f)?;
                    }
                }
            }

            writeln!(f)?;
        }

        writeln!(f, "* String literals:")?;
        for (id, lit) in self.metadata.strings() {
            writeln!(f, "`{}`: '{}'", id, lit)?;
        }
        writeln!(f)?;

        let mut funcs: Vec<_> = self.functions.iter().collect();
        funcs.sort_by_key(|(id, _)| **id);

        writeln!(f, "* Functions")?;
        for (id, func) in funcs {
            writeln!(f, "{} ({}):", id, self.metadata.func_desc(*id))?;

            for instruction in &func.body {
                self.metadata.format_instruction(instruction, f)?;
                writeln!(f)?;
            }
            writeln!(f)?;
        }

        writeln!(f, "* Init:")?;
        for instruction in &self.init {
            self.metadata.format_instruction(instruction, f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

pub fn translate_units(units: &[pas_ty::ast::Unit]) -> Module {
    let mut metadata = Metadata::new();
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
                ast::TypeDecl::Variant(variant) => {
                    metadata.define_variant(variant);
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
                    let method_index = metadata.ifaces()[&iface_id].method_index(&method_name)
                        .unwrap_or_else(|| panic!("method {} not found in interface {}", method_name, iface_id));

                    let self_ty = metadata.translate_type(&impl_iface.for_ty);

                    metadata
                        .find_impl(&self_ty, iface_id, method_index)
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
