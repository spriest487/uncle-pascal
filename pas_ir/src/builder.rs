use pas_typecheck as pas_ty;

use crate::{
    metadata::*, CachedFunction, FunctionCacheKey, FunctionDeclKey, IROptions, IdentPath,
    Instruction, Label, LocalID, Module, Ref, Value,
};

use std::fmt;

use pas_common::span::Span;
use pas_typecheck::Specializable;

#[derive(Clone, Debug)]
pub enum Local {
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
    pub fn id(&self) -> LocalID {
        match self {
            Local::New { id, .. } | Local::Temp { id, .. } | Local::Param { id, .. } => *id,
            Local::Return { .. } => LocalID(0),
        }
    }

    pub fn name(&self) -> Option<&String> {
        match self {
            Local::New { name, .. } => name.as_ref(),
            Local::Param { name, .. } => Some(&name),
            Local::Temp { .. } | Local::Return { .. } => None,
        }
    }

    /// if a local is by-ref, it's treated in pascal syntax like a value of this type but in the IR
    /// it's actually a pointer. if this returns true, it's necessary to wrap Ref::Local values
    /// that reference this local in a Ref::Deref to achieve the same effect as the pascal syntax
    pub fn by_ref(&self) -> bool {
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

    //positional list of type args that can be used to reify types in the current context
    type_args: Vec<pas_ty::Type>,

    instructions: Vec<Instruction>,
    scopes: Vec<Scope>,
    next_label: Label,

    loop_stack: Vec<LoopBlock>,
}

impl<'m> Builder<'m> {
    pub fn new(module: &'m mut Module) -> Self {
        Self {
            module,
            type_args: Vec::new(),

            instructions: vec![Instruction::LocalBegin],
            next_label: Label(0),

            scopes: vec![Scope { locals: Vec::new() }],

            loop_stack: Vec::new(),
        }
    }

    pub fn opts(&self) -> &IROptions {
        &self.module.opts
    }

    pub fn with_type_args(self, type_args: Vec<pas_ty::Type>) -> Self {
        let any_generic = type_args
            .iter()
            .any(|a| a.is_generic_param() || a.is_generic());
        if any_generic {
            panic!(
                "type args in a builder scope must be real types, got: [{}]",
                type_args_to_string(&type_args)
            );
        }

        Self { type_args, ..self }
    }

    pub fn translate_variant_case<'ty>(
        &'ty mut self,
        variant: &pas_ty::QualifiedDeclName,
        case: &pas_syn::Ident,
    ) -> (StructID, usize, Option<&'ty Type>) {
        let name_path = NamePath::from_decl(variant.clone(), &self.module.metadata);

        let (id, variant_struct) = match self.module.metadata.find_variant(&name_path) {
            Some((id, variant_struct)) => (id, variant_struct),
            None => panic!("missing IR metadata for variant {}", variant),
        };

        let case_index = variant_struct
            .cases
            .iter()
            .position(|c| c.name == case.name);

        match case_index {
            Some(index) => (id, index, variant_struct.cases[index].ty.as_ref()),
            None => panic!("missing case {} for {} in IR metadata", case, variant),
        }
    }

    fn translate_name(&mut self, name: &pas_ty::QualifiedDeclName) -> NamePath {
        let path_parts = name
            .qualified
            .clone()
            .into_parts()
            .into_iter()
            .map(|ident| ident.to_string());

        let type_args = name
            .type_args
            .iter()
            .map(|arg| self.translate_type(arg))
            .collect();

        NamePath {
            path: pas_syn::Path::from_parts(path_parts),
            type_args,
        }
    }

    pub fn translate_type(&mut self, src_ty: &pas_ty::Type) -> Type {
        let real_ty = src_ty.clone().substitute_type_args(&self.type_args, &self.module.src_metadata);

        // instantiate types which may contain generic params
        match &real_ty {
            pas_ty::Type::Variant(variant_def) => {
                let name_path = self.translate_name(&variant_def.name);

                if self.module.metadata.find_variant(&name_path).is_none() {
                    self.module.metadata.define_variant(&variant_def);
                }
            }

            pas_ty::Type::Record(class_def) | pas_ty::Type::Class(class_def) => {
                let name_path = self.translate_name(&class_def.name);

                if self.module.metadata.find_struct(&name_path).is_none() {
                    self.module.metadata.define_struct(&class_def);
                }
            }

            pas_ty::Type::Interface(iface_def) => {
                if self.module.metadata.find_iface(&iface_def.name).is_none() {
                    self.module.metadata.define_iface(&iface_def);
                }
            }

            pas_ty::Type::DynArray { element } => {
                self.translate_dyn_array_struct(&element);
            }

            _ => {
                // nothing to be instantiated
            }
        }

        self.module.add_type_instance(real_ty.clone());

        self.module.metadata.translate_type(&real_ty)
    }

    pub fn translate_dyn_array_struct(&mut self, element_ty: &pas_ty::Type) -> StructID {
        let element_ty = self.translate_type(element_ty);

        match self.module.metadata.find_dyn_array_struct(&element_ty) {
            Some(id) => id,
            None => self.module.metadata.define_dyn_array_struct(element_ty),
        }
    }

    pub fn translate_func(
        &mut self,
        func_name: IdentPath,
        type_args: Vec<pas_ty::Type>,
        span: &Span,
    ) -> CachedFunction {
        // specialize type args for current context
        let type_args: Vec<_> = type_args
            .iter()
            .map(|arg| arg.specialize_generic(&self.type_args, span).unwrap())
            .collect();

        let key = FunctionCacheKey {
            type_args,
            decl_key: FunctionDeclKey::Function { name: func_name },
        };

        self.module.translate_func_usage(key)
    }

    pub fn pretty_ty_name(&self, ty: &Type) -> String {
        self.module.metadata.pretty_ty_name(ty)
    }

    pub fn find_or_insert_string(&mut self, s: &str) -> StringID {
        self.module.metadata.find_or_insert_string(s)
    }

    pub fn get_struct(&self, id: StructID) -> Option<&Struct> {
        self.module.metadata.get_struct(id)
    }

    pub fn get_iface(&self, id: InterfaceID) -> Option<&Interface> {
        self.module.metadata.ifaces().get(&id)
    }

    #[allow(unused)]
    pub fn find_function(&self, name: &GlobalName) -> Option<FunctionID> {
        self.module.metadata.find_function(name)
    }

    pub fn find_impl(
        &self,
        ty: &Type,
        iface_id: InterfaceID,
        method_id: MethodID,
    ) -> Option<FunctionID> {
        self.module.metadata.find_impl(ty, iface_id, method_id)
    }

    pub fn finish(mut self) -> Vec<Instruction> {
        while !self.scopes.is_empty() {
            self.end_scope();
        }

        // cleanup: remove empty begin/end pairs, which we seem to create a lot of
        let empty_blocks_at: Vec<_> = self
            .instructions
            .windows(2)
            .enumerate()
            .filter_map(|(pos, pair)| match (&pair[0], &pair[1]) {
                (Instruction::LocalBegin, Instruction::LocalEnd) => Some(pos),
                _ => None,
            })
            .rev()
            .collect();

        for pos in empty_blocks_at {
            self.instructions.remove(pos);
            self.instructions.remove(pos);
        }

        self.instructions
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

    pub fn bind_local(&mut self, id: LocalID, ty: Type, name: impl Into<String>, by_ref: bool) {
        let name = name.into();

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
    pub fn bind_return(&mut self, ty: Type) {
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

    pub fn find_local(&self, name: &str) -> Option<&Local> {
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

    pub fn visit_deep<Visitor>(&mut self, at: Ref, ty: &Type, f: Visitor)
    where
        Visitor: Fn(&mut Builder, &Type, Ref) + Copy,
    {
        match ty {
            Type::Struct(struct_id) => {
                let struct_def = self.module.metadata.get_struct(*struct_id).unwrap();

                let fields: Vec<_> = struct_def
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
                let cases = &self
                    .module
                    .metadata
                    .get_variant(*id)
                    .unwrap()
                    .cases
                    .to_vec();

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

        if self.opts().annotate_scopes {
            self.comment(&format!("begin scope {}", self.scopes.len()));
        }
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

        let last_scope = self.scopes.len() - 1;

        if self.opts().annotate_scopes {
            if to_scope == last_scope {
                self.comment(&format!("cleanup scope {}", to_scope + 1));
            } else {
                self.comment(&format!(
                    "cleanup scopes {}..{}",
                    to_scope + 1,
                    self.scopes.len()
                ));
            }
        }

        // locals from all scopes up to the target scope, in order of deepest->shallowest,
        // then in reverse allocation order
        let locals: Vec<_> = (to_scope..=last_scope)
            .map(|i| &self.scopes[i])
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

                Local::Temp { id, .. } => {
                    // no cleanup required
                    self.comment(&format!("expire {}", id));
                }

                Local::Return { .. } => {
                    self.comment("expire return slot");
                }
            }
        }
    }

    pub fn end_scope(&mut self) {
        self.cleanup_scope(self.scopes.len() - 1);

        if self.opts().annotate_scopes {
            self.comment(&format!("end scope {}", self.scopes.len()));
        }

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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn end_loop_scope_ends_at_right_scope_level() {
        let ctx = pas_ty::Context::root(true);
        let mut module = Module::new(ctx, Metadata::default(), IROptions::default());
        let mut builder = Builder::new(&mut module);

        let initial_scope = builder.scopes.len();

        let continue_label = builder.alloc_label();
        let break_label = builder.alloc_label();
        builder.begin_loop_body_scope(continue_label, break_label);
        builder.end_loop_body_scope();

        assert_eq!(initial_scope, builder.scopes.len());
    }

    #[test]
    fn break_cleans_up_loop_locals() {
        let ctx = pas_ty::Context::root(true);
        let mut module = Module::new(ctx, Metadata::default(), IROptions::default());
        let mut builder = Builder::new(&mut module);

        let continue_label = builder.alloc_label();
        let break_label = builder.alloc_label();

        builder.begin_loop_body_scope(continue_label, break_label);
        builder.local_new(Type::RcPointer(None), Some("local1".to_string()));
        builder.local_new(Type::RcPointer(None), Some("local2".to_string()));

        builder.comment("before_break");
        builder.break_loop();

        let from = builder
            .instructions
            .iter()
            .position(|i| match i {
                Instruction::Comment(c) => c == "before_break",
                _ => false,
            })
            .unwrap();

        // Both locals should be freed, which since they're RC variables means each one should
        // be release then have NULL mov'd into it. Because the implementation of `release` uses
        // `visit_deep` it'll create a block around each one too.
        let expect = &[
            Instruction::LocalBegin,
            Instruction::Release {
                at: Ref::Local(LocalID(1)),
            },
            Instruction::Move {
                out: Ref::Local(LocalID(1)),
                new_val: Value::LiteralNull,
            },
            Instruction::LocalEnd,
            Instruction::LocalBegin,
            Instruction::Release {
                at: Ref::Local(LocalID(0)),
            },
            Instruction::Move {
                out: Ref::Local(LocalID(0)),
                new_val: Value::LiteralNull,
            },
            Instruction::LocalEnd,
            // and the final jmp for the break
            Instruction::Jump { dest: break_label },
        ];

        assert_eq!(expect, &builder.instructions[from + 1..]);
    }
}

fn type_args_to_string(args: &[pas_ty::Type]) -> String {
    args.iter()
        .map(|a| a.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}
