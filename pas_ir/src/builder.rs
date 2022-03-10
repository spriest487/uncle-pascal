use pas_typecheck as pas_ty;

use crate::{metadata::*, Function, FunctionDefKey, FunctionDef, FunctionInstance, GlobalRef, IROptions, IdentPath, Instruction, Label, LocalID, Module, RcBoilerplatePair, Ref, Type, Value, EXIT_LABEL, FunctionDeclKey};

use std::fmt;

use self::scope::*;
use crate::ty::{FieldID, Interface, Struct};
use pas_common::span::{Span, Spanned};
use pas_syn::ast::TypeList;
use pas_syn::Ident;
use pas_typecheck::Specializable;
use std::borrow::Cow;

pub mod scope;

#[derive(Debug)]
pub struct Builder<'m> {
    module: &'m mut Module,

    //positional list of type args that can be used to reify types in the current context
    type_args: Option<pas_ty::TypeList>,

    instructions: Vec<Instruction>,
    scopes: Vec<Scope>,
    next_label: Label,

    loop_stack: Vec<LoopScope>,

    module_span: Span,
}

impl<'m> Builder<'m> {
    pub fn new(module: &'m mut Module) -> Self {
        let module_span = module.module_span().clone();

        let mut instructions = Vec::new();
        if module.opts.debug_info {
            instructions.push(Instruction::DebugPush(module_span.clone()));
        }
        instructions.push(Instruction::LocalBegin);

        Self {
            module,
            type_args: None,

            instructions,

            // the EXIT label is always reserved, so start one after that
            next_label: Label(EXIT_LABEL.0 + 1),

            scopes: vec![Scope::new()],

            loop_stack: Vec::new(),

            module_span,
        }
    }

    pub fn opts(&self) -> &IROptions {
        &self.module.opts
    }

    pub fn type_args(&self) -> Option<&pas_ty::TypeList> {
        self.type_args.as_ref()
    }

    pub fn with_type_args(self, type_args: pas_ty::TypeList) -> Self {
        let any_generic = type_args
            .items
            .iter()
            .any(|a| a.is_generic_param() || a.is_unspecialized_generic());
        if any_generic {
            panic!(
                "type args in a builder scope must be real types, got: [{}]",
                type_args_to_string(&type_args)
            );
        }

        Self {
            type_args: Some(type_args),
            ..self
        }
    }

    pub fn translate_variant_case<'ty>(
        &'ty mut self,
        variant: &pas_ty::Symbol,
        case: &pas_syn::Ident,
    ) -> (TypeDefID, usize, Option<&'ty Type>) {
        let name_path = self.translate_name(variant);

        let (id, variant_struct) = match self.module.metadata.find_variant_def(&name_path) {
            Some((id, variant_struct)) => (id, variant_struct),
            None => panic!("missing IR metadata definition for variant {}", variant),
        };

        let case_index = variant_struct
            .cases
            .iter()
            .position(|c| c.name == *case.name);

        match case_index {
            Some(index) => (id, index, variant_struct.cases[index].ty.as_ref()),
            None => panic!("missing case {} for {} in IR metadata", case, variant),
        }
    }

    pub fn translate_name(&mut self, name: &pas_ty::Symbol) -> NamePath {
        self.module
            .translate_name(name, self.type_args().cloned().as_ref())
    }

    pub fn translate_class(&mut self, class_def: &pas_ty::ast::Class) -> Struct {
        self.module
            .translate_class(class_def, self.type_args().cloned().as_ref())
    }

    pub fn translate_iface(&mut self, iface_def: &pas_ty::ast::Interface) -> Interface {
        self.module
            .translate_iface(iface_def, self.type_args().cloned().as_ref())
    }

    pub fn translate_type(&mut self, src_ty: &pas_ty::Type) -> Type {
        self.module
            .translate_type(src_ty, self.type_args().cloned().as_ref())
    }

    pub fn translate_dyn_array_struct(&mut self, element_ty: &pas_ty::Type) -> TypeDefID {
        self.module
            .translate_dyn_array_struct(element_ty, self.type_args().cloned().as_ref())
    }

    pub fn translate_method_impl(
        &mut self,
        iface: IdentPath,
        method: Ident,
        self_ty: pas_ty::Type,
    ) -> FunctionInstance {
        self.module.translate_method_impl(iface, method, self_ty)
    }

    pub fn translate_func(
        &mut self,
        func_name: IdentPath,
        type_args: Option<pas_ty::TypeList>,
        span: &Span,
    ) -> FunctionInstance {
        // specialize type args for current context
        let instance_type_args = match (type_args, self.type_args.as_ref()) {
            (Some(func_ty_args), Some(current_ty_args)) => {
                let items = func_ty_args
                    .items
                    .iter()
                    .map(|arg| arg.specialize_generic(&current_ty_args, span).unwrap());

                Some(TypeList::new(items, func_ty_args.span().clone()))
            }

            (Some(func_ty_args), None) => Some(func_ty_args),

            (None, ..) => None,
        };

        let key = FunctionDefKey {
            type_args: instance_type_args,
            decl_key: FunctionDeclKey::Function { name: func_name },
        };

        self.module.instantiate_func(key)
    }

    pub fn translate_func_ty(&mut self, func_sig: &pas_ty::FunctionSig) -> TypeDefID {
        self.module.translate_func_ty(func_sig, self.type_args.as_ref())
    }

    pub fn build_closure_expr(&mut self, func: &pas_ty::ast::AnonymousFunctionDef) -> Ref {
        let closure = self.module.build_closure_instance(func, self.type_args.clone());

        if func.captures.len() == 0 {
            let static_closure = self.module.build_static_closure_instance(closure);

            Ref::Global(GlobalRef::StaticClosure(static_closure.id))
        } else {
            self.build_closure_instance(closure)
        }
    }

    pub fn build_closure_instance(&mut self, closure: ClosureInstance) -> Ref {
        let closure_def = self.module.metadata.get_struct_def(closure.closure_id).cloned().unwrap();

        let closure_ptr_ty = closure.closure_ptr_ty();

        let closure_ref = self.local_new(closure_ptr_ty.clone(), None);
        self.scope(|builder| {
            builder.append(Instruction::RcNew {
                out: closure_ref.clone(),
                struct_id: closure.closure_id,
            });

            let func_ty = Type::Function(closure.func_ty_id);
            let func_field_ptr = builder.local_new(func_ty.ptr(), None);
            builder.field(func_field_ptr.clone(), closure_ref.clone(), closure_ptr_ty.clone(), CLOSURE_PTR_FIELD);

            // initialize closure reference to function
            let func_ref = Ref::Global(GlobalRef::Function(closure.func_instance.id));
            builder.mov(func_field_ptr.clone().to_deref(), func_ref);

            // initialize closure capture fields - copy from local scope into closure object
            for (field_id, field_def) in closure_def.fields.iter() {
                if *field_id == CLOSURE_PTR_FIELD {
                    continue;
                }

                let capture_field_ptr = builder.local_temp(field_def.ty.clone().ptr());
                builder.field(capture_field_ptr.clone(), closure_ref.clone(), closure_ptr_ty.clone(), *field_id);

                let capture_field = capture_field_ptr.to_deref();

                let captured_local_id = builder.find_local(&field_def.name).unwrap().id();
                builder.mov(capture_field.clone(), Ref::Local(captured_local_id));

                builder.retain(capture_field.clone(), &field_def.ty);
            }
        });

        closure_ref
    }

    pub fn pretty_ty_name(&self, ty: &Type) -> Cow<str> {
        self.module.metadata.pretty_ty_name(ty)
    }

    pub fn find_or_insert_string(&mut self, s: &str) -> StringID {
        self.module.metadata.find_or_insert_string(s)
    }

    pub fn get_struct(&self, id: TypeDefID) -> Option<&Struct> {
        self.module.metadata.get_struct_def(id)
    }

    pub fn get_iface(&self, id: InterfaceID) -> Option<&Interface> {
        self.module.metadata.get_iface_def(id)
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

        if self.module.opts.debug_info {
            self.append(Instruction::DebugPop);
        }

        self.instructions
    }

    pub fn push_debug_context(&mut self, ctx: Span) {
        if !self.opts().debug_info {
            return;
        }

        self.current_scope_mut().inc_debug_ctx_count();

        self.append(Instruction::DebugPush(ctx));
    }

    pub fn pop_debug_context(&mut self) {
        if !self.opts().debug_info {
            return;
        }

        self.current_scope_mut().dec_debug_ctx_count();

        self.append(Instruction::DebugPop);
    }

    pub fn append(&mut self, instruction: Instruction) {
        if instruction.should_discard() {
            return;
        }

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

    pub fn add(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Add {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        });
    }

    pub fn sub(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Sub {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        });
    }

    pub fn mul(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Mul {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        });
    }

    pub fn idiv(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::IDiv {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        });
    }

    pub fn and(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::And {
            a: a.into(),
            b: b.into(),
            out: out.into(),
        });
    }

    pub fn and_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        match (a.into(), b.into()) {
            (Value::LiteralBool(a), Value::LiteralBool(b)) => Value::LiteralBool(a && b),

            (a, b) => {
                let result = self.local_temp(Type::Bool);
                self.and(result.clone(), a, b);
                Value::Ref(result)
            }
        }
    }

    pub fn not_to_val(&mut self, bool_val: impl Into<Value>) -> Value {
        match bool_val.into() {
            Value::LiteralBool(b) => Value::LiteralBool(!b),

            other_val => {
                let result = self.local_temp(Type::Bool);
                self.not(result.clone(), other_val);
                Value::Ref(result)
            }
        }
    }

    pub fn not(&mut self, out: impl Into<Ref>, bool_val: impl Into<Value>) {
        self.append(Instruction::Not {
            a: bool_val.into(),
            out: out.into(),
        });
    }

    pub fn eq(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Eq {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        })
    }

    pub fn gt(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Gt {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        })
    }

    pub fn lt(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        let out = out.into();
        let a = a.into();
        let b = b.into();

        // let out := a lte b
        self.lte(out.clone(), a.clone(), b.clone());

        // let neq := !(a = b)
        let neq = self.local_temp(Type::Bool);
        self.eq(neq.clone(), a, b);
        self.not(neq.clone(), neq.clone());

        // let out := out and neq
        self.append(Instruction::And {
            a: Value::Ref(out.clone()),
            b: Value::Ref(neq),
            out,
        })
    }

    pub fn lt_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        match (a.into(), b.into()) {
            (Value::LiteralI32(lit_a), Value::LiteralI32(lit_b)) => {
                Value::LiteralBool(lit_a < lit_b)
            }
            (Value::LiteralByte(lit_a), Value::LiteralByte(lit_b)) => {
                Value::LiteralBool(lit_a < lit_b)
            }
            (Value::LiteralF32(lit_a), Value::LiteralF32(lit_b)) => {
                Value::LiteralBool(lit_a < lit_b)
            }

            (a, b) => {
                let result = self.local_temp(Type::Bool);
                self.lt(result.clone(), a, b);
                Value::Ref(result)
            }
        }
    }

    #[allow(unused)]
    pub fn lte(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        let out = out.into();

        // out := a gt b
        self.gt(out.clone(), a, b);

        // out := not out
        self.not(out.clone(), out);
    }

    #[allow(unused)]
    pub fn gte(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        let out = out.into();
        let a = a.into();
        let b = b.into();

        // out := a > b
        self.gt(out.clone(), a.clone(), b.clone());

        // let eq := a = b
        let eq = self.local_temp(Type::Bool);
        self.append(Instruction::Eq {
            a,
            b,
            out: eq.clone(),
        });

        // out := out or eq
        self.append(Instruction::Or {
            a: Value::Ref(out.clone()),
            b: Value::Ref(eq),
            out,
        })
    }

    pub fn gte_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        match (a.into(), b.into()) {
            (Value::LiteralI32(lit_a), Value::LiteralI32(lit_b)) => {
                Value::LiteralBool(lit_a >= lit_b)
            }
            (Value::LiteralByte(lit_a), Value::LiteralByte(lit_b)) => {
                Value::LiteralBool(lit_a >= lit_b)
            }
            (Value::LiteralF32(lit_a), Value::LiteralF32(lit_b)) => {
                Value::LiteralBool(lit_a >= lit_b)
            }

            (a, b) => {
                let result = self.local_temp(Type::Bool);
                self.gte(result.clone(), a, b);
                Value::Ref(result)
            }
        }
    }

    pub fn field(
        &mut self,
        out: impl Into<Ref>,
        base: impl Into<Ref>,
        base_ty: impl Into<Type>,
        field: FieldID,
    ) {
        self.append(Instruction::Field {
            out: out.into(),
            a: base.into(),
            of_ty: base_ty.into(),
            field,
        })
    }

    pub fn label(&mut self, label: Label) {
        self.append(Instruction::Label(label))
    }

    pub fn jmp(&mut self, dest: Label) {
        self.append(Instruction::Jump { dest })
    }

    pub fn jmp_if(&mut self, dest: Label, cond: impl Into<Value>) {
        self.append(Instruction::JumpIf {
            dest,
            test: cond.into(),
        })
    }

    pub fn addr_of(&mut self, out: impl Into<Ref>, a: impl Into<Ref>) {
        self.append(Instruction::AddrOf {
            out: out.into(),
            a: a.into(),
        })
    }

    pub fn cast(&mut self, out: impl Into<Ref>, val: impl Into<Value>, ty: Type) {
        self.append(Instruction::Cast {
            out: out.into(),
            a: val.into(),
            ty,
        })
    }

    pub fn variant_tag(&mut self, out: impl Into<Ref>, a: impl Into<Ref>, of_ty: Type) {
        self.append(Instruction::VariantTag {
            out: out.into(),
            a: a.into(),
            of_ty
        })
    }

    pub fn variant_data(&mut self, out: impl Into<Ref>, a: impl Into<Ref>, of_ty: Type, tag: usize) {
        self.append(Instruction::VariantData {
            out: out.into(),
            a: a.into(),
            of_ty,
            tag,
        })
    }

    pub fn bind_param(&mut self, id: LocalID, ty: Type, name: impl Into<String>, by_ref: bool) {
        self.current_scope_mut().bind_param(id, name, ty, by_ref);
    }

    // binds an anonymous return local in %0 with the indicated type
    pub fn bind_return(&mut self, ty: Type) {
        self.current_scope_mut().bind_return(ty);
    }

    // binds an anonymous local binding for the closure pointer of a function
    pub fn bind_closure_ptr(&mut self, ty: Type) -> LocalID {
        let id = self.next_local_id();
        self.current_scope_mut().bind_temp(id, ty);

        id
    }

    pub fn next_local_id(&self) -> LocalID {
        self.scopes
            .iter()
            .flat_map(|scope| scope.locals().iter())
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

        let id = self.next_local_id();
        self.instructions
            .push(Instruction::LocalAlloc(id, ty.clone()));

        self.current_scope_mut().bind_temp(id, ty);

        Ref::Local(id)
    }

    // creates a managed local with type `ty`
    pub fn local_new(&mut self, ty: Type, name: Option<String>) -> Ref {
        assert_ne!(Type::Nothing, ty);

        let id = self.next_local_id();
        self.instructions
            .push(Instruction::LocalAlloc(id, ty.clone()));

        self.current_scope_mut().bind_new(id, name, ty);

        Ref::Local(id)
    }

    pub fn find_local(&self, name: &str) -> Option<&Local> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.local_by_name(name))
    }

    pub fn local_closure_capture(&mut self, ty: Type, name: String) -> Ref {
        assert_ne!(Type::Nothing, ty);
        let id = self.next_local_id();

        self.instructions.push(Instruction::LocalAlloc(id, ty.clone()));

        self.current_scope_mut().bind_param(id, name, ty, true);

        Ref::Local(id)
    }

    /// call `f` for every structural member of the object of type `ty` found at the `at`
    /// reference.
    ///
    /// returns `true` if calling `f` for any of the members (or members of members) returns `true`
    pub fn visit_deep<Visitor>(&mut self, at: Ref, ty: &Type, f: Visitor) -> bool
    where
        Visitor: Fn(&mut Builder, &Type, Ref) -> bool + Copy,
    {
        match ty {
            Type::Struct(struct_id) => {
                let struct_def = self.module.metadata.get_struct_def(*struct_id).unwrap();

                let fields: Vec<_> = struct_def
                    .fields
                    .iter()
                    .map(|(field_id, field)| (*field_id, field.ty.clone()))
                    .collect();

                let mut result = false;
                for (field, field_ty) in fields {
                    if !(field_ty.is_rc() || field_ty.is_complex()) {
                        continue;
                    }

                    // store the field pointer in a temp slot
                    let field_val = self.local_temp(field_ty.clone().ptr());
                    self.append(Instruction::Field {
                        out: field_val.clone(),
                        a: at.clone(),
                        of_ty: Type::Struct(*struct_id),
                        field,
                    });

                    result |= self.visit_deep(field_val.to_deref(), &field_ty, f);
                }

                result
            }

            Type::Variant(id) => {
                let cases = &self
                    .module
                    .metadata
                    .get_variant_def(*id)
                    .unwrap_or_else(|| panic!("missing variant def {}", id))
                    .cases
                    .to_vec();

                let tag_ptr = self.local_temp(Type::I32.ptr());
                let is_not_case = self.local_temp(Type::Bool);

                // get the tag
                self.variant_tag(tag_ptr.clone(), at.clone(), Type::Variant(*id));

                // jump out of the search loop if we find the matching case
                let break_label = self.alloc_label();

                let mut result = false;

                // for each case, check if the tag matches and jump past it if not

                for (tag, case) in cases.iter().enumerate() {
                    if self.opts().debug_info {
                        self.comment(&format!("testing for variant case {} ({})", tag, case.name));
                    }

                    if let Some(data_ty) = &case.ty {
                        if !(data_ty.is_rc() || data_ty.is_complex()) {
                            continue;
                        }

                        let skip_case_label = self.alloc_label();

                        // is_not_case := tag_ptr^ != tag
                        let tag_val = Value::LiteralI32(tag as i32);
                        self.eq(is_not_case.clone(), tag_ptr.clone().to_deref(), tag_val);
                        self.not(is_not_case.clone(), is_not_case.clone());

                        self.jmp_if(skip_case_label, is_not_case.clone());

                        // get ptr into case data and visit it

                        // only one of these allocations will occur depending on which case is
                        // active, so a scope is needed here to stop the local counter being
                        // incremented once per case
                        self.scope(|builder| {
                            let data_ptr = builder.local_temp(data_ty.clone().ptr());
                            builder.variant_data(data_ptr.clone(), at.clone(), Type::Variant(*id), tag);

                            result |= builder.visit_deep(data_ptr.to_deref(), &data_ty, f);
                        });

                        // break after any case executes
                        self.jmp(break_label);

                        // jump to here if this case isn't active
                        self.label(skip_case_label);
                    }
                }

                self.label(break_label);

                result
            }

            Type::Array { element, dim } => {
                if !element.is_rc() && !element.is_complex() {
                    return false;
                }

                let element_ptr = self.local_temp((**element).clone().ptr());
                let mut result = false;

                for i in 0..*dim {
                    self.append(Instruction::Element {
                        out: element_ptr.clone(),
                        a: at.clone(),
                        element: (**element).clone(),
                        index: Value::LiteralI32(i as i32), // todo: real usize type,
                    });

                    result |= self.visit_deep(element_ptr.clone().to_deref(), element, f);
                }

                result
            }

            // field or element
            _ => f(self, ty, at),
        }
    }

    // generate deep (retain, release) funcs for complex types
    pub fn gen_rc_boilerplate(&mut self, ty: &Type) -> RcBoilerplatePair {
        if let Some(boilerplate) = self.module.metadata.find_rc_boilerplate(ty) {
            return boilerplate.clone();
        }

        // declare new func IDs then define them here
        let funcs = self.module.metadata.declare_rc_boilerplate(ty);

        let release_body = {
            let mut release_builder = Builder::new(&mut self.module);
            release_builder.bind_param(LocalID(0), ty.clone().ptr(), "target", true);
            let target_ref = Ref::Local(LocalID(0)).to_deref();

            release_builder.visit_deep(target_ref, ty, |builder, el_ty, el_ref| {
                builder.release(el_ref, el_ty)
            });
            release_builder.finish()
        };

        self.module.insert_func(
            funcs.release,
            Function::Local(FunctionDef {
                body: release_body,
                return_ty: Type::Nothing,
                params: vec![ty.clone().ptr()],
                debug_name: format!("<generated releaser for {}>", self.pretty_ty_name(ty)),
                src_span: self.module_span.clone(),
            }),
        );

        let retain_body = {
            let mut retain_builder = Builder::new(&mut self.module);
            retain_builder.bind_param(LocalID(0), ty.clone().ptr(), "target", true);
            let target_ref = Ref::Local(LocalID(0)).to_deref();
            retain_builder.visit_deep(target_ref, ty, |builder, el_ty, el_ref| {
                builder.retain(el_ref, el_ty)
            });
            retain_builder.finish()
        };

        self.module.insert_func(
            funcs.retain,
            Function::Local(FunctionDef {
                body: retain_body,
                return_ty: Type::Nothing,
                params: vec![ty.clone().ptr()],
                debug_name: format!("generated RC retain func for {}", self.pretty_ty_name(ty)),
                src_span: self.module_span.clone(),
            }),
        );

        funcs
    }

    pub fn retain(&mut self, at: Ref, ty: &Type) -> bool {
        if self.opts().annotate_rc {
            self.comment(&format!("retain: {}", self.pretty_ty_name(ty)));
        }

        match ty {
            Type::Array { .. } | Type::Struct(..) | Type::Variant(..) => {
                let rc_funcs = self.gen_rc_boilerplate(ty);

                let at_ptr = self.local_temp(ty.clone().ptr());
                self.append(Instruction::AddrOf {
                    out: at_ptr.clone(),
                    a: at,
                });

                self.append(Instruction::Call {
                    function: Value::Ref(Ref::Global(GlobalRef::Function(rc_funcs.retain))),
                    args: vec![Value::Ref(at_ptr)],
                    out: None,
                });

                true
            }

            _ if ty.is_rc() => {
                self.append(Instruction::Retain { at });

                true
            }

            _ => {
                // not an RC type, nor a complex type containing RC types
                false
            }
        }
    }

    pub fn release(&mut self, at: Ref, ty: &Type) -> bool {
        if self.opts().annotate_rc {
            self.comment(&format!("release: {}", self.pretty_ty_name(ty)));
        }

        match ty {
            Type::Array { .. } | Type::Struct(..) | Type::Variant(..) => {
                let rc_funcs = self.gen_rc_boilerplate(ty);

                let at_ptr = self.local_temp(ty.clone().ptr());
                self.append(Instruction::AddrOf {
                    out: at_ptr.clone(),
                    a: at,
                });

                self.append(Instruction::Call {
                    function: Value::Ref(Ref::Global(GlobalRef::Function(rc_funcs.release))),
                    args: vec![Value::Ref(at_ptr)],
                    out: None,
                });

                true
            }

            _ if ty.is_rc() => {
                self.append(Instruction::Release { at });

                true
            }

            _ => {
                // not an RC type, nor a complex type containing RC types
                false
            }
        }
    }

    pub fn begin_loop_body_scope(&mut self, continue_label: Label, break_label: Label) {
        self.loop_stack.push(LoopScope {
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

    pub fn loop_body_scope<F>(
        &mut self,
        continue_label: Label,
        break_label: Label,
        f: F,
    ) -> &[Instruction]
    where
        F: FnOnce(&mut Self),
    {
        let start_instruction = self.instructions.len();

        self.begin_loop_body_scope(continue_label, break_label);

        f(self);

        self.end_loop_body_scope();

        &self.instructions[start_instruction..]
    }

    pub fn current_loop(&self) -> Option<&LoopScope> {
        self.loop_stack.last()
    }

    pub fn begin_scope(&mut self) {
        self.append(Instruction::LocalBegin);
        self.scopes.push(Scope::new());

        if self.opts().annotate_scopes {
            self.comment(&format!("begin scope {}", self.scopes.len()));
        }
    }

    pub fn scope<F>(&mut self, f: F) -> &[Instruction]
    where
        F: FnOnce(&mut Self),
    {
        let start_index = self.instructions.len();

        self.begin_scope();
        f(self);
        self.end_scope();

        &self.instructions[start_index..]
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

        let cleanup_range = to_scope..=last_scope;

        if self.opts().debug_info {
            let debug_pops: usize = self.scopes[cleanup_range.clone()]
                .iter()
                .map(|scope| scope.debug_ctx_count())
                .sum();

            for _ in 0..debug_pops {
                // don't call the helper func to do this, we don't want to modify the scope here
                self.append(Instruction::DebugPop);
            }
        }

        // locals from all scopes up to the target scope, in order of deepest->shallowest,
        // then in reverse allocation order
        let locals: Vec<_> = self.scopes[cleanup_range]
            .iter()
            .rev()
            .flat_map(|scope| scope.locals().iter().rev().cloned())
            .collect();

        // release local bindings that will be lost when the current scope is popped.
        // of course. releasing a ref should either insert a release instruction directly
        // (for an RC pointer) or insert a call to a structural release function (for
        // complex types containing RC pointers), so should never introduce new locals
        // in the scope being popped
        for local in locals {
            if self.opts().annotate_rc {
                self.comment(&format!("expire {}", local.id()));
            }

            match local {
                Local::Param { id, ty, by_ref, .. } => {
                    if !by_ref {
                        self.release(Ref::Local(id), &ty);
                    }
                }

                Local::New { id, ty, .. } => {
                    self.release(Ref::Local(id), &ty);
                }

                Local::Temp { .. } => {
                    // no cleanup required
                }

                Local::Return { .. } => {
                    if self.opts().annotate_rc {
                        self.comment("expire return slot");
                    }
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
        self.append(Instruction::LocalEnd);
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

    pub fn exit_function(&mut self) {
        self.cleanup_scope(0);

        self.append(Instruction::Jump { dest: EXIT_LABEL })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn end_loop_scope_ends_at_right_scope_level() {
        let ctx = pas_ty::Context::root(true, Span::zero("test"));
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
        let ctx = pas_ty::Context::root(true, Span::zero("test"));
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

        // Both locals should be released
        let expect = &[
            Instruction::Release {
                at: Ref::Local(LocalID(1)),
            },
            Instruction::Release {
                at: Ref::Local(LocalID(0)),
            },
            // and the final jmp for the break
            Instruction::Jump { dest: break_label },
        ];

        assert_eq!(expect, &builder.instructions[from + 1..]);
    }
}

fn type_args_to_string(args: &pas_ty::TypeList) -> String {
    args.items
        .iter()
        .map(|a| a.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}
