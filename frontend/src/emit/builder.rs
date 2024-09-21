pub mod scope;

#[cfg(test)]
mod test;

use self::scope::*;
use crate::emit::metadata::*;
use crate::emit::module_builder::{FunctionDeclKey, FunctionDefKey, ModuleBuilder};
use crate::emit::FunctionInstance;
use crate::emit::IROptions;
use common::span::Span;
use common::span::Spanned;
use crate::ast as syn;
use crate::typ as typ;
use crate::typ::Specializable;
use crate::typ::SYSTEM_UNIT_NAME;
use ir_lang::*;
use std::borrow::Cow;
use std::fmt;
use syn::Ident;
use syn::IdentPath;
use syn::TypeList;

#[derive(Debug)]
pub struct Builder<'m> {
    module: &'m mut ModuleBuilder,

    //positional list of type args that can be used to reify types in the current context
    type_args: Option<typ::TypeList>,

    instructions: Vec<Instruction>,
    scopes: Vec<Scope>,
    next_label: Label,

    loop_stack: Vec<LoopScope>,
}

impl<'m> Builder<'m> {
    pub fn new(module: &'m mut ModuleBuilder) -> Self {
        let module_span = module.module_span().clone();

        let mut instructions = Vec::new();
        if module.opts().debug {
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
        }
    }

    pub fn opts(&self) -> &IROptions {
        &self.module.opts()
    }

    pub fn type_args(&self) -> Option<&typ::TypeList> {
        self.type_args.as_ref()
    }

    pub fn with_type_args(self, type_args: typ::TypeList) -> Self {
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
        variant: &typ::Symbol,
        case: &Ident,
    ) -> (TypeDefID, usize, Option<&'ty Type>) {
        let name_path = self.translate_name(variant);

        let (id, variant_struct) = match self.module.metadata().find_variant_def(&name_path) {
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

    pub fn translate_name(&mut self, name: &typ::Symbol) -> NamePath {
        translate_name(name, self.type_args().cloned().as_ref(), self.module)
    }

    pub fn translate_class(&mut self, class_def: &typ::ast::StructDef) -> Struct {
        translate_struct_def(class_def, self.type_args().cloned().as_ref(), self.module)
    }

    pub fn translate_iface(&mut self, iface_def: &typ::ast::InterfaceDecl) -> Interface {
        translate_iface(iface_def, self.type_args().cloned().as_ref(), self.module)
    }

    pub fn translate_type(&mut self, src_ty: &typ::Type) -> Type {
        self.module
            .translate_type(src_ty, self.type_args().cloned().as_ref())
    }

    pub fn translate_dyn_array_struct(&mut self, element_ty: &typ::Type) -> TypeDefID {
        self.module
            .translate_dyn_array_struct(element_ty, self.type_args().cloned().as_ref())
    }

    pub fn translate_method_impl(
        &mut self,
        iface: IdentPath,
        method: Ident,
        self_ty: typ::Type,
    ) -> FunctionInstance {
        self.module.translate_method_impl(iface, method, self_ty, self.type_args.clone())
    }

    pub fn translate_func(
        &mut self,
        func_name: IdentPath,
        type_args: Option<typ::TypeList>,
    ) -> FunctionInstance {
        // specialize type args for current context
        let instance_type_args = match (type_args, self.type_args.as_ref()) {
            (Some(func_ty_args), Some(current_ty_args)) => {
                let items = func_ty_args
                    .items
                    .iter()
                    .map(|arg| self.module.specialize_generic_type(arg, current_ty_args));

                Some(TypeList::new(items, func_ty_args.span().clone()))
            },

            (Some(func_ty_args), None) => Some(func_ty_args),

            (None, ..) => None,
        };

        let key = FunctionDefKey {
            type_args: instance_type_args,
            decl_key: FunctionDeclKey::Function { name: func_name },
        };

        self.module.instantiate_func(key)
    }

    pub fn translate_func_ty(&mut self, func_sig: &typ::FunctionSig) -> TypeDefID {
        self.module
            .translate_func_ty(func_sig, self.type_args.as_ref())
    }

    pub fn build_closure_expr(&mut self, func: &typ::ast::AnonymousFunctionDef) -> Ref {
        let closure = self
            .module
            .build_closure_instance(func, self.type_args.clone());

        if func.captures.len() == 0 {
            let static_closure = self.module.build_static_closure_instance(closure);

            Ref::Global(GlobalRef::StaticClosure(static_closure.id))
        } else {
            self.build_closure_instance(closure)
        }
    }
    
    pub fn build_function_closure(&mut self, func: &FunctionInstance) -> Ref {
        let static_closure = self.module.build_func_static_closure_instance(func);

        Ref::Global(GlobalRef::StaticClosure(static_closure.id))
    }

    pub fn build_closure_instance(&mut self, closure: ClosureInstance) -> Ref {
        let closure_def = self
            .module
            .metadata()
            .get_struct_def(closure.closure_id)
            .cloned()
            .unwrap();

        let closure_ptr_ty = closure.closure_ptr_ty();

        // virtual pointer to the closure
        let closure_ref = self.local_new(closure_ptr_ty.clone(), None);

        self.scope(|builder| {
            builder.append(Instruction::RcNew {
                out: closure_ref.clone(),
                struct_id: closure.closure_id,
            });

            // the closure pointer type (a virtual pointer to any closure of this function type)
            // and the closure structure pointer type are different - we need to use the closure
            // *structure* pointer type to set members of this specific closure instance!
            let closure_struct_ty = Type::Struct(closure.closure_id);
            let closure_struct_ptr_ty = closure_struct_ty.clone().ptr();

            // downcast virtual closure ptr to the concrete closure struct
            let closure_struct_ref = builder.local_temp(closure_struct_ptr_ty.clone());
            builder.cast(closure_struct_ref.clone(), closure_ref.clone(), closure_struct_ptr_ty.clone());

            let func_ptr_ty = closure_def.fields[&CLOSURE_PTR_FIELD].ty.clone();

            let func_field_ptr = builder.local_new(func_ptr_ty.clone().ptr(), None);
            builder.field(
                func_field_ptr.clone(),
                closure_struct_ref.clone().to_deref(),
                closure_struct_ty.clone(),
                CLOSURE_PTR_FIELD,
            );

            // initialize closure reference to function
            let func_ref = Ref::Global(GlobalRef::Function(closure.func_instance.id));
            // builder.cast(func_field_ptr.clone().to_deref(), func_ref, func_ptr_ty.clone());
            builder.mov(func_field_ptr.clone().to_deref(), func_ref);
            
            // initialize closure capture fields - copy from local scope into closure object
            for (field_id, field_def) in closure_def.fields.iter() {
                // skip the closure pointer field
                if *field_id == CLOSURE_PTR_FIELD {
                    continue;
                }

                // skip unnamed (padding) fields
                let field_name = match &field_def.name {
                    None => continue,
                    Some(name) => name,
                };

                let capture_field_ptr = builder.local_temp(field_def.ty.clone().ptr());
                builder.field(
                    capture_field_ptr.clone(),
                    closure_struct_ref.clone().to_deref(),
                    closure_struct_ty.clone(),
                    *field_id,
                );

                let capture_field = capture_field_ptr.to_deref();

                let captured_local_id = builder.find_local(field_name).unwrap().id();
                builder.mov(capture_field.clone(), Ref::Local(captured_local_id));

                builder.retain(capture_field.clone(), &field_def.ty);
            }
        });

        closure_ref
    }

    pub fn pretty_ty_name(&self, ty: &Type) -> Cow<str> {
        self.module.metadata().pretty_ty_name(ty)
    }

    pub fn find_or_insert_string(&mut self, s: &str) -> StringID {
        self.module.metadata_mut().find_or_insert_string(s)
    }

    pub fn get_struct(&self, id: TypeDefID) -> Option<&Struct> {
        self.module.metadata().get_struct_def(id)
    }

    pub fn get_iface(&self, id: InterfaceID) -> Option<&Interface> {
        self.module.metadata().get_iface_def(id)
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

        if self.module.opts().debug {
            self.append(Instruction::DebugPop);
        }

        self.instructions
    }

    pub fn push_debug_context(&mut self, ctx: Span) {
        if !self.opts().debug {
            return;
        }

        self.current_scope_mut().inc_debug_ctx_count();

        self.append(Instruction::DebugPush(ctx));
    }

    pub fn pop_debug_context(&mut self) {
        if !self.opts().debug {
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
        if !self.opts().debug {
            return;
        }

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
        self.append(Instruction::Div {
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
            },
        }
    }

    pub fn not_to_val(&mut self, bool_val: impl Into<Value>) -> Value {
        match bool_val.into() {
            Value::LiteralBool(b) => Value::LiteralBool(!b),

            other_val => {
                let result = self.local_temp(Type::Bool);
                self.not(result.clone(), other_val);
                Value::Ref(result)
            },
        }
    }

    pub fn not(&mut self, out: impl Into<Ref>, bool_val: impl Into<Value>) {
        self.append(Instruction::Not {
            a: bool_val.into(),
            out: out.into(),
        });
    }

    pub fn or(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.append(Instruction::Or {
            a: a.into(),
            b: b.into(),
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
        
        // out := not (a >= b)
        let gte = self.gte_to_val(a, b);
        self.not(out, gte);
    }

    pub fn lt_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            Value::LiteralBool(a_val < b_val)
        } else {
            let result = self.local_temp(Type::Bool);
            self.lt(result.clone(), a, b);
            Value::Ref(result)
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
        self.eq(eq.clone(), a, b);

        // out := out or eq
        self.or(out.clone(), eq, out)
    }

    pub fn gte_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            Value::LiteralBool(a_val >= b_val)
        } else {
            let result = self.local_temp(Type::Bool);
            self.gte(result.clone(), a, b);
            Value::Ref(result)
        }
    }

    pub fn size_of(&mut self, out: impl Into<Ref>, ty: Type) {
        self.mov(out, Value::SizeOf(ty));
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

    pub fn field_val(
        &mut self,
        out: impl Into<Ref>,
        base: impl Into<Ref>,
        base_ty: impl Into<Type>,
        field: FieldID,
        field_ty: Type,
    ) {
        self.scope(|builder| {
            let field_ptr = builder.local_temp(field_ty.ptr());
            builder.field(field_ptr.clone(), base, base_ty, field);

            builder.mov(out, field_ptr.to_deref())
        });
    }

    pub fn set_field(
        &mut self,
        base: impl Into<Ref>,
        base_ty: impl Into<Type>,
        field: FieldID,
        field_ty: Type,
        val: impl Into<Value>,
    ) {
        self.scope(|builder| {
            let field_ptr = builder.local_temp(field_ty.ptr());
            builder.field(field_ptr.clone(), base, base_ty, field);

            builder.mov(field_ptr.to_deref(), val);
        });
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

    pub fn call(
        &mut self,
        function: impl Into<Value>,
        args: impl IntoIterator<Item = Value>,
        out: Option<Ref>,
    ) {
        self.append(Instruction::Call {
            function: function.into(),
            args: args.into_iter().collect(),
            out,
        })
    }

    fn instantiate_system_func(&mut self, name: &str) -> FunctionID {
        let zero_span = Span::zero("");
        let ident_path = syn::IdentPath::new(Ident::new(name, zero_span.clone()), [
            Ident::new(SYSTEM_UNIT_NAME, zero_span),
        ]);

        let instance = self.module.instantiate_func(FunctionDefKey {
            decl_key: FunctionDeclKey::Function {
                name: ident_path,
            },
            type_args: None,
        });
        instance.id
    }

    pub fn get_mem(&mut self, count: impl Into<Value>, out: Ref) {
        let get_mem_func_id = self.instantiate_system_func("GetMem");
        let function_ref = Ref::Global(GlobalRef::Function(get_mem_func_id));
        self.call(function_ref, [count.into()], Some(out));
    }

    pub fn free_mem(&mut self, at: impl Into<Value>) {
        let free_mem_func_id = self.instantiate_system_func("FreeMem");
        let function_ref = Ref::Global(GlobalRef::Function(free_mem_func_id));
        self.call(function_ref, [at.into()], None);
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
            of_ty,
        })
    }

    pub fn variant_data(
        &mut self,
        out: impl Into<Ref>,
        a: impl Into<Ref>,
        of_ty: Type,
        tag: usize,
    ) {
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
    pub fn bind_return(&mut self) {
        self.current_scope_mut().bind_return();
    }

    // binds an anonymous local binding for the closure pointer of a function
    pub fn bind_closure_ptr(&mut self) -> LocalID {
        let id = self.next_local_id();
        self.current_scope_mut().bind_temp(id);

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

        self.current_scope_mut().bind_temp(id);

        Ref::Local(id)
    }

    // creates a managed local with type `ty_def`
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

        self.instructions
            .push(Instruction::LocalAlloc(id, ty.clone()));

        self.current_scope_mut().bind_param(id, name, ty, true);

        Ref::Local(id)
    }

    /// call `f` for every structural member of the object of type `ty_def` found at the `at`
    /// reference.
    ///
    /// returns `true` if calling `f` for any of the members (or members of members) returns `true`
    pub fn visit_deep<Visitor>(&mut self, at: Ref, ty: &Type, f: Visitor) -> bool
    where
        Visitor: Fn(&mut Builder, &Type, Ref) -> bool + Copy,
    {
        match ty {
            Type::Struct(struct_id) => {
                let struct_def = self.module.metadata().get_struct_def(*struct_id).unwrap();

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
            },

            Type::Variant(id) => {
                let cases = &self
                    .module
                    .metadata()
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
                    if self.opts().debug {
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
                            builder.variant_data(
                                data_ptr.clone(),
                                at.clone(),
                                Type::Variant(*id),
                                tag,
                            );

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
            },

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
            },

            // field or element
            _ => f(self, ty, at),
        }
    }

    pub fn retain(&mut self, at: Ref, ty: &Type) -> bool {
        if self.opts().annotate_rc {
            self.comment(&format!("retain: {}", self.pretty_ty_name(ty)));
        }

        match ty {
            Type::Array { .. } | Type::Struct(..) | Type::Variant(..) => {
                let rc_funcs = self.module.runtime_type(ty);

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
            },

            _ if ty.is_rc() => {
                self.append(Instruction::Retain { at });

                true
            },

            _ => {
                // not an RC type, nor a complex type containing RC types
                false
            },
        }
    }

    pub fn release(&mut self, at: Ref, ty: &Type) -> bool {
        if self.opts().annotate_rc {
            self.comment(&format!("release: {}", self.pretty_ty_name(ty)));
        }

        match ty {
            Type::Array { .. } | Type::Struct(..) | Type::Variant(..) => {
                let rc_funcs = self.module.runtime_type(ty);

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
            },

            _ if ty.is_rc() => {
                self.append(Instruction::Release { at });

                true
            },

            _ => {
                // not an RC type, nor a complex type containing RC types
                false
            },
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

        if self.opts().debug {
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

        if self.opts().debug {
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

        if self.opts().debug {
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
                },

                Local::New { id, ty, .. } => {
                    self.release(Ref::Local(id), &ty);
                },

                Local::Temp { .. } => {
                    // no cleanup required
                },

                Local::Return { .. } => {
                    if self.opts().annotate_rc {
                        self.comment("expire return slot");
                    }
                },
            }
        }
    }

    pub fn end_scope(&mut self) {
        self.cleanup_scope(self.scopes.len() - 1);

        if self.opts().debug {
            self.comment(&format!("end scope {}", self.scopes.len()));
        }

        self.scopes.pop().unwrap();
        self.append(Instruction::LocalEnd);
    }

    pub fn break_loop(&mut self) {
        let (break_label, break_scope) = {
            let current_loop = self
                .current_loop()
                .expect("break stmt must appear in a loop");

            (current_loop.break_label, current_loop.block_level)
        };

        // write cleanup code for the broken scope and its children
        self.cleanup_scope(break_scope);

        // jump to the label (presumably somewhere outside the broken scope!)
        self.append(Instruction::Jump { dest: break_label });
    }

    pub fn continue_loop(&mut self) {
        let (continue_label, continue_scope) = {
            let current_loop = self
                .current_loop()
                .expect("continue stmt must appear in a loop");

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

fn type_args_to_string(args: &typ::TypeList) -> String {
    args.items
        .iter()
        .map(|a| a.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

pub fn jmp_exists(instructions: &[Instruction], to_label: Label) -> bool {
    instructions.iter().any(|i| match i {
        Instruction::Jump { dest } | Instruction::JumpIf { dest, .. } => *dest == to_label,
        _ => false,
    })
}
