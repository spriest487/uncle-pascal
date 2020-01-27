use pas_typecheck as pas_ty;

use crate::{
    metadata::*, CachedFunction, Function, FunctionCacheKey, FunctionDeclKey, FunctionDef,
    GlobalRef, IROptions, IdentPath, Instruction, Label, LocalID, Module, RcBoilerplatePair, Ref,
    Value,
};

use std::fmt;

use pas_common::span::Span;
use pas_syn::Ident;
use pas_typecheck::{builtin_string_name, Specializable};
use std::borrow::Cow;
use std::collections::HashMap;

pub const RETURN_REF: Ref = Ref::Local(LocalID(0));
pub const EXIT_LABEL: Label = Label(0);

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

            // the EXIT label is always reserved, so start one after that
            next_label: Label(EXIT_LABEL.0 + 1),

            scopes: vec![Scope { locals: Vec::new() }],

            loop_stack: Vec::new(),
        }
    }

    pub fn opts(&self) -> &IROptions {
        &self.module.opts
    }

    pub fn type_args(&self) -> &[pas_ty::Type] {
        &self.type_args
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

    #[allow(unused)]
    pub fn get_method_decl(
        &self,
        ty: &pas_ty::Type,
        method_ident: &Ident,
    ) -> Option<pas_ty::ast::FunctionDecl> {
        ty.get_method(method_ident, &self.module.src_metadata)
            .unwrap()
    }

    pub fn translate_variant_case<'ty>(
        &'ty mut self,
        variant: &pas_ty::QualifiedDeclName,
        case: &pas_syn::Ident,
    ) -> (StructID, usize, Option<&'ty Type>) {
        let name_path = self.translate_name(variant);

        let (id, variant_struct) = match self.module.metadata.find_variant_def(&name_path) {
            Some((id, variant_struct)) => (id, variant_struct),
            None => panic!("missing IR metadata definition for variant {}", variant),
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

    pub fn translate_name(&mut self, name: &pas_ty::QualifiedDeclName) -> NamePath {
        if name.is_generic() {
            panic!("can't translate unspecialized generic name: {}", name);
        }

        if name.type_args.iter().any(|arg| arg.is_generic_param()) {
            panic!(
                "can't translate name containing generic parameters: {}",
                name
            )
        }

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

    pub fn translate_class(&mut self, class_def: &pas_ty::ast::Class) -> Struct {
        let name_path = self.translate_name(&class_def.name);

        let mut fields = HashMap::new();
        for (id, member) in class_def.members.iter().enumerate() {
            let name = member.ident.to_string();
            let ty = self.translate_type(&member.ty);
            let rc = member.ty.is_rc();

            fields.insert(FieldID(id), StructField { name, ty, rc });
        }

        Struct::new(name_path).with_fields(fields)
    }

    pub fn translate_iface(&mut self, iface_def: &pas_ty::ast::Interface) -> Interface {
        let name = self.translate_name(&iface_def.name);

        // it needs to be declared to reference its own ID in the Self type
        let id = self.module.metadata.declare_iface(&name);

        let methods: Vec<_> = iface_def
            .methods
            .iter()
            .map(|method| {
                let self_ty = Type::RcPointer(Some(ClassID::Interface(id)));

                Method {
                    name: method.ident.to_string(),
                    return_ty: match &method.return_ty {
                        Some(pas_ty::Type::MethodSelf) => self_ty.clone(),
                        Some(return_ty) => self.translate_type(return_ty),
                        None => Type::Nothing,
                    },
                    params: method
                        .params
                        .iter()
                        .map(|param| match &param.ty {
                            pas_ty::Type::MethodSelf => self_ty.clone(),
                            param_ty => self.translate_type(param_ty),
                        })
                        .collect(),
                }
            })
            .collect();

        Interface::new(name, methods)
    }

    pub fn translate_variant(&mut self, variant_def: &pas_ty::ast::Variant) -> Variant {
        let name_path = NamePath::from_decl(variant_def.name.clone(), &self.module.metadata);

        let mut cases = Vec::new();
        for case in &variant_def.cases {
            let (case_ty, case_rc) = match &case.data_ty {
                Some(data_ty) => (Some(self.translate_type(data_ty)), data_ty.is_rc()),
                None => (None, false),
            };
            cases.push(VariantCase {
                name: case.ident.to_string(),
                ty: case_ty,
                rc: case_rc,
            });
        }

        Variant {
            name: name_path,
            cases,
        }
    }

    pub fn translate_type(&mut self, src_ty: &pas_ty::Type) -> Type {
        let src_ty = src_ty.clone().substitute_type_args(&self.type_args);

        if let Some(cached) = self.module.type_cache.get(&src_ty) {
            //            println!("{} -> {}", src_ty, cached);

            return cached.clone();
        }

        //        println!("\ntype cache miss for {}, translating...", src_ty);
        //        for (cache_ty, _) in &self.module.type_cache {
        //            println!("{} == {}? {}", src_ty, cache_ty, src_ty == *cache_ty);
        //        }

        // instantiate types which may contain generic params
        let ty = match &src_ty {
            pas_ty::Type::Variant(variant) => {
                let variant_def = self
                    .module
                    .src_metadata
                    .instantiate_variant(variant)
                    .unwrap();

                let id = self.module.metadata.reserve_new_struct();
                let ty = Type::Variant(id);
                self.module.type_cache.insert(src_ty.clone(), ty.clone());
                //                println!("{} <- {}", src_ty, self.pretty_ty_name(&ty));

                let name_path = self.translate_name(&variant);
                self.module.metadata.declare_struct(id, &name_path);

                let variant_meta = self.translate_variant(&variant_def);

                self.module.metadata.define_variant(id, variant_meta);
                ty
            }

            pas_ty::Type::Record(name) | pas_ty::Type::Class(name) => {
                // handle builtin types
                if **name == builtin_string_name() {
                    let string_ty = Type::RcPointer(Some(ClassID::Class(STRING_ID)));
                    self.module
                        .type_cache
                        .insert(src_ty.clone(), string_ty.clone());
                    return string_ty;
                }

                let class_def = self.module.src_metadata.instantiate_class(name).unwrap();

                let id = self.module.metadata.reserve_new_struct();

                let ty = match class_def.kind {
                    pas_syn::ast::ClassKind::Object => Type::RcPointer(Some(ClassID::Class(id))),
                    pas_syn::ast::ClassKind::Record => Type::Struct(id),
                };

                self.module.type_cache.insert(src_ty.clone(), ty.clone());
                //                println!("{} <- {}", src_ty, ty);

                let name_path = self.translate_name(&name);

                self.module.metadata.declare_struct(id, &name_path);

                let struct_meta = self.translate_class(&class_def);
                self.module.metadata.define_struct(id, struct_meta);

                ty
            }

            pas_ty::Type::Interface(iface_def) => {
                let iface_def = self.module.src_metadata.find_iface_def(iface_def).unwrap();

                let iface_name = self.translate_name(&iface_def.name);
                let id = self.module.metadata.declare_iface(&iface_name);
                let ty = Type::RcPointer(Some(ClassID::Interface(id)));

                self.module.type_cache.insert(src_ty.clone(), ty.clone());
                //                println!("{} <- {}", src_ty, ty);

                let iface_meta = self.translate_iface(&iface_def);
                self.module.metadata.define_iface(iface_meta);

                ty
            }

            pas_ty::Type::DynArray { element } => {
                let id = self.translate_dyn_array_struct(&element);

                let ty = Type::RcPointer(Some(ClassID::Class(id)));
                self.module.type_cache.insert(src_ty.clone(), ty.clone());
                //                println!("{} <- {}", src_ty, ty);

                ty
            }

            real_ty => {
                // nothing to be instantiated
                let ty = self.module.metadata.find_type(real_ty);
                self.module.type_cache.insert(src_ty.clone(), ty.clone());
                //                println!("{} <- {}", src_ty, ty);

                ty
            }
        };

        ty
    }

    pub fn translate_dyn_array_struct(&mut self, element_ty: &pas_ty::Type) -> StructID {
        let element_ty = self.translate_type(element_ty);

        match self.module.metadata.find_dyn_array_struct(&element_ty) {
            Some(id) => id,
            None => self.module.metadata.define_dyn_array_struct(element_ty),
        }
    }

    pub fn translate_method_impl(
        &mut self,
        iface: IdentPath,
        method: Ident,
        self_ty: pas_ty::Type,
    ) -> CachedFunction {
        self.module.translate_method_impl(iface, method, self_ty)
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

        self.module.instantiate_func(key)
    }

    pub fn pretty_ty_name(&self, ty: &Type) -> Cow<str> {
        self.module.metadata.pretty_ty_name(ty)
    }

    pub fn find_or_insert_string(&mut self, s: &str) -> StringID {
        self.module.metadata.find_or_insert_string(s)
    }

    pub fn get_struct(&self, id: StructID) -> Option<&Struct> {
        self.module.metadata.get_struct_def(id)
    }

    pub fn get_iface(&self, id: InterfaceID) -> Option<&Interface> {
        self.module.metadata.get_iface_def(id)
    }

    #[allow(unused)]
    pub fn find_function(&self, name: &GlobalName) -> Option<FunctionID> {
        self.module.metadata.find_function(name)
    }

    #[allow(unused)]
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

    pub fn not(&mut self, bool_val: Value) -> Value {
        match bool_val {
            Value::LiteralBool(b) => Value::LiteralBool(!b),

            other_val => {
                let result = self.local_temp(Type::Bool);
                self.append(Instruction::Not {
                    a: other_val,
                    out: result.clone(),
                });
                Value::Ref(result)
            }
        }
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

    // creates a managed local with type `ty`
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
                let struct_def = self.module.metadata.get_struct_def(*struct_id).unwrap();

                let fields: Vec<_> = struct_def
                    .fields
                    .iter()
                    .map(|(field_id, field)| (*field_id, field.ty.clone()))
                    .collect();

                for (field, field_ty) in fields {
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

            Type::Variant(id) => {
                let cases = &self
                    .module
                    .metadata
                    .get_variant_def(*id)
                    .unwrap_or_else(|| panic!("missing variant def {}", id))
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
                    self.comment(&format!("testing for variant case {} ({})", tag, case.name));

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

    // generate deep (retain, release) funcs for complex types
    pub fn translate_rc_boilerplate(&mut self, ty: &Type) -> RcBoilerplatePair {
        if let Some(boilerplate) = self.module.metadata.find_rc_boilerplate(ty) {
            return boilerplate.clone();
        }

        // declare new func IDs then define them here
        let funcs = self.module.metadata.declare_rc_boilerplate(ty);

        let release_body = {
            let mut release_builder = Builder::new(&mut self.module);
            release_builder.bind_local(LocalID(0), ty.clone().ptr(), "target", true);
            let target_ref = Ref::Local(LocalID(0)).deref();

            release_builder.visit_deep(target_ref, ty, |builder, el_ty, el_ref| {
                builder.release(el_ref, el_ty);
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
            }),
        );

        let retain_body = {
            let mut retain_builder = Builder::new(&mut self.module);
            retain_builder.bind_local(LocalID(0), ty.clone().ptr(), "target", true);
            let target_ref = Ref::Local(LocalID(0)).deref();
            retain_builder.visit_deep(target_ref, ty, |builder, el_ty, el_ref| {
                builder.retain(el_ref, el_ty);
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
            }),
        );

        funcs
    }

    pub fn retain(&mut self, at: Ref, ty: &Type) {
        if self.opts().annotate_rc {
            self.comment(&format!("retain: {}", self.pretty_ty_name(ty)));
        }

        match ty {
            Type::Array { .. } | Type::Struct(..) | Type::Variant(..) => {
                let rc_funcs = self.translate_rc_boilerplate(ty);

                let at_ptr = self.local_temp(ty.clone().ptr());
                self.append(Instruction::AddrOf {
                    out: at_ptr.clone(),
                    a: at,
                });

                self.append(Instruction::Call {
                    function: Value::Ref(Ref::Global(GlobalRef::Function(rc_funcs.retain))),
                    args: vec![Value::Ref(at_ptr)],
                    out: None,
                })
            }

            _ if ty.is_rc() => {
                self.append(Instruction::Retain { at });
            }

            _ => {
                // not an RC type, nor a complex type containing RC types
            }
        }
    }

    pub fn release(&mut self, at: Ref, ty: &Type) {
        if self.opts().annotate_rc {
            self.comment(&format!("release: {}", self.pretty_ty_name(ty)));
        }

        match ty {
            Type::Array { .. } | Type::Struct(..) | Type::Variant(..) => {
                let rc_funcs = self.translate_rc_boilerplate(ty);

                let at_ptr = self.local_temp(ty.clone().ptr());
                self.append(Instruction::AddrOf {
                    out: at_ptr.clone(),
                    a: at,
                });

                self.append(Instruction::Call {
                    function: Value::Ref(Ref::Global(GlobalRef::Function(rc_funcs.release))),
                    args: vec![Value::Ref(at_ptr)],
                    out: None,
                })
            }

            _ if ty.is_rc() => {
                self.append(Instruction::Release { at });
            }

            _ => {
                // not an RC type, nor a complex type containing RC types
            }
        }
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

    pub fn exit_function(&mut self) {
        self.cleanup_scope(0);

        self.append(Instruction::Jump {
            dest: EXIT_LABEL
        })
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

fn type_args_to_string(args: &[pas_ty::Type]) -> String {
    args.iter()
        .map(|a| a.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}
