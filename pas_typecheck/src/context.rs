use {
    crate::{
        ast::{
            Class,
            Interface,
            FunctionDecl,
            FunctionParam,
        },
        FunctionSig,
        Type,
        Primitive,
        TypeAnnotation,
    },
    pas_common::{
        span::*,
    },
    pas_syn::{
        ast::{
            self,
            ClassKind,
        },
        Ident,
    },
    std::{
        collections::hash_map::{
            HashMap,
            Entry,
        },
        fmt,
        rc::Rc,
    },
};

#[derive(Debug)]
pub enum NameError {
    NotFound(Ident),
    ExpectedType(Ident, Decl),
    ExpectedInterface(Ident, Decl),
    ExpectedBinding(Ident, Decl),
    ExpectedFunction(Ident, Decl),
    AlreadyDeclared { new: Ident, existing: Ident },
    AlreadyDefined { ident: Ident, existing: Span },
    Ambiguous { ident: Ident, options: Vec<Ident> },
}

impl Spanned for NameError {
    fn span(&self) -> &Span {
        match self {
            NameError::NotFound(ident) => &ident.span,
            NameError::ExpectedType(ident, _) => &ident.span,
            NameError::ExpectedInterface(ident, _) => &ident.span,
            NameError::ExpectedBinding(ident, _) => &ident.span,
            NameError::ExpectedFunction(ident, _) => &ident.span,
            NameError::AlreadyDeclared { new, .. } => &new.span,
            NameError::AlreadyDefined { ident, .. } => &ident.span,
            NameError::Ambiguous { ident, .. } => &ident.span,
        }
    }

    fn fmt_context(&self, mut f: impl fmt::Write, source: &str) -> fmt::Result {
        match self {
            NameError::AlreadyDeclared { new, existing } => {
                new.span.fmt_context(&mut f, source)?;
                writeln!(f, "Previously declared at:")?;
                existing.span.fmt_context(f, source)
            }

            NameError::AlreadyDefined { ident, existing } => {
                ident.span.fmt_context(&mut f, source)?;
                writeln!(f, "Previously defined at:")?;
                existing.fmt_context(f, source)
            }

            NameError::Ambiguous { ident, options } => {
                ident.span.fmt_context(&mut f, source)?;
                writeln!(f, "Could be one of the following:")?;
                for option in options {
                    option.span.fmt_context(&mut f, source)?;
                }
                Ok(())
            }

            _ => {
                writeln!(f, "{}", self)?;
                self.span().fmt_context(f, source)
            }
        }
    }
}

impl fmt::Display for NameError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NameError::NotFound(ident) => {
                write!(f, "`{}` was not found in this scope", ident)
            }
            NameError::ExpectedType(ident, unexpected) => {
                write!(f, "`{}` did not refer to a type in this scope (found: {})", ident, unexpected)
            }
            NameError::ExpectedInterface(ident, unexpected) => {
                write!(f, "`{}` did not refer to an interface in this scope (found: {})", ident, unexpected)
            }
            NameError::ExpectedBinding(ident, unexpected) => {
                write!(f, "`{}` did not refer to a value in this scope (found: {})", ident, unexpected)
            }
            NameError::ExpectedFunction(ident, unexpected) => {
                write!(f, "`{}` did not refer to a function in this scope (found: {})", ident, unexpected)
            }
            NameError::AlreadyDeclared { new, .. } => {
                write!(f, "`{}` was already declared in this scope", new)
            }
            NameError::AlreadyDefined { ident, .. } => {
                write!(f, "`{}` was already defined", ident)
            }
            NameError::Ambiguous { ident, .. } => {
                write!(f, "`{}` is ambiguous in this context", ident)
            }
        }
    }
}

pub type NamingResult<T> = Result<T, NameError>;

#[derive(Clone, Debug, PartialEq, Copy, Eq, Hash)]
pub enum ValueKind {
    /// local value in an immutable location
    Immutable,

    /// local value in mutable location
    Mutable,

    /// rvalue, e.g. value returned from function, result of operation,
    /// with no binding
    Temporary,

    /// top-level named function
    Function,
}

impl fmt::Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ValueKind::Immutable => write!(f, "Immutable binding"),
            ValueKind::Mutable => write!(f, "Mutable binding"),
            ValueKind::Temporary => write!(f, "Temporary value"),
            ValueKind::Function => write!(f, "Function"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Binding {
    pub ty: Type,
    pub kind: ValueKind,
    pub def: Option<Span>,
}

#[derive(Copy, Clone, Debug)]
pub enum InstanceMember<'a> {
    Data {
        ty: &'a Type,
    },
    Method {
        iface_ty: &'a Type,
        decl: &'a FunctionDecl,
    },
}

#[derive(Copy, Clone, Debug)]
pub enum TypeMember<'a> {
    Method {
        decl: &'a FunctionDecl,
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Type(Type),
    BoundValue(Binding),
//    Function(Binding),
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decl::Type(ty) => write!(f, "type `{}`", ty),
            Decl::BoundValue(binding) => write!(f, "{} of `{}`", binding.kind, binding.ty),
//            Decl::Function(func_binding) => write!(f, "{}", func_binding.ty),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct MethodImpl {
    def: bool,
}

#[derive(Clone, Debug, PartialEq)]
struct InterfaceImpl {
    methods: HashMap<Ident, MethodImpl>,
}

impl InterfaceImpl {
    fn new() -> Self {
        Self {
            methods: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash, Copy)]
pub struct ScopeId(usize);

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    id: ScopeId,
    decls: HashMap<Ident, Decl>,

    /// iface ident -> self ty -> impl details
    iface_impls: HashMap<Ident, HashMap<Type, InterfaceImpl>>,
}

impl Scope {
    fn new(id: ScopeId) -> Self {
        Self {
            id,
            decls: HashMap::new(),
            iface_impls: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Context {
    next_id: ScopeId,
    scopes: Vec<Scope>,

    string_class: Rc<Class>,
}

impl Context {
    pub fn root() -> Self {
        let builtin_span = Span {
            file: Rc::new("<builtin>".into()),
            start: Location { line: 0, col: 0 },
            end: Location { line: 0, col: 0 },
        };

        let disposable_iface = Rc::new(Interface {
            ident: Ident::new("Disposable", builtin_span.clone()),
            methods: vec![
                FunctionDecl {
                    ident: Ident::new("Dispose", builtin_span.clone()),
                    span: builtin_span.clone(),
                    impl_iface: None,
                    return_ty: Some(Type::Nothing),
                    params: vec![
                        FunctionParam {
                            ident: Ident::new("self", builtin_span.clone()),
                            span: builtin_span.clone(),
                            ty: Type::GenericSelf,
                        }
                    ],
                }
            ],
            span: builtin_span.clone(),
        });

        let string_class = Rc::new(Class {
            kind: ClassKind::Object,
            ident: Ident::new("String", builtin_span.clone()),
            span: builtin_span.clone(),
            members: vec![
                ast::Member {
                    ident: Ident::new("chars", builtin_span.clone()),
                    ty: Type::Primitive(Primitive::Byte).ptr(),
                    span: builtin_span.clone(),
                },
                ast::Member {
                    ident: Ident::new("len", builtin_span.clone()),
                    ty: Type::Primitive(Primitive::Int32),
                    span: builtin_span.clone(),
                },
            ],
        });

        let mut root_ctx = Self {
            scopes: vec![Scope::new(ScopeId(0))],
            next_id: ScopeId(1),
            string_class,
        };

        let nothing_ident = Ident::new("Nothing", builtin_span.clone());
        root_ctx.declare_type(nothing_ident, Type::Nothing).unwrap();

        let bool_ident = Ident::new(Primitive::Boolean.name(), builtin_span.clone());
        root_ctx.declare_type(bool_ident, Primitive::Boolean).unwrap();

        let byte_ident = Ident::new(Primitive::Byte.name(), builtin_span.clone());
        root_ctx.declare_type(byte_ident, Primitive::Byte).unwrap();

        let int_ident = Ident::new(Primitive::Int32.name(), builtin_span.clone());
        root_ctx.declare_type(int_ident, Primitive::Int32).unwrap();

        let single_ident = Ident::new(Primitive::Real32.name(), builtin_span.clone());
        root_ctx.declare_type(single_ident, Primitive::Real32).unwrap();

        let string_ident = root_ctx.string_class.ident.clone();
        root_ctx.declare_type(string_ident, Type::Class(root_ctx.string_class.clone())).unwrap();

        let disposable_ident = disposable_iface.ident.clone();
        root_ctx.declare_type(disposable_ident, Type::Interface(disposable_iface.clone())).unwrap();

        root_ctx.declare_method_impl(
            disposable_iface.ident.clone(),
            Type::Class(root_ctx.string_class.clone()),
            Ident::new("Dispose", builtin_span.clone()),
        ).unwrap();

        root_ctx.declare_function(
            Ident::new("GetMem", builtin_span.clone()),
            FunctionSig {
                params: vec![Type::Primitive(Primitive::Int32)],
                return_ty: Type::Primitive(Primitive::Byte).ptr(),
            })
            .unwrap();

        root_ctx.declare_function(
            Ident::new("FreeMem", builtin_span.clone()),
            FunctionSig {
                params: vec![Type::Primitive(Primitive::Byte).ptr()],
                return_ty: Type::Nothing,
            })
            .unwrap();

        root_ctx.declare_function(
            Ident::new("IntToStr", builtin_span.clone()),
            FunctionSig {
                params: vec![Primitive::Int32.into()],
                return_ty: Type::Class(root_ctx.string_class.clone()),
            })
            .unwrap();

        root_ctx.declare_function(
            Ident::new("WriteLn", builtin_span.clone()),
            FunctionSig {
                params: vec![Type::Class(root_ctx.string_class.clone())],
                return_ty: Type::Nothing,
            })
            .unwrap();

        // builtins are in scope 0, unit is scope 1
        root_ctx.push_scope();
        root_ctx
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        let i = self.scopes.len() - 1;
        &mut self.scopes[i]
    }

    pub fn push_scope(&mut self) -> ScopeId {
        let new_id = self.next_id;
        self.next_id = ScopeId(self.next_id.0 + 1);

        self.scopes.push(Scope::new(new_id));
        new_id
    }

    pub fn pop_scope(&mut self, id: ScopeId) {
        assert_ne!(ScopeId(0), id, "can't pop the root scope");

        loop {
            let popped = self.scopes.pop().expect("popped scope must exist!");
            if popped.id == id {
                break;
            }
        }
    }

    pub fn get(&self, name: &Ident) -> NamingResult<TypeAnnotation> {
        match self.find(name) {
            Some((_, Decl::Type(type_decl))) => {
                let sem = TypeAnnotation::Type(type_decl.clone(), name.span().clone());
                Ok(sem)
            },

            Some((_, Decl::BoundValue(binding))) => {
                let sem = TypeAnnotation::TypedValue {
                    value_kind: binding.kind,
                    ty: binding.ty.clone(),
                    span: name.span.clone(),
                };
                Ok(sem)
            },

            None => Err(NameError::NotFound(name.clone())),
        }
    }

    pub fn find(&self, name: &Ident) -> Option<(&Ident, &Decl)> {
        self.scopes.iter().rev()
            .find(|scope: &&Scope| scope.decls.contains_key(name))
            .and_then(|scope: &Scope| {
                scope.decls.iter().find(|(k, _v)| *k == name)
            })
    }

    fn declare(&mut self, name: Ident, decl: Decl) -> NamingResult<()> {
        match self.find(&name) {
            Some((old_ident, _old_decl)) => Err(NameError::AlreadyDeclared {
                new: name.clone(),
                existing: old_ident.clone(),
            }),

            None => {
                self.current_scope_mut().decls.insert(name, decl);
                Ok(())
            }
        }
    }

    pub fn declare_binding(&mut self, name: Ident, binding: Binding) -> NamingResult<()> {
        self.declare(name, Decl::BoundValue(binding))
    }

    pub fn declare_type(&mut self, name: Ident, ty: impl Into<Type>) -> NamingResult<()> {
        self.declare(name, Decl::Type(ty.into()))
    }

    pub fn declare_function(&mut self, name: Ident, sig: FunctionSig) -> NamingResult<()> {
        self.declare_function_and_def(name, sig, None)
    }

    fn method_impl_entry(&mut self,
                         iface_ident: Ident,
                         self_ty: Type,
                         method: Ident,
    ) -> NamingResult<Entry<Ident, MethodImpl>> {
        // check the method exists
        let iface = self.find_iface(&iface_ident)?;
        if iface.get_method(&method).is_none() {
            return Err(NameError::NotFound(method));
        }

        let ty_impls = self.current_scope_mut().iface_impls.entry(iface_ident)
            .or_insert_with(|| HashMap::new());
        let impl_for_ty = ty_impls.entry(self_ty)
            .or_insert_with(|| InterfaceImpl::new());

        Ok(impl_for_ty.methods.entry(method))
    }

    pub fn declare_method_impl(&mut self,
                               iface_ident: Ident,
                               self_ty: Type,
                               method: Ident,
    ) -> NamingResult<()> {
        self.method_impl_entry(iface_ident, self_ty, method)?
            .or_insert_with(|| MethodImpl { def: false });
        Ok(())
    }

    pub fn define_method_impl(&mut self,
                              iface_ident: Ident,
                              self_ty: Type,
                              method: Ident,
    ) -> NamingResult<()> {
        match self.method_impl_entry(iface_ident, self_ty, method.clone())? {
            Entry::Occupied(mut entry) => {
                if entry.get().def {
                    return Err(NameError::AlreadyDefined {
                        ident: method.clone(),
                        existing: entry.key().span.clone(),
                    });
                } else {
                    entry.get_mut().def = true;
                }
            }

            Entry::Vacant(entry) => {
                entry.insert(MethodImpl { def: true });
            }
        }

        Ok(())
    }

    fn declare_function_and_def(
        &mut self,
        name: Ident,
        sig: FunctionSig,
        def: Option<Span>)
        -> NamingResult<()>
    {
        self.declare(name, Decl::BoundValue(Binding {
            kind: ValueKind::Function,
            ty: Type::Function(Rc::new(sig)),
            def,
        }))
    }

    pub fn define_function(&mut self, name: Ident, sig: FunctionSig, def: Span) -> NamingResult<()> {
        let decl = self.scopes.iter_mut().rev()
            .find_map(|scope| scope.decls.get_mut(&name));

        match decl {
            Some(Decl::BoundValue(Binding { kind: ValueKind::Function, def: Some(existing_def), .. })) => {
                return Err(NameError::AlreadyDefined {
                    ident: name,
                    existing: existing_def.clone(),
                });
            }

            Some(Decl::BoundValue(ref mut binding @ Binding { kind: ValueKind::Function, def: None, .. })) => {
                binding.def = Some(def);
                Ok(())
            }

            Some(other) => {
                return Err(NameError::ExpectedFunction(name.clone(), other.clone()));
            }

            None => {
                self.declare_function_and_def(name, sig, Some(def))
            }
        }
    }

    pub fn find_type(&self, ty: &ast::TypeName) -> NamingResult<Type> {
        match ty {
            ast::TypeName::Ident { ident, indirection } => {
                match self.find(ident) {
                    Some((_, Decl::Type(ty))) => {
                        let ty = ty.clone().indirect_by(*indirection);
                        Ok(ty)
                    }
                    Some((_, unexpected)) => Err(NameError::ExpectedType(ident.clone(), unexpected.clone())),
                    None => Err(NameError::NotFound(ident.clone())),
                }
            }

            ast::TypeName::Unknown(_) => unreachable!("trying to resolve unknown type"),
        }
    }

    pub fn find_iface(&self, name: &Ident) -> NamingResult<&Interface> {
        match self.find(name) {
            Some((_, Decl::Type(Type::Interface(iface)))) => Ok(iface.as_ref()),
            Some((_, other)) => Err(NameError::ExpectedInterface(name.clone(), other.clone())),
            None => Err(NameError::NotFound(name.clone())),
        }
    }

    /// an instance method is an interface impl method for `ty` that takes Self as the first argument
    /// TODO: or any function taking `ty` as its first argument
    fn instance_methods_of(&self, ty: &Type) -> Vec<(&Type, &FunctionDecl)> {
        let mut methods = Vec::new();

        for scope in self.scopes.iter().rev() {
            for (iface_ident, iface_impls) in &scope.iface_impls {
                let (iface_ty, iface) = match self.find(iface_ident).unwrap() {
                    (_, Decl::Type(iface_ty @ Type::Interface(_))) => {
                        match iface_ty {
                            Type::Interface(iface) => (iface_ty, iface),
                            _ => unreachable!()
                        }
                    },
                    _ => panic!("invalid type referenced in iface impl"),
                };

                if iface_impls.contains_key(ty) {
                    let iface_instance_methods = iface.methods.iter()
                        .filter(|m| m.params.get(0)
                            .map(|arg_0| arg_0.ty == Type::GenericSelf)
                            .unwrap_or(false));

                    // add all the methods, we don't need to check if they're actually defined
                    // or implemented - we should check that elsewhere
                    for method in iface_instance_methods {
                        methods.push((iface_ty, method))
                    }
                }
            }
        }

        methods
    }

    pub fn find_instance_member<'ty, 'ctx: 'ty>(
        &'ctx self,
        of_ty: &'ty Type,
        member: &Ident,
    ) -> NamingResult<InstanceMember<'ty>> {
        let data_member = of_ty.find_member(member);

        let methods = self.instance_methods_of(of_ty);
        let matching_methods: Vec<_> = methods.iter()
            .filter(|(_of_ty, m)| m.ident == *member)
            .collect();

        match (data_member, matching_methods.len()) {
            (Some(data_member), 0) => {
                Ok(InstanceMember::Data {
                    ty: &data_member.ty,
                })
            }

            // unambiguous method
            (None, 1) => {
                let (iface_ty, method) = matching_methods[0];

                Ok(InstanceMember::Method {
                    iface_ty,
                    decl: *method,
                })
            }

            (None, 0) => {
                Err(NameError::NotFound(member.clone()))
            }

            (Some(data_member), _) => {
                Err(NameError::Ambiguous {
                    ident: member.clone(),
                    options: matching_methods.into_iter()
                        .map(|(_, m)| m.ident.clone())
                        .chain(vec![data_member.ident.clone()])
                        .collect()
                })
            }

            (None, _) => {
                Err(NameError::Ambiguous {
                    ident: member.clone(),
                    options: matching_methods.into_iter()
                        .map(|(_, m)| m.ident.clone())
                        .collect()
                })
            }
        }
    }

    pub fn find_type_member<'ty>(
        &self,
        ty: &'ty Type,
        member_ident: &Ident
    ) -> NamingResult<TypeMember<'ty>> {
        match ty {
            Type::Interface(iface) => {
                let method_decl = iface.get_method(member_ident)
                    .ok_or_else(|| NameError::NotFound(member_ident.clone()))?;

                Ok(TypeMember::Method {
                    decl: method_decl,
                })
            }

            _ => Err(NameError::NotFound(member_ident.clone())),
        }
    }

    pub fn find_named(&self, ident: &Ident) -> NamingResult<&Binding> {
        match self.find(ident) {
            Some((_, Decl::BoundValue(binding))) => Ok(binding),
            Some((_, unexpected)) => Err(NameError::ExpectedBinding(ident.clone(), unexpected.clone())),
            None => Err(NameError::NotFound(ident.clone())),
        }
    }

    pub fn string_type(&self) -> Type {
        Type::Class(self.string_class.clone())
    }

//    pub fn find_function(&self, ident: &Ident) -> NamingResult<&FunctionSig> {
//        match self.find(ident) {
//            Some((_, Decl::Function(sig))) => Ok(sig),
//            Some((_, Decl::BoundValue(Binding { ty: Type::Function(sig) }))) => {
//                Ok(sig)
//            }
//            Some((_, unexpected)) => Err(NameError::ExpectedFunction(ident.clone(), unexpected.clone())),
//            None => Err(NameError::NotFound(ident.clone())),
//        }
//    }
}
