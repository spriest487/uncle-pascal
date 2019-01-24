use {
    crate::{
        ast::{
            Class,
        },
        FunctionSig,
        Type,
        Primitive,
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
        collections::HashMap,
        fmt,
        rc::Rc,
    },
};

#[derive(Debug)]
pub enum NameError {
    NotFound(Ident),
    ExpectedType(Ident, Decl),
    ExpectedBinding(Ident, Decl),
    ExpectedFunction(Ident, Decl),
    AlreadyDeclared { new: Ident, existing: Ident },
}

impl Spanned for NameError {
    fn span(&self) -> &Span {
        match self {
            NameError::NotFound(ident) => &ident.span,
            NameError::ExpectedType(ident, _) => &ident.span,
            NameError::ExpectedBinding(ident, _) => &ident.span,
            NameError::ExpectedFunction(ident, _) => &ident.span,
            NameError::AlreadyDeclared { new, .. } => &new.span,
        }
    }

    fn fmt_context(&self, mut f: impl fmt::Write, source: &str) -> fmt::Result {
        match self {
            NameError::AlreadyDeclared { new, existing } => {
                new.span.fmt_context(&mut f, source)?;
                writeln!(f, "Previously declared at:")?;
                existing.span.fmt_context(f, source)
            }

            _ => self.span().fmt_context(f, source)
        }
    }
}

impl fmt::Display for NameError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NameError::NotFound(ident) => {
                write!(f, "symbol `{}` was not found in this scope", ident)
            }
            NameError::ExpectedType(ident, unexpected) => {
                write!(f, "`{}` did not refer to a type in this scope (found: {})", ident, unexpected)
            }
            NameError::ExpectedBinding(ident, unexpected) => {
                write!(f, "`{}` did not refer to a value in this scope (found: {})", ident, unexpected)
            }
            NameError::ExpectedFunction(ident, unexpected) => {
                write!(f, "`{}` did not refer to a function in this scope (found: {})", ident, unexpected)
            }
            NameError::AlreadyDeclared { new, .. } => {
                write!(f, "name `{}` was already declared in this scope", new)
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
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Type(Type),
    BoundValue(Binding),
    Function(Binding),
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decl::Type(ty) => write!(f, "type `{}`", ty),
            Decl::BoundValue(binding) => write!(f, "{} of `{}`", binding.kind, binding.ty),
            Decl::Function(func_binding) => write!(f, "{}", func_binding.ty),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, Hash, Copy)]
pub struct ScopeId(usize);

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    id: ScopeId,
    decls: HashMap<Ident, Decl>,
}

impl Scope {
    fn new(id: ScopeId) -> Self {
        Self {
            id,
            decls: HashMap::new(),
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

        root_ctx.declare_function(Ident::new("GetMem", builtin_span.clone()),
            FunctionSig {
                params: vec![Type::Primitive(Primitive::Int32)],
                return_ty: Type::Primitive(Primitive::Byte).ptr(),
            })
            .unwrap();

        root_ctx.declare_function(Ident::new("FreeMem", builtin_span.clone()),
            FunctionSig {
                params: vec![Type::Primitive(Primitive::Byte).ptr()],
                return_ty: Type::Nothing,
            })
            .unwrap();

        root_ctx.declare_function(Ident::new("IntToStr", builtin_span.clone()),
            FunctionSig {
                params: vec![Primitive::Int32.into()],
                return_ty: Type::Class(root_ctx.string_class.clone()),
            })
            .unwrap();

        root_ctx.declare_function(Ident::new("WriteLn", builtin_span.clone()),
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

    fn find(&self, name: &Ident) -> Option<(&Ident, &Decl)> {
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
        self.declare(name, Decl::BoundValue(Binding {
            kind: ValueKind::Function,
            ty: Type::Function(Rc::new(sig)),
        }))
    }

    pub fn find_type(&self, ty: &ast::TypeName) -> NamingResult<Type> {
        match ty {
            ast::TypeName::Ident { ident, indirection } => {
                match self.find(ident) {
                    Some((_, Decl::Type(ty))) => {
                        let ty = ty.clone().indirect_by(*indirection);
                        Ok(ty)
                    },
                    Some((_, unexpected)) => Err(NameError::ExpectedType(ident.clone(), unexpected.clone())),
                    None => Err(NameError::NotFound(ident.clone())),
                }
            }
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
