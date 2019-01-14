use {
    crate::{
        FunctionSig,
        Type,
    },
    pas_common::{
        span::*,
    },
    pas_syn::{
        ast,
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
}

impl Context {
    pub fn root() -> Self {
        let mut root_ctx = Self {
            scopes: vec![Scope::new(ScopeId(0))],
            next_id: ScopeId(1),
        };

        let builtin_span = Span {
            file: Rc::new("<builtin>".into()),
            start: Location { line: 0, col: 0 },
            end: Location { line: 0, col: 0 },
        };

        root_ctx.declare_type(Ident::new("Integer", builtin_span.clone()), Type::Integer)
            .unwrap();

        root_ctx.declare_function(Ident::new("WriteLn", builtin_span.clone()),
            FunctionSig {
                params: vec![Type::Integer],
                return_ty: None,
            }).unwrap();

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

    pub fn declare_type(&mut self, name: Ident, ty: Type) -> NamingResult<()> {
        self.declare(name, Decl::Type(ty))
    }

    pub fn declare_function(&mut self, name: Ident, sig: FunctionSig) -> NamingResult<()> {
        self.declare(name, Decl::BoundValue(Binding {
            kind: ValueKind::Function,
            ty: Type::Function(Box::new(sig)),
        }))
    }

    pub fn find_type(&self, ty: &ast::TypeName) -> NamingResult<&Type> {
        match ty {
            ast::TypeName::Ident(ident) => {
                match self.find(ident) {
                    Some((_, Decl::Type(ty))) => Ok(ty),
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