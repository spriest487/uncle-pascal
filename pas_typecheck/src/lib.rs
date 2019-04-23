mod annotation;
mod context;
mod result;

pub mod ast {
    mod block;
    mod cond;
    mod ctor;
    mod expression;
    mod function;
    mod iter;
    mod op;
    mod statement;
    mod typedecl;
    mod unit;

    mod prelude {
        pub use crate::{
            ast::*,
            context::*,
            result::*,
            FunctionSig,
            MethodAnnotation,
            Primitive,
            Type,
            TypeAnnotation,
        };
        pub use pas_common::span::*;
        pub use pas_syn::{
            ast,
            ident::*,
        };
    }

    pub use self::{
        block::*,
        cond::*,
        ctor::*,
        expression::*,
        function::*,
        iter::*,
        op::*,
        statement::*,
        typedecl::*,
        unit::*,
    };
}

pub use self::{
    annotation::*,
    context::*,
    result::*,
    ty::*,
};

pub mod ty {
    use crate::{
        ast::{
            Class,
            FunctionDecl,
            Interface,
        },
        TypeAnnotation,
    };
    use pas_common::span::*;
    use pas_syn::{
        ast::{
            self,
            ClassKind,
            Typed,
        },
        ident::*,
        Operator,
    };
    use std::{
        fmt,
        rc::Rc,
    };

    #[derive(Eq, PartialEq, Hash, Clone, Debug)]
    pub struct FunctionSig {
        pub return_ty: Type,
        pub params: Vec<Type>,
    }

    impl FunctionSig {
        pub fn of_decl(decl: &ast::FunctionDecl<TypeAnnotation>) -> Self {
            Self {
                params: decl.params.iter().map(|p| p.ty.clone()).collect(),
                return_ty: decl.return_ty.clone().unwrap_or(Type::Nothing),
            }
        }

        /// replace all `Self`-typed args with `self_ty`
        pub fn with_self(&self, self_ty: &Type) -> Self {
            let mut result = self.clone();
            for param in &mut result.params {
                if *param == Type::GenericSelf {
                    *param = self_ty.clone();
                }
            }

            result
        }

        /// given that `self` is the sig of an interface method with one
        /// or more `Self`-typed arguments, find the expected self-type for
        pub fn impl_ty_from_args<'arg>(&self, args: &'arg [Type]) -> Option<&'arg Type> {
            if args.len() != self.params.len() {
                return None;
            }

            let self_arg_pos = self
                .params
                .iter()
                .position(|arg| *arg == Type::GenericSelf)?;

            Some(&args[self_arg_pos])
        }

        /// given that `self` is the sig of an interface method,
        /// for what type does `impl_func` implement this method, if any?
        pub fn impl_ty<'func>(&self, impl_func: &'func Self) -> Option<&'func Type> {
            if self.params.len() != impl_func.params.len() {
                return None;
            }

            let self_type = if self.return_ty == Type::GenericSelf {
                &impl_func.return_ty
            } else {
                self.params
                    .iter()
                    .position(|param| *param == Type::GenericSelf)
                    .map(|pos| &impl_func.params[pos])?
            };

            // `Nothing` can't have interface impls
            if *self_type == Type::Nothing {
                return None;
            }

            let self_positions: Vec<_> = self
                .params
                .iter()
                .enumerate()
                .filter_map(|(pos, param)| {
                    if *param == Type::GenericSelf {
                        Some(pos)
                    } else {
                        None
                    }
                })
                .collect();

            for pos in 0..self.params.len() {
                if self_positions.contains(&pos) {
                    // self-typed params must all be the same type as either the
                    // first such param, or the return type if it's self-typed too
                    if impl_func.params[pos] != *self_type {
                        return None;
                    }
                } else if impl_func.params[pos] != self.params[pos] {
                    // non-self params must match exactly
                    return None;
                }
            }

            Some(self_type)
        }
    }

    impl fmt::Display for FunctionSig {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "function(")?;

            for (i, param) in self.params.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", param)?;
            }
            write!(f, ")")?;

            match &self.return_ty {
                Type::Nothing => Ok(()),
                ty => write!(f, ": {}", ty),
            }
        }
    }

    #[derive(Eq, PartialEq, Hash, Clone, Debug)]
    pub enum Primitive {
        Boolean,
        Byte,
        Int32,
        Real32,
    }

    impl Primitive {
        pub fn name(&self) -> &str {
            match self {
                Primitive::Boolean => "Boolean",
                Primitive::Byte => "Byte",
                Primitive::Int32 => "Integer",
                Primitive::Real32 => "Real32",
            }
        }
    }

    #[derive(Eq, PartialEq, Hash, Clone, Debug)]
    pub enum Type {
        Nothing,
        Nil,
        Primitive(Primitive),
        Pointer(Box<Type>),
        Function(Rc<FunctionSig>),
        Record(Rc<Class>),
        Class(Rc<Class>),
        Interface(Rc<Interface>),
        GenericSelf,
    }

    impl From<Primitive> for Type {
        fn from(primitive: Primitive) -> Self {
            Type::Primitive(primitive)
        }
    }

    impl Type {
        pub fn full_path(&self) -> Option<IdentPath> {
            fn builtin_path(name: &str) -> IdentPath {
                let builtin_span = Span::zero("<builtin>");
                IdentPath::new(Ident::new(name, builtin_span), vec![])
            }

            match self {
                Type::Nothing => Some(builtin_path("Nothing")),
                Type::GenericSelf => Some(builtin_path("Self")),
                Type::Primitive(p) => Some(builtin_path(p.name())),
                Type::Interface(iface) => Some(IdentPath::new(iface.ident.clone(), vec![])),
                Type::Record(class) | Type::Class(class) => {
                    Some(IdentPath::new(class.ident.clone(), vec![]))
                },
                _ => None,
            }
        }

        pub fn full_name(&self) -> Option<String> {
            match self {
                Type::Nothing => Some("Nothing".to_string()),
                Type::GenericSelf => Some("Self".to_string()),
                Type::Primitive(p) => Some(p.name().to_string()),
                Type::Interface(iface) => Some(iface.ident.to_string()),
                Type::Record(class) | Type::Class(class) => Some(class.ident.to_string()),
                _ => None,
            }
        }

        pub fn of_decl(type_decl: &ast::TypeDecl<TypeAnnotation>) -> Self {
            match type_decl {
                ast::TypeDecl::Class(
                    class @ ast::Class {
                        kind: ClassKind::Record,
                        ..
                    },
                ) => Type::Record(Rc::new(class.clone())),

                ast::TypeDecl::Class(
                    class @ ast::Class {
                        kind: ClassKind::Object,
                        ..
                    },
                ) => Type::Class(Rc::new(class.clone())),

                ast::TypeDecl::Interface(iface) => Type::Interface(Rc::new(iface.clone())),
            }
        }

        pub fn find_member(&self, ident: &Ident) -> Option<MemberRef> {
            match self {
                Type::Record(class) | Type::Class(class) => {
                    class.find_member(ident).map(|m| MemberRef {
                        ident: &m.ident,
                        ty: &m.ty,
                    })
                },

                _ => None,
            }
        }

        pub fn get_member(&self, index: usize) -> Option<MemberRef> {
            match self {
                Type::Record(class) | Type::Class(class) => {
                    class.members.get(index).map(|m| MemberRef {
                        ty: &m.ty,
                        ident: &m.ident,
                    })
                },
                _ => None,
            }
        }

        pub fn members_len(&self) -> usize {
            match self {
                Type::Record(class) | Type::Class(class) => class.members.len(),
                _ => 0,
            }
        }

        pub fn members<'ty>(&'ty self) -> impl Iterator<Item = MemberRef<'ty>> {
            (0..self.members_len()).map(move |m| self.get_member(m).unwrap())
        }

        pub fn is_rc(&self) -> bool {
            match self {
                Type::Class(_) => true,
                Type::Record(_) => false,
                _ => false,
            }
        }

        pub fn deref_ty(&self) -> Option<&Type> {
            match self {
                Type::Pointer(ty) => Some(ty.as_ref()),
                _ => None,
            }
        }

        pub fn ptr(self) -> Self {
            Type::Pointer(Box::new(self))
        }

        pub fn indirect_by(self, indirection: usize) -> Self {
            (0..indirection).fold(self, |ty, _| ty.ptr())
        }

        pub fn self_comparable(&self) -> bool {
            match self {
                Type::Class(_) | Type::Record(_) | Type::Nothing | Type::Function(_) => false,
                _ => true,
            }
        }

        pub fn assignable_from(&self, from: &Self) -> bool {
            match self {
                Type::Pointer(_) => *self == *from || *from == Type::Nil,
                Type::Function(_) => false,
                _ => *self == *from,
            }
        }

        pub fn valid_math_op(&self, op: Operator, rhs: &Self) -> bool {
            match (self, op, rhs) {
                (Type::Pointer(_), Operator::Plus, Type::Pointer(_))
                | (Type::Pointer(_), Operator::Minus, Type::Pointer(_))
                | (Type::Pointer(_), Operator::Plus, Type::Primitive(Primitive::Int32))
                | (Type::Pointer(_), Operator::Minus, Type::Primitive(Primitive::Int32)) => true,

                _ => *self == *rhs,
            }
        }

        pub fn get_method(&self, method: &Ident) -> Option<&FunctionDecl> {
            match self {
                Type::Interface(iface) => iface.methods.iter().find(|m| m.ident == *method),
                _ => None,
            }
        }

        pub fn expect_something(self, msg: &str) -> Self {
            match self {
                Type::Nothing => panic!("expected a type: {}", msg),
                x => x,
            }
        }
    }

    impl fmt::Display for Type {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self.full_name() {
                Some(name) => write!(f, "{}", name),
                None => match self {
                    Type::Nil => write!(f, "nil"),
                    Type::Class(class) => write!(f, "{}", class.ident),
                    Type::Record(class) => write!(f, "{}", class.ident),
                    Type::Interface(iface) => write!(f, "{}", iface.ident),
                    Type::Pointer(target_ty) => write!(f, "^{}", target_ty),
                    _ => unimplemented!("type with no Display impl: {:?}", self),
                },
            }
        }
    }

    impl Typed for Type {
        fn is_known(&self) -> bool {
            true
        }
    }

    #[derive(Debug, Copy, Clone)]
    pub struct MemberRef<'ty> {
        pub ident: &'ty Ident,
        pub ty: &'ty Type,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INT32: Type = Type::Primitive(Primitive::Int32);
    const BOOL: Type = Type::Primitive(Primitive::Boolean);

    #[test]
    fn sig_without_self_is_invalid_impl() {
        let iface_sig = FunctionSig {
            return_ty: BOOL,
            params: vec![],
        };

        let impl_sig = FunctionSig {
            return_ty: INT32,
            params: vec![],
        };

        assert_eq!(None, iface_sig.impl_ty(&impl_sig));
    }

    #[test]
    fn sig_with_self_return_is_valid_impl() {
        let iface_sig = FunctionSig {
            return_ty: Type::GenericSelf,
            params: vec![],
        };

        let impl_sig = FunctionSig {
            return_ty: INT32,
            params: vec![],
        };

        assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
    }

    #[test]
    fn sig_with_no_params_is_invalid_impl() {
        let iface_sig = FunctionSig {
            return_ty: Type::Nothing,
            params: vec![],
        };

        let impl_sig = FunctionSig {
            return_ty: Type::Nothing,
            params: vec![],
        };

        assert_eq!(None, iface_sig.impl_ty(&impl_sig));
    }

    #[test]
    fn sig_with_self_param_is_valid_impl() {
        let iface_sig = FunctionSig {
            return_ty: Type::Nothing,
            params: vec![Type::GenericSelf],
        };

        let impl_sig = FunctionSig {
            return_ty: Type::Nothing,
            params: vec![INT32],
        };

        assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
    }

    #[test]
    fn sig_with_self_param_and_return_is_valid_impl() {
        let iface_sig = FunctionSig {
            return_ty: Type::GenericSelf,
            params: vec![Type::GenericSelf],
        };

        let impl_sig = FunctionSig {
            return_ty: INT32,
            params: vec![INT32],
        };

        assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
    }

    #[test]
    fn sig_with_mismatched_self_param_and_return_is_invalid_impl() {
        let iface_sig = FunctionSig {
            return_ty: Type::GenericSelf,
            params: vec![Type::GenericSelf],
        };

        let impl_sig = FunctionSig {
            return_ty: INT32,
            params: vec![BOOL],
        };

        assert_eq!(None, iface_sig.impl_ty(&impl_sig));
    }
}
