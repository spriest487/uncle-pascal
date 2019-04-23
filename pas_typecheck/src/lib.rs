mod result;
mod context;
mod annotation;

pub mod ast {
    mod expression;
    mod statement;
    mod unit;
    mod function;
    mod typedecl;
    mod block;
    mod ctor;
    mod op;
    mod cond;
    mod iter;

    mod prelude {
        pub use {
            crate::{
                context::*,
                ast::*,
                result::*,
                Type,
                TypeAnnotation,
                Primitive,
                FunctionSig,
            },
            pas_common::{
                span::*,
            },
            pas_syn::{
                ast,
            },
        };
    }

    pub use self::{
        op::*,
        cond::*,
        expression::*,
        statement::*,
        unit::*,
        function::*,
        typedecl::*,
        block::*,
        ctor::*,
        iter::*,
    };
}

pub use self::{
    result::*,
    context::*,
    ty::*,
    annotation::*,
};

pub mod ty {
    use {
        crate::{
            TypeAnnotation,
            ast::{
                Class,
                Interface,
            }
        },
        std::{
            rc::Rc,
            fmt,
        },
        pas_syn::{
            ast::{
                self,
                ClassKind,
            },
            Ident,
            Operator,
        },
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
                return_ty: decl.return_ty.clone()
                    .unwrap_or(Type::Nothing),
            }
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
        pub fn full_name(&self) -> Option<String> {
            match self {
                Type::Nothing => Some("Nothing".to_string()),
                Type::GenericSelf => Some("Self".to_string()),
                Type::Primitive(p) => Some(p.name().to_string()),
                Type::Record(class) | Type::Class(class) => Some(class.ident.to_string()),
                _ => None,
            }
        }

        pub fn of_decl(type_decl: &ast::TypeDecl<TypeAnnotation>) -> Self {
            match type_decl {
                ast::TypeDecl::Class(class @ ast::Class { kind: ClassKind::Record, .. }) => {
                    Type::Record(Rc::new(class.clone()))
                }

                ast::TypeDecl::Class(class @ ast::Class { kind: ClassKind::Object, .. }) => {
                    Type::Class(Rc::new(class.clone()))
                }

                ast::TypeDecl::Interface(iface) => {
                    Type::Interface(Rc::new(iface.clone()))
                }
            }
        }

        pub fn find_member(&self, ident: &Ident) -> Option<&Type> {
            match self {
                Type::Record(class) | Type::Class(class) => class.find_member(ident)
                    .map(|m| &m.ty),

                _ => None,
            }
        }

        pub fn get_member(&self, index: usize) -> Option<MemberRef> {
            match self {
                Type::Record(class) | Type::Class(class) => class.members.get(index)
                    .map(|m| MemberRef { ty: &m.ty, ident: &m.ident }),
                _ => None,
            }
        }

        pub fn members_len(&self) -> usize {
            match self {
                Type::Record(class) | Type::Class(class) => class.members.len(),
                _ => 0,
            }
        }

        pub fn members<'ty>(&'ty self) -> impl Iterator<Item=MemberRef<'ty>> {
            (0..self.members_len())
                .map(move |m| self.get_member(m).unwrap())
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
                (Type::Pointer(_), Operator::Plus, Type::Pointer(_)) |
                (Type::Pointer(_), Operator::Minus, Type::Pointer(_)) |
                (Type::Pointer(_), Operator::Plus, Type::Primitive(Primitive::Int32)) |
                (Type::Pointer(_), Operator::Minus, Type::Primitive(Primitive::Int32)) => true,

                _ => *self == *rhs,
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
                    Type::Pointer(target_ty) => write!(f, "^{}", target_ty),
                    _ => unimplemented!("type with no Display impl: {:?}", self),
                }
            }
        }
    }

    #[derive(Debug, Copy, Clone)]
    pub struct MemberRef<'ty> {
        pub ident: &'ty Ident,
        pub ty: &'ty Type,
    }
}
