mod result;
mod context;
mod annotation;

pub mod ast {
    mod expression;
    mod statement;
    mod unit;
    mod function;
    mod block;

    mod prelude {
        pub use {
            pas_common::{
                span::*,
            },
            pas_syn::{
                ast,
            },
            crate::{
                context::*,
                ast::*,
                result::*,
                Type,
                TypeAnnotation,
                FunctionSig,
            },
        };
    }

    pub use self::{
        expression::*,
        statement::*,
        unit::*,
        function::*,
        block::*,
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
        std::fmt,
        crate::ast,
    };

    #[derive(Eq, PartialEq, Hash, Clone, Debug)]
    pub struct FunctionSig {
        pub return_ty: Option<Type>,
        pub params: Vec<Type>,
    }

    impl FunctionSig {
        pub fn of_decl(decl: &ast::FunctionDecl) -> &FunctionSig {
            match &decl.annotation.ty {
                Type::Function(sig) => sig.as_ref(),
                _ => unreachable!("functions always have function type"),
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
                Some(ty) => write!(f, ": {}", ty),
                None => Ok(()),
            }
        }
    }

    #[derive(Eq, PartialEq, Hash, Clone, Debug)]
    pub enum Type {
        None,
        Integer,
        Function(Box<FunctionSig>),
    }

    impl Type {
        fn full_name(&self) -> Option<String> {
            match self {
                Type::Integer => Some("Integer".to_string()),
                _ => None,
            }
        }
    }

    impl fmt::Display for Type {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self.full_name() {
                Some(name) => write!(f, "{}", name),
                None => match self {
                    Type::None => write!(f, "(no type)"),
                    _ => unimplemented!("type with no Display impl: {:?}", self),
                }
            }
        }
    }
}
