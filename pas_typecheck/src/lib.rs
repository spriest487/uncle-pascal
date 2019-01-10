mod result;
mod context;
mod annotation;

pub mod ast {
    mod expression;
    mod statement;
    mod unit;

    pub use self::{
        expression::*,
        statement::*,
        unit::*,
    };
}

pub use self::{
    result::*,
    context::*,
    ty::Type,
    annotation::*,
};

pub mod ty {
    use {
        std::fmt,
    };

    #[derive(Eq, PartialEq, Hash, Clone, Debug)]
    pub enum Type {
        None,
        Integer,
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
