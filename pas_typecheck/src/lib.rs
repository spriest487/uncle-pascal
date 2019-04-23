pub use self::{annotation::*, context::*, result::*, ty::*};

mod annotation;
mod context;
mod result;

pub mod ast {
    pub use self::{
        block::*, cond::*, ctor::*, expression::*, function::*, iter::*, op::*, statement::*,
        typedecl::*, unit::*,
    };

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
        pub use pas_common::span::*;
        pub use pas_syn::{
            ast::{self, FunctionParamMod},
            ident::*,
            parse::InvalidStatement,
        };

        pub use crate::{
            annotation::*, ast::*, context::*, result::*, ty::*,
        };
    }
}

pub mod ty;

#[cfg(test)]
mod test {
    use crate::FunctionParamSig;

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
            params: vec![FunctionParamSig::by_val(Type::GenericSelf)],
        };

        let impl_sig = FunctionSig {
            return_ty: Type::Nothing,
            params: vec![FunctionParamSig::by_val(INT32)],
        };

        assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
    }

    #[test]
    fn sig_with_self_param_and_return_is_valid_impl() {
        let iface_sig = FunctionSig {
            return_ty: Type::GenericSelf,
            params: vec![FunctionParamSig::by_val(Type::GenericSelf)],
        };

        let impl_sig = FunctionSig {
            return_ty: INT32,
            params: vec![FunctionParamSig::by_val(INT32)],
        };

        assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
    }

    #[test]
    fn sig_with_mismatched_self_param_and_return_is_invalid_impl() {
        let iface_sig = FunctionSig {
            return_ty: Type::GenericSelf,
            params: vec![FunctionParamSig::by_val(Type::GenericSelf)],
        };

        let impl_sig = FunctionSig {
            return_ty: INT32,
            params: vec![FunctionParamSig::by_val(BOOL)],
        };

        assert_eq!(None, iface_sig.impl_ty(&impl_sig));
    }
}
