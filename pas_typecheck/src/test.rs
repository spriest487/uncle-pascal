use crate::FunctionParamSig;

use super::*;

const INT32: Type = Type::Primitive(Primitive::Int32);
const BOOL: Type = Type::Primitive(Primitive::Boolean);

#[test]
fn sig_without_self_is_invalid_impl() {
    let iface_sig = FunctionSig {
        return_ty: BOOL,
        type_params_len: 0,
        params: vec![],
    };

    let impl_sig = FunctionSig {
        return_ty: INT32,
        type_params_len: 0,
        params: vec![],
    };

    assert_eq!(None, iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_self_return_is_valid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::MethodSelf,
        type_params_len: 0,
        params: vec![],
    };

    let impl_sig = FunctionSig {
        return_ty: INT32,
        type_params_len: 0,
        params: vec![],
    };

    assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_no_params_is_invalid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::Nothing,
        type_params_len: 0,
        params: vec![],
    };

    let impl_sig = FunctionSig {
        return_ty: Type::Nothing,
        type_params_len: 0,
        params: vec![],
    };

    assert_eq!(None, iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_self_param_is_valid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::Nothing,
        type_params_len: 0,
        params: vec![FunctionParamSig::by_val(Type::MethodSelf)],
    };

    let impl_sig = FunctionSig {
        return_ty: Type::Nothing,
        type_params_len: 0,
        params: vec![FunctionParamSig::by_val(INT32)],
    };

    assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_self_param_and_return_is_valid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::MethodSelf,
        type_params_len: 0,
        params: vec![FunctionParamSig::by_val(Type::MethodSelf)],
    };

    let impl_sig = FunctionSig {
        return_ty: INT32,
        type_params_len: 0,
        params: vec![FunctionParamSig::by_val(INT32)],
    };

    assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_mismatched_self_param_and_return_is_invalid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::MethodSelf,
        type_params_len: 0,
        params: vec![FunctionParamSig::by_val(Type::MethodSelf)],
    };

    let impl_sig = FunctionSig {
        return_ty: INT32,
        type_params_len: 0,
        params: vec![FunctionParamSig::by_val(BOOL)],
    };

    assert_eq!(None, iface_sig.impl_ty(&impl_sig));
}
