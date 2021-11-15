use crate::{FunctionParamSig, FunctionSig, Module, ModuleUnit, Primitive, Type};

use pas_common::span::Span;
use pas_common::BuildOptions;
use pas_syn::parse::TokenStream;
use pas_syn::{ast, Ident, IdentPath, TokenTree};

const INT32: Type = Type::Primitive(Primitive::Int32);
const BOOL: Type = Type::Primitive(Primitive::Boolean);

pub fn module_from_src(unit_name: &'static str, src: &'static str) -> Module {
    module_from_srcs(vec![(unit_name, src)])
}

pub fn module_from_srcs<UnitSources>(unit_srcs: UnitSources) -> Module
where
    UnitSources: IntoIterator<Item = (&'static str, &'static str)>,
{
    let mut units = Vec::new();

    for (unit_name, src) in unit_srcs {
        let tokens = TokenTree::tokenize(unit_name, src, &BuildOptions::default()).unwrap();
        let mut stream = TokenStream::new(tokens, Span::zero(unit_name));

        let unit_ident = Ident::new(unit_name, Span::zero(unit_name));

        let unit = ast::Unit::parse(&mut stream, IdentPath::from_parts(vec![unit_ident])).unwrap();

        units.push(unit);
    }

    Module::typecheck(&units, true).unwrap()
}

pub fn unit_from_src(unit_name: &'static str, src: &'static str) -> ModuleUnit {
    let mut module = module_from_src(unit_name, src);

    module.units.pop().unwrap()
}

pub fn units_from_src<UnitSources>(unit_srcs: UnitSources) -> Vec<ModuleUnit>
where
    UnitSources: IntoIterator<Item = (&'static str, &'static str)>,
{
    let module = module_from_srcs(unit_srcs);
    module.units
}

#[test]
fn sig_without_self_is_invalid_impl() {
    let iface_sig = FunctionSig {
        return_ty: BOOL,
        type_params: None,
        params: vec![],
    };

    let impl_sig = FunctionSig {
        return_ty: INT32,
        type_params: None,
        params: vec![],
    };

    assert_eq!(None, iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_self_return_is_valid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::MethodSelf,
        type_params: None,
        params: vec![],
    };

    let impl_sig = FunctionSig {
        return_ty: INT32,
        type_params: None,
        params: vec![],
    };

    assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_no_params_is_invalid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::Nothing,
        type_params: None,
        params: vec![],
    };

    let impl_sig = FunctionSig {
        return_ty: Type::Nothing,
        type_params: None,
        params: vec![],
    };

    assert_eq!(None, iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_self_param_is_valid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::Nothing,
        type_params: None,
        params: vec![FunctionParamSig::by_val(Type::MethodSelf)],
    };

    let impl_sig = FunctionSig {
        return_ty: Type::Nothing,
        type_params: None,
        params: vec![FunctionParamSig::by_val(INT32)],
    };

    assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_self_param_and_return_is_valid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::MethodSelf,
        type_params: None,
        params: vec![FunctionParamSig::by_val(Type::MethodSelf)],
    };

    let impl_sig = FunctionSig {
        return_ty: INT32,
        type_params: None,
        params: vec![FunctionParamSig::by_val(INT32)],
    };

    assert_eq!(Some(&INT32), iface_sig.impl_ty(&impl_sig));
}

#[test]
fn sig_with_mismatched_self_param_and_return_is_invalid_impl() {
    let iface_sig = FunctionSig {
        return_ty: Type::MethodSelf,
        type_params: None,
        params: vec![FunctionParamSig::by_val(Type::MethodSelf)],
    };

    let impl_sig = FunctionSig {
        return_ty: INT32,
        type_params: None,
        params: vec![FunctionParamSig::by_val(BOOL)],
    };

    assert_eq!(None, iface_sig.impl_ty(&impl_sig));
}
