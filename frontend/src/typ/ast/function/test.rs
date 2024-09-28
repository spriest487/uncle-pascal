use crate::ast::TypeConstraint;
use crate::ast::TypeParam;
use crate::typ::ast::specialize_func_decl;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionParam;
use crate::typ::ast::TypedFunctionName;
use crate::typ::builtin_displayable_name;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeParamList;
use crate::typ::TypeParamType;
use crate::Ident;
use common::span::Span;

fn test_span() -> Span {
    Span::zero("test")
}

fn test_ident(name: &str) -> Ident {
    Ident::new(name, test_span())
}

fn make_ty_param(name: &str) -> TypeParam<Type> {
    TypeParam {
        name: test_ident(name),
        constraint: None,
    }
}

fn make_ty_param_of(name: &str, constraint: Type) -> TypeParam<Type> {
    let name = test_ident(name);
    
    TypeParam {
        name: name.clone(),
        constraint: Some(TypeConstraint {
            span: name.span.clone(),
            param_ident: name,
            is_ty: constraint,
        }),
    }
}

fn make_ty_param_ty(param_list: &[TypeParam<Type>], pos: usize) -> Type {
    let param = &param_list[pos];

    Type::GenericParam(Box::new(TypeParamType {
        name: param.name.clone(),
        is_iface: param
            .constraint
            .as_ref()
            .map(|constraint| Box::new(constraint.is_ty.clone())),
        pos,
    }))
}

fn make_decl(
    owning_ty: Option<Type>,
    name: &str,
    ty_params: impl IntoIterator<Item=TypeParam<Type>>,
    params: impl IntoIterator<Item=Type>,
    return_ty: Type
) -> FunctionDecl {
    let test_span = test_span();
    
    let ty_params: Vec<_> = ty_params.into_iter().collect();
    let ty_params_list = if ty_params.len() == 0 {
        None
    } else {
        Some(TypeParamList::new(ty_params, test_span.clone()))
    };

    FunctionDecl {
        name: TypedFunctionName{
            ident: test_ident(name),
            owning_ty,
            span: test_span.clone(),
        },
        params: params
            .into_iter()
            .enumerate()
            .map(|(pos, ty)| FunctionParam {
                ident: test_ident(&format!("arg{}", pos)),
                ty,
                modifier: None,
                span: test_span.clone(),
            })
            .collect(),
        type_params: ty_params_list,
        return_ty: return_ty.into_something(),
        mods: Vec::new(),
        span: test_span.clone(),
    }
}

#[test]
fn specialized_func_decl_has_specialized_return_ty() {
    let ty_params = [make_ty_param("T")];
    let return_ty = make_ty_param_ty(&ty_params, 0);

    let decl = make_decl(None, "A", ty_params, [], return_ty);
    let ctx = Context::root(test_span());
    
    let args = TypeArgList::new([Primitive::Int32.into()], test_span());

    let specialized = specialize_func_decl(&decl, &args, &ctx)
        .unwrap();
    
    assert_eq!(Some(Type::Primitive(Primitive::Int32)), specialized.return_ty);
}

#[test]
fn specialized_func_decl_has_specialized_param_tys() {
    let ty_params = [make_ty_param("T0"), make_ty_param("T1")];

    let arg0_ty = make_ty_param_ty(&ty_params, 1);
    let arg1_ty = make_ty_param_ty(&ty_params, 0);

    let decl = make_decl(None, "A", ty_params, [arg0_ty, arg1_ty], Type::Nothing);
    let ctx = Context::root(test_span());

    let args = TypeArgList::new([
        Primitive::Int32.into(),
        Primitive::Boolean.into(),
    ], test_span());

    let specialized = specialize_func_decl(&decl, &args, &ctx)
        .unwrap();

    assert_eq!(Type::Primitive(Primitive::Boolean), specialized.params[0].ty);
    assert_eq!(Type::Primitive(Primitive::Int32), specialized.params[1].ty);
}

#[test]
fn specialized_func_decl_checks_constraint() {
    let displayable = Type::interface(builtin_displayable_name().full_path);
    
    let ty_params = [make_ty_param_of("T", displayable)];

    let decl = make_decl(None, "A", ty_params, [], Type::Nothing);
    
    let ctx = Context::root(test_span());
    
    // Any should implement no interfaces
    let args = TypeArgList::new([Type::Any], test_span());

    match specialize_func_decl(&decl, &args, &ctx) {
        Err(GenericError::ConstraintNotSatisfied { .. }) => { 
            // ok
        },
        Err(other) => panic!("expected constraint violation error, but got {other}"),
        Ok(..) => panic!("expected constraint violation error, but succeeded"),
    }
}
