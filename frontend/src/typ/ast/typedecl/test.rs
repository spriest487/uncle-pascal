use crate::ast;
use crate::typ::ast::InterfaceDecl;
use crate::typ::{Module, NameError, TypeError, TypeResult};
use crate::typ::test::{module_from_src, try_module_from_src};

fn get_decl_iface<'a>(module: &'a Module, unit_name: &str, name: &str) -> &'a InterfaceDecl {
    let unit = module.units.iter()
        .find(|u| u.unit.ident.to_string() == unit_name)
        .unwrap();

    unit.unit
        .type_decl_items()
        .find_map(|(_, item)| {
            if item.name().full_path.last().as_str() != name {
                eprintln!("nope not {}", item.name());
                return None;
            }

            match item {
                ast::TypeDeclItem::Interface(iface_decl) => Some(iface_decl),
                _ => None,
            }})
        .unwrap()
        .as_ref()
}

#[test]
fn self_ty_is_valid_in_interface() {
    let module = module_from_src(
        "test", 
        r"implementation
            type MyInterface = interface
                function A(a: Self): Self;
            end;
        end."
    );
    
    let my_iface_decl = get_decl_iface(&module, "test", "MyInterface");    
    let method_a = &my_iface_decl.methods[0];
    assert_eq!("Self", method_a.decl.params[1].ty.to_string());
    assert_eq!("Self", method_a.decl.return_ty.to_string());
}

#[test]
fn self_ty_is_valid_in_interface_sig_with_arrays() {
    let module = module_from_src(
        "test",
        r"implementation
            type MyInterface = interface
                function A(a: array of Self): array[2] of Self;
            end;
        end."
    );

    let my_iface_decl = get_decl_iface(&module, "test", "MyInterface");
    let method_a = &my_iface_decl.methods[0];
    assert_eq!("array of Self", method_a.decl.params[1].ty.to_string());
    assert_eq!("array[2] of Self", method_a.decl.return_ty.to_string());
}

#[test]
fn self_ty_is_valid_in_interface_sig_with_generics() {
    let module = module_from_src(
        "test",
        r"implementation
            type MyGeneric[T] = class end;

            type MyInterface = interface
                function A(a: MyGeneric[Self]): MyGeneric[Self];
            end;
        end."
    );

    let my_iface_decl = get_decl_iface(&module, "test", "MyInterface");
    let method_a = &my_iface_decl.methods[0];
    assert_eq!("test.MyGeneric[Self]", method_a.decl.params[1].ty.to_string());
    assert_eq!("test.MyGeneric[Self]", method_a.decl.return_ty.to_string());
}

fn expect_self_not_found_err(result: TypeResult<Module>) {
    match result {
        Ok(..) => panic!("expected type error, but got success"),
        Err(TypeError::NameError { err: NameError::NotFound { ident, .. }, .. }) => {
            assert_eq!("Self", ident.to_string());
        }
        Err(err) => panic!("expected NotFound for Self, got:\n{}", err)
    }
}

#[test]
fn self_ty_not_valid_in_class() {
    let result = try_module_from_src(
        "test",
        r"implementation
            type MyClass = class
                function A: Self;
            end;
        end."
    );

    expect_self_not_found_err(result);
}

#[test]
fn self_ty_not_valid_in_variant() {
    let result = try_module_from_src(
        "test",
        r"implementation
            type MyVariant = variant
                A: Self;
            end;
        end."
    );

    expect_self_not_found_err(result);
}