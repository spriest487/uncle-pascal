use crate::ast::util::*;
use crate::ast::TypeDeclItem;
use crate::ast::{StructDef, Unit, Visibility};
use crate::parse::ParseError;
use crate::Separator;
use common::span::Span;
use common::TracedError;

/// empty class and record types should be allowed - only the typechecker should enforce
/// non-empty records 
#[test]
pub fn empty_struct_is_valid() {
    let unit = unit_from_string("empty_struct_is_valid", r"
        implementation
        
        type 
            MyClass = class
            end;
        
            MyRecord = record
            end;
        
        end
    ");
    
    let decls = unit.type_decls().collect::<Vec<_>>();
    assert_eq!(1, decls.len());
    
    let (vis, type_decls) = decls[0];
    assert_eq!(Visibility::Implementation, vis);
    
    assert_eq!(2, type_decls.items.len());
    
    for item in &type_decls.items {
        match item { 
            TypeDeclItem::Struct(struct_def) => {
                assert_eq!(0, struct_def.members.len());    
            }
            
            other => panic!("expected a struct def, found: {}", other),
        }
    }
}

fn get_single_struct_def(unit: &Unit<Span>) -> &StructDef<Span> {
    match unit.type_decl_items().nth(0).unwrap().1 {
        TypeDeclItem::Struct(struct_def) => struct_def.as_ref(),
        other => panic!("expected a struct def, got {}", other),
    }
}

#[test]
pub fn semicolon_separator_is_valid() {
    let unit = unit_from_string("semicolon_separator_is_valid", r"
        implementation
        
        type MyClass = class
            i: Int32;
        end;

        end
    ");
    
    let class_def = get_single_struct_def(&unit);
    assert_eq!(&class_def.members[0].ident, "i");
    assert_eq!(&class_def.members[0].ty.to_string(), "Int32");
}

#[test]
pub fn semicolon_separator_is_optional() {
    let unit = unit_from_string("semicolon_separator_is_optional", r"
        implementation
        
        type MyClass = class
            i: Int32
        end;

        end
    ");

    let class_def = get_single_struct_def(&unit);
    assert_eq!(&class_def.members[0].ident, "i");
    assert_eq!(&class_def.members[0].ty.to_string(), "Int32");
}

#[test]
pub fn semicolon_separator_is_single() {
    let unit = try_unit_from_string("semicolon_separator_is_optional", r"
        implementation
        
        type MyClass = class
            i: Int32;;
        end;

        end
    ");
    
    assert!(match unit.map_err(TracedError::into_inner) {
        Err(ParseError::UnexpectedToken(tt, ..)) => {
            tt.is_separator(Separator::Semicolon)
        },
        _ => false,
    }, "should fail on the unexpected semicolon token")
}

#[test]
pub fn multi_field_def_is_valid() {
    let unit = unit_from_string("multi_field_def_is_valid", r"
        implementation
        
        type MyClass = class
            a, b: Int32;
        end;

        end
    ");

    let class_def = get_single_struct_def(&unit);
    assert_eq!(&class_def.members[0].ident, "a");
    assert_eq!(&class_def.members[0].ty.to_string(), "Int32");
    assert_eq!(&class_def.members[1].ident, "b");
    assert_eq!(&class_def.members[1].ty.to_string(), "Int32");
}
