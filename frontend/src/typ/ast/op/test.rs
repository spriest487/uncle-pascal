use crate::ast::Access;
use crate::typ::test::try_module_from_srcs;
use crate::typ::TypeError;

#[test]
fn private_field_not_accessible_from_other_unit() {
    let src_a = r"
        interface
        uses System;
        type
            AClass = class
            private
                a: Integer;
            end;
            
        function NewAClass: AClass;

        implementation
        function NewAClass: AClass;
        begin
            AClass(a: 0);
        end;

        end.
    ";

    let src_b = r"
        implementation
        uses UnitA;

        initialization
            var instance := NewAClass();
            var a := instance.a;
        end.
    ";

    let srcs = [
        ("UnitA", src_a),
        ("UnitB", src_b),
    ];

    match try_module_from_srcs(srcs) {
        Ok(..) => panic!("expected access error"),
        Err(TypeError::TypeMemberInaccessible { ty, member, access, .. }) => {
            assert_eq!("UnitA.AClass", ty.to_string());
            assert_eq!("a", member.name.as_str());
            assert_eq!(Access::Private, access);
        }
        Err(other) => panic!("expected access error, got {}", other),
    }
}

#[test]
fn public_field_accessible_from_other_unit() {
    let src_a = r"
        interface
        uses System;
        type
            AClass = class
            public
                a: Integer;
            end;
            
        function NewAClass: AClass;

        implementation
        function NewAClass: AClass;
        begin
            AClass(a: 0);
        end;

        end.
    ";

    let src_b = r"
        implementation
        uses UnitA;

        initialization
            var instance := NewAClass();
            var a := instance.a;
        end.
    ";

    let srcs = [
        ("UnitA", src_a),
        ("UnitB", src_b),
    ];

    try_module_from_srcs(srcs)
        .expect("should be valid");
}

#[test]
fn private_field_accessible_from_same_unit() {
    let src_a = r"
        interface
        uses System;
        type
            AClass = class
            private
                a: Integer;
            end;

        initialization
            var instance := AClass(a: 0);
            var a := instance.a; 
        end.
    ";

    let srcs = [
        ("UnitA", src_a),
    ];

    try_module_from_srcs(srcs)
        .expect("should be valid");
}
