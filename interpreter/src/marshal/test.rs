use super::*;

#[test]
fn rc_cell_roundtrips_ok() {
    let marshalled_rc_cell = RcCell {
        ref_count: 123,
        resource_ptr: Pointer {
            addr: 87873232,
            ty: Type::Struct(StructID(9911)),
        },
        struct_id: StructID(9911),
    };

    let marshaller = Marshaller::new();

    let mut bytes = vec![0u8; 1024];
    let marshalled_size = marshaller.marshal_rc_cell(&marshalled_rc_cell, &mut bytes).unwrap();

    let (unmarshalled_rc_cell, unmarshalled_size) = marshaller.unmarshal_rc_cell(&bytes[0..marshalled_size]).unwrap();

    assert_eq!(marshalled_size, unmarshalled_size);
    assert_eq!(marshalled_rc_cell, unmarshalled_rc_cell);
}