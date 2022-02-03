use super::*;

#[test]
fn rc_val_roundtrips_ok() {
    let marshalled_rc = RcValue {
        ref_count: 123,
        resource_ptr: Pointer {
            addr: 87873232,
            ty: Type::Struct(StructID(9911)),
        },
        struct_id: StructID(9911),
    };

    let marshaller = Marshaller::new();

    let mut bytes = vec![0u8; 1024];
    let marshalled_size = marshaller.marshal_rc(&marshalled_rc, &mut bytes).unwrap();

    let result = marshaller.unmarshal_rc(&bytes[0..marshalled_size]).unwrap();

    assert_eq!(marshalled_size, result.byte_count);
    assert_eq!(marshalled_rc, result.value);
}