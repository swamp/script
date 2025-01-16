use sparse_slot::Id;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_core::extra::SparseValueMap;
use swamp_script_core::qck_des::quick_deserialize;
use swamp_script_core::value::{Value, SPARSE_ID_TYPE_ID, SPARSE_TYPE_ID};
use swamp_script_semantic::ResolvedRustType;
use swamp_script_semantic::ResolvedType;
use tracing::info;

#[test_log::test]
fn serialize() {
    let mut buf = [0u8; 256];

    let id = ResolvedRustType {
        number: SPARSE_ID_TYPE_ID,
        type_name: "SparseId".to_string(),
    };

    let sparse_map_rust_type = ResolvedRustType {
        number: SPARSE_TYPE_ID,
        type_name: "Sparse".to_string(),
    };

    let sparse_map_type = ResolvedType::RustType(sparse_map_rust_type.clone().into());

    let value_type = ResolvedType::Int;

    let mut sparse_map = SparseValueMap::new(id.into(), value_type);

    let id = Id::new(23, 14);
    sparse_map
        .sparse_slot
        .try_set(id, Rc::new(RefCell::new(Value::Int(1904))))
        .expect("sparse_slot try_set");

    let id_257 = Id::new(257, 129);
    sparse_map
        .sparse_slot
        .try_set(id_257, Rc::new(RefCell::new(Value::Int(-2388))))
        .expect("sparse_slot try_set");

    let sparse_map_val = Value::RustValue(
        sparse_map_rust_type.into(),
        Rc::new(RefCell::new(Box::new(sparse_map))),
    );

    let serialized_octet_size = sparse_map_val.quick_serialize(&mut buf);

    assert_eq!(serialized_octet_size, 18);

    let sparse_generic_type =
        ResolvedType::Generic(Box::from(sparse_map_type), vec![ResolvedType::Int]);

    let (deserialized_value, deserialized_octet_size) =
        quick_deserialize(&sparse_generic_type, &buf);

    assert_eq!(serialized_octet_size, deserialized_octet_size);
    info!(value=?deserialized_value, x=?sparse_map_val, "deserialized");
    assert_eq!(deserialized_value.to_string(), sparse_map_val.to_string());
}
