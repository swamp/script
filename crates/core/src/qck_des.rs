use crate::extra::SparseValueMap;
use crate::prelude::Value;
use crate::value::{QuickDeserialize, RustType, SPARSE_ID_TYPE_ID, SPARSE_TYPE_ID};
use fixed32::Fp;
use seq_map::SeqMap;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_semantic::{ResolvedRustType, ResolvedType};

#[inline]
#[allow(clippy::too_many_lines)]
pub fn quick_deserialize(resolved_type: &ResolvedType, buf: &[u8]) -> (Value, usize) {
    match resolved_type {
        ResolvedType::Int => {
            let i = i32::from_ne_bytes(buf[0..4].try_into().expect("REASON"));
            (Value::Int(i), 4)
        }

        ResolvedType::Float => {
            let i = i32::from_ne_bytes(buf[0..4].try_into().expect("couldn't convert to Fp"));
            (Value::Float(Fp::from_raw(i)), 4)
        }
        ResolvedType::String => {
            let octet_len =
                u16::from_ne_bytes(buf[0..2].try_into().expect("could not convert strlen"));
            let str =
                String::from_utf8(buf[2..2 + octet_len as usize].to_owned()).expect("utf8 error");
            (Value::String(str), (octet_len + 2) as usize)
        }
        ResolvedType::Bool => (Value::Bool(buf[0] != 0), 1),
        ResolvedType::Unit => (Value::Unit, 0),
        ResolvedType::Array(array_type_ref) => {
            let mut offset = 0;
            let count = u16::from_ne_bytes(
                buf[offset..offset + 2]
                    .try_into()
                    .expect("should work with u16"),
            );
            let item_ref = &array_type_ref.item_type;
            offset += 2;
            let mut values = Vec::new();
            for _index in 0..count {
                let (value, item_octet_size) = quick_deserialize(item_ref, &buf[offset..]);
                offset += item_octet_size;
                values.push(Rc::new(RefCell::new(value)));
            }
            (Value::Array(array_type_ref.clone(), values), offset)
        }
        ResolvedType::Tuple(tuple_type_ref) => {
            let mut offset = 0;
            let mut values = Vec::new();
            for tuple_item_type in &tuple_type_ref.0 {
                let (value, item_octet_size) = quick_deserialize(tuple_item_type, &buf[offset..]);
                values.push(Rc::new(RefCell::new(value)));
                offset += item_octet_size;
            }
            (Value::Tuple(tuple_type_ref.clone(), values), offset)
        }
        ResolvedType::Struct(struct_type_ref) => {
            let mut values = Vec::new();
            let mut offset = 0;
            for struct_field_type in struct_type_ref
                .borrow()
                .anon_struct_type
                .defined_fields
                .values()
            {
                let (value, octet_size) =
                    quick_deserialize(&struct_field_type.field_type, &buf[offset..]);
                values.push(Rc::new(RefCell::new(value)));
                offset += octet_size;
            }
            (Value::Struct(struct_type_ref.clone(), values), offset)
        }
        ResolvedType::Map(map_type_ref) => {
            let mut offset = 0;
            let key_type = &map_type_ref.key_type;
            let value_type = &map_type_ref.value_type;
            let count = u16::from_ne_bytes(
                buf[offset..offset + 2]
                    .try_into()
                    .expect("should work with u16"),
            );
            offset += 2;
            let mut seq_map = SeqMap::new(); //SeqMap<Value, ValueRef>
            for _map_index in 0..count {
                let (key_val, key_octet_size) = quick_deserialize(&key_type, &buf[offset..]);
                offset += key_octet_size;

                let (value_val, value_octet_size) = quick_deserialize(&value_type, &buf[offset..]);
                offset += value_octet_size;

                let value_ref = Rc::new(RefCell::new(value_val));

                seq_map
                    .insert(key_val, value_ref)
                    .expect("should work to insert");
            }
            (Value::Map(map_type_ref.clone(), seq_map), offset)
        }
        ResolvedType::Enum(enum_type) => {
            let variant_type = &enum_type
                .borrow()
                .get_variant_from_index(0)
                .expect("should be able to find variant");
            (Value::Unit, 0)
        }
        ResolvedType::Generic(base_type, type_parameters) => {
            if let ResolvedType::RustType(found_rust_type) = &**base_type {
                if found_rust_type.number == SPARSE_TYPE_ID {
                    let sparse_type_id_rust_type = Rc::new(ResolvedRustType {
                        type_name: "SparseId".to_string(),
                        number: SPARSE_ID_TYPE_ID, // TODO: FIX hardcoded number
                    });

                    let value_type = &type_parameters[0];

                    let mut internal_map =
                        SparseValueMap::new(sparse_type_id_rust_type, value_type.clone());

                    let sparse_value_map_octet_size = internal_map.quick_deserialize(buf);

                    let wrapped_internal_map: Rc<RefCell<Box<dyn RustType>>> =
                        Rc::new(RefCell::new(Box::new(internal_map)));

                    let sparse_collection_rust_type = Rc::new(ResolvedRustType {
                        type_name: "Sparse".to_string(),
                        number: SPARSE_TYPE_ID, // TODO: FIX hardcoded number
                    });

                    (
                        Value::RustValue(sparse_collection_rust_type, wrapped_internal_map),
                        sparse_value_map_octet_size,
                    )
                } else {
                    panic!("unknown generic type");
                }
            } else {
                panic!("unknown generic type")
            }
        }
        ResolvedType::Function(_) => {
            panic!("can not serialize function")
        }
        ResolvedType::Iterable(_) => {
            panic!("can not serialize iterables")
        }
        ResolvedType::Optional(optional_type_ref) => {
            let mut offset = 0;
            let has_some = buf[0] != 0;
            offset += 1;
            if has_some {
                let (v, octet_size) = quick_deserialize(optional_type_ref, &buf[1..]);
                offset += octet_size;
                (
                    Value::Option(Some(Box::from(Rc::new(RefCell::new(v))))),
                    offset,
                )
            } else {
                (Value::Option(None), offset)
            }
        }
        ResolvedType::RustType(rust_type_ref) => {
            panic!("can not deserialize rust types {}", rust_type_ref.type_name)
        }
        ResolvedType::Any => {
            panic!("can not deserialize any");
        }
    }
}
