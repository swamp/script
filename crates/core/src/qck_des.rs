use crate::extra::{SparseValueId, SparseValueMap};
use crate::prelude::Value;
use crate::value::{QuickDeserialize, RustType, SPARSE_ID_TYPE_ID, SPARSE_TYPE_ID};
use fixed32::Fp;
use seq_map::SeqMap;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_semantic::{ResolvedEnumVariantContainerType, ResolvedRustType, ResolvedType};

#[inline]
#[allow(clippy::too_many_lines)]
pub fn quick_deserialize(resolved_type: &ResolvedType, buf: &[u8], depth: usize) -> (Value, usize) {
    let (val, octet_size) = match resolved_type {
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
            offset += 2;

            let item_ref = &array_type_ref.item_type;

            let mut values = Vec::new();
            for _index in 0..count {
                let (value, item_octet_size) =
                    quick_deserialize(item_ref, &buf[offset..], depth + 1);

                offset += item_octet_size;

                values.push(Rc::new(RefCell::new(value)));
            }

            (Value::Array(array_type_ref.clone(), values), offset)
        }
        ResolvedType::Tuple(tuple_type_ref) => {
            let mut offset = 0;
            let mut values = Vec::new();
            for tuple_item_type in &tuple_type_ref.0 {
                let (value, item_octet_size) =
                    quick_deserialize(tuple_item_type, &buf[offset..], depth + 1);
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
                    quick_deserialize(&struct_field_type.field_type, &buf[offset..], depth + 1);
                values.push(Rc::new(RefCell::new(value)));
                offset += octet_size;
            }
            (Value::Struct(struct_type_ref.clone(), values), offset)
        }
        ResolvedType::Map(map_type_ref) => {
            let mut offset = 0;
            let count = u16::from_ne_bytes(
                buf[offset..offset + 2]
                    .try_into()
                    .expect("should work with u16"),
            );
            offset += 2;

            let key_type = &map_type_ref.key_type;
            let value_type = &map_type_ref.value_type;

            let mut seq_map = SeqMap::new(); //SeqMap<Value, ValueRef>
            for _map_index in 0..count {
                let (key_val, key_octet_size) =
                    quick_deserialize(&key_type, &buf[offset..], depth + 1);
                offset += key_octet_size;

                let (value_val, value_octet_size) =
                    quick_deserialize(&value_type, &buf[offset..], depth + 1);
                offset += value_octet_size;

                let value_ref = Rc::new(RefCell::new(value_val));

                seq_map
                    .insert(key_val, value_ref)
                    .expect("should work to insert");
            }
            (Value::Map(map_type_ref.clone(), seq_map), offset)
        }
        ResolvedType::Enum(enum_type) => {
            let mut offset = 0;
            let enum_lookup_index = buf[offset];
            offset += 1;
            assert!(enum_lookup_index < 8);

            let borrowed_enum = enum_type.borrow();

            let variant_type = borrowed_enum
                .get_variant_from_index(enum_lookup_index as usize)
                .expect("should be able to find variant");

            let val = match &variant_type.data {
                ResolvedEnumVariantContainerType::Struct(_) => {
                    todo!("struct containers not done yet")
                }
                ResolvedEnumVariantContainerType::Tuple(tuple_type_ref) => {
                    let mut vals_in_order = Vec::new();
                    for tuple_type in &tuple_type_ref.fields_in_order {
                        let (tuple_value, tuple_octet_size) =
                            quick_deserialize(tuple_type, &buf[offset..], depth + 1);
                        vals_in_order.push(tuple_value);
                        offset += tuple_octet_size;
                    }
                    Value::EnumVariantTuple(tuple_type_ref.clone(), vals_in_order)
                }
                ResolvedEnumVariantContainerType::Nothing => {
                    offset += 0;
                    Value::EnumVariantSimple(variant_type.clone())
                }
            };

            (val, offset)
        }
        ResolvedType::Generic(base_type, type_parameters) => {
            if let ResolvedType::RustType(found_rust_type) = &**base_type {
                if found_rust_type.number == SPARSE_TYPE_ID {
                    let sparse_type_id_rust_type = Rc::new(ResolvedRustType {
                        type_name: "SparseId".to_string(),
                        number: SPARSE_ID_TYPE_ID, // TODO: FIX hardcoded number
                    });

                    let value_type = &type_parameters[0];

                    let (internal_map, sparse_value_map_octet_size) =
                        SparseValueMap::quick_deserialize(
                            sparse_type_id_rust_type,
                            value_type.clone(),
                            buf,
                        );

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
                let (v, octet_size) = quick_deserialize(optional_type_ref, &buf[1..], depth + 1);
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
            match rust_type_ref.number {
                SPARSE_ID_TYPE_ID => {
                    let sparse_id_rust_type = Rc::new(ResolvedRustType {
                        type_name: "SparseId".to_string(),
                        number: SPARSE_ID_TYPE_ID, // TODO: FIX hardcoded number
                    });

                    let (sparse_value_id, octet_size) = SparseValueId::quick_deserialize(&buf);
                    (
                        {
                            let boxed_sparse_value_id: Rc<RefCell<Box<dyn RustType>>> =
                                Rc::new(RefCell::new(Box::new(sparse_value_id)));
                            Value::RustValue(sparse_id_rust_type, boxed_sparse_value_id)
                        },
                        octet_size,
                    )
                }
                _ => panic!("can not deserialize rust types {}", rust_type_ref.type_name),
            }
        }
        ResolvedType::Any => {
            panic!("can not deserialize any");
        }
    };

    (val, octet_size)
}
