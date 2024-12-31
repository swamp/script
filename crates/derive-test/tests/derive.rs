/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use std::rc::Rc;
use swamp_script_core::prelude::*;
use swamp_script_derive::{swamp_fn, SwampExport, SwampExportEnum};
use swamp_script_semantic::prelude::*;

#[derive(SwampExport, Debug, Eq, PartialEq)]
struct TestStruct {
    number: i32,
    text: String,
    flag: bool,
}

#[test]
fn to_resolved_type() {
    let registry = TypeRegistry::new();

    let resolved = TestStruct::get_resolved_type(&registry);

    if let ResolvedType::Struct(def) = resolved {
        let d = def.borrow();
        assert_eq!(d.name.text, "TestStruct");
        assert_eq!(d.number, 1);
        assert_eq!(d.fields.len(), 3);
        assert_eq!(
            d.fields
                .get(&IdentifierName("text".to_string()))
                .unwrap()
                .display_name(),
            "String"
        );
        assert_eq!(
            d.fields
                .get(&IdentifierName("flag".to_string()))
                .unwrap()
                .display_name(),
            "Bool"
        );
    } else {
        panic!("test failed")
    }
}

#[test]
fn convert_back_and_forth() {
    let registry = TypeRegistry::new();

    let original = TestStruct {
        number: 42,
        text: "hello, World!".to_string(),
        flag: true,
    };

    let value = original.to_swamp_value(&registry);

    let roundtrip = TestStruct::from_swamp_value(&value).expect("Should deserialize successfully");

    assert_eq!(roundtrip, original);
}

#[swamp_fn]
fn add(ctx: &mut MyContext, a: i32, b: i32) -> i32 {
    a + b + ctx.secret_value
}

struct MyContext {
    pub secret_value: i32,
}

#[test]
fn test_function() {
    let registry = TypeRegistry::new();
    let mut context = MyContext { secret_value: 99 };

    let add_fn = swamp_add::Function::new(0);
    let mut handler = add_fn.handler(&registry);
    let _def = add_fn.get_definition(&registry);

    let answer = handler(&[Value::Int(1), Value::Int(2)], &mut context).unwrap();
    assert_eq!(answer, Value::Int(99 + 1 + 2));
}

#[derive(SwampExportEnum)]
enum ExampleEnum {
    Struct { x: i32, y: i32 },
    Tuple(i32, f32, bool),
    Single,
}

#[test]
fn test_enum_variants() {
    let registry = TypeRegistry::new();

    let struct_variant = ExampleEnum::Struct { x: 42, y: -7 };
    let struct_value = struct_variant.to_swamp_value(&registry);
    assert!(matches!(struct_value, Value::EnumVariantStruct(_, _)));

    let tuple_variant = ExampleEnum::Tuple(123, 3.29, true);
    let tuple_value = tuple_variant.to_swamp_value(&registry);
    assert!(matches!(tuple_value, Value::EnumVariantTuple(_, _)));

    let simple_variant = ExampleEnum::Single;
    let simple_value = simple_variant.to_swamp_value(&registry);
    assert!(matches!(simple_value, Value::EnumVariantSimple(_)));
}

#[test]
fn test_enum_struct_variant_fields() {
    let registry = TypeRegistry::new();

    let variant = ExampleEnum::Struct { x: 42, y: -7 };
    let value = variant.to_swamp_value(&registry);

    if let Value::EnumVariantStruct(struct_type, values) = value {
        assert_eq!(struct_type.common.variant_name.text, "Struct");
        assert_eq!(values.len(), 2);
        assert!(matches!(&values[0], Value::Int(42)));
        assert!(matches!(&values[1], Value::Int(-7)));
    } else {
        panic!("expected EnumVariantStruct");
    }
}

#[test]
fn test_enum_tuple_variant_fields() {
    let registry = TypeRegistry::new();

    let variant = ExampleEnum::Tuple(123, 3.40, true);
    let value = variant.to_swamp_value(&registry);

    if let Value::EnumVariantTuple(tuple_type, values) = value {
        assert_eq!(tuple_type.common.variant_name.text, "Tuple");
        assert_eq!(values.len(), 3);
        assert!(matches!(&values[0], Value::Int(123)));
        assert!(matches!(&values[1], Value::Float(fp) if fp.inner() == Fp::from(3.40).inner()));
        assert!(matches!(&values[2], Value::Bool(true)));
    } else {
        panic!("Expected EnumVariantTuple");
    }
}

#[derive(SwampExportEnum)]
enum ComplexEnum {
    Point {
        x: f32,
        y: f32,
        label: String,
    },
    Colors(String, String, String),
    Settings {
        enabled: bool,
        value: i32,
        name: String,
    },
    None,
}

#[test]
fn test_complex_enum() {
    let registry = TypeRegistry::new();

    let variants = vec![
        ComplexEnum::Point {
            x: 1.0,
            y: 2.0,
            label: "Origin".to_string(),
        },
        ComplexEnum::Colors("Red".to_string(), "Green".to_string(), "Blue".to_string()),
        ComplexEnum::Settings {
            enabled: true,
            value: 42,
            name: "Config1".to_string(),
        },
        ComplexEnum::None,
    ];

    for variant in variants {
        let value = variant.to_swamp_value(&registry);
        match value {
            Value::EnumVariantStruct(_, _) => {}
            Value::EnumVariantTuple(_, _) => {}
            Value::EnumVariantSimple(_) => {}
            _ => panic!("Unexpected variant type"),
        }
    }
}

#[test]
fn test_enum_definition() {
    let registry = TypeRegistry::new();

    let resolved_type = ExampleEnum::get_resolved_type(&registry);

    if let ResolvedType::Enum(_enum_type) = resolved_type {
        let struct_variant = ExampleEnum::Struct { x: 42, y: -7 };
        let struct_value = struct_variant.to_swamp_value(&registry);

        if let Value::EnumVariantStruct(struct_type, _) = struct_value {
            assert_eq!(struct_type.common.variant_name.text, "Struct");
            let fields = &struct_type.anonym_struct_ty;
            assert_eq!(fields.len(), 2);

            assert!(matches!(
                fields.get(&IdentifierName("x".to_string())).unwrap(),
                ResolvedType::Int(_)
            ));
            assert!(matches!(
                fields.get(&IdentifierName("y".to_string())).unwrap(),
                ResolvedType::Int(_)
            ));
        }

        let tuple_variant = ExampleEnum::Tuple(123, 3.334, true);
        let tuple_value = tuple_variant.to_swamp_value(&registry);

        if let Value::EnumVariantTuple(tuple_type, _) = tuple_value {
            assert_eq!(tuple_type.common.variant_name.text, "Tuple");
            let fields = &tuple_type.fields_in_order;
            assert_eq!(fields.len(), 3);

            assert!(matches!(fields[0], ResolvedType::Int(_)));
            assert!(matches!(fields[1], ResolvedType::Float(_)));
            assert!(matches!(fields[2], ResolvedType::Bool(_)));
        }

        let simple_variant = ExampleEnum::Single;
        let simple_value = simple_variant.to_swamp_value(&registry);

        if let Value::EnumVariantSimple(variant_type) = simple_value {
            assert_eq!(variant_type.name.text, "Single");
            assert!(matches!(
                variant_type.data,
                ResolvedEnumVariantContainerType::Nothing
            ));
        }
    } else {
        panic!("Expected Enum type");
    }
}
