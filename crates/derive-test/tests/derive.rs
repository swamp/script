use swamp_script_core::prelude::*;
use swamp_script_derive::{swamp_fn, SwampExport};
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
