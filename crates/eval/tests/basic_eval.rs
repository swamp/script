/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::util::{check, check_fail, check_value, eval, eval_string};
use fixed32::Fp;
use seq_map::SeqMap;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_core_extra::prelude::Value;
use swamp_script_core_extra::qck_des::quick_deserialize;
use swamp_script_eval::values_to_value_refs_owned;
use swamp_script_semantic::{StructTypeField, AnonymousStructType, Node, StructType, Type};

mod util;

/*
#[test_log::test]
fn block() {
    let result = eval(
        "

    x = 4
    {
        x = 3
    }
    x
    ",
    );

    assert_eq!(result, Value::Int(4));
}

#[test_log::test]
fn match_array() {
    let result = eval(
        r#"

        pos = [1, 4, 11]

        match pos {
           [1, 0, 11] => 0,
           [1, 4, 11] => 1,
        }
    "#,
    );

    assert_eq!(result, Value::Int(1));
}

#[test_log::test]
fn match_enum_literal() {
    let result = eval(
        r#"

        enum Action {
            Jumping,
            Target { x: Int, y: Int },
            Other(String),
        }

        action = Action::Target { x:42, y: -999 }

        match action {
            Jumping => "jumping",
            Target x:42, y:0 => 1,
            Target x:42, y:-999 => 2,
            Target x:42, y => y,
            _ => "can not find it!",
        }
    "#,
    );

    assert_eq!(result, Value::Int(2));
}

#[test_log::test]
fn match_map_literal() {
    let result = eval(
        r#"

        action = [2: 9, 3: 18]

        match action {
            [2: 9] => 1,
            [2: 9, 3: 18] => 2,
        }
    "#,
    );

    assert_eq!(result, Value::Int(2));
}


 */

#[test_log::test]
fn basic_eval() {
    check_value(
        r#"
        a = 3

    "#,
        Value::Int(3),
    );
}

#[test_log::test]
fn basic_eval_float() {
    check_value(
        r#"
        a = 3.3

    "#,
        Value::Float(Fp::from(3.3)),
    );
}

#[test_log::test]
fn basic_eval_with_doc_comment() {
    check_value(
        r#"
        /// This is important documentation
        fn add() -> Int {
            2
        }

        a = 3

    "#,
        Value::Int(3),
    );
}

#[test_log::test]
fn basic_eval_244() {
    check_value(
        r#"
        a = 3
    "#,
        Value::Int(3),
    );
}

#[test_log::test]
fn basic_eval_3() {
    check_value(
        r#"
        a = false
        b = if a == true { 4 } else { -13 }
    "#,
        Value::Int(-13),
    );
}

#[test_log::test]
fn basic_eval_4() {
    check_value(
        r"
        is_attacking = false
        c = if is_attacking { 3.5 } else { -13.3 }
    ",
        Value::Float(Fp::from(-13.3)),
    );
}

#[test_log::test]
fn basic_eval_6() {
    check_value(
        r"

        fn add(a: Int, b: Int) -> Int {
            a + b
        }

        add(4, 8)
    ",
        Value::Int(12),
    );
}

#[test_log::test]
fn basic_eval_7() {
    let result = eval(
        r"
        mut a = 5
        mut counter = 0

        while a < 10 {
            a = a + 1
            counter = counter + 1
        }

        (a, counter)
    ",
    );

    match result { Value::Tuple(_, values) => {
        let owned_values: Vec<_> = values.iter().map(|item| item.borrow().clone()).collect();
        assert_eq!(owned_values, vec![Value::Int(10), Value::Int(5)]);
    } _ => {
        panic!()
    }}
}

#[test_log::test]
fn basic_eval_8() {
    let result = eval(
        r#"

        health = 80

        match health {
            0 => "scary!",
            100 => "full health!",
            _ => "so mid"
        }
    "#,
    );

    assert_eq!(result, Value::String("so mid".to_string()));
}

#[test_log::test]
fn basic_eval_9() {
    let result = eval(
        r#"

        health = 100

        match health {
            0 => "scary!",
            100 => "full health!",
            _ => "so mid"
        }
    "#,
    );

    assert_eq!(result, Value::String("full health!".to_string()));
}

#[test_log::test]
fn basic_eval_10() {
    let result = eval(
        r#"

        /// The character actions
        enum Action {
            Jumping,
            Target { x: Int, y: Int },
            Other,
        }

        action = Action::Jumping

        match action {
            Jumping => "jumping",
            _ => "can not find it!",
        }
    "#,
    );

    assert_eq!(result, Value::String("jumping".to_string()));
}

#[test_log::test]
fn basic_eval_11() {
    let result = eval(
        r#"

        enum Action {
            Jumping,
            Target { x: Int, y: Int },
            Other,
        }

        action = Action::Target {x:1, y: 42}

        match action {
            Jumping => "jumping",
            _ => "can not find it!",
        }
    "#,
    );

    assert_eq!(result, Value::String("can not find it!".to_string()));
}

#[test_log::test]
fn basic_eval_13() {
    let result = eval(
        r#"

        struct Ossian {
            happy: Int,
        }

        impl Ossian {
            fn coding(mut self) {
                self.happy = 42
            }

            fn pixel_art(mut self, number_of_pixels: Int) {
                self.happy = self.happy + 13 + number_of_pixels
            }

            fn happiness(self) -> Int {
                self.happy
            }

            fn unrelated_function(v: Float) -> Float {
                v + 2.0
            }
        }


        mut ossian = Ossian { happy: 3 }

        Ossian::unrelated_function(3.5) // TODO: change to Ossian::unrelated_function(3.5) ?
        ossian.coding()
        ossian.pixel_art(1)
        ossian.happy
    "#,
    );

    assert_eq!(result, Value::Int(42 + 13 + 1));
}

#[test_log::test]
fn basic_eval_14() {
    eval_string(
        r#"

        struct Position {
            x: Int,
            y: Int,
        }

        struct Button {
            position: Position,
            layer: Int,
        }

        buttons = [
            Button { position: Position { x: -49, y: 101, }, layer: 1 },
            Button { position: Position { x: 20, y: -88, }, layer: 44 }
        ]

    "#,
        "[Button { position: Position { x: -49, y: 101 }, layer: 1 }, Button { position: Position { x: 20, y: -88 }, layer: 44 }]"
    );
}

//#[test_log::test]
fn _basic_eval_15() {
    let _result = eval_string(
        r#"

        struct Position {
            x: Int,
            y: Int,
        }

        struct Button {
            position: Position,
            layer: Int,
        }


        fn find_closest_button(buttons: [Button], position: Position) -> Button {
            // this is easy, we just return the first one
            buttons[0]
        }

        buttons = [
            Button { position: Position { x: -49, y: 101, }, layer: 1 },
            Button { position: Position { x: 20, y: -88, }, layer: 44 }
        ]

        find_closest_button(buttons, Position { x: 20, y: -88 })
    "#,  "[Button { position: Position { x: -49, y: 101 }, layer: 1 }, Button { position: Position { x: 20, y: -88 }, layer: 44 }]"
    );
}

#[test_log::test]
fn basic_eval_16() {
    eval_string(
        r#"

        struct Position {
            x: Int,
            y: Int,
        }

        struct Button {
            position: Position,
            layer: Int,
        }

        fn find_closest_button(position: Position) -> (Int, Button) {
            // this is easy, we just return the first one
            (0,  Button { position: Position { x: -49, y: 101, }, layer: 1 } )
        }

        find_closest_button(Position { x: 20, y: -88 })
    "#,
        "(0, Button { position: Position { x: -49, y: 101 }, layer: 1 })",
    );
}

#[test_log::test]
fn basic_eval_18() {
    let result = eval(
        r"

    a: Int? = none
    ",
    );

    assert_eq!(result, Value::Option(None));
}

#[test_log::test]
fn basic_eval_19() {
    let result = eval(
        r#"

    a: Int? = none
    b = a?
    "#,
    );

    assert_eq!(result, Value::Option(None));
}

#[test_log::test]
fn compound() {
    let result = eval(
        "
    mut x = 0
    x += 4
    x *= 5
    x /= 3
    x -= 1
    x
    ",
    );

    assert_eq!(result, Value::Int(4 * 5 / 3 - 1));
}

/*
#[test_log::test]
fn member_chain_call() {
    let result = eval(
        "

     struct Something {
    a: Int,
}

    impl Something {
        fn hello(mut self) {
            self.x += 1
        }

        fn create() -> Something {
            Something {
                a: 99
            }
        }
    }

    s = Something::create().hello()

    ",
    );

    assert_eq!(result, Value::Int(4));
}
*/

#[test_log::test]
fn format_specifiers() {
    check(
        "
    v = 3
    print('this is binary {v:b}')

    ",
        "this is binary 11",
    );
}

#[test_log::test]
fn eval_map() {
    eval_string(
        "

    a = [2: 'hello', -1: 'world']

    ",
        r#"[2: "hello", -1: "world"]"#,
    );
}

#[test_log::test]
fn eval2_map() {
    let x = eval(
        "

    a = [2: 'hello', -1: 'world']
    b = a[2]
    ",
    );

    assert_eq!(
        x,
        Value::Option(Some(Rc::new(RefCell::new(Value::String(
            "hello".to_string()
        )))))
    );
}

#[test_log::test]
fn map_index_not_found() {
    let x = eval(
        "

    a = [2: 'hello', -1: 'world']
    b = a[42]
    ",
    );

    assert_eq!(x, Value::Option(None));
}

#[test_log::test]
fn map_index_if() {
    let x = eval(
        "

    a = [2: 'hello', -1: 'world']
    b = a[42]
    x = when b {
       1
    } else { -19 }
    ",
    );

    assert_eq!(x, Value::Int(-19));
}

#[test_log::test]
fn array_fn_return() {
    eval_string(
        "

    fn float_creator() -> [Float] {
        [2.4, 5.6, 8.9]
    }

    a = float_creator()
    ",
        "[2.40, 5.60, 8.90]",
    );
}

#[test_log::test]
fn array_remove() {
    eval_string(
        "

    mut a = [10, 20]

    a += [30, 40, 50]

    a.remove(3)
    ",
        "[10, 20, 30, 50]",
    );
}

#[test_log::test]
fn chain_optional_functions() {
    let x = eval(
        r#"

struct Player {
    health: Int,
}

fn find_player_by_name(name: String) -> Player? {
    if name == "Hero" {
        Player {
            health: 100,
        }
    } else {
        none
    }
}

fn get_health_if_alive(player: Player?) -> Int? {
    when player {
        // We can use player directly here since it was unwrapped
        if player.health > 0 {
            player.health  // Automatically converts to Some(player.health)
        } else {
            none
        }
    } else {
        none
    }
}

mut result = 0
when health = get_health_if_alive(find_player_by_name("Hero"))? {
    result = 1
    print('Player is alive with health: {health}')
} else {
    result = 0
    print('Player not found or not alive')
}
result
"#,
    );

    assert_eq!(x, Value::Int(1));
}

#[test_log::test]
fn struct_instantiation_rest() {
    check(
        "
    struct Test {
        a: Int,
        b: Float,
    }

    print(Test { b: 2.4, .. })

    ",
        "Test { a: 0, b: 2.40 }",
    );
}

#[test_log::test]
fn struct_instantiation_missing_fail() {
    check_fail(
        "
    struct Test {
        a: Int,
        b: Float,
    }

    print(Test { b: 2.4 })

    ",
        r#"Error(Error { node: <70:4>, kind: MissingFieldInStructInstantiation(["a"], a: Some(<27:1>):Int, b: Some(<43:1>):Float) })"#,
    );
}

#[test_log::test]
fn function_call_basic() {
    let result = eval(
        "
        fn some_fn(a: Int) -> Int {
            a * 2
        }
        some_fn(-10)
    ",
    );

    assert_eq!(result, Value::Int(-20));
}

#[test_log::test]
fn string_range_access() {
    let result = eval(
        "
        a = 'some string'
        a[2..4]
            ",
    );

    assert_eq!(result, Value::String("me".to_string()));
}

#[test_log::test]
fn serialize_octets() {
    let v = Value::Int(2);
    let mut buf = [0u8; 4];
    let size = v.quick_serialize(&mut buf, 0);
    assert_eq!(size, 4);
    assert_eq!(buf[0], 2);

    let (deserialized_value, deserialized_octet_size) =
        quick_deserialize(&Type::Int, &buf, 0);
    assert_eq!(deserialized_value, v);
    assert_eq!(size, deserialized_octet_size);
}

#[test_log::test]
fn serialize_struct_octets() {
    let a = Value::Int(2);
    let b = Value::String("some string".to_string());
    let c = Value::Float(Fp::from(2.0));

    let mut defined_fields = SeqMap::new();
    defined_fields
        .insert(
            "a".to_string(),
            StructTypeField {
                identifier: None,
                field_type: Type::Int,
            },
        )
        .expect("");
    defined_fields
        .insert(
            "b".to_string(),
            StructTypeField {
                identifier: None,
                field_type: Type::String,
            },
        )
        .expect("");
    defined_fields
        .insert(
            "c".to_string(),
            StructTypeField {
                identifier: None,
                field_type: Type::Float,
            },
        )
        .expect("");

    let struct_type = StructType {
        name: Node::default(),
        assigned_name: "SomeStructType".to_string(),
        anon_struct_type: AnonymousStructType { defined_fields },
        functions: SeqMap::default(),
    };
    let struct_type_ref = Rc::new(RefCell::new(struct_type));

    let fields = values_to_value_refs_owned(vec![a, b, c]);
    let struct_value = Value::Struct(struct_type_ref.clone(), fields);

    let mut buf = [0u8; 256];
    let size = struct_value.quick_serialize(&mut buf, 0);
    assert_eq!(size, 21);

    let complete_struct_type = Type::Struct(struct_type_ref);

    let (deserialized_value, deserialized_octet_size) =
        quick_deserialize(&complete_struct_type, &buf, 0);
    assert_eq!(size, deserialized_octet_size);

    assert_eq!(struct_value.to_string(), deserialized_value.to_string());
}

#[test_log::test]
fn val_assign_coerce() {
    let val = eval(
        r"
booster_value: Int? = if false 0 else none
         ",
    );

    assert_eq!(val, Value::Option(None));
}

#[test_log::test]
fn val_assign_coerce_2() {
    let val = eval(
        r"

booster: Int? = 3

         ",
    );

    assert_eq!(
        val,
        Value::Option(Option::from(Rc::new(RefCell::new(Value::Int(3)))))
    );
}

#[test_log::test]
fn val_assign_coerce_3() {
    let val = eval(
        r"

booster: Int? = 3
when booster {
    booster
}
         ",
    );

    assert_eq!(val, Value::Int(3));
}

#[test_log::test]
fn val_assign_coerce_4() {
    let val = eval(
        r"

booster: Int? = none
if booster? {
    booster
} else {
    booster
}

         ",
    );

    assert_eq!(val, Value::Option(None));
}

#[test_log::test]
fn val_assign_coerce_5() {
    let val = eval(
        r"

booster: Int? = none

         ",
    );

    assert_eq!(val, Value::Option(None));
}

#[test_log::test]
fn fail_compound_op_mutable() {
    check_fail(
        r"
fn not_allowed(x : Int) {
    x += 1
}

not_allowed(2)
         ",
        "Error(Error { node: <31:1>, kind: VariableIsNotMutable })",
    );
}

#[test_log::test]
fn compound_op_in_member_mutable() {
    let v = eval(
        r"

struct Something

struct Position {
    x: Int,
    y: Int,
}

impl Something {
    fn not_allowed(mut pos : Position) {
        pos.y += 1
    }
}

mut pos = Position { x: 10, y : 20 }
Something::not_allowed(mut pos)
pos.y
         ",
    );

    assert_eq!(v, Value::Int(21));
}
