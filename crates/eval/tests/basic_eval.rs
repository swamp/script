/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::util::{check_fail, check_value, eval};
use fixed32::Fp;
use swamp_script_eval::value::Value;

mod util;

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
        }

        a = 3

    "#,
        Value::Int(3),
    );
}

#[test_log::test]
fn basic_eval_2() {
    check_value(
        r#"
        mut a = 3
        a = a + 1
    "#,
        Value::Int(4),
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
        r#"
        is_attacking = false
        c = if is_attacking { 3.5 } else { -13.3 }
    "#,
        Value::Float(Fp::from(-13.3)),
    );
}

#[test_log::test]
fn basic_eval_5() {
    check_value(
        r#"

        mut counter = 0
        for i in 1..3 {
            counter = counter + 1
            counter = counter + i
        }
        counter

    "#,
        Value::Int(1 + 1 + 1 + 2),
    );
}

#[test_log::test]
fn basic_eval_6() {
    check_value(
        r#"

        fn add(a: Int, b: Int) -> Int {
            a + b
        }

        add(4, 8)
    "#,
        Value::Int(12),
    );
}

#[test_log::test]
fn basic_eval_7() {
    let result = eval(
        r#"

        mut a = 5
        mut counter = 0

        while a < 10 {
            a = a + 1
            counter = counter + 1
        }

        (a, counter)
    "#,
    );

    if let Value::Tuple(_, values) = result {
        assert_eq!(values, vec![Value::Int(10), Value::Int(5)]);
    } else {
        panic!()
    }
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
fn basic_eval_8_5() {
    let result = eval(
        r#"

        health = 80

        match health {
            0 => "scary!",
            100 => "full health!",
            x => 'so mid {x}'
        }
    "#,
    );

    assert_eq!(result, Value::String("so mid 80".to_string()));
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
fn basic_eval_12() {
    let result = eval(
        r#"

        enum Action {
            Jumping,
            Target { x: Int, y: Int },
            Other(String),
        }

        action = Action::Target { x:42, y: -999 }

        /*
        match the actions
            - Jumping
            - Target
        */

        match action {
            Jumping => "jumping",
            Target { y } => y,
            _ => "can not find it!",
        }
    "#,
    );

    assert_eq!(result, Value::Int(-999));
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

        type CoolPerson = Ossian

        mut ossian = CoolPerson { happy: 3 }

        Ossian::unrelated_function(3.5) // TODO: change to Ossian::unrelated_function(3.5) ?
        ossian.coding()
        ossian.pixel_art(1)
    "#,
    );

    assert_eq!(result, Value::Int(42 + 13 + 1));
}

#[test_log::test]
fn basic_eval_14() {
    let result = eval(
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
    );

    assert_eq!(result.to_string(), "[Button { position: Position { x: -49, y: 101 }, layer: 1 }, Button { position: Position { x: 20, y: -88 }, layer: 44 }]");
}

//#[test_log::test]
fn _basic_eval_15() {
    let result = eval(
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
    "#,
    );

    assert_eq!(result.to_string(), "[Button { position: Position { x: -49, y: 101 }, layer: 1 }, Button { position: Position { x: 20, y: -88 }, layer: 44 }]");
}

#[test_log::test]
fn basic_eval_16() {
    let result = eval(
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
    );

    assert_eq!(
        result.to_string(),
        "(0, Button { position: Position { x: -49, y: 101 }, layer: 1 })"
    );
}

#[test_log::test]
fn basic_eval_18() {
    let result = eval(
        r#"

    a = none
    "#,
    );

    assert_eq!(result, Value::Option(None));
}

#[test_log::test]
fn basic_eval_19() {
    let result = eval(
        r#"

    a = none
    b = a?
    "#,
    );

    assert_eq!(result, Value::Option(None));
}

#[test_log::test]
fn basic_eval_20() {
    let result = eval(
        r#"

    a = 4
    mut c = if b = a > 3 {
       b
    } else {
       b
    }
    "#,
    );

    assert_eq!(result, Value::Bool(true));
}

#[test_log::test]
fn basic_eval_22() {
    let result = eval(
        "

    struct SomeStruct {
       some_field: Int?
    }

    s = SomeStruct { some_field: 2 } // It should implicitly convert `some_field` to Some(2)

    mut c = if s.some_field { // this should evaluate to true, since Some is truthy
       'it was some'
    } else {
       'none'
    }
    ",
    );

    assert_eq!(result, Value::String("it was some".to_string()));
}

#[test_log::test]
fn basic_eval_23() {
    let result = eval(
        "

    struct SomeStruct {
       some_field: Int?
    }

    s = SomeStruct { some_field: 2 } // It should implicitly convert `some_field` to Some(2)

    s.some_field? // this should unwrap the value
    ",
    );

    assert_eq!(result, Value::Int(2));
}

#[test_log::test]
fn basic_eval_24() {
    let result = eval(
        "

    struct SomeStruct {
       some_field: Int?
    }

    s = SomeStruct { some_field: 2 }

    x = if result = s.some_field? {
        result * 3
    } else {
        0
    }
    ",
    );

    assert_eq!(result, Value::Int(2 * 3));
}

#[test_log::test]
fn basic_eval_25() {
    let result = eval(
        "

    struct SomeStruct {
       some_field: Int?
    }

    s = SomeStruct { some_field: 2 }

    a = s.some_field

    x = if a? {
        a * 3
    } else {
        0
    }
    ",
    );

    assert_eq!(result, Value::Int(2 * 3));
}

#[test_log::test]
fn eval_map() {
    let x = eval(
        "

    a = [2: 'hello', -1: 'world']

    ",
    );

    assert_eq!(x.to_string(), r#"[2: "hello", -1: "world"]"#);
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
        Value::Option(Some(Box::new(Value::String("hello".to_string()))))
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
    x = if b? {
       1
    } else { -19 }
    ",
    );

    assert_eq!(x, Value::Int(-19));
}

#[test_log::test]
fn map_index_if_found() {
    let x = eval(
        "

    a = [2: 'hello', -1: 'world']
    b = a[-1]
    x = if b? {
       b + ' was found'
    } else { -19 }
    ",
    );

    assert_eq!(x, Value::String("world was found".to_string()));
}

#[test_log::test]
fn map_index_if_found_expression() {
    let x = eval(
        "

    a = [2: 'hello', -1: 'world']
    x = if found = a[-1]? {
       found + ' was found'
    } else { -19 }
    ",
    );

    assert_eq!(x, Value::String("world was found".to_string()));
}

#[test_log::test]
fn map_insert_with_immutable() {
    check_fail(
        "

    a = [2:'hello', -1:    'world']
    a[3] = 'ossian'

    ",
        "ExecuteError(Error(\"Invalid map assignment: must be mutable\"))",
    );
}

#[test_log::test]
fn map_insert_wrong_type() {
    check_fail(
        "

    mut a = [2:'hello', -1:    'world']
    a[3] = 5.5

    ",
        "ResolveError(MapValueTypeMismatch { expected: String(ResolvedStringType), found: Float(ResolvedFloatType) })",
    );
}

#[test_log::test]
fn map_insert() {
    let x = eval(
        "

    mut a = [2: 'hello', -1: 'world']
    a[3] = 'ossian'

    ",
    );

    assert_eq!(x, Value::String("ossian".to_string()));
}

//#[test_log::test]
#[allow(unused)]
fn map_overwrite() {
    let x = eval(
        "

    mut a = [2: 'hello', -1: 'world']
    a[-1] = 'ossian'

    ",
    );

    assert_eq!(x, Value::String("ossian".to_string()));
}

#[test_log::test]
fn map_fn_return() {
    let x = eval(
        "

    fn map_creator() -> [Int:String] {
        [2: 'hello', -1: 'world']
    }

    mut a = map_creator()
    a[3] = 'ossian'

    ",
    );

    assert_eq!(x, Value::String("ossian".to_string()));
}

#[test_log::test]
fn array_fn_return() {
    let x = eval(
        "

    fn float_creator() -> [Float] {
        [2.4, 5.6, 8.9]
    }

    a = float_creator()
    ",
    );

    assert_eq!(x.to_string(), "[2.40, 5.60, 8.90]");
}

#[test_log::test]
fn array_push() {
    let x = eval(
        "

    mut a = [10, 20]
    a += 30
    ",
    );

    assert_eq!(x.to_string(), "[10, 20, 30]");
}

#[test_log::test]
fn array_extend() {
    let x = eval(
        "

    mut a = [10, 20]
    
    a += [30, 40, 50]
    ",
    );

    assert_eq!(x.to_string(), "[10, 20, 30, 40, 50]");
}

#[test_log::test]
fn array_remove() {
    let x = eval(
        "

    mut a = [10, 20]
    
    a += [30, 40, 50]

    a.remove(3)
    ",
    );

    assert_eq!(x.to_string(), "[10, 20, 30, 50]");
}

#[test_log::test]
fn array_remove_fail() {
    check_fail(
        "

    a = [10, 20]
    
    a.remove(1)
    ",
        "ResolveError(VariableIsNotMutable(a))",
    );
}

#[test_log::test]
fn array_clear() {
    let x = eval(
        "

    mut a = [10, 20]
    
    a += [30, 40, 50]

    a.clear()
    ",
    );

    assert_eq!(x.to_string(), "[]");
}
