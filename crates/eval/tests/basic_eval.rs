/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::util::{check_value, eval};
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
