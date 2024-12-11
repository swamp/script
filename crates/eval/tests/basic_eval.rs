/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::util::{check, check_fail, check_value, eval};
use fixed32::Fp;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_core::prelude::Value;

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
fn match_enum_struct() {
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
            Target y => y,
            _ => "can not find it!",
        }
    "#,
    );

    assert_eq!(result, Value::Int(-999));
}

#[test_log::test]
fn match_map_tuple() {
    let result = eval(
        r#"

        pos = (-1, 10)

        match pos {
            (2, 3) => 1,
            (0, 0) => 2,
            (-1, 10) => 3,
            (-1, -1) => 4,
        }
    "#,
    );

    assert_eq!(result, Value::Int(3));
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
fn if_statement_optional() {
    let result = eval(
        "

    struct SomeStruct {
       some_field: Int?
    }

    s = SomeStruct { some_field: 2 }

    mut result = 0
    if shadow = s.some_field? {
        result = shadow * 3
    } else {
        0
    }
    result
    ",
    );

    assert_eq!(result, Value::Int(2 * 3));
}

#[test_log::test]
fn if_statement_optional_3() {
    check(
        r#"

fn lookup_age(name: String) -> Int? {
    if name == "Hero" {
        43
    } else {
        none
    }
}

maybe_age = lookup_age("Hero")
if age = maybe_age? {  // Shortened syntax for optional check
    print('Found age! {age}')
}
    "#,
        "
    Found age! 43",
    );
}

#[test_log::test]
fn if_statement_optional_2() {
    let result = eval(
        "

    struct SomeStruct {
       some_field: Int?
    }

    s = SomeStruct { some_field: 2 }
    a = s.some_field
    mut result = 0

    if a? {
        result = a * 3
    }
    result

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

    mut x = 0
    if a? {
        x = a * 3
    }
    x
    ",
    );

    assert_eq!(result, Value::Int(2 * 3));
}

#[test_log::test]
fn optional_field_mut() {
    let result = eval(
        "

    struct SomeStruct {
       some_field: Int?
    }

    mut s = SomeStruct { some_field: 2 }

    s.some_field = 3

    mut x = 0
    if a = s.some_field? {
        x += 3
    }
    x
    ",
    );

    assert_eq!(
        result,
        Value::Reference(Rc::new(RefCell::new(Value::Int(3))))
    );
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
    ",
    );

    assert_eq!(result, Value::Int(4 * 5 / 3 - 1));
}

#[test_log::test]
fn multi_variables() {
    let result = eval(
        "

     mut x = y = mut z = 3
    z = 8
    ",
    );

    assert_eq!(result, Value::Int(8));
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

#[test_log::test]
fn sparse_map_create() {
    let x = eval(
        "

    sparse = Sparse<Int>::new()
    ",
    );

    assert_eq!(x.to_string(), "Sparse<Int> len:0");
}

#[test_log::test]
fn sparse_map_add() {
    let x = eval(
        "

    mut sparse = Sparse<Int>::new()
    print(sparse)
    id = sparse.add(2)
    print(id)
    id
    ",
    );

    assert_eq!(x.to_string(), "id:0:0");
}

#[test_log::test]
fn sparse_map_remove() {
    let x = eval(
        "

    mut sparse = Sparse<Int>::new()
    sparse_id = sparse.add(2)

    print(sparse)
    print(sparse_id)

    sparse.remove(sparse_id)
    ",
    );

    assert_eq!(x.to_string(), "Sparse<Int> len:0");
}

#[test_log::test]
fn sparse_map_iterate() {
    check(
        "

    mut sparse = Sparse<Int>::new()
    sparse_id = sparse.add(2)
    sparse.add(3)

    print(sparse)
    print(sparse_id)

    for x in sparse {
        print(x)
    }
    ",
        "\
    Sparse<Int> len:2
    id:0:0
    2
    3
    ",
    );
}

#[test_log::test]
fn sparse_map_struct_iterate() {
    check(
        "

    struct Projectile {
        x: Float,
        y: Float,
    }

    mut sparse = Sparse<Projectile>::new()
    sparse_id = sparse.add(Projectile {x: 1.0, y: 2.0})
    sparse.add(Projectile {x: 1.0, y: 3.0})

    print(sparse)
    print(sparse_id)

    for mut x in sparse {
        print(x)
    }
    ",
        "\
    Sparse<Projectile { x: Float, y: Float }> len:2
    id:0:0
    Projectile { x: 1.00, y: 2.00 }
    Projectile { x: 1.00, y: 3.00 }
    ",
    );
}

#[test_log::test]
fn sparse_map_struct_empty_iterator() {
    check(
        "

    struct Projectile {
        x: Float,
        y: Float,
    }

    mut sparse = Sparse<Projectile>::new()
    print('before')
    for x in sparse {
        print(x)
    }
    ",
        "\
        before
    ",
    );
}

#[test_log::test]
fn sparse_map_iterate_pairs() {
    check(
        "

    mut sparse = Sparse<Int>::new()
    sparse_id = sparse.add(2)
    sparse.add(3)

    print(sparse)
    print(sparse_id)

    for x, y in sparse {
        print(x)
        print(y)
    }
    ",
        "\
    Sparse<Int> len:2
    id:0:0
    id:0:0
    2
    id:1:0
    3
    ",
    );
}

#[test_log::test]
fn sparse_map_in_field() {
    check(
        "
    struct Hello {
        sparse: std::Sparse<Int>,
    }

    h = Hello {
        sparse: Sparse<Int>::new()
    }
    ",
        "\
    ",
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
    if player? {
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
if health = get_health_if_alive(find_player_by_name("Hero"))? {
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
