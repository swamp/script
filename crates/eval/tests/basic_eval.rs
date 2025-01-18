/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::util::{check, check_fail, check_value, eval, eval_string};
use fixed32::Fp;
use seq_map::SeqMap;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_core::prelude::Value;
use swamp_script_core::qck_des::quick_deserialize;
use swamp_script_core::value::ValueRef;
use swamp_script_semantic::{
    ResolvedAnonymousStructFieldType, ResolvedAnonymousStructType, ResolvedNode,
    ResolvedStructType, ResolvedType,
};

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
        let owned_values: Vec<_> = values.iter().map(|item| item.borrow().clone()).collect();
        assert_eq!(owned_values, vec![Value::Int(10), Value::Int(5)]);
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


        mut ossian = Ossian { happy: 3 }

        Ossian::unrelated_function(3.5) // TODO: change to Ossian::unrelated_function(3.5) ?
        ossian.coding()
        ossian.pixel_art(1)
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
    print(s)

    mut x = 0
    if a = s.some_field? {
        x += 3
    }
    x
    ",
    );

    assert_eq!(result, Value::Int(3));
}

#[test_log::test]
fn optional_field_mut_print() {
    check(
        "

    struct SomeStruct {
       some_field: Int?
    }

    mut s = SomeStruct { some_field: 2 }

print('before: {s.some_field}')
    s.some_field = 3
print('after: {s.some_field}')
    ",
        r"
before: Some(Int(2))
after: Some(Int(3))
    ",
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
        "ExecuteError(VariableWasNotMutable)",
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
fn array_push() {
    eval_string(
        "

    mut a = [10, 20]
    a += 30
    ",
        "[10, 20, 30]",
    );
}

#[test_log::test]
fn array_extend() {
    eval_string(
        "

    mut a = [10, 20]

    a += [30, 40, 50]
    ",
        "[10, 20, 30, 40, 50]",
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
fn array_remove_fail() {
    check_fail(
        "

    a = [10, 20]

    a.remove(1)
    ",
        "ResolveError(VariableIsNotMutable(<6:1>))",
    );
}

#[test_log::test]
fn array_clear() {
    eval_string(
        "

    mut a = [10, 20]

    a += [30, 40, 50]

    a.clear()
    ",
        "[]",
    );
}

#[test_log::test]
fn array_clear2() {
    let x = eval(
        "

    mut a = [10, 20]

    a[1]
    ",
    );

    assert_eq!(x, Value::Int(20));
}

#[test_log::test]
fn sparse_map_create() {
    eval_string(
        "

    sparse = Sparse<Int>::new()
    ",
        "Sparse<Int(ResolvedIntType)> len:0",
    );
}

#[test_log::test]
fn sparse_map_add() {
    eval_string(
        "

    mut sparse = Sparse<Int>::new()
    print(sparse)
    id = sparse.add(2)
    print(id)
    id
    ",
        "id:0:0",
    );
}

#[test_log::test]
fn sparse_map_remove() {
    let _x = eval_string(
        "

    mut sparse = Sparse<Int>::new()
    sparse_id = sparse.add(2)

    print(sparse)
    print(sparse_id)

    sparse.remove(sparse_id)
    ",
        "Sparse<Int(ResolvedIntType)> len:0",
    );
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
    Sparse<Int(ResolvedIntType)> len:2
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
    Sparse<Int(ResolvedIntType)> len:2
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

#[test_log::test]
fn field_chain_problem() {
    let result = eval(
        "
    struct Inner {
        v: Int
    }

    struct SomeStruct {
       inner: Inner
    }

    mut s = SomeStruct { inner: Inner { v : 4 } }

    s.inner.v = 5
    ",
    );

    assert_eq!(result, Value::Int(5));
}
#[test_log::test]
fn field_chain_problem_output() {
    check(
        "
    struct Inner {
        v: Int
    }

    struct SomeStruct {
        something: Int,
       inner: Inner
    }

    mut s = SomeStruct { something: 3, inner: Inner { v : 4 } }

    print(s)

    s.inner.v = 5

    print(s)
    ",
        r#"
            SomeStruct { something: 3, inner: Inner { v: 4 } }
            SomeStruct { something: 3, inner: Inner { v: 5 } }
        "#,
    );
}

#[test_log::test]
fn field_chain_array_problem_output() {
    check(
        "
    struct Inner {
        v: Int
    }

    struct SomeStruct {
        something: Int,
       inner: Inner
    }

    s = SomeStruct { something: 3, inner: Inner { v : 4 } }

    mut arr = [ s ]

    print(arr)

    arr[0].inner.v = 5

    print(arr)
    ",
        r#"
            [SomeStruct { something: 3, inner: Inner { v: 4 } }]
            [SomeStruct { something: 3, inner: Inner { v: 5 } }]
        "#,
    );
}

#[test_log::test]
fn tuple_destructuring() {
    let result = eval(
        "
    x, y = (2, 3)
    y
    ",
    );

    assert_eq!(result, Value::Int(3));
}

#[test_log::test]
fn tuple_destructuring_1() {
    let result = eval(
        "
    x, y = (2, 3)
    x
    ",
    );

    assert_eq!(result, Value::Int(2));
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
        r#"ResolveError(MissingFieldInStructInstantiation("a", ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<27:1>), field_type: Int(ResolvedIntType), index: 0 }, "b": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<43:1>), field_type: Float(ResolvedFloatType), index: 0 }) }))"#,
    );
}

#[test_log::test]
fn constants_in_function() {
    let result = eval(
        "
    fn hello() -> Int {
        const SOMETHING = 42

        SOMETHING
    }
    hello()
    ",
    );

    assert_eq!(result, Value::Int(42));
}

#[test_log::test]
fn constants_outside_function() {
    let result = eval(
        "
    const SOMETHING_HERE = 42

    fn hello() -> Int {
        SOMETHING_HERE
    }
    hello()
    ",
    );

    assert_eq!(result, Value::Int(42));
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
fn function_ref() {
    let result = eval(
        "
        fn caller(fn: (Int) -> Int, arg: Int) -> Int {
            fn(arg)
        }

        fn some_fn(a: Int) -> Int {
            a * 2
        }

        caller(some_fn, -10)
    ",
    );

    assert_eq!(result, Value::Int(-20));
}

#[test_log::test]
fn function_member_ref() {
    let result = eval(
        "
        struct Test {
            i: Int,
        }

        impl Test {
            fn static_in_member(a: Int) -> Int {
              a * 2
            }
        }

        fn caller(fn: (Int) -> Int, arg: Int) -> Int {
            fn(arg)
        }

        caller(Test::static_in_member, -10)
    ",
    );

    assert_eq!(result, Value::Int(-20));
}

#[test_log::test]
fn function_static_member_call() {
    let result = eval(
        "
        struct Test {
            i: Int,
        }

        impl Test {
            fn static_in_member(a: Int) -> Int {
              a * 2
            }
        }

        Test::static_in_member(4)
    ",
    );

    assert_eq!(result, Value::Int(8));
}

#[test_log::test]
fn function_value_call() {
    let result = eval(
        "
struct StructWithFunction {
    some_fn: (Int) -> Int,
}

fn add_two(a: Int) -> Int {
    a + 2
}

s = StructWithFunction { some_fn: add_two }
s.some_fn(10)
    ",
    );

    assert_eq!(result, Value::Int(12));
}

#[test_log::test]
fn function_member_fn_call() {
    let result = eval(
        "
        struct Test {
            i: Int,
        }

        impl Test {
            fn member(self, a: Int) -> Int {
              self.i * a
            }
        }

        t = Test { i: 20 }
        t.member(10)
    ",
    );

    assert_eq!(result, Value::Int(200));
}

#[test_log::test]
fn with_block() {
    let result = eval(
        "
        x = 4
        t = 6

        result = with x {
           x + 5
        }
    ",
    );

    assert_eq!(result, Value::Int(9));
}

#[test_log::test]
fn fail_with_block() {
    check_fail(
        "
        x = 4
        t = 6

        result = with x {
           x + t + 5 // t is not available in this scope

        }
    ",
        "ResolveError(UnknownVariable(<71:1>))",
    );
}

#[test_log::test]
fn with_normal_block() {
    let result = eval(
        "
        x = 4
        t = 6

        with x {
           x + 5
           t = 3
        }
    t // t should not have been affected
            ",
    );

    assert_eq!(result, Value::Int(6));
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
fn array_range_access() {
    check(
        "
        a = [0, 1, 1, 2, 3, 5, 8, 13]
        print(a [ 4 .. 6 ])
            ",
        r"
        [3, 5]
        ",
    );
}

#[test_log::test]
fn array_range_assignment() {
    check(
        "
        mut a = [0, 1, 1, 2, 3, 5, 8, 13]
        print(a[0..2])
        a[0..2] = [-1, 99]
        print(a[0..2])
        print(a [ 4 .. 6 ])
            ",
        r"
        [0, 1]
        [-1, 99]
        [3, 5]
        ",
    );
}

#[test_log::test]
fn string_range_mut_assignment() {
    check(
        "
        mut str = 'h4ck3r'
        print(str)
        str[2..5] = '123'
        print(str)
            ",
        r"
        h4ck3r
        h4123r
        ",
    );
}

#[test_log::test]
fn string_range_mut_assignment_inclusive() {
    check(
        "
        mut str = 'h4ck3r'
        print(str)
        str[2..=5] = '1234'
        print(str)
            ",
        r"
        h4ck3r
        h41234
        ",
    );
}

#[test_log::test]
fn string_iterable() {
    check(
        "
        str = 'hello'
        for s in str {
            print('{s}')
        }
            ",
        r"
        h
        e
        l
        l
        o
        ",
    );
}

#[test_log::test]
fn string_iterable_with_index() {
    check(
        "
        str = 'hello'
        for i, s in str {
            print('{i}:{s}')
        }
            ",
        r"
        0:h
        1:e
        2:l
        3:l
        4:o
        ",
    );
}

#[test_log::test]
fn option_assign() {
    check(
        r"
        map = [2: 'hello']
        a = map[3]
        if x = a? {
            print('x: {x}')
        } else {
            print('was not found')
        }
         ",
        r"

was not found

",
    );
}

#[test_log::test]
fn option_assign_found() {
    check(
        r"
        map = [2: 'hello']
        a = map[2]
        if x = a? {
            print('x: {x}')
        } else {
            print('was not found')
        }
         ",
        r"

x: hello

",
    );
}

#[test_log::test]
fn option_assign_found_mut() {
    check(
        r"
        mut map = [2: 'hello']
        mut a = mut map[2]
        if mut x = a? {
            x = x + '-hi'
            print('x: {x}')
        } else {
            print('was not found')
        }
        print('affected {map[2]}')
         ",
        r#"

x: hello-hi
affected Option("hello-hi")
"#,
    );
}

#[test_log::test]
fn option_array_assign_found_mut() {
    check(
        r"
        mut arr = ['hello', 'goodbye']
        print('before {arr[1]}')
        mut a = mut arr[1]
        a = 'hi'
        print('affected {arr[1]}')
         ",
        r"

before goodbye
affected hi

",
    );
}

#[test_log::test]
fn option_var() {
    check(
        r"
        map = [2: 'hello']
        a = map[3]
        if a? {
            print('a: {a}')
        } else {
            print('not found')
        }
         ",
        r"

not found

",
    );
}

#[test_log::test]
fn option_var_mut() {
    check(
        r"
        mut map = [2: 'hello']
        mut a = mut map[2]
        if a? {
            print('before: {a}')
            a = a + ' goodbye'
            print('after: {a}')
        } else {
            print('not found')
        }
        print('affected map: {map[2]}')
         ",
        r#"

before: hello
after: hello goodbye
affected map: Option("hello goodbye")
"#,
    );
}

#[test_log::test]
fn option_expr_intentionally_immutable() {
    check(
        r"
        map = [2: 'hello']
        mut a = map[2]
        if mut another = a? {
            print('before: {another}')
            another = another + ' goodbye'
            print('after: {another}')
        } else {
            print('not found')
        }
        print('affected {map[2]}')
         ",
        r#"

before: hello
after: hello goodbye
affected Option("hello")
"#,
    );
}

#[test_log::test]
fn option_expr_mut_struct() {
    check(
        r"
        struct Pos {
            x: Int,
            y: Int,
        }
        struct Player {
            pos: Pos,
        }

        mut map = [2: Player { pos: Pos { x: 10, y: -91 } } ]
        mut a = mut map[2]
        if mut another = a? {
            print('before: {another}')
            another.pos.y = 16
            print('after: {another}')
        } else {
            print('not found')
        }
        print('affected map: {map[2]}')
         ",
        r"

    before: Player { pos: Pos { x: 10, y: -91 } }
    after: Player { pos: Pos { x: 10, y: 16 } }
    affected map: Option(Player { pos: Pos { x: 10, y: 16 } })
",
    );
}

#[test_log::test]
fn assign_map_mut() {
    check(
        r"
mut map = [2: 'hi']
print('before {map[2]}')
mut a = mut map[2]
a = 'hello'
print('after {map[2]}')
         ",
        r#"

before Option("hi")
after Option("hello")

"#,
    );
}

#[test_log::test]
fn serialize_octets() {
    let v = Value::Int(2);
    let mut buf = [0u8; 4];
    let size = v.quick_serialize(&mut buf, 0);
    assert_eq!(size, 4);
    assert_eq!(buf[0], 2);

    let (deserialized_value, deserialized_octet_size) =
        quick_deserialize(&ResolvedType::Int, &buf, 0);
    assert_eq!(deserialized_value, v);
    assert_eq!(size, deserialized_octet_size);
}

#[allow(unused)]
fn values_to_value_refs(values: &[Value]) -> Vec<ValueRef> {
    let mut items = Vec::new();

    for x in values.iter().cloned() {
        items.push(Rc::new(RefCell::new(x)));
    }

    items
}

fn values_to_value_refs_owned(values: Vec<Value>) -> Vec<ValueRef> {
    values
        .into_iter()
        .map(|x| Rc::new(RefCell::new(x)))
        .collect()
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
            ResolvedAnonymousStructFieldType {
                identifier: None,
                field_type: ResolvedType::Int,
            },
        )
        .expect("");
    defined_fields
        .insert(
            "b".to_string(),
            ResolvedAnonymousStructFieldType {
                identifier: None,
                field_type: ResolvedType::String,
            },
        )
        .expect("");
    defined_fields
        .insert(
            "c".to_string(),
            ResolvedAnonymousStructFieldType {
                identifier: None,
                field_type: ResolvedType::Float,
            },
        )
        .expect("");

    let struct_type = ResolvedStructType {
        name: ResolvedNode::default(),
        assigned_name: "SomeStructType".to_string(),
        anon_struct_type: ResolvedAnonymousStructType { defined_fields },
        functions: SeqMap::default(),
    };
    let struct_type_ref = Rc::new(RefCell::new(struct_type));

    let fields = values_to_value_refs_owned(vec![a, b, c]);
    let struct_value = Value::Struct(struct_type_ref.clone(), fields);

    let mut buf = [0u8; 256];
    let size = struct_value.quick_serialize(&mut buf, 0);
    assert_eq!(size, 21);

    let complete_struct_type = ResolvedType::Struct(struct_type_ref);

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
if booster? {
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
fn mutable_location_field_access() {
    let v = eval(
        r"
struct Brain {
    i: Int,
}

struct Enemy {
    brain: Brain,
}

impl Brain {
    fn think(mut self) {
        self.i += 1
    }
}

mut enemy = Enemy { brain: Brain { i: 0 } }

want_to_attack = enemy.brain.think()
enemy.brain.i
         ",
    );

    assert_eq!(v, Value::Int(1));
}
