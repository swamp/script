mod util;

use crate::util::{check, eval};
use swamp_script_core_extra::prelude::Value;

#[test_log::test]
fn option_assign() {
    check(
        r"
        map = [2: 'hello']
        a = map[3]
        when x = a {
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
        when x = a {
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
        when mut x = mut a {
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
        when a {
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
        when mut a {
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
        when mut another = a {
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
        when mut another = mut a {
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
fn basic_eval_22() {
    let result = eval(
        "

    struct SomeStruct {
       some_field: Int?
    }

    s = SomeStruct { some_field: 2 } // It should implicitly convert `some_field` to Some(2)

    mut c = when _ = s.some_field { // this should evaluate to true, since Some is truthy
       'it was some'
    } else {
       'none'
    }
    ",
    );

    assert_eq!(result, Value::String("it was some".to_string()));
}

#[test_log::test]
fn unwrap_some() {
    let result = eval(
        "

    struct Inner {
        i: Int
    }

    struct SomeStruct {
       some_field: Inner?
    }

    s = SomeStruct { some_field: Inner { i: 2 } }

    when a=s.some_field?.i {a} else {-99}

    ",
    );

    assert_eq!(result, Value::Int(2));
}

#[test_log::test]
fn unwrap_none() {
    let result = eval(
        "

    struct Inner {
        i: Int
    }

    struct SomeStruct {
       some_field: Inner?
    }

    s = SomeStruct { some_field: none }

    s.some_field?.i

    ",
    );

    assert_eq!(result, Value::Option(None));
}

#[test_log::test]
fn unwrap_none_with_default() {
    let result = eval(
        "

    struct Inner {
        i: Int
    }

    struct SomeStruct {
       some_field: Inner?
    }

    s = SomeStruct { some_field: none }

    s.some_field?.i ?? -99

    ",
    );

    assert_eq!(result, Value::Int(-99));
}

#[test_log::test]
fn option_none_to_bool_coalesce() {
    let result = eval(
        "

    struct Inner {
        i: Int
    }

    struct SomeStruct {
       some_field: Inner?
    }

    s = SomeStruct { some_field: none }

    if s.some_field {
        2
    } else {
        0
    }

    ",
    );

    assert_eq!(result, Value::Int(0));
}

#[test_log::test]
fn option_some_to_bool_coalesce() {
    let result = eval(
        "

    struct Inner {
        i: Int
    }

    struct SomeStruct {
       some_field: Inner?
    }

    s = SomeStruct { some_field: Inner { i: 42 } }

    if s.some_field {
        2
    } else {
        0
    }

    ",
    );

    assert_eq!(result, Value::Int(2));
}

#[test_log::test]
fn basic_eval_23() {
    let result = eval(
        "

    struct SomeStruct {
       some_field: Int?
    }

    s = SomeStruct { some_field: 2 } // It should implicitly convert `some_field` to Some(2)

    s.some_field?
    ",
    );

    assert!(matches!(result, Value::Option(_)));
}

#[test_log::test]
fn basic_eval_24() {
    let result = eval(
        "

    struct SomeStruct {
       some_field: Int?
    }

    s = SomeStruct { some_field: 2 }

    x = when result = s.some_field {
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
    when shadow = s.some_field {
        result = shadow * 3
        result
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
when  maybe_age {  // Shortened syntax for optional check
    print('Found age! {maybe_age}')
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

    when a {
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
    when a {
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
    when a = s.some_field {
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
before: Option(2)
after: Option(3)
    ",
    );
}
