mod util;

use crate::util::eval;
use swamp_script_core_extra::prelude::Value;

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
