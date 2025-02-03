mod util;

use crate::util::{check, check_fail, check_value, eval, eval_string};
use swamp_script_core::prelude::Value;

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
fn array_push() {
    eval_string(
        "

    mut a = [10, 20]
    a += 30
    a
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
    a
    ",
        "[10, 20, 30, 40, 50]",
    );
}

#[test_log::test]
fn array_remove_fail() {
    check_fail(
        "

    a = [10, 20]

    a.remove(1)
    ",
        "ResolveError(ResolveError { node: <26:6>, kind: ExpectedMutableLocation })",
    );
}

#[test_log::test]
fn array_remove() {
    check(
        "

    mut a = [10, 20]

    a.remove(1)
    print('a: {a}')
    ",
        "a: [10]",
    );
}

#[test_log::test]
fn array_clear() {
    eval_string(
        "

    mut a = [10, 20]

    a += [30, 40, 50]

    a.clear()
    a
    ",
        "[]",
    );
}

#[test_log::test]
fn array_access() {
    let x = eval(
        "

    mut a = [10, 20]

    a[1]
    ",
    );

    assert_eq!(x, Value::Int(20));
}
