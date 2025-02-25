use crate::util::{check, check_fail};

mod util;

#[test_log::test]
fn basic_generic() {
    check(
        r"

struct Something<T> {
    a: Float,
    value: T,
}

instantiated = Something<Int> {
    a: 2.3,
    value: 3,
}

         ",
        "
Something: Something {
....a: Float, 
....value: T, 
..}<T>

---
let instantiated = Something<Int> {
..a: 2.30f, 
..value: 3i, 
}

        ",
    );
}

#[test_log::test]
fn fail_basic_generic() {
    check_fail(
        r"

struct Something<T> {
    a: Float,
    value: T,
}

a = Something<Int> {
    a: 2.3,
    value: 4.0,
}

         ",
        r"
Error { node: <99:3>, kind: IncompatibleTypes(Int, Float) }
        ",
    );
}

#[test_log::test]
fn basic_generic_inception() {
    check(
        "

struct Else<SomeType> {
    another: SomeType,
    b: Float,
}

struct Something<T> {
    a: Float,
    value: Else<T>,
}

a = Something<Int> {
    value: Else<Int> {
        b: 23.0,
        another: 44,
    },
    a: 2.3,
}
         ",
        "
Else: <SomeType>Else {
..another: SomeType, 
..b: Float, 
}

Something: <T>Something {
..a: Float, 
..value: Else<T>, 
}
---
let a = Something<Int> {
..value: Else<Int> {
....b: 23.00f, 
....another: 44i, 
..}, 
..a: 2.30f, 
}
        ",
    );
}
