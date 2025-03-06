/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

mod util;

use crate::util::{check, check_fail, check_value, eval};
use swamp_script_core_extra::prelude::Value;

#[test_log::test]
fn map_fn_return() {
    let x = eval(
        "

    fn map_creator() -> [Int:String] {
        [2: 'hello', -1: 'world']
    }

    mut a = map_creator()
    a[3] = 'ossian'
    a[3] ?? 'failed'
    ",
    );

    assert_eq!(x, Value::String("ossian".to_string()));
}

#[test_log::test]
fn map_x() {
    check(
        "

mut map = ['hi': 2]
z = mut map['hi']
print('z : {z}')

mut a = mut map['hi']
print('a : {a}')
    ",
        "
    a : Option(2)
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
fn map_index_if_found() {
    let x = eval(
        "
    a = [2: 'hello', -1: 'world']
    b = a[-1]
    x = when b {
       b + ' was found'
    } else { 'not found' }
    ",
    );

    assert_eq!(x, Value::String("world was found".to_string()));
}

#[test_log::test]
fn map_index_if_found_expression() {
    let x = eval(
        "

    a = [2: 'hello', -1: 'world']
    x = when found = a[-1] {
       found + ' was found'
    } else { 'not found' }
    ",
    );

    assert_eq!(x, Value::String("world was found".to_string()));
}

#[test_log::test]
fn map_fail_insert_with_immutable() {
    check_fail(
        "

    a = [2:'hello', -1:    'world']
    a[3] = 'ossian'

    ",
        "Error(Error { node: <42:4>, kind: NotValidLocationStartingPoint })",
    );
}

#[test_log::test]
fn map_fail_insert() {
    check_fail(
        "

    a = [2:'hello', -1:    'world']
    a[3] = 'ossian'

    ",
        "Error(Error { node: <42:4>, kind: NotValidLocationStartingPoint })",
    );
}

#[test_log::test]
fn map_insert() {
    check_value(
        "

    mut a = [2:'hello', -1:    'world']
    a[3] = 'ossian'
    a[3] ?? 'didnt work'
    ",
        Value::String("ossian".to_string()),
    );
}

#[test_log::test]
fn map_insert_wrong_type() {
    check_fail(
        "

    mut a = [2:'hello', -1:    'world']
    a[3] = 5.5

    ",
        "Error(Error { node: <53:3>, kind: IncompatibleTypes(String, Float) })",
    );
}

#[test_log::test]
fn map_fail_overwrite() {
    check(
        "

    mut a = [2: 'hello', -1: 'world']
    a[-1] = 'ossian'

    ",
        "ExecuteError(ExecuteError { node: <44:5>, kind: MapKeyAlreadyExists })",
    );
}

#[test_log::test]
fn map_for() {
    check(
        "

    mut a = [2: 'hello', -1: 'world']
    for entry in a {
        print('entry {entry}')
    }

    ",
        "
    entry hello
    entry world
    ",
    );
}

#[test_log::test]
fn map_for_mut() {
    check(
        "

    mut a = [2: 'hello', -1: 'world']
    for mut entry in a {
        print('before {entry}')
        entry = 'overwritten {entry}'
    }

    for entry in a {
        print('after {entry}')
    }

    ",
        "
    before hello
    before world
    after overwritten hello
    after overwritten world
    ",
    );
}
