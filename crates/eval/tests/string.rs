/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
mod util;

use crate::util::check;

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
