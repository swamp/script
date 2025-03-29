/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
mod util;

use crate::util::check_value;
use swamp_core_extra::prelude::Value;

#[test_log::test]
fn basic_eval_5() {
    check_value(
        r"

        mut counter = 0
        for i in 1..3 {
            counter = counter + 1
            counter = counter + i
        }
        counter

    ",
        Value::Int(1 + 1 + 1 + 2),
    );
}

#[test_log::test]
fn for_inclusive() {
    check_value(
        r"

        mut counter = 0
        for i in 1..=3 {
            counter = counter + 1
            counter += i
        }
        counter

    ",
        Value::Int((1 + 1) + (1 + 2) + (1 + 3)),
    );
}
