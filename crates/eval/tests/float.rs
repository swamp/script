/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
mod util;

use crate::util::check_value;
use fixed32::Fp;
use swamp_script_core_extra::prelude::Value;

#[test_log::test]
fn float_sin() {
    check_value(
        r"
        2.2.sin()
    ",
        Value::Float(Fp::from_raw(53581)), // 52937
    );
}

#[test_log::test]
fn float_cos() {
    check_value(
        r"
        0.5.cos()
    ",
        Value::Float(Fp::from_raw(57798)), // 57_511
    );
}

#[test_log::test]
fn float_floor() {
    check_value(
        r"
        0.5.floor()
    ",
        Value::Int(0),
    );
}

#[test_log::test]
fn float_floor_2() {
    check_value(
        r"
        42.12.floor()
    ",
        Value::Int(42),
    );
}

#[test_log::test]
fn float_clamp() {
    check_value(
        r"
        42.12.clamp(43.0, 90.0)
    ",
        Value::Float(Fp::from(43.0)),
    );
}
