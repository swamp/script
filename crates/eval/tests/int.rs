/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
mod util;

use crate::util::check_value;
use fixed32::Fp;
use swamp_script_core_extra::prelude::Value;

#[test_log::test]
fn int_sign() {
    check_value(
        r"
        a = -2
        a.abs()
    ",
        Value::Int(2),
    );
}

#[test_log::test]
fn int_rnd() {
    check_value(
        r"
        23232.rnd()
    ",
        Value::Int(-314_933_885),
    );
}

#[test_log::test]
fn int_to_float() {
    check_value(
        r"
        -99.to_float()
    ",
        Value::Float(Fp::from(-99.00)),
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
