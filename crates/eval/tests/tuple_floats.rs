/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
mod util;

use crate::util::check_value;
use fixed32::Fp;
use swamp_core_extra::prelude::Value;

#[test_log::test]
fn float_2_magnitude() {
    check_value(
        r"
        (1.3, 4.7).magnitude()
    ",
        Value::Float(Fp::from_raw(319_584)), // about 4.876
    );
}
