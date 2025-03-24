/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

mod util;

use crate::util::eval;
use swamp_script_core_extra::prelude::Value;

#[test_log::test]
fn constants_outside_function() {
    let result = eval(
        "
    const SOMETHING_HERE = 42

    fn hello() -> Int {
        SOMETHING_HERE
    }
    hello()
    ",
    );

    assert_eq!(result, Value::Int(42));
}
