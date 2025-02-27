mod util;

use crate::util::{check, check_fail, check_value, eval, eval_string};
use swamp_script_core::prelude::Value;

#[test_log::test]
fn with_block() {
    let result = eval(
        "
        x = 4
        t = 6

        result = with x {
           x + 5
        }
    ",
    );

    assert_eq!(result, Value::Int(9));
}

#[test_log::test]
fn fail_with_block() {
    check_fail(
        "
        x = 4
        t = 6

        result = with x {
           x + t + 5 // t is not available in this scope

        }
    ",
        "Error(Error { node: <71:1>, kind: UnknownVariable })",
    );
}

#[test_log::test]
fn with_normal_block() {
    let result = eval(
        "
        x = 4
        t = 6

        with x {
           x + 5
           t = 3
        }
    t // t should not have been affected
            ",
    );

    assert_eq!(result, Value::Int(6));
}
