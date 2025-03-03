mod util;

use crate::util::{check, eval};
use swamp_script_core_extra::prelude::Value;

#[test_log::test]
fn tuple_destructuring() {
    let result = eval(
        "
    x, y = (2, 3)
    y
    ",
    );

    assert_eq!(result, Value::Int(3));
}

#[test_log::test]
fn tuple_destructuring_1() {
    let result = eval(
        "
    x, y = (2, 3)
    x
    ",
    );

    assert_eq!(result, Value::Int(2));
}
