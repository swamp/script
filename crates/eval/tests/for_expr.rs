mod util;

use crate::util::{check, check_fail, check_value, eval, eval_string};
use swamp_script_core::prelude::Value;

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
