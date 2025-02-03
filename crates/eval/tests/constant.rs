mod util;

use crate::util::{check, eval};
use swamp_script_core::prelude::Value;

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
