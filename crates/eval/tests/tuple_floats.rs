mod util;

use crate::util::{check, check_fail, check_value, eval, eval_string};
use fixed32::Fp;
use swamp_script_core_extra::prelude::Value;

#[test_log::test]
fn float_2_magnitude() {
    check_value(
        r"
        (1.3, 4.7).magnitude()
    ",
        Value::Float(Fp::from_raw(319_584)), // about 4.876
    );
}
