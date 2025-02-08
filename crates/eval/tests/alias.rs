mod util;

use crate::util::{check, check_fail, eval};
use swamp_script_core::prelude::Value;

#[test_log::test]
fn alias_struct() {
    let result = eval(
        "
        struct Something {
            x: Int,
            y: Int,
        }

        type Another = Something

        v = Another {
            x: 10,
            y: 20,
        }
        v.y
    ",
    );

    assert_eq!(result, Value::Int(20));
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
        "ResolveError(ResolveError { node: <71:1>, kind: UnknownVariable })",
    );
}

#[test_log::test]
fn alias_tuple() {
    check(
        "
        type SomePos = (Int, Int)

        fn pos(x: Int) -> SomePos {
            (x, 20)
        }

        print('{pos(3)}')
            ",
        "
            (3, 20)
            ",
    );
}

#[test_log::test]
fn alias_tuple_param() {
    check(
        "
        type SomePos = (Int, Int)

        fn pos(other: SomePos) -> SomePos {
            x, y = other
            // something

            v = (20, y)
            v
        }

        print('{pos((42, 98))}')


            ",
        "
            (20, 98)
            ",
    );
}
