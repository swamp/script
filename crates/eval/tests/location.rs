mod util;

use crate::util::{check, eval};
use swamp_script_core_extra::prelude::Value;

#[test_log::test]
fn mutable_location_field_access() {
    let v = eval(
        r"
struct Brain {
    i: Int,
}

struct Enemy {
    brain: Brain,
}

impl Brain {
    fn think(mut self) -> Bool {
        self.i += 1
        self.i > 20
    }
}

mut enemy = Enemy { brain: Brain { i: 0 } }

want_to_attack = enemy.brain.think()
enemy.brain.i
         ",
    );

    assert_eq!(v, Value::Int(1));
}

#[test_log::test]
fn field_chain_problem() {
    let result = eval(
        "
    struct Inner {
        v: Int
    }

    struct SomeStruct {
       inner: Inner
    }

    mut s = SomeStruct { inner: Inner { v : 4 } }

    s.inner.v = 5
    s.inner.v
    ",
    );

    assert_eq!(result, Value::Int(5));
}

#[test_log::test]
fn field_chain_problem_output() {
    check(
        "
        struct Inner {
            v: Int
        }

        struct SomeStruct {
            something: Int,
           inner: Inner
        }

        mut s = SomeStruct { something: 3, inner: Inner { v : 4 } }

        print(s)

        s.inner.v = 5

        print(s)
    ",
        "
        SomeStruct { something: 3, inner: Inner { v: 4 } }
        SomeStruct { something: 3, inner: Inner { v: 5 } }
        ",
    );
}

#[test_log::test]
fn field_chain_array_problem_output() {
    check(
        "
    struct Inner {
        v: Int
    }

    struct SomeStruct {
        something: Int,
       inner: Inner
    }

    s = SomeStruct { something: 3, inner: Inner { v : 4 } }

    mut arr = [ s ]

    print(arr)

    arr[0].inner.v = 5

    print(arr)
    ",
        r#"
            [SomeStruct { something: 3, inner: Inner { v: 4 } }]
            [SomeStruct { something: 3, inner: Inner { v: 5 } }]
        "#,
    );
}
