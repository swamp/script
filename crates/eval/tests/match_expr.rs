/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
mod util;

use crate::util::eval;
use swamp_core_extra::prelude::Value;

#[test_log::test]
fn int_literal() {
    let result = eval(
        r#"

        health = 80

        match health {
            0 => "scary!",
            100 => "full health!",
            x => 'so mid {x}'
        }
    "#,
    );

    assert_eq!(result, Value::String("so mid 80".to_string()));
}

#[test_log::test]
fn match_enum_struct() {
    let result = eval(
        r#"

        enum Action {
            Jumping,
            Target { x: Int, y: Int },
            Other(String),
        }

        action = Action::Target { x:42, y: -999 }

        /*
        match the actions
            - Jumping
            - Target
        */

        //print('action {action}')
        match action {
            Jumping => "jumping",
            Target y => '{y}',
            _ => "can not find it!",
        }
    "#,
    );

    assert_eq!(result, Value::String("-999".to_string()));
}

#[test_log::test]
fn match_enum_struct_mut() {
    let result = eval(
        r#"

        enum Action {
            Jumping,
            Target { x: Int, y: Int },
            Other(String),
        }

        action = Action::Target { x:42, y: -999 }

        /*
        match the actions
            - Jumping
            - Target
        */

        //print('action {action}')
        match action {
            Jumping => "jumping",
            Target y => '{y}',
            _ => "can not find it!",
        }
    "#,
    );

    assert_eq!(result, Value::String("-999".to_string()));
}

#[test_log::test]
fn match_tuple() {
    let result = eval(
        "

        pos = (-1, 10)

        match pos {
            (2, 3) => 1,
            (0, 0) => 2,
            (-1, 10) => 3,
            (-1, -1) => 4,
        }
    ",
    );

    assert_eq!(result, Value::Int(3));
}
