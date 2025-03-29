/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_script_core_extra::prelude::Value;

pub struct TestContext {
    pub secret: i32,
    pub output: Vec<String>,
}

#[allow(dead_code)] // TODO: this should not be needed since it is under tests/
pub fn check(script: &str, expected_result: &str) {
    todo!()
}

#[allow(dead_code)] // TODO: this should not be needed since it is under tests/
pub fn check_fail(script: &str, expected_err: &str) {
    todo!()
}

#[allow(dead_code)] // TODO: This should not be needed since it is under tests/
pub fn check_value(script: &str, expected_value: Value) {
    todo!()
}

#[allow(dead_code)] // TODO: This should not be needed since it is under tests/
pub fn eval(script: &str) -> Value {
    todo!()
}

#[allow(dead_code)] // TODO: This should not be needed since it is under tests/
pub fn eval_string(script: &str, expected_string: &str) {
    todo!()
}
