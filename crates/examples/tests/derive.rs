/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_derive::{swamp_fn, SwampExport};
use swamp_eval::{
    value::{SwampExport, Value},
    SwampFunction,
}; // macros

#[derive(SwampExport)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

#[swamp_fn]
fn x_dist(p1: Point, p2: Point) -> i32 {
    let dx = p2.x - p1.x;
    dx
}

#[test]
fn point_definition() {
    let definition = Point::generate_swamp_definition();

    assert_eq!(definition, "struct Point {\n    x: Int,\n    y: Int\n}\n");
}

#[test]
fn call_x_dist() {
    let func: &SwampFunction = &swamp_x_dist::FUNCTION;

    assert_eq!(func.name, "x_dist");

    let definition = swamp_x_dist::generate_swamp_definition();
    assert_eq!(
        definition,
        "external fn x_dist(p1: Point, p2: Point) -> Int\n"
    );

    let p1 = Point { x: 1, y: -2 };
    let p1_swamp = p1.to_swamp_value();

    let p2 = Point { x: 99, y: -2 };
    let p2_swamp = p2.to_swamp_value();

    let x_distance_swamp = (func.handler)(&[p1_swamp, p2_swamp]).expect("should eval");

    assert_eq!(x_distance_swamp, Value::Int(98));
}

#[test]
fn point_conversion() {
    let p = Point { x: 1, y: -2 };
    let value = p.to_swamp_value();
    let p2 = Point::from_swamp_value(&value).unwrap();
    assert_eq!(p2.x, 1);
    assert_eq!(p2.y, -2);
}
