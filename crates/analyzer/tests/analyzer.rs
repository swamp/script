/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::util::{check, check_fail};

mod util;
#[test_log::test]
fn basic() {
    check(
        r#"
        struct Hello {
            x: Int,
            y: Int,
        }
        "#,
        r#"
modules:
::test
namespace:
structs:
Hello <2:9-5:10> { x: Int, y: Int }
impl:
        "#,
    )
}

#[test_log::test]
fn unknown_variable() {
    check_fail(
        r#"
        a = 3
        b = c
        "#,
        r#"
UnknownVariable(c)
        "#,
    )
}

#[test_log::test]
fn wrong_array_index() {
    check_fail(
        r#"
        a = [23.0, 42.9]
        b = a[c]
        "#,
        r#"
            UnknownVariable(c)
        "#,
    )
}

#[test_log::test]
fn correct_array_index() {
    check(
        r#"
        a = [23.0, 42.9]
        b = a[1]
        "#,
        r#"
modules:
::test
namespace:
statements:
let a<0:0>: [Float] = Array([Literal(FloatLiteral(fp:23.000 (1507328), ResolvedFloatType)), Literal(FloatLiteral(fp:42.900 (2811494), ResolvedFloatType))])
let b<0:1>: Float = [Float]
        "#,
    )
}

#[test_log::test]
fn wrong_array_index_float() {
    check_fail(
        r#"
        a = [23.0, 42.9]
        b = a[3.14]
        "#,
        r#"
 ArrayIndexMustBeInt(Literal(Float(3.14)))
        "#,
    )
}

#[test_log::test]
fn wrong_call() {
    check_fail(
        r#"
        fn add(a: Int, b: Int) -> Int {
            a + b
        }
        b = add(2, 3.14)
        "#,
        r#"
IncompatibleArguments(Float(ResolvedFloatType), Int(ResolvedIntType))
        "#,
    )
}

#[test_log::test]
fn call() {
    check(
        r#"
        fn add(a: Int, b: Int) -> Int {
            a + b
        }
        b = add(2, 22)
        mut c = "hello"
        c = "another"
        "#,
        r#"
modules:
::test
namespace:
statements:
let b<0:0>: Int = ((fn_def add <2:9-4:10>(a: Int, b: Int) -> Int)(IntLit(2), IntLit(22)))
let mut c<0:1>: String = StringLit(hello)
set mut c<0:1>: String = StringLit(another)
        "#,
    )
}

#[test_log::test]
fn call_2() {
    check(
        r#"
        fn add(a: Int, b: Int) -> Int {
            a + b
        }
        a = b = add(2, 22)
        mut c = "hello"
        c = "another"
        "#,
        r#"
modules:
::test
namespace:
statements:
let a<0:1>: Int = < b<0:0>: Int=((fn_def add <2:9-4:10>(a: Int, b: Int) -> Int)(IntLit(2), IntLit(22))) >
let mut c<0:2>: String = StringLit(hello)
set mut c<0:2>: String = StringLit(another)
        "#,
    )
}

#[test_log::test]
fn wrong_call_arg_count() {
    check_fail(
        r#"
        fn add(a: Int, b: Int) -> Int {
            a + b
        }
        b = add(2, 34, 44)
        "#,
        r#"
WrongNumberOfArguments(3, 2)
        "#,
    )
}

#[test_log::test]
fn enum_basic() {
    check(
        r#"
        struct Hello {
        x: Int,}

        enum SomeEnum {
            Simple,
            WithTuple(Int, String, Hello),
        }
        "#,
        r#"
modules:
::test
namespace:
structs:
Hello <2:9-3:17> { x: Int }
impl:
enum_variants:
SomeEnum <5:14-5:22>::Simple <6:13-6:19>
SomeEnum <5:14-5:22>::WithTuple <7:13-7:42>(Int, String, Hello <2:9-3:17> { x: Int })
        "#,
    )
}

#[test_log::test]
fn impl_vector() {
    check(
        r#"
    // Struct definition
struct Vector2 {
    x: Float,
    y: Float,
}

// Implementation block
impl Vector2 {
    fn sqr_len(self) -> Float {
        self.x * self.x + self.y * self.y
    }

    fn scale(mut self, factor: Float) -> Vector2 {
        self.x = self.x * factor
        self.y = self.y * factor
        self
    }
}

mut pos = Vector2 { x: 10.0, y: 20.0 }
pos.scale(2.5).sqr_len()

    "#,
        r#"

modules:
::test
namespace:
structs:
Vector2 <3:1-6:2> { x: Float, y: Float }
impl:
..sqr_len: (sqr_len <10:5-12:6>(self: Vector2 <3:1-6:2> { x: Float, y: Float }) -> Float)
..scale: (scale <14:5-18:6>(self: Vector2 <3:1-6:2> { x: Float, y: Float }, factor: Float) -> Vector2 <3:1-6:2> { x: Float, y: Float })
statements:
let mut pos<0:0>: Vector2 <3:1-6:2> { x: Float, y: Float } = { x: FloatLit(fp:10.000 (655360)), y: FloatLit(fp:20.000 (1310720)) }
(< (< VarRead(mut pos<0:0>: Vector2 <3:1-6:2> { x: Float, y: Float }) >.(scale <14:5-18:6>(self: Vector2 <3:1-6:2> { x: Float, y: Float }, factor: Float) -> Vector2 <3:1-6:2> { x: Float, y: Float }) <- FloatLit(fp:2.500 (163840))) >.(sqr_len <10:5-12:6>(self: Vector2 <3:1-6:2> { x: Float, y: Float }) -> Float))


    "#,
    )
}

#[test_log::test]
fn enum_variants() {
    check(
        r#"
// Enum
enum Shape {
    Circle(Float),
    Rectangle { width: Float, height: Float },
    Point,
}

"#,
        r#"

modules:
::test
namespace:
enum_variants:
Shape <3:6-3:11>::Circle <4:5-4:18>(Float)
Shape <3:6-3:11>::Rectangle <5:5-5:46> { width: Float, height: Float }
Shape <3:6-3:11>::Point <6:5-6:10>

    "#,
    )
}

#[test_log::test]
fn enum_variants_array() {
    check(
        r#"
// Enum
enum Shape {
    Circle(Float),
    Rectangle { width: Float, height: Float },
    Point,
}

shapes = [
    Shape::Circle(5.0),
    Shape::Rectangle { width: 10.0, height: 20.0 },
    Shape::Point
]



"#,
        r#"

modules:
::test
namespace:
enum_variants:
Shape <3:6-3:11>::Circle <4:5-4:18>(Float)
Shape <3:6-3:11>::Rectangle <5:5-5:46> { width: Float, height: Float }
Shape <3:6-3:11>::Point <6:5-6:10>
statements:
let shapes<0:0>: [Shape <3:6-3:11>] = Array([Literal(EnumVariantLiteral(Shape <3:6-3:11>::Circle <4:5-4:18>, Tuple([Literal(FloatLiteral(5.0, ResolvedFloatType))]))), Literal(EnumVariantLiteral(Shape <3:6-3:11>::Rectangle <5:5-5:46>, Struct([Literal(FloatLiteral(10.0, ResolvedFloatType)), Literal(FloatLiteral(20.0, ResolvedFloatType))]))), Literal(EnumVariantLiteral(Shape <3:6-3:11>::Point <6:5-6:10>, Nothing))])


    "#,
    )
}

#[test_log::test]
fn math_literals() {
    check(
        r#"

struct Player {
    something_else: Float,
    health: Int,
}

player = Player { health: 23, something_else: -1919.99 }

status = match player.health {
    100 => "Full health",
    health => 'Critical: {health}'
}
"#,
        r#"

modules:
::test
namespace:
structs:
Player <3:1-6:2> { something_else: Float, health: Int }
impl:
statements:
let player<0:0>: Player <3:1-6:2> { something_else: Float, health: Int } = { something_else: Negate(FloatLit(1919.99)), health: IntLit(23) }
let status<0:1>: String = Match Player <3:1-6:2> { something_else: Float, health: Int }.health <10:16-10:30><1>
..pattern(IntLit(100)) => StringLit(Full health)
..pattern(health<1:0>: Int) => '"Critical: ", VarRead(health<1:0>: Int)'


    "#,
    )
}
