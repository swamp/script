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
Hello {x: Inty: Int}
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
let a: [Float] = [[FloatLiteral(23.0, ResolvedFloatType), FloatLiteral(42.9, ResolvedFloatType)]]
let b: Float = [Float]
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
let b: Int = ((fn_def add(a: Int, b: Int) -> Int)(IntLit(2), IntLit(22)))
let mut c: String = StringLit(hello)
set mut c: String = StringLit(another)
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
let a: Int = < b: Int=((fn_def add(a: Int, b: Int) -> Int)(IntLit(2), IntLit(22))) >
let mut c: String = StringLit(hello)
set mut c: String = StringLit(another)
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
Hello { x: Int }
impl:
enum_variants:
SomeEnum::Simple
SomeEnum::WithTuple(Int, String, Hello { x: Int })
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
pos.scale(2.5)

    "#,
        r#"

modules:
::test
namespace:
structs:
Vector2 { x: Float, y: Float }
impl:
..sqr_len: impl(sqr_len() -> Float)
..scale: impl(scale(factor: Float) -> Vector2 { x: Float, y: Float })
statements:
let mut pos: Vector2 { x: Float, y: Float } = { x: FloatLit(10.0), y: FloatLit(20.0) }
(impl(scale(factor: Float) -> Vector2 { x: Float, y: Float }) <- FloatLit(2.5))

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
Shape::Circle(Float)
Shape::Rectangle { width: Float, height: Float }
Shape::Point

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
Shape::Circle(Float)
Shape::Rectangle { width: Float, height: Float }
Shape::Point
statements:
let shapes: [Shape] = [[EnumVariantLiteral(Shape::Circle, Tuple([FloatLiteral(5.0, ResolvedFloatType)])), EnumVariantLiteral(Shape::Rectangle, Struct([FloatLiteral(10.0, ResolvedFloatType), FloatLiteral(20.0, ResolvedFloatType)])), EnumVariantLiteral(Shape::Point, Nothing)]]


    "#,
    )
}

#[test_log::test]
fn math_literals() {
    check(
        r#"
status = match player.health {
    100 => "Full health",
    health => 'Critical: {health}'
}
"#,
        r#"



    "#,
    )
}
