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
Variable(a, [Float]) [[FloatLiteral(23.0, ResolvedFloatType), FloatLiteral(42.9, ResolvedFloatType)]]
Variable(b, Float) [Float]
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
Hello {x: Int}
enum_variants:
SomeEnum::Simple
SomeEnum::WithTupleResolvedTupleType([Int(ResolvedIntType), String(StringType), Struct(ResolvedStructType { number: 0, module_path: ModulePath([LocalIdentifier { node: Node { span: Span { start: Position { offset: 0, line: 0, column: 0 }, end: Position { offset: 0, line: 0, column: 0 } } }, text: "test" }]), fields: SeqMap(LocalIdentifier { node: Node { span: Span { start: Position { offset: 9, line: 2, column: 9 }, end: Position { offset: 40, line: 3, column: 17 } } }, text: "x" }: Int(ResolvedIntType)), name: LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 9, line: 2, column: 9 }, end: Position { offset: 40, line: 3, column: 17 } } }, text: "Hello" }, ast_struct: StructType { identifier: LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 9, line: 2, column: 9 }, end: Position { offset: 40, line: 3, column: 17 } } }, text: "Hello" }, fields: SeqMap(LocalIdentifier { node: Node { span: Span { start: Position { offset: 9, line: 2, column: 9 }, end: Position { offset: 40, line: 3, column: 17 } } }, text: "x" }: Int) }, impl_members: SeqMap() })])

        "#,
    )
}
