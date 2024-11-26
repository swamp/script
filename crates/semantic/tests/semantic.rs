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
structs:
Hello {x: Int}
enum_variants:
SomeEnum::Simple
SomeEnum::WithTupleResolvedTupleType([Int(ResolvedIntType), String(StringType), Struct(ResolvedStructType { number: 1, module_path: ModulePath([LocalIdentifier("test")]), fields: SeqMap(LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 9, line: 2, column: 9 }, end: Position { offset: 40, line: 3, column: 17 } } }, text: "x" }: Int(ResolvedIntType)), name: LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 9, line: 2, column: 9 }, end: Position { offset: 40, line: 3, column: 17 } } }, text: "Hello" }, ast_struct: StructType { identifier: LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 9, line: 2, column: 9 }, end: Position { offset: 40, line: 3, column: 17 } } }, text: "Hello" }, fields: SeqMap(LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 9, line: 2, column: 9 }, end: Position { offset: 40, line: 3, column: 17 } } }, text: "x" }: Int) } })])

        "#,
    )
}
