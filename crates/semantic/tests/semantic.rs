use crate::util::check;

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
