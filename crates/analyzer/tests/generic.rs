use crate::util::{check, check_fail};

mod util;

#[test_log::test]
fn basic_generic() {
    check(
        r"

struct Something<T> {
    a: Float,
    value: T,
}

a = Something<Int> {
    a: 2.3,
    value: 3,
}

         ",
        r#"
Something: RefCell { value: GenericType { type_parameters: SeqMap("T": TypeParameterName { resolved_node: <19:1>, assigned_name: "T" }), base_type: Struct(StructType { identifier: LocalTypeIdentifierWithOptionalTypeParams { name: <9:9>, parameter_names: [<19:1>] }, fields: [FieldType { field_name: FieldName(<28:1>), field_type: Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<31:5>), module_path: None, generic_params: [] }) }, FieldType { field_name: FieldName(<42:5>), field_type: Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<49:1>), module_path: None, generic_params: [] }) }] }), ast_functions: SeqMap(), file_id: 65535 } }
---
some_path::main::Something<Int> ==> Something<Int> { a: Float, value: Int, }
---
(),VariableDefinition(Variable { name: <55:1>, resolved_type: Something<Int> { a: Float, value: Int, }, mutable_node: None, scope_index: 0, variable_index: 0 }, MutOrImmutableExpression { expression_or_location: Expression(<59:9>Something<Int>,StructInstantiation(StructInstantiation { source_order_expressions: [(0, <83:3>Float,Literal(FloatLiteral(fp:2.300 (150732)))), (1, <99:1>Int,Literal(IntLiteral(3)))], struct_type_ref: struct "Something<Int>" })), is_mutable: None })

        "#,
    );
}

#[test_log::test]
fn fail_basic_generic() {
    check_fail(
        r"

struct Something<T> {
    a: Float,
    value: T,
}

a = Something<Int> {
    a: 2.3,
    value: 4.0,
}

         ",
        r"
Error { node: <99:3>, kind: IncompatibleTypes(Int, Float) }
        ",
    );
}
