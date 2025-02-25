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

instantiated = Something<Int> {
    a: 2.3,
    value: 3,
}

         ",
        "
Something: Something {
....a: Float, 
....value: T, 
..}<T>

---
let instantiated = Something<Int> {
..a: 2.30f, 
..value: 3i, 
}

        ",
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

#[test_log::test]
fn basic_generic_inception() {
    check(
        "

struct Else<SomeType> {
    another: SomeType,
    b: Float,
}

struct Something<T> {
    a: Float,
    value: Else<T>,
}

a = Something<Int> {
    a: 2.3,
    value: Else<Int> {
        another: 44,
        b: 23.0
    },
}

         ",
        r#"
Else: RefCell { value: GenericType { type_parameters: SeqMap("SomeType": TypeParameterName { resolved_node: <22:8>, assigned_name: "SomeType" }), base_type: Struct(StructType { identifier: LocalTypeIdentifierWithOptionalTypeParams { name: <17:4>, parameter_names: [<22:8>] }, fields: [FieldType { field_name: FieldName(<38:7>), field_type: Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<47:8>), module_path: None, generic_params: [] }) }, FieldType { field_name: FieldName(<61:1>), field_type: Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<64:5>), module_path: None, generic_params: [] }) }] }), ast_functions: SeqMap(), file_id: 65535 } }
Something: RefCell { value: GenericType { type_parameters: SeqMap("T": TypeParameterName { resolved_node: <91:1>, assigned_name: "T" }), base_type: Struct(StructType { identifier: LocalTypeIdentifierWithOptionalTypeParams { name: <81:9>, parameter_names: [<91:1>] }, fields: [FieldType { field_name: FieldName(<100:1>), field_type: Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<103:5>), module_path: None, generic_params: [] }) }, FieldType { field_name: FieldName(<114:5>), field_type: Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<121:4>), module_path: None, generic_params: [Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<126:1>), module_path: None, generic_params: [] })] }) }] }), ast_functions: SeqMap(), file_id: 65535 } }
---
some_path::main::Else<Int> ==> Else<Int> { another: Int, b: Float, }
some_path::main::Something<Int> ==> Something<Int> { a: Float, value: Else<Int>, }
---
(),VariableDefinition(Variable { name: <133:1>, resolved_type: Something<Int> { a: Float, value: Else<Int>, }, mutable_node: None, scope_index: 0, variable_index: 0 }, MutOrImmutableExpression { expression_or_location: Expression(<137:9>Something<Int>,StructInstantiation(StructInstantiation { source_order_expressions: [(0, <161:3>Float,Literal(FloatLiteral(fp:2.300 (150732)))), (1, <177:4>Else<Int>,StructInstantiation(StructInstantiation { source_order_expressions: [(0, <206:2>Int,Literal(IntLiteral(44))), (1, <221:4>Float,Literal(FloatLiteral(fp:23.000 (1507328))))], struct_type_ref: struct "Else<Int>" }))], struct_type_ref: struct "Something<Int>" })), is_mutable: None })

        "#,
    );
}
