use crate::util::{check, check_fail};

mod util;

#[test_log::test]
fn init_var() {
    check(
        "a = 3",
        r"
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <0:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(3, <4:1>, ResolvedIntType)) }))
",
    );
}

#[test_log::test]
fn fail_compound_unknown_variable() {
    check_fail(
        "a += 3",
        r"
UnknownVariable(<0:1>)",
    );
}

#[test_log::test]
fn fail_overwrite_variable() {
    check_fail(
        r"
        a = 3
        a = 4
        ",
        r"
CanOnlyOverwriteVariableWithMut(<23:1>)",
    );
}

#[test_log::test]
fn fail_reassign_different_types() {
    check_fail(
        r"
        mut a = 3
        a = 'test'
        ",
        r"
OverwriteVariableWithAnotherType(<27:1>)",
    );
}

#[test_log::test]
fn array_push() {
    check(
        r"
        mut a = [1]
        a += 3
        ",
        r"
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <13:1>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: Some(<9:3>), scope_index: 0, variable_index: 0 }], expression: Literal(Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }, [Literal(IntLiteral(1, <18:1>, ResolvedIntType))])) }))
Expression(ArrayPush(ResolvedVariable { name: <13:1>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: Some(<9:3>), scope_index: 0, variable_index: 0 }, Literal(IntLiteral(3, <34:1>, ResolvedIntType))))

",
    );
}

#[test_log::test]
fn array_extend() {
    check(
        r"
        mut a = [1]
        a += [3, 4, 5]
        ",
        r"

Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <13:1>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: Some(<9:3>), scope_index: 0, variable_index: 0 }], expression: Literal(Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }, [Literal(IntLiteral(1, <18:1>, ResolvedIntType))])) }))
Expression(ArrayExtend(ResolvedVariable { name: <13:1>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: Some(<9:3>), scope_index: 0, variable_index: 0 }, Literal(Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }, [Literal(IntLiteral(3, <35:1>, ResolvedIntType)), Literal(IntLiteral(4, <38:1>, ResolvedIntType)), Literal(IntLiteral(5, <41:1>, ResolvedIntType))]))))

",
    );
}

#[test_log::test]
fn struct_def() {
    check(
        r"
        struct Something {
            a: Int,
            b: Int,
        }

        Something { b: 3, a: 4 }
        ",
        r#"

StructType(RefCell { value: ResolvedStructType { name: ResolvedLocalTypeIdentifier(<16:9>), anon_struct_type: ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<40:1>), field_type: Int(ResolvedIntType), index: 0 }, "b": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<60:1>), field_type: Int(ResolvedIntType), index: 0 }) }, number: 1, functions: SeqMap() } })
---
Expression(StructInstantiation(ResolvedStructInstantiation { source_order_expressions: [(1, Literal(IntLiteral(3, <102:1>, ResolvedIntType))), (0, Literal(IntLiteral(4, <108:1>, ResolvedIntType)))], struct_type_ref: RefCell { value: ResolvedStructType { name: ResolvedLocalTypeIdentifier(<16:9>), anon_struct_type: ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<40:1>), field_type: Int(ResolvedIntType), index: 0 }, "b": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<60:1>), field_type: Int(ResolvedIntType), index: 0 }) }, number: 1, functions: SeqMap() } }, display_type_ref: Struct(RefCell { value: ResolvedStructType { name: ResolvedLocalTypeIdentifier(<16:9>), anon_struct_type: ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<40:1>), field_type: Int(ResolvedIntType), index: 0 }, "b": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<60:1>), field_type: Int(ResolvedIntType), index: 0 }) }, number: 1, functions: SeqMap() } }) }))


"#,
    );
}

#[test_log::test]
fn add_fn() {
    check(
        r"
fn add(a: Int, b: Int) -> Int {
    a+b
}
add(2, 3)
        ",
        r"
FunctionDef(Internal(InternalFuncDef))
---
Expression(FunctionInternalCall(InFuncCall(InternalFunctionAccess(InternalFuncDef) [Literal(IntLiteral(2, <47:1>, ResolvedIntType)), Literal(IntLiteral(3, <50:1>, ResolvedIntType))])))


",
    );
}

#[test_log::test]
fn add_enum() {
    check(
        r"

enum Test {
    Simple,
    Tuple(Int, Int)
}

a = Test::Simple

        ",
        r"

EnumType(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 }, [ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<18:6>), number: 0 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Tuple(ResolvedEnumVariantTupleType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<30:5>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 } }, fields_in_order: [Int(ResolvedIntType), Int(ResolvedIntType)] }), name: ResolvedLocalTypeIdentifier(<30:5>), number: 2 }])
---
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <49:1>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(EnumVariantLiteral(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<18:6>), number: 0 }, Nothing)) }))

",
    );
}

#[test_log::test]
fn add_enum_struct() {
    check(
        r"

enum Test {
    Struct { a: Int, b: Float },
    Simple,
        Tuple(Int, Int)

}

a = Test::Struct { a: 10, b: 2.3 }

        ",
        r#"

EnumType(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 }, [ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Struct(ResolvedEnumVariantStructType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<18:6>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 } }, anon_struct: ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<27:1>), field_type: Int(ResolvedIntType), index: 0 }, "b": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<35:1>), field_type: Float(ResolvedFloatType), index: 1 }) } }), name: ResolvedLocalTypeIdentifier(<18:6>), number: 2 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<51:6>), number: 0 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Tuple(ResolvedEnumVariantTupleType { common: CommonEnumVariantType { number: 3, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<67:5>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 } }, fields_in_order: [Int(ResolvedIntType), Int(ResolvedIntType)] }), name: ResolvedLocalTypeIdentifier(<67:5>), number: 3 }])
---
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <87:1>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(EnumVariantLiteral(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Struct(ResolvedEnumVariantStructType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<18:6>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), module_path: ResolvedModulePath([]), number: 1 } }, anon_struct: ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<27:1>), field_type: Int(ResolvedIntType), index: 0 }, "b": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<35:1>), field_type: Float(ResolvedFloatType), index: 1 }) } }), name: ResolvedLocalTypeIdentifier(<18:6>), number: 2 }, Struct([Literal(IntLiteral(10, <109:2>, ResolvedIntType)), Literal(FloatLiteral(fp:2.300 (150732), <116:3>, ResolvedFloatType))]))) }))

"#,
    );
}

#[test_log::test]
fn fail_enum_struct() {
    check_fail(
        r"

enum Test {
    Struct { a: Int, b: Float },
    Simple,
        Tuple(Int, Int)

}

a = Test::Struct { a: 10, b: 2 }

        ",
        r#"

EnumType(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 }, [ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Struct(ResolvedEnumVariantStructType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<26:6>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 } }, anon_struct: ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<35:1>), field_type: Int(ResolvedIntType), index: 0 }, "b": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<43:1>), field_type: Float(ResolvedFloatType), index: 1 }) } }), name: ResolvedLocalTypeIdentifier(<26:6>), number: 2 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<59:6>), number: 0 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Tuple(ResolvedEnumVariantTupleType { common: CommonEnumVariantType { number: 3, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<75:5>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 } }, fields_in_order: [Int(ResolvedIntType), Int(ResolvedIntType)] }), name: ResolvedLocalTypeIdentifier(<75:5>), number: 3 }])
---
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <99:1>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(EnumVariantLiteral(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Struct(ResolvedEnumVariantStructType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<26:6>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 } }, anon_struct: ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<35:1>), field_type: Int(ResolvedIntType), index: 0 }, "b": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<43:1>), field_type: Float(ResolvedFloatType), index: 1 }) } }), name: ResolvedLocalTypeIdentifier(<26:6>), number: 2 }, Struct([Literal(IntLiteral(10, <121:2>, ResolvedIntType)), Literal(FloatLiteral(fp:2.300 (150732), <128:3>, ResolvedFloatType))]))) }))

"#,
    );
}

#[test_log::test]
fn integer() {
    check(
        r"
a = 3
        ",
        r"

Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(3, <5:1>, ResolvedIntType)) }))

",
    );
}

#[test_log::test]
fn float() {
    check(
        r"
a = -44.4
        ",
        r"

Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:1>, resolved_type: Float(ResolvedFloatType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: UnaryOp(ResolvedUnaryOperator { left: Literal(FloatLiteral(fp:44.400 (2909798), <6:4>, ResolvedFloatType)), kind: Negate, resolved_type: Float(ResolvedFloatType), node: <5:1> }) }))

",
    );
}

#[test_log::test]
fn string() {
    check(
        r"
b = 'world'
a = 'hello {b}'
        ",
        r#"

Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:1>, resolved_type: String(ResolvedStringType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: InterpolatedString(ResolvedStringType, [Literal(<6:5>, "world")]) }))
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <13:1>, resolved_type: String(ResolvedStringType), mutable_node: None, scope_index: 0, variable_index: 1 }], expression: InterpolatedString(ResolvedStringType, [Literal(<18:6>, "hello "), Interpolation(VariableAccess(ResolvedVariable { name: <1:1>, resolved_type: String(ResolvedStringType), mutable_node: None, scope_index: 0, variable_index: 0 }), None)]) }))

"#,
    );
}

#[test_log::test]
fn map() {
    check(
        r#"
b = [2: "something", 3: "other"]
b[3]
        "#,
        r#"

Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:1>, resolved_type: Map(ResolvedMapType { key_type: Int(ResolvedIntType), value_type: String(ResolvedStringType) }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(Map(ResolvedMapType { key_type: Int(ResolvedIntType), value_type: String(ResolvedStringType) }, [(Literal(IntLiteral(2, <6:1>, ResolvedIntType)), Literal(StringLiteral("something", <9:11>, ResolvedStringType))), (Literal(IntLiteral(3, <22:1>, ResolvedIntType)), Literal(StringLiteral("other", <25:7>, ResolvedStringType)))])) }))
Expression(MapIndexAccess(ResolvedMapIndexLookup { map_type: Map(ResolvedMapType { key_type: Int(ResolvedIntType), value_type: String(ResolvedStringType) }), item_type: Optional(String(ResolvedStringType)), map_type_ref: ResolvedMapType { key_type: Int(ResolvedIntType), value_type: String(ResolvedStringType) }, index_expression: Literal(IntLiteral(3, <36:1>, ResolvedIntType)), map_expression: VariableAccess(ResolvedVariable { name: <1:1>, resolved_type: Map(ResolvedMapType { key_type: Int(ResolvedIntType), value_type: String(ResolvedStringType) }), mutable_node: None, scope_index: 0, variable_index: 0 }) }))

"#,
    );
}

#[test_log::test]
fn map_set() {
    check(
        r#"
mut b = [2: "something", 3: "other"]
b[3] = "hello"
        "#,
        r#"

Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <5:1>, resolved_type: Map(ResolvedMapType { key_type: Int(ResolvedIntType), value_type: String(ResolvedStringType) }), mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }], expression: Literal(Map(ResolvedMapType { key_type: Int(ResolvedIntType), value_type: String(ResolvedStringType) }, [(Literal(IntLiteral(2, <10:1>, ResolvedIntType)), Literal(StringLiteral("something", <13:11>, ResolvedStringType))), (Literal(IntLiteral(3, <26:1>, ResolvedIntType)), Literal(StringLiteral("other", <29:7>, ResolvedStringType)))])) }))
Expression(MapAssignment(ResolvedMutMap { expression: VariableAccess(ResolvedVariable { name: <5:1>, resolved_type: Map(ResolvedMapType { key_type: Int(ResolvedIntType), value_type: String(ResolvedStringType) }), mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }), map_type_ref: ResolvedMapType { key_type: Int(ResolvedIntType), value_type: String(ResolvedStringType) } }, ResolvedIndexType { expression: Literal(IntLiteral(3, <40:1>, ResolvedIntType)), resolved_type: Int(ResolvedIntType) }, Literal(StringLiteral("hello", <45:7>, ResolvedStringType))))

"#,
    );
}

#[test_log::test]
fn tuple() {
    check(
        r#"
mut b = (2.4, "hello", 4)
        "#,
        r#"

Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <5:1>, resolved_type: Tuple(ResolvedTupleType([Float(ResolvedFloatType), String(ResolvedStringType), Int(ResolvedIntType)])), mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }], expression: Literal(TupleLiteral(ResolvedTupleType([Float(ResolvedFloatType), String(ResolvedStringType), Int(ResolvedIntType)]), [Literal(FloatLiteral(fp:2.400 (157286), <10:3>, ResolvedFloatType)), Literal(StringLiteral("hello", <15:7>, ResolvedStringType)), Literal(IntLiteral(4, <24:1>, ResolvedIntType))])) }))

"#,
    );
}

#[test_log::test]
fn fail_if_condition() {
    check_fail(
        r"
if 2 {
 5
} else {
 6
}
        ",
        r"
ExpectedBooleanExpression
",
    );
}

#[test_log::test]
fn if_condition() {
    check(
        r"
if true {
 5
} else {
 6
}
        ",
        r"
If(ResolvedBooleanExpression { expression: Literal(BoolLiteral(true, <4:4>, ResolvedBoolType)) }, [Expression(Literal(IntLiteral(5, <12:1>, ResolvedIntType)))], Some([Expression(Literal(IntLiteral(6, <24:1>, ResolvedIntType)))]))

",
    );
}

#[test_log::test]
fn for_loop() {
    check(
        r"
mut c = 0
for a in 0..3 {
    c += a
}
        ",
        r"
        
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <5:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(0, <9:1>, ResolvedIntType)) }))
ForLoop(Single(ResolvedVariable { name: <15:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }), ResolvedIterator { key_type: None, value_type: Int(ResolvedIntType), resolved_expression: ExclusiveRange(ResolvedExclusiveRangeType, Literal(IntLiteral(0, <20:1>, ResolvedIntType)), Literal(IntLiteral(3, <23:1>, ResolvedIntType))), mutable_node: None }, [Expression(VariableCompoundAssignment(ResolvedVariableCompoundAssignment { variable_ref: ResolvedVariable { name: <5:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }, expression: VariableAccess(ResolvedVariable { name: <15:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }), compound_operator: ResolvedCompoundOperator { node: <33:2>, kind: Add } }))])

",
    );
}

#[test_log::test]
fn for_loop_arr() {
    check(
        r"
arr = [40, 50, 60]
mut c = 0
for a in arr {
    c += a
}
        ",
        r"
        
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:3>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }, [Literal(IntLiteral(40, <8:2>, ResolvedIntType)), Literal(IntLiteral(50, <12:2>, ResolvedIntType)), Literal(IntLiteral(60, <16:2>, ResolvedIntType))])) }))
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <24:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<20:3>), scope_index: 0, variable_index: 1 }], expression: Literal(IntLiteral(0, <28:1>, ResolvedIntType)) }))
ForLoop(Single(ResolvedVariable { name: <34:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }), ResolvedIterator { key_type: Some(Int(ResolvedIntType)), value_type: Int(ResolvedIntType), resolved_expression: VariableAccess(ResolvedVariable { name: <1:3>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: None, scope_index: 0, variable_index: 0 }), mutable_node: None }, [Expression(VariableCompoundAssignment(ResolvedVariableCompoundAssignment { variable_ref: ResolvedVariable { name: <24:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<20:3>), scope_index: 0, variable_index: 1 }, expression: VariableAccess(ResolvedVariable { name: <34:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }), compound_operator: ResolvedCompoundOperator { node: <51:2>, kind: Add } }))])

",
    );
}

#[test_log::test]
fn for_loop_arr_index() {
    check(
        r"
arr = [40, 50, 60]
mut c = 0
for index, a in arr {
    c += a
}
        ",
        r"
        
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:3>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }, [Literal(IntLiteral(40, <8:2>, ResolvedIntType)), Literal(IntLiteral(50, <12:2>, ResolvedIntType)), Literal(IntLiteral(60, <16:2>, ResolvedIntType))])) }))
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <24:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<20:3>), scope_index: 0, variable_index: 1 }], expression: Literal(IntLiteral(0, <28:1>, ResolvedIntType)) }))
ForLoop(Pair(ResolvedVariable { name: <34:5>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }, ResolvedVariable { name: <41:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 1 }), ResolvedIterator { key_type: Some(Int(ResolvedIntType)), value_type: Int(ResolvedIntType), resolved_expression: VariableAccess(ResolvedVariable { name: <1:3>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: None, scope_index: 0, variable_index: 0 }), mutable_node: None }, [Expression(VariableCompoundAssignment(ResolvedVariableCompoundAssignment { variable_ref: ResolvedVariable { name: <24:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<20:3>), scope_index: 0, variable_index: 1 }, expression: VariableAccess(ResolvedVariable { name: <41:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 1 }), compound_operator: ResolvedCompoundOperator { node: <58:2>, kind: Add } }))])

",
    );
}

#[test_log::test]
fn for_loop_mut_arr_index() {
    check(
        r"
arr = [40, 50, 60]
mut c = 0
for index, a in mut arr {
    c += a
}
        ",
        r"
        
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:3>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }, [Literal(IntLiteral(40, <8:2>, ResolvedIntType)), Literal(IntLiteral(50, <12:2>, ResolvedIntType)), Literal(IntLiteral(60, <16:2>, ResolvedIntType))])) }))
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <24:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<20:3>), scope_index: 0, variable_index: 1 }], expression: Literal(IntLiteral(0, <28:1>, ResolvedIntType)) }))
ForLoop(Pair(ResolvedVariable { name: <34:5>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }, ResolvedVariable { name: <41:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 1 }), ResolvedIterator { key_type: Some(Int(ResolvedIntType)), value_type: Int(ResolvedIntType), resolved_expression: VariableAccess(ResolvedVariable { name: <1:3>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: None, scope_index: 0, variable_index: 0 }), mutable_node: Some(<46:3>) }, [Expression(VariableCompoundAssignment(ResolvedVariableCompoundAssignment { variable_ref: ResolvedVariable { name: <24:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<20:3>), scope_index: 0, variable_index: 1 }, expression: VariableAccess(ResolvedVariable { name: <41:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 1 }), compound_operator: ResolvedCompoundOperator { node: <62:2>, kind: Add } }))])

",
    );
}

#[test_log::test]
fn for_loop_break() {
    check(
        r"
mut c = 0
for a in 0..3 {
    c += a
    break
}
        ",
        r"
        
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <5:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(0, <9:1>, ResolvedIntType)) }))
ForLoop(Single(ResolvedVariable { name: <15:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }), ResolvedIterator { key_type: None, value_type: Int(ResolvedIntType), resolved_expression: ExclusiveRange(ResolvedExclusiveRangeType, Literal(IntLiteral(0, <20:1>, ResolvedIntType)), Literal(IntLiteral(3, <23:1>, ResolvedIntType))), mutable_node: None }, [Expression(VariableCompoundAssignment(ResolvedVariableCompoundAssignment { variable_ref: ResolvedVariable { name: <5:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }, expression: VariableAccess(ResolvedVariable { name: <15:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }), compound_operator: ResolvedCompoundOperator { node: <33:2>, kind: Add } })), Break(<42:5>)])

",
    );
}

#[test_log::test]
fn match_enum() {
    check(
        r"
enum GameState {
    Playing,
    Paused,
    GameOver,
}

game_state = GameState::Paused
        
match game_state {
    Playing => 1,
    Paused => 2,
    GameOver => 3,
    _ => -1
}
        ",
        r"
        
EnumType(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, [ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<22:7>), number: 0 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<35:6>), number: 0 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<47:8>), number: 0 }])
---
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <60:10>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(EnumVariantLiteral(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<35:6>), number: 0 }, Nothing)) }))
Expression(Match(ResolvedMatch { arms: [ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<22:7>), number: 0 }, None), expression: Literal(IntLiteral(1, <134:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<35:6>), number: 0 }, None), expression: Literal(IntLiteral(2, <151:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<47:8>), number: 0 }, None), expression: Literal(IntLiteral(3, <170:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: PatternList([Wildcard(<177:1>)]), expression: UnaryOp(ResolvedUnaryOperator { left: Literal(IntLiteral(1, <183:1>, ResolvedIntType)), kind: Negate, resolved_type: Int(ResolvedIntType), node: <182:1> }), expression_type: Int(ResolvedIntType) }], expression: VariableAccess(ResolvedVariable { name: <60:10>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }) }))

",
    );
}

#[test_log::test]
fn match_enum_2() {
    check(
        r"
enum GameState {
    Playing,
    Paused { time: Int },
    GameOver,
}

game_state = GameState::Paused
        
match game_state {
    Playing => 1,
    Paused time => 2,
    GameOver => 3,
    _ => -1
}
        ",
        r#"
        
EnumType(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, [ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<22:7>), number: 0 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Struct(ResolvedEnumVariantStructType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<35:6>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 } }, anon_struct: ResolvedAnonymousStructType { defined_fields: SeqMap("time": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<44:4>), field_type: Int(ResolvedIntType), index: 0 }) } }), name: ResolvedLocalTypeIdentifier(<35:6>), number: 2 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<61:8>), number: 0 }])
---
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <74:10>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(EnumVariantLiteral(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Struct(ResolvedEnumVariantStructType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<35:6>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 } }, anon_struct: ResolvedAnonymousStructType { defined_fields: SeqMap("time": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<44:4>), field_type: Int(ResolvedIntType), index: 0 }) } }), name: ResolvedLocalTypeIdentifier(<35:6>), number: 2 }, Nothing)) }))
Expression(Match(ResolvedMatch { arms: [ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<22:7>), number: 0 }, None), expression: Literal(IntLiteral(1, <148:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Struct(ResolvedEnumVariantStructType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<35:6>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 } }, anon_struct: ResolvedAnonymousStructType { defined_fields: SeqMap("time": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<44:4>), field_type: Int(ResolvedIntType), index: 0 }) } }), name: ResolvedLocalTypeIdentifier(<35:6>), number: 2 }, Some([VariableWithFieldIndex(ResolvedVariable { name: <162:4>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }, 0)])), expression: Literal(IntLiteral(2, <170:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<61:8>), number: 0 }, None), expression: Literal(IntLiteral(3, <189:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: PatternList([Wildcard(<196:1>)]), expression: UnaryOp(ResolvedUnaryOperator { left: Literal(IntLiteral(1, <202:1>, ResolvedIntType)), kind: Negate, resolved_type: Int(ResolvedIntType), node: <201:1> }), expression_type: Int(ResolvedIntType) }], expression: VariableAccess(ResolvedVariable { name: <74:10>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }) }))

"#,
    );
}

#[test_log::test]
fn match_enum_3() {
    check(
        r"
enum GameState {
    Playing,
    Paused (Float, Float),
    GameOver,
}

game_state = GameState::Paused
        
match game_state {
    Playing => 1,
    Paused time, _ => 2,
    GameOver => 3,
    _ => -1
}
        ",
        r"
        
EnumType(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, [ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<22:7>), number: 0 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Tuple(ResolvedEnumVariantTupleType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<35:6>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 } }, fields_in_order: [Float(ResolvedFloatType), Float(ResolvedFloatType)] }), name: ResolvedLocalTypeIdentifier(<35:6>), number: 2 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<62:8>), number: 0 }])
---
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <75:10>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(EnumVariantLiteral(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Tuple(ResolvedEnumVariantTupleType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<35:6>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 } }, fields_in_order: [Float(ResolvedFloatType), Float(ResolvedFloatType)] }), name: ResolvedLocalTypeIdentifier(<35:6>), number: 2 }, Nothing)) }))
Expression(Match(ResolvedMatch { arms: [ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<22:7>), number: 0 }, None), expression: Literal(IntLiteral(1, <149:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Tuple(ResolvedEnumVariantTupleType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<35:6>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 } }, fields_in_order: [Float(ResolvedFloatType), Float(ResolvedFloatType)] }), name: ResolvedLocalTypeIdentifier(<35:6>), number: 2 }, Some([Variable(ResolvedVariable { name: <163:4>, resolved_type: Float(ResolvedFloatType), mutable_node: None, scope_index: 1, variable_index: 0 }), Wildcard(<169:1>)])), expression: Literal(IntLiteral(2, <174:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<62:8>), number: 0 }, None), expression: Literal(IntLiteral(3, <193:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: PatternList([Wildcard(<200:1>)]), expression: UnaryOp(ResolvedUnaryOperator { left: Literal(IntLiteral(1, <206:1>, ResolvedIntType)), kind: Negate, resolved_type: Int(ResolvedIntType), node: <205:1> }), expression_type: Int(ResolvedIntType) }], expression: VariableAccess(ResolvedVariable { name: <75:10>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), module_path: ResolvedModulePath([]), number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }) }))

",
    );
}

#[test_log::test]
fn match_value() {
    check(
        r"
score = 2
        
match score {
    2 => 'low',
    _ => 'high',
}
        ",
        r#"
        
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:5>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(2, <9:1>, ResolvedIntType)) }))
Expression(Match(ResolvedMatch { arms: [ResolvedMatchArm { pattern: Literal(IntLiteral(2, <38:1>, ResolvedIntType)), expression: InterpolatedString(ResolvedStringType, [Literal(<44:3>, "low")]), expression_type: String(ResolvedStringType) }, ResolvedMatchArm { pattern: PatternList([Wildcard(<54:1>)]), expression: InterpolatedString(ResolvedStringType, [Literal(<60:4>, "high")]), expression_type: String(ResolvedStringType) }], expression: VariableAccess(ResolvedVariable { name: <1:5>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }) }))

"#,
    );
}

/*
#[test_log::test]
fn match_value_struct() {
    check(
        r"
struct Cool {
    score: Int,
    other: Int,
}

let cool = Cool { score: 99, other: 99 }

match cool {
    other: 100, score => score,
    _ => -11,
}
        ",
        r#"

Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:5>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(2, <9:1>, ResolvedIntType)) }))
Expression(Match(ResolvedMatch { arms: [ResolvedMatchArm { pattern: Literal(IntLiteral(2, <38:1>, ResolvedIntType)), expression: InterpolatedString(ResolvedStringType, [Literal(<44:3>, "low")]), expression_type: String(ResolvedStringType) }, ResolvedMatchArm { pattern: PatternList([Wildcard(<54:1>)]), expression: InterpolatedString(ResolvedStringType, [Literal(<60:4>, "high")]), expression_type: String(ResolvedStringType) }], expression: VariableAccess(ResolvedVariable { name: <1:5>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }) }))

"#,
    );
}
 */

#[test_log::test]
fn binary_op() {
    check(
        r"
a = 44
b = a > 43 && false
        ",
        r"
        
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(44, <5:2>, ResolvedIntType)) }))
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <8:1>, resolved_type: Bool(ResolvedBoolType), mutable_node: None, scope_index: 0, variable_index: 1 }], expression: BinaryOp(ResolvedBinaryOperator { left: BinaryOp(ResolvedBinaryOperator { left: VariableAccess(ResolvedVariable { name: <1:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }), right: Literal(IntLiteral(43, <16:2>, ResolvedIntType)), kind: GreaterThan, node: <14:1>, resolved_type: Bool(ResolvedBoolType) }), right: Literal(BoolLiteral(false, <22:5>, ResolvedBoolType)), kind: LogicalAnd, node: <19:2>, resolved_type: Bool(ResolvedBoolType) }) }))

",
    );
}

#[test_log::test]
fn unary_op() {
    check(
        r"
a = 44
b = !(a > 43)
        ",
        r"
        
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(44, <5:2>, ResolvedIntType)) }))
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <8:1>, resolved_type: Bool(ResolvedBoolType), mutable_node: None, scope_index: 0, variable_index: 1 }], expression: UnaryOp(ResolvedUnaryOperator { left: BinaryOp(ResolvedBinaryOperator { left: VariableAccess(ResolvedVariable { name: <1:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }), right: Literal(IntLiteral(43, <18:2>, ResolvedIntType)), kind: GreaterThan, node: <16:1>, resolved_type: Bool(ResolvedBoolType) }), kind: Not, resolved_type: Bool(ResolvedBoolType), node: <12:1> }) }))

",
    );
}

#[test_log::test]
fn fn_mut_param() {
    check(
        r"
fn changing(mut x: Int) {
    x = 909
}

mut a = 43
changing(mut a)

        ",
        r"
        
FunctionDef(Internal(InternalFuncDef))
---
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <46:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<42:3>), scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(43, <50:2>, ResolvedIntType)) }))
Expression(FunctionInternalCall(InFuncCall(InternalFunctionAccess(InternalFuncDef) [MutRef(ResolvedMutVariable { variable_ref: ResolvedVariable { name: <46:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<42:3>), scope_index: 0, variable_index: 0 } })])))

",
    );
}

#[test_log::test]
fn impl_pos() {
    check(
        r"
struct Position {
    x: Float,
    y: Float,
}

impl Position {
    fn new() -> Position {
        Position {
            x: 101.99,
            y: -102.18,
        }
    }
    
    fn move(mut self, pos: Position) {
        self.x += pos.x
        self.y += pos.y
    }
}

        ",
        r#"
        
StructType(RefCell { value: ResolvedStructType { name: ResolvedLocalTypeIdentifier(<8:8>), anon_struct_type: ResolvedAnonymousStructType { defined_fields: SeqMap("x": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<23:1>), field_type: Float(ResolvedFloatType), index: 0 }, "y": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<37:1>), field_type: Float(ResolvedFloatType), index: 0 }) }, number: 1, functions: SeqMap("new": Internal(InternalFuncDef), "move": Internal(InternalFuncDef)) } })
ImplType(Struct(RefCell { value: ResolvedStructType { name: ResolvedLocalTypeIdentifier(<8:8>), anon_struct_type: ResolvedAnonymousStructType { defined_fields: SeqMap("x": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<23:1>), field_type: Float(ResolvedFloatType), index: 0 }, "y": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<37:1>), field_type: Float(ResolvedFloatType), index: 0 }) }, number: 1, functions: SeqMap("new": Internal(InternalFuncDef), "move": Internal(InternalFuncDef)) } }))

"#,
    );
}

#[test_log::test]
fn check_some_bug() {
    check(
        r#"
        is_attacking = false
        c = if is_attacking { 3.5 } else { -13.3 }
    "#,
        r"
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <9:12>, resolved_type: Bool(ResolvedBoolType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(BoolLiteral(false, <24:5>, ResolvedBoolType)) }))
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <38:1>, resolved_type: Float(ResolvedFloatType), mutable_node: None, scope_index: 0, variable_index: 1 }], expression: IfElse(ResolvedBooleanExpression { expression: VariableAccess(ResolvedVariable { name: <9:12>, resolved_type: Bool(ResolvedBoolType), mutable_node: None, scope_index: 0, variable_index: 0 }) }, Literal(FloatLiteral(fp:3.500 (229376), <60:3>, ResolvedFloatType)), UnaryOp(ResolvedUnaryOperator { left: Literal(FloatLiteral(fp:13.300 (871628), <74:4>, ResolvedFloatType)), kind: Negate, resolved_type: Float(ResolvedFloatType), node: <73:1> })) }))
",
    );
}
