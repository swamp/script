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
Expression(StructInstantiation(ResolvedStructInstantiation { source_order_expressions: [(1, Literal(IntLiteral(3, <110:1>, ResolvedIntType))), (0, Literal(IntLiteral(4, <116:1>, ResolvedIntType)))], struct_type_ref: RefCell { value: ResolvedStructType { name: ResolvedLocalTypeIdentifier(<16:9>), anon_struct_type: ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<40:1>), field_type: Int(ResolvedIntType), index: 0 }, "b": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<60:1>), field_type: Int(ResolvedIntType), index: 0 }) }, number: 1, functions: SeqMap() } }, display_type_ref: Struct(RefCell { value: ResolvedStructType { name: ResolvedLocalTypeIdentifier(<16:9>), anon_struct_type: ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<40:1>), field_type: Int(ResolvedIntType), index: 0 }, "b": ResolvedAnonymousStructFieldType { identifier: ResolvedFieldName(<60:1>), field_type: Int(ResolvedIntType), index: 0 }) }, number: 1, functions: SeqMap() } }) }))


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
        
EnumType(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 }, [ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<26:6>), number: 0 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Tuple(ResolvedEnumVariantTupleType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<38:5>), enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 } }, fields_in_order: [Int(ResolvedIntType), Int(ResolvedIntType)] }), name: ResolvedLocalTypeIdentifier(<38:5>), number: 2 }])
---
Expression(InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <57:1>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(EnumVariantLiteral(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<15:4>), module_path: ResolvedModulePath([]), number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<26:6>), number: 0 }, Nothing)) }))

",
    );
}
