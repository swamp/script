use crate::util::{check, check_fail};

mod util;

#[test_log::test]
fn check_value() {
    check(
        r"

fn takes_anything(value: ffi::Value) -> Int {
    2
}

takes_anything(2.03)
takes_anything(42)


         ",
        "
takes_anything: Internal((value: External(ffi::Value<0xFFFFFFFF>)) -> Int
<46:9>Int,Block([<52:1>Int,Literal(IntLiteral(2))]))
---
..Int,PostfixChain(<57:14>function (value: External(ffi::Value<0xFFFFFFFF>)) -> Int,InternalFunctionAccess((value: External(ffi::Value<0xFFFFFFFF>)) -> Int
<46:9>Int,Block([<52:1>Int,Literal(IntLiteral(2))])), [Postfix { node: <71:6>, ty: Int, kind: FunctionCall([Expression(<72:4>Float,Literal(FloatLiteral(fp:2.030 (133038))))]) }])
..Int,PostfixChain(<78:14>function (value: External(ffi::Value<0xFFFFFFFF>)) -> Int,InternalFunctionAccess((value: External(ffi::Value<0xFFFFFFFF>)) -> Int
<46:9>Int,Block([<52:1>Int,Literal(IntLiteral(2))])), [Postfix { node: <92:4>, ty: Int, kind: FunctionCall([Expression(<93:2>Int,Literal(IntLiteral(42)))]) }])
        ",
    );
}

#[test_log::test]
fn sparse_like() {
    check(
        r#"
external fn sparse_add(map: External<"Something">, value: ffi::Value)
external fn sparse_new() -> External<"Something">

struct SparseLike<T> {
    map: External<"Something">,
}

impl SparseLike<T> {
    fn add(mut self, a: ffi::Value) {
        sparse_add(self.map, a)
    }
}

struct Something {
    a: String,
}

x = SparseLike<Something> {
    map: sparse_new(),
}

         "#,
        r#"
sparse_add: External(external fn)
sparse_new: External(external fn)
SparseLike: RefCell { value: GenericType { type_parameters: SeqMap("T": TypeParameterName { resolved_node: <140:1>, assigned_name: "T" }), base_type: Struct(StructType { identifier: LocalTypeIdentifierWithOptionalTypeParams { name: <129:10>, parameter_names: [<140:1>] }, fields: [FieldType { field_name: FieldName(<149:3>), field_type: External(<164:9>) }] }), ast_functions: SeqMap("add": Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <208:3>, params: [Parameter { variable: <222:1>, param_type: Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<230:5>), module_path: Some(ModulePath([<225:3>])), generic_params: [] }) }], self_parameter: Some(SelfParameter { is_mutable: Some(<212:3>), self_node: <216:4> }), return_type: None }, body: <237:39>Block([<247:23>PostfixChain(PostfixChain { base: <247:10>IdentifierReference(<247:10>), postfixes: [FunctionCall(<257:13>, [MutableOrImmutableExpression { is_mutable: None, expression: <258:8>PostfixChain(PostfixChain { base: <258:4>IdentifierReference(<258:4>), postfixes: [FieldAccess(<263:3>)] }) }, MutableOrImmutableExpression { is_mutable: None, expression: <268:1>IdentifierReference(<268:1>) }])] })]) })), file_id: 65535 } }
Something: Something { a: String, }
---
some_path::main::SparseLike<Something> ==> SparseLike<Something> { map: ExternalType<Something>, }
---
(),VariableDefinition(Variable { name: <317:1>, resolved_type: SparseLike<Something> { map: ExternalType<Something>, }, mutable_node: None, scope_index: 0, variable_index: 0 }, MutOrImmutableExpression { expression_or_location: Expression(<321:10>SparseLike<Something>,StructInstantiation(StructInstantiation { source_order_expressions: [(0, <354:10>ExternalType<Something>,PostfixChain(<354:10>function () -> ExternalType<Something>,ExternalFunctionAccess(external fn), [Postfix { node: <364:2>, ty: External(Something<0x0000>), kind: FunctionCall([]) }]))], struct_type_ref: struct "SparseLike<Something>" })), is_mutable: None })

        "#,
    );
}
