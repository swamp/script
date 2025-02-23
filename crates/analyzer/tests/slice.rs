use crate::util::check;
mod util;
#[test_log::test]
fn slice_construct() {
    check(
        r"
        fn construct(v: Slice<Int>)
        {
            
        }
        ",
        r"
<13:1>[Int],VariableDefinition(Variable { name: <13:1>, resolved_type: [Int], mutable_node: Some(<9:3>), scope_index: 0, variable_index: 0 }, MutOrImmutableExpression { expression_or_location: Expression(<17:3>[Int],Literal(Array(ArrayType { item_type: Int }, [<18:1>Int,Literal(IntLiteral(1))]))), is_mutable: None })
<29:1>(),CompoundAssignment(SingleMutLocationExpression(SingleLocationExpression { kind: MutVariableRef, node: <29:1>, ty: [Int], starting_variable: Variable { name: <13:1>, resolved_type: [Int], mutable_node: Some(<9:3>), scope_index: 0, variable_index: 0 }, access_chain: [] }), Add, <34:1>Int,Literal(IntLiteral(3)))

",
    );
}
