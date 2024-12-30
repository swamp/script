/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::util::check;

mod util;

/*

//#[test_log::test]
fn pipe() {
    let script = r#"
        double <| abs(-10)
     "#;
    check(&script, r#""#);
}

#[test_log::test]
fn match_value_expressions() {
    let script = r#"
    match temperature {
        x > 15 => "Warm"
        x < -10 => "Cold"
        _ -> "Neither warm nor cold"
    }
        "#;

    check(&script, r#""#);
}
*/

#[test_log::test]
fn assignment() {
    check("a = 3", "VariableAssignment(<0:1>, Literal(Int(<4:1>)))");
}

#[test_log::test]
fn assignment_add() {
    check(
        "a = n + 6",
        "VariableAssignment(<0:1>, BinaryOp(VariableAccess(<4:1>), Add(<6:1>), Literal(Int(<8:1>))))",
    );
}

#[test_log::test]
fn function_call() {
    let script = "
            fn add(x: Int, y: Int) -> Int {
                x + y
            }
            result = add(10, 20)
        ";
    check(
        script,
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <16:3>, params: [Parameter { variable: <20:1>, param_type: Int(<23:3>) }, Parameter { variable: <28:1>, param_type: Int(<31:3>) }], self_parameter: None, return_type: Some(Int(<39:3>)) }, body: Block([BinaryOp(VariableAccess(<61:1>), Add(<63:1>), VariableAccess(<65:1>))]) }))
---
VariableAssignment(<93:6>, FunctionCall(VariableAccess(<102:3>), [Literal(Int(<106:2>)), Literal(Int(<110:2>))]))



    ",
    );
}

#[test_log::test]
fn struct_def() {
    let script = "
            struct Person { first_field: Int, second_field: String }
        ";
    check(
        script,
        "StructDef(StructType { identifier: LocalTypeIdentifier(<20:6>), fields: [FieldType { field_name: FieldName(<29:11>), field_type: Int(<42:3>) }, FieldType { field_name: FieldName(<47:12>), field_type: String(<61:6>) }] })",
    )
}

#[test_log::test]
fn struct_init() {
    let script = "
            person = Person { first_field: 1, second_field: \"Bob\" }
        ";
    check(
        script,
        r"
 VariableAssignment(<13:6>, StructInstantiation(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<22:6>), module_path: None }, [FieldExpression { field_name: FieldName(<31:11>), expression: Literal(Int(<44:1>)) }, FieldExpression { field_name: FieldName(<47:12>), expression: Literal(String(<61:5>)) }]))
",
    );
}

#[test_log::test]
fn while_loop() {
    let script = "
            a = 0
            while a < 3 {
                print(a)
                a = a + 1
            }
        ";
    check(
        script,
        r#"
VariableAssignment(<13:1>, Literal(Int(<17:1>)))
WhileLoop(BinaryOp(VariableAccess(<37:1>), LessThan(<39:1>), Literal(Int(<41:1>))), Block([FunctionCall(VariableAccess(<61:5>), [VariableAccess(<67:1>)]), VariableAssignment(<86:1>, BinaryOp(VariableAccess(<90:1>), Add(<92:1>), Literal(Int(<94:1>))))]))

        "#,
    );
}

#[test_log::test]
fn if_expression() {
    let script = "
            c = if true {
                a
            } else {
                b
            }
        ";

    check(
        script,
        "VariableAssignment(<13:1>, If(Literal(Bool(<20:4>)), Block([VariableAccess(<43:1>)]), Some(Block([VariableAccess(<82:1>)]))))",
    );
}

#[test_log::test]
fn struct_def_and_instantiation() {
    check(
        r#"
            struct Person { first_field: Int, second_field: String }
            person = Person { first_field: 1, second_field: "Bob" }
        "#,
        r#"
StructDef(StructType { identifier: LocalTypeIdentifier(<20:6>), fields: [FieldType { field_name: FieldName(<29:11>), field_type: Int(<42:3>) }, FieldType { field_name: FieldName(<47:12>), field_type: String(<61:6>) }] })
---
VariableAssignment(<82:6>, StructInstantiation(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<91:6>), module_path: None }, [FieldExpression { field_name: FieldName(<100:11>), expression: Literal(Int(<113:1>)) }, FieldExpression { field_name: FieldName(<116:12>), expression: Literal(String(<130:5>)) }]))

    "#,
    );
}

#[test_log::test]
fn nested_function_calls() {
    check(
        "result = add(mul(2, 3), div(10, 2))",
        "VariableAssignment(<0:6>, FunctionCall(VariableAccess(<9:3>), [FunctionCall(VariableAccess(<13:3>), [Literal(Int(<17:1>)), Literal(Int(<20:1>))]), FunctionCall(VariableAccess(<24:3>), [Literal(Int(<28:2>)), Literal(Int(<32:1>))])]))
",
    );
}

#[test_log::test]
fn complex_expressions() {
    let script = "
        result = (2 + 3) * 4 - (10 / 2)
        flag = true && !false || 5 > 3
    ";
    check(
        &script,
        r#"

VariableAssignment(<9:6>, BinaryOp(BinaryOp(BinaryOp(Literal(Int(<19:1>)), Add(<21:1>), Literal(Int(<23:1>))), Multiply(<26:1>), Literal(Int(<28:1>))), Subtract(<30:1>), BinaryOp(Literal(Int(<33:2>)), Divide(<36:1>), Literal(Int(<38:1>)))))
VariableAssignment(<49:4>, BinaryOp(BinaryOp(Literal(Bool(<56:4>)), LogicalAnd(<61:2>), UnaryOp(Not(<64:1>), Literal(Bool(<65:5>)))), LogicalOr(<71:2>), BinaryOp(Literal(Int(<74:1>)), GreaterThan(<76:1>), Literal(Int(<78:1>)))))

        "#,
    );
}

#[test_log::test]
fn not_operator() {
    let script = "
        !x
    ";
    check(
        &script,
        r#"
UnaryOp(Not(<9:1>), VariableAccess(<10:1>))
        "#,
    );
}

#[test_log::test]
fn struct_field_access_with_struct_init() {
    let script = "
        person = Person { first_field: 1, second_field: \"Bob\" }
        name = person.second_field
    ";
    check(
        script,
        r"

VariableAssignment(<9:6>, StructInstantiation(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<18:6>), module_path: None }, [FieldExpression { field_name: FieldName(<27:11>), expression: Literal(Int(<40:1>)) }, FieldExpression { field_name: FieldName(<43:12>), expression: Literal(String(<57:5>)) }]))
VariableAssignment(<73:4>, FieldAccess(VariableAccess(<80:6>), <87:12>))

    ",
    );
}

#[test_log::test]
fn struct_field_access() {
    let script = "
        name = person.second_field
    ";
    check(
        script,
        "VariableAssignment(<9:4>, FieldAccess(VariableAccess(<16:6>), <23:12>))",
    );
}

#[test_log::test]
fn struct_field_assignment_chain() {
    let script = "
        a.b.c.d = 3
    ";
    check(
        script,
        "FieldAssignment(FieldAccess(FieldAccess(VariableAccess(<9:1>), <11:1>), <13:1>), <15:1>, Literal(Int(<19:1>)))",
    );
}

#[test_log::test]
fn struct_field_assignment_chain2() {
    let script = "
        a = [10, 20]
        a[1]
    ";
    check(
        script,
        r#"
VariableAssignment(<9:1>, Literal(Array([Literal(Int(<14:2>)), Literal(Int(<18:2>))])))
IndexAccess(VariableAccess(<30:1>), Literal(Int(<32:1>)))
"#,
    );
}

#[test_log::test]
fn struct_field_assignment_chain7() {
    let script = "
        a = [10, 20]
        a.remove(99)
    ";
    check(
        script,
        r#"

VariableAssignment(<9:1>, Literal(Array([Literal(Int(<14:2>)), Literal(Int(<18:2>))])))
MemberCall(VariableAccess(<30:1>), <32:6>, [Literal(Int(<39:2>))])

"#,
    );
}

#[test_log::test]
fn compound_conditions() {
    let script = "
        while x > 0 && y < 10 {
            x = x - 1
            y = y + 1
        }
    ";
    check(
        script,
        r#"
WhileLoop(BinaryOp(BinaryOp(VariableAccess(<15:1>), GreaterThan(<17:1>), Literal(Int(<19:1>))), LogicalAnd(<21:2>), BinaryOp(VariableAccess(<24:1>), LessThan(<26:1>), Literal(Int(<28:2>)))), Block([VariableAssignment(<45:1>, BinaryOp(VariableAccess(<49:1>), Subtract(<51:1>), Literal(Int(<53:1>)))), VariableAssignment(<67:1>, BinaryOp(VariableAccess(<71:1>), Add(<73:1>), Literal(Int(<75:1>))))]))
"#,
    );
}

#[test_log::test]
fn nested_loops() {
    let script = "
        x = 0
        while x < 3 {
            y = 0
            while y < 2 {
                print(x + y)
                y = y + 1
            }
            x = x + 1
        }
    ";
    check(
        script,
        r#"
VariableAssignment(<9:1>, Literal(Int(<13:1>)))
WhileLoop(BinaryOp(VariableAccess(<29:1>), LessThan(<31:1>), Literal(Int(<33:1>))), Block([VariableAssignment(<49:1>, Literal(Int(<53:1>))), WhileLoop(BinaryOp(VariableAccess(<73:1>), LessThan(<75:1>), Literal(Int(<77:1>))), Block([FunctionCall(VariableAccess(<97:5>), [BinaryOp(VariableAccess(<103:1>), Add(<105:1>), VariableAccess(<107:1>))]), VariableAssignment(<126:1>, BinaryOp(VariableAccess(<130:1>), Add(<132:1>), Literal(Int(<134:1>))))])), VariableAssignment(<162:1>, BinaryOp(VariableAccess(<166:1>), Add(<168:1>), Literal(Int(<170:1>))))]))
 "#,
    );
}

#[test_log::test]
fn mixed_expressions_with_chain() {
    let script = "
        struct Point { x: Int, y: Int }
        p1 = Point { x: 5, y: 10 }
        dist = add(mul(p1.x, p1.x), mul(p1.y, p1.y))
    ";
    check(
        script,
        r"

StructDef(StructType { identifier: LocalTypeIdentifier(<16:5>), fields: [FieldType { field_name: FieldName(<24:1>), field_type: Int(<27:3>) }, FieldType { field_name: FieldName(<32:1>), field_type: Int(<35:3>) }] })
---
VariableAssignment(<49:2>, StructInstantiation(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<54:5>), module_path: None }, [FieldExpression { field_name: FieldName(<62:1>), expression: Literal(Int(<65:1>)) }, FieldExpression { field_name: FieldName(<68:1>), expression: Literal(Int(<71:2>)) }]))
VariableAssignment(<84:4>, FunctionCall(VariableAccess(<91:3>), [FunctionCall(VariableAccess(<95:3>), [FieldAccess(VariableAccess(<99:2>), <102:1>), FieldAccess(VariableAccess(<105:2>), <108:1>)]), FunctionCall(VariableAccess(<112:3>), [FieldAccess(VariableAccess(<116:2>), <119:1>), FieldAccess(VariableAccess(<122:2>), <125:1>)])]))

    ",
    );
}

#[test_log::test]
fn small_call() {
    let script = "
        mul(2)
    ";
    check(
        script,
        "FunctionCall(VariableAccess(<9:3>), [Literal(Int(<13:1>))])",
    );
}

#[test_log::test]
fn small_chain() {
    let script = "
        p1.x.mul(2)
    ";
    check(
        script,
        "MemberCall(FieldAccess(VariableAccess(<9:2>), <12:1>), <14:3>, [Literal(Int(<18:1>))])",
    );
}

#[test_log::test]
fn only_chain() {
    let script = "
        p1.x.mul(p1.x).add(p1.y.mul(p1.y))
    ";
    check(
        script,
        r#"
MemberCall(MemberCall(FieldAccess(VariableAccess(<9:2>), <12:1>), <14:3>, [FieldAccess(VariableAccess(<18:2>), <21:1>)]), <24:3>, [MemberCall(FieldAccess(VariableAccess(<28:2>), <31:1>), <33:3>, [FieldAccess(VariableAccess(<37:2>), <40:1>)])])
    "#,
    );
}

#[test_log::test]
fn method_chaining() {
    let script = r#"
        struct Point { x: Int, y: Int }
        p1 = Point { x: 5, y: 10 }
        dist = p1.x.mul(p1.x).add(p1.y.mul(p1.y))
    "#;

    check(
        &script,
        r#"
StructDef(StructType { identifier: LocalTypeIdentifier(<16:5>), fields: [FieldType { field_name: FieldName(<24:1>), field_type: Int(<27:3>) }, FieldType { field_name: FieldName(<32:1>), field_type: Int(<35:3>) }] })
---
VariableAssignment(<49:2>, StructInstantiation(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<54:5>), module_path: None }, [FieldExpression { field_name: FieldName(<62:1>), expression: Literal(Int(<65:1>)) }, FieldExpression { field_name: FieldName(<68:1>), expression: Literal(Int(<71:2>)) }]))
VariableAssignment(<84:4>, MemberCall(MemberCall(FieldAccess(VariableAccess(<91:2>), <94:1>), <96:3>, [FieldAccess(VariableAccess(<100:2>), <103:1>)]), <106:3>, [MemberCall(FieldAccess(VariableAccess(<110:2>), <113:1>), <115:3>, [FieldAccess(VariableAccess(<119:2>), <122:1>)])]))

    "#,
    );
}

#[test_log::test]
fn function_definition() {
    let script = r"
        fn add(x: Int, y: Int) -> Int {
            x + y
        }
    ";

    check(
        script,
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <12:3>, params: [Parameter { variable: <16:1>, param_type: Int(<19:3>) }, Parameter { variable: <24:1>, param_type: Int(<27:3>) }], self_parameter: None, return_type: Some(Int(<35:3>)) }, body: Block([BinaryOp(VariableAccess(<53:1>), Add(<55:1>), VariableAccess(<57:1>))]) }))
    ",
    );
}

#[test_log::test]
fn function_with_no_parameters() {
    let script = r"
        fn add() -> Int {
            42
        }
    ";

    check(
        &script,
        r"
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <12:3>, params: [], self_parameter: None, return_type: Some(Int(<21:3>)) }, body: Block([Literal(Int(<39:2>))]) }))


    ",
    );
}

#[test_log::test]
fn function_with_no_parameters_return() {
    let script = "
        fn add() -> Int {
            return 42
        }
    ";

    check(
        script,
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <12:3>, params: [], self_parameter: None, return_type: Some(Int(<21:3>)) }, body: Block([Return(Some(Literal(Int(<46:2>))))]) }))

    ",
    );
}

#[test_log::test]
fn function_call_with_no_parameters() {
    let script = "
        fn single() -> Int {
            42
        }
        result = single()
    ";

    check(
        script,
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <12:6>, params: [], self_parameter: None, return_type: Some(Int(<24:3>)) }, body: Block([Literal(Int(<42:2>))]) }))
---
VariableAssignment(<63:6>, FunctionCall(VariableAccess(<72:6>), []))

    ",
    );
}

#[test_log::test]
fn array() {
    let script = r#"
        x = [1, 2, 3]
    "#;

    check(
        &script,
        r#"
VariableAssignment(<9:1>, Literal(Array([Literal(Int(<14:1>)), Literal(Int(<17:1>)), Literal(Int(<20:1>))])))
    "#,
    );
}

#[test_log::test]
fn empty_array() {
    let script = r#"
        x = []
    "#;

    check(
        &script,
        r#"
VariableAssignment(<9:1>, Literal(Array([])))
    "#,
    );
}

#[test_log::test]
fn empty_array_expression() {
    let script = r#"
        []
    "#;

    check(
        &script,
        r#"
Literal(Array([]))
    "#,
    );
}

#[test_log::test]
fn empty_array_expression_call() {
    let script = r"
        [].len()
    ";

    check(
        script,
        r"
MemberCall(Literal(Array([])), <12:3>, [])
        ",
    );
}

#[test_log::test]
fn string_expression_call() {
    let script = r#"
        "hello, world".len()
    "#;

    check(
        &script,
        r#"
MemberCall(Literal(String(<9:14>)), <24:3>, [])
        "#,
    );
}

#[test_log::test]
fn real_round() {
    let script = r#"
        2.2.round()
    "#;

    check(
        &script,
        r#"
MemberCall(Literal(Float(<9:3>)), <13:5>, [])
        "#,
    );
}

#[test_log::test]
fn real_negative() {
    let script = r#"
        -2.2
    "#;

    check(
        &script,
        r#"
UnaryOp(Negate(<9:1>), Literal(Float(<10:3>)))
        "#,
    );
}

#[test_log::test]
fn real_negative_round() {
    let script = r#"
        -2.2.round()
    "#;

    check(
        &script,
        r#"
UnaryOp(Negate(<9:1>), MemberCall(Literal(Float(<10:3>)), <14:5>, []))
        "#,
    );
}

#[test_log::test]
fn real_literal() {
    let script = r#"
        2.2
    "#;

    check(
        &script,
        r#"
Literal(Float(<9:3>))
        "#,
    );
}

#[test_log::test]
fn string_literal() {
    let script = r#"
    "hello"
     "#;
    check(
        &script,
        r#"
Literal(String(<5:7>))
        "#,
    );
}

#[test_log::test]
fn tuple_literal() {
    let script = r#"
    ( "hello", 1, 2.2 )
     "#;
    check(
        &script,
        r#"
Literal(Tuple([Literal(String(<7:7>)), Literal(Int(<16:1>)), Literal(Float(<19:3>))]))
        "#,
    );
}

#[test_log::test]
fn tuple_type() {
    let script = r#"
    fn some_tuple() -> ( String, Int, Float ) {
        ( "hello", 1, 2.2 )
         }
     "#;
    check(
        &script,
        r"

FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <8:10>, params: [], self_parameter: None, return_type: Some(Tuple([String(<26:6>), Int(<34:3>), Float(<39:5>)])) }, body: Block([Literal(Tuple([Literal(String(<59:7>)), Literal(Int(<68:1>)), Literal(Float(<71:3>))]))]) }))

        ",
    );
}

#[test_log::test]
fn enum_type() {
    let script = "
    enum Custom {
        Idle,
        Running(Int, Float),
        Sleeping { hours: Int },
    }
    ";
    check(
        script,
        "
EnumDef(<10:6>, [Simple(<27:4>), Tuple(<41:7>, [Int(<49:3>), Float(<54:5>)]), Struct(<70:8>, AnonymousStructType { fields: [FieldType { field_name: FieldName(<81:5>), field_type: Int(<88:3>) }] })])

        ",
    );
}

#[test_log::test]
fn enum_type2() {
    let script = "
    enum Custom {
        Idle { one: Int, two: (Int, Float) },
        Running(Int, Float, String, Int),
    }
    ";
    check(
        script,
        "

EnumDef(<10:6>, [Struct(<27:4>, AnonymousStructType { fields: [FieldType { field_name: FieldName(<34:3>), field_type: Int(<39:3>) }, FieldType { field_name: FieldName(<44:3>), field_type: Tuple([Int(<50:3>), Float(<55:5>)]) }] }), Tuple(<73:7>, [Int(<81:3>), Float(<86:5>), String(<93:6>), Int(<101:3>)])])

        ",
    );
}

#[test_log::test]
fn match_expression() {
    let script = r"
        enum Custom {
            Idle,
            Running(Int, Float),
            Sleeping { hours: Int },
        }


        v = match state {
            Running speed, _ => speed,
            Sleeping  hours  => hours + 10,
            _ => 0,
        }

";

    check(
        script,
        r"

EnumDef(<14:6>, [Simple(<35:4>), Tuple(<53:7>, [Int(<61:3>), Float(<66:5>)]), Struct(<86:8>, AnonymousStructType { fields: [FieldType { field_name: FieldName(<97:5>), field_type: Int(<104:3>) }] })])
---
VariableAssignment(<131:1>, Match(VariableAccess(<141:5>), [MatchArm { pattern: EnumPattern(<161:7>, Some([Variable(<169:5>), Wildcard(<176:1>)])), expression: VariableAccess(<181:5>) }, MatchArm { pattern: EnumPattern(<200:8>, Some([Variable(<210:5>)])), expression: BinaryOp(VariableAccess(<220:5>), Add(<226:1>), Literal(Int(<228:2>))) }, MatchArm { pattern: PatternList([Wildcard(<244:1>)]), expression: Literal(Int(<249:1>)) }]))

",
    );
}

#[test_log::test]
fn match_expression_minimal() {
    let script = r#"
       match state {
            _ => 0,
        }
"#;

    check(
        &script,
        r"
Match(VariableAccess(<14:5>), [MatchArm { pattern: PatternList([Wildcard(<34:1>)]), expression: Literal(Int(<39:1>)) }])
",
    );
}

#[test_log::test]
fn match_expression_minimal_two_arms() {
    let script = r#"
       match state {
            EnumType ident, _ => ident,
            _ => 0,
        }
"#;

    check(
        &script,
        r"

Match(VariableAccess(<14:5>), [MatchArm { pattern: EnumPattern(<34:8>, Some([Variable(<43:5>), Wildcard(<50:1>)])), expression: VariableAccess(<55:5>) }, MatchArm { pattern: PatternList([Wildcard(<74:1>)]), expression: Literal(Int(<79:1>)) }])

",
    );
}

#[test_log::test]
fn match_expression_minimal_two_arms_enum_struct() {
    let script = r"
       match state {
            EnumType something, another => another,
            _ => 0,
        }
";

    check(
        script,
        r"

Match(VariableAccess(<14:5>), [MatchArm { pattern: EnumPattern(<34:8>, Some([Variable(<43:9>), Variable(<54:7>)])), expression: VariableAccess(<65:7>) }, MatchArm { pattern: PatternList([Wildcard(<86:1>)]), expression: Literal(Int(<91:1>)) }])

",
    );
}

#[test_log::test]
fn match_expression_simple_variant() {
    let script = r"
       match state {
            EnumType
            _ => 0,
        }
";

    check(
        script,
        r"

Match(VariableAccess(<14:5>), [MatchArm { pattern: EnumPattern(<34:8>, Some([Wildcard(<55:1>)])), expression: Literal(Int(<60:1>)) }])
",
    );
}

#[test_log::test]
fn deconstructing_struct() {
    let script = "x, y = pos"; // No raw string literal
    check(
        script,
        "MultiVariableAssignment([<0:1>, <3:1>], VariableAccess(<7:3>))",
    );
}

#[test_log::test]
fn operator_precedence() {
    let script = "z = y * 2 - x";
    check(script, "VariableAssignment(<0:1>, BinaryOp(BinaryOp(VariableAccess(<4:1>), Multiply(<6:1>), Literal(Int(<8:1>))), Subtract(<10:1>), VariableAccess(<12:1>)))");
}

#[test_log::test]
fn operator_precedence_expression() {
    let script = "y * 2 - x";
    check(
        &script,
        "
BinaryOp(BinaryOp(VariableAccess(<0:1>), Multiply(<2:1>), Literal(Int(<4:1>))), Subtract(<6:1>), VariableAccess(<8:1>))",
    );
}

#[test_log::test]
fn for_range() {
    let script = r"
for x in 1..10 {
}

    ";
    check(
        script,
        "
ForLoop(Single(ForVar { identifier: <5:1>, is_mut: None }), IteratableExpression { is_mut: None, expression: ExclusiveRange(Literal(Int(<10:1>)), Literal(Int(<13:2>))) }, Block([]))

",
    );
}

#[test_log::test]
fn range_literal() {
    let script = r#"

    0..39

    "#;
    check(
        &script,
        "
ExclusiveRange(Literal(Int(<6:1>)), Literal(Int(<9:2>)))",
    );
}

#[test_log::test]
fn for_array() {
    let script = r#"

for x in [1, 2, 3] {
}

    "#;
    check(
        &script,
        "
ForLoop(Single(ForVar { identifier: <6:1>, is_mut: None }), IteratableExpression { is_mut: None, expression: Literal(Array([Literal(Int(<12:1>)), Literal(Int(<15:1>)), Literal(Int(<18:1>))])) }, Block([]))

"
    );
}

#[test_log::test]
fn enum_literal_basic() {
    check(
        r#"
        state = State::Running

    "#,
        r#"

VariableAssignment(<9:5>, Literal(EnumVariant(Simple(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<17:5>), module_path: None }, LocalTypeIdentifier(<24:7>)))))
        "#,
    );
}

#[test_log::test]
fn enum_literal() {
    check(
        r#"
        enum State {
            Running,
            Stopped,
        }

        state = State::Running
        print(state)
    "#,
        r#"

EnumDef(<14:5>, [Simple(<34:7>), Simple(<55:7>)])
---
VariableAssignment(<83:5>, Literal(EnumVariant(Simple(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<91:5>), module_path: None }, LocalTypeIdentifier(<98:7>)))))
FunctionCall(VariableAccess(<114:5>), [VariableAccess(<120:5>)])

        "#,
    );
}

/*
#[test_log::test]
fn enum_literal_with_path() {
    check(
        r"
        enum State {
            Running,
            Stopped,
        }

        state = mangrove::save_game::State::Running
        print(state)
    ",
        r#"

EnumDef(<14:5>, [Simple(<34:7>), Simple(<55:7>)])
---
VariableAssignment(<83:5>, Literal(EnumVariant(Simple(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<91:5>), module_path: None })))))
FunctionCall(VariableAccess(<114:5>), [VariableAccess(<120:5>)]))

        "#,
    );
}
*/

#[test_log::test]
fn mut_parameter() {
    let script = r#"
/// increments the value
fn increment(mut x: Int) -> Int {
    x = x + 1
    x
}

    "#;
    check(
        &script,
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <29:9>, params: [Parameter { variable: mut <39:3> <43:1>, param_type: Int(<46:3>) }], self_parameter: None, return_type: Some(Int(<54:3>)) }, body: Block([VariableAssignment(<64:1>, BinaryOp(VariableAccess(<68:1>), Add(<70:1>), Literal(Int(<72:1>)))), VariableAccess(<78:1>)]) }))
",
    );
}

#[test_log::test]
fn mut_let() {
    check(
        "mut x = 3",
        "VariableAssignment(mut <0:3> <4:1>, Literal(Int(<8:1>)))",
    );
}

#[test_log::test]
fn impl_def() {
    check(
        r"
            impl SomeTypeName {
                fn something(self) -> Int {
                    self.x
                }

                fn another(mut self, v: Int) -> Int {
                    self.x = 3
                }

                fn no_self_here() -> Float {
                    3.2
                }
            }
        ",
        r"

ImplDef(<18:12>, [Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <52:9>, params: [], self_parameter: Some(SelfParameter { is_mutable: None, self_node: <62:4> }), return_type: Some(Int(<71:3>)) }, body: Block([FieldAccess(VariableAccess(<97:4>), <102:1>)]) }), Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <142:7>, params: [Parameter { variable: <160:1>, param_type: Int(<163:3>) }], self_parameter: Some(SelfParameter { is_mutable: Some(<150:3>), self_node: <154:4> }), return_type: Some(Int(<171:3>)) }, body: Block([FieldAssignment(VariableAccess(<197:4>), <202:1>, Literal(Int(<206:1>)))]) }), Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <246:12>, params: [], self_parameter: None, return_type: Some(Float(<264:5>)) }, body: Block([Literal(Float(<292:3>))]) })])

        ",
    );
}

#[test_log::test]
fn match_pattern_literal() {
    check(
        r#"
        match x {
            5 => "five",
            "hello" => "greeting",
            true => "yes",
            _ => "something else"
        }"#,
        r"

Match(VariableAccess(<15:1>), [MatchArm { pattern: Literal(Int(<31:1>)), expression: Literal(String(<36:6>)) }, MatchArm { pattern: Literal(String(<56:7>)), expression: Literal(String(<67:10>)) }, MatchArm { pattern: Literal(Bool(<91:4>)), expression: Literal(String(<99:5>)) }, MatchArm { pattern: PatternList([Wildcard(<118:1>)]), expression: Literal(String(<123:16>)) }])

        ",
    );
}

#[test_log::test]
fn match_comment() {
    check(
        r#"
        print("hello") // first
        /// this has some documentation
        print("world") // second
       "#,
        r#"

FunctionCall(VariableAccess(<9:5>), [Literal(String(<15:7>))])
FunctionCall(VariableAccess(<81:5>), [Literal(String(<87:7>))])

        "#,
    );
}

#[test_log::test]
fn multiple_assignments() {
    check(
        r#"
        x = y = z = 10
        "#,
        r"
VariableAssignment(<9:1>, VariableAssignment(<13:1>, VariableAssignment(<17:1>, Literal(Int(<21:2>)))))
        ",
    );
}

#[test_log::test]
fn enum_variant_construction() {
    check(
        r"

        shape = Shape::Rectangle { width: 10, height: 20 }

        ",
        r"
VariableAssignment(<10:5>, Literal(EnumVariant(Struct(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<18:5>), module_path: None }, LocalTypeIdentifier(<25:9>), [FieldExpression { field_name: FieldName(<37:5>), expression: Literal(Int(<44:2>)) }, FieldExpression { field_name: FieldName(<48:6>), expression: Literal(Int(<56:2>)) }]))))

",
    );
}

#[test_log::test]
fn enum_variant_tuple_construction() {
    check(
        r#"
        shape = Shape::Something(2, 4.4)
        "#,
        r#"
VariableAssignment(<9:5>, Literal(EnumVariant(Tuple(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<17:5>), module_path: None }, LocalTypeIdentifier(<24:9>), [Literal(Int(<34:1>)), Literal(Float(<37:3>))]))))

        "#,
    );
}

#[test_log::test]
fn array_access() {
    check(
        r#"
        arr[3]
        "#,
        "IndexAccess(VariableAccess(<9:3>), Literal(Int(<13:1>)))",
    );
}

#[test_log::test]
fn array_set() {
    check(
        r#"
        arr[3] = 42
        "#,
        "IndexAssignment(VariableAccess(<9:3>), Literal(Int(<13:1>)), Literal(Int(<18:2>)))",
    );
}

#[test_log::test]
fn string_interpolation_basic_spaces() {
    check(
        "'   this is interpolated {x}   with hex  {y}  '",
        r#"
InterpolatedString([Literal(<1:24>), Interpolation(VariableAccess(<26:1>), None), Literal(<28:13>), Interpolation(VariableAccess(<42:1>), None), Literal(<44:2>)])
    "#,
    );
}

#[test_log::test]
fn string_interpolation() {
    check(
        "'this is interpolated {x} with hex {y:x}'",
        r#"
InterpolatedString([Literal(<1:21>), Interpolation(VariableAccess(<23:1>), None), Literal(<25:10>), Interpolation(VariableAccess(<36:1>), Some(LowerHex(<38:1>)))])
    "#,
    );
}

#[test_log::test]
fn string_interpolation_call() {
    check(
        "'this is interpolated {x:x}    with hex  {mul(a, 2)}'",
        r#"
InterpolatedString([Literal(<1:21>), Interpolation(VariableAccess(<23:1>), Some(LowerHex(<25:1>))), Literal(<27:14>), Interpolation(FunctionCall(VariableAccess(<42:3>), [VariableAccess(<46:1>), Literal(Int(<49:1>))]), None)])
    "#,
    );
}

#[test_log::test]
fn string_interpolation_call_simple() {
    check(
        "'result: {mul(a,2)}'",
        r#"
InterpolatedString([Literal(<1:8>), Interpolation(FunctionCall(VariableAccess(<10:3>), [VariableAccess(<14:1>), Literal(Int(<16:1>))]), None)])
    "#,
    );
}

#[test_log::test]
fn string_interpolation_simple() {
    check(
        "'this is interpolated {x}'",
        r#"
InterpolatedString([Literal(<1:21>), Interpolation(VariableAccess(<23:1>), None)])
    "#,
    );
}

#[test_log::test]
fn string_interpolation_simple_no_space() {
    check(
        "'this is interpolated{x}'",
        r#"
InterpolatedString([Literal(<1:20>), Interpolation(VariableAccess(<22:1>), None)])
    "#,
    );
}

#[test_log::test]
fn function_call_mul() {
    check(
        "mul(a, 2)",
        r#"
FunctionCall(VariableAccess(<0:3>), [VariableAccess(<4:1>), Literal(Int(<7:1>))])
    "#,
    );
}

#[test_log::test]
fn enum_match() {
    check(
        "
        msg = match result {
            Ok value => value,
            Err err => err
        }
        ",
        "

VariableAssignment(<9:3>, Match(VariableAccess(<21:6>), [MatchArm { pattern: EnumPattern(<42:2>, Some([Variable(<45:5>)])), expression: VariableAccess(<54:5>) }, MatchArm { pattern: EnumPattern(<73:3>, Some([Variable(<77:3>)])), expression: VariableAccess(<84:3>) }]))
",
    );
}

#[test_log::test]
fn enum_match_simple() {
    check(
        "
        msg = match result {
            Ok value => value,
            Err err => err,
            Simple => 2
        }
        ",
        "
 VariableAssignment(<9:3>, Match(VariableAccess(<21:6>), [MatchArm { pattern: EnumPattern(<42:2>, Some([Variable(<45:5>)])), expression: VariableAccess(<54:5>) }, MatchArm { pattern: EnumPattern(<73:3>, Some([Variable(<77:3>)])), expression: VariableAccess(<84:3>) }, MatchArm { pattern: EnumPattern(<101:6>, None), expression: Literal(Int(<111:1>)) }]))
     ",
    );
}

#[test_log::test]
fn enum_match_wildcard() {
    check(
        "
        msg = match result {
            Ok(value) => value,
            Err(err) => err,
            Simple => 2,
            _ => 99
        }
        ",
        "
VariableAssignment(<9:3>, Match(VariableAccess(<21:6>), [MatchArm { pattern: EnumPattern(<42:2>, Some([Expression(VariableAccess(<45:5>))])), expression: VariableAccess(<55:5>) }, MatchArm { pattern: EnumPattern(<74:3>, Some([Expression(VariableAccess(<78:3>))])), expression: VariableAccess(<86:3>) }, MatchArm { pattern: EnumPattern(<103:6>, None), expression: Literal(Int(<113:1>)) }, MatchArm { pattern: PatternList([Wildcard(<128:1>)]), expression: Literal(Int(<133:2>)) }]))
      ",
    );
}

#[test_log::test]
fn enum_match_with_wildcard() {
    check(
        r"
        msg = match result {
            Ok(value) => value,
            _ => 99
        }
        ",
        "
VariableAssignment(<9:3>, Match(VariableAccess(<21:6>), [MatchArm { pattern: EnumPattern(<42:2>, Some([Expression(VariableAccess(<45:5>))])), expression: VariableAccess(<55:5>) }, MatchArm { pattern: PatternList([Wildcard(<74:1>)]), expression: Literal(Int(<79:2>)) }]))
",
    );
}
#[test_log::test]
fn enum_match_struct_y() {
    check(
        r#"

        enum Action {
            Jumping,
            Target { x: Int, y: Int },
            Other(String),
        }

        action = Action::Target { x:42, y: -999 }

        /*
        match the actions
            - Jumping
            - Target
        */

        match action {
            Jumping => "jumping",
            Target y => y,
            _ => "can not find it!",
        }
    "#,
        "

EnumDef(<15:6>, [Simple(<36:7>), Struct(<57:6>, AnonymousStructType { fields: [FieldType { field_name: FieldName(<66:1>), field_type: Int(<69:3>) }, FieldType { field_name: FieldName(<74:1>), field_type: Int(<77:3>) }] }), Tuple(<96:5>, [String(<102:6>)])])
---
VariableAssignment(<130:6>, Literal(EnumVariant(Struct(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<139:6>), module_path: None }, LocalTypeIdentifier(<147:6>), [FieldExpression { field_name: FieldName(<156:1>), expression: Literal(Int(<158:2>)) }, FieldExpression { field_name: FieldName(<162:1>), expression: UnaryOp(Negate(<165:1>), Literal(Int(<166:3>))) }]))))
Match(VariableAccess(<279:6>), [MatchArm { pattern: EnumPattern(<300:7>, None), expression: Literal(String(<311:9>)) }, MatchArm { pattern: EnumPattern(<334:6>, Some([Variable(<341:1>)])), expression: VariableAccess(<346:1>) }, MatchArm { pattern: PatternList([Wildcard(<361:1>)]), expression: Literal(String(<366:18>)) }])
        ",
    );
}

#[test_log::test]
fn enum_match_tuple_basic() {
    check(
        r#"
           match v {
                Tuple(i, s, b) => { // Tuple is an imaginary enum variant with name Tuple
                    print("Tuple:")
                    print(i)
                    print(s)
                    print(b)
                }
            }
    "#,
        "

Match(VariableAccess(<18:1>), [MatchArm { pattern: EnumPattern(<38:5>, Some([Expression(Literal(Tuple([VariableAccess(<44:1>), VariableAccess(<47:1>), VariableAccess(<50:1>)])))])), expression: Block([FunctionCall(VariableAccess(<132:5>), [Literal(String(<138:8>))]), FunctionCall(VariableAccess(<168:5>), [VariableAccess(<174:1>)]), FunctionCall(VariableAccess(<197:5>), [VariableAccess(<203:1>)]), FunctionCall(VariableAccess(<226:5>), [VariableAccess(<232:1>)])]) }])

    ",
    );
}

#[test_log::test]
fn print_if() {
    check(
        r#"
       if x > 41 {
            "x is greater than 41"
        }
    "#,
        "If(BinaryOp(VariableAccess(<11:1>), GreaterThan(<13:1>), Literal(Int(<15:2>))), Block([Literal(String(<32:22>))]), None)
",
    );
}

#[test_log::test]
fn for_continue() {
    check(
        r#"
       for i in 1..5 {

            if i % 2 == 0 {
                continue
            }

            print("no continue")
        }
        "#,
        r"

ForLoop(Single(ForVar { identifier: <12:1>, is_mut: None }), IteratableExpression { is_mut: None, expression: ExclusiveRange(Literal(Int(<17:1>)), Literal(Int(<20:1>))) }, Block([If(BinaryOp(BinaryOp(VariableAccess(<40:1>), Modulo(<42:1>), Literal(Int(<44:1>))), Equal(<46:2>), Literal(Int(<49:1>))), Block([Continue(<69:8>)]), None), FunctionCall(VariableAccess(<105:5>), [Literal(String(<111:13>))])]))

    ",
    );
}

#[test_log::test]
fn else_problem() {
    check(
        r#"
         if x < 0 {
                return "negative"
        } else if x == 0 {
            return "positive"
        }
            "#,
        r#"

If(BinaryOp(VariableAccess(<13:1>), LessThan(<15:1>), Literal(Int(<17:1>))), Block([Return(Some(Literal(String(<44:10>))))]), Some(If(BinaryOp(VariableAccess(<73:1>), Equal(<75:2>), Literal(Int(<78:1>))), Block([Return(Some(Literal(String(<101:10>))))]), None)))

            "#,
    )
}

#[test_log::test]
fn option_operator() {
    check(
        r#"
         a?
            "#,
        r#"
PostfixOp(Unwrap(<11:1>), VariableAccess(<10:1>))

            "#,
    )
}

#[test_log::test]
fn option_operator_expr() {
    check(
        r#"
         b + a?
            "#,
        r#"

BinaryOp(VariableAccess(<10:1>), Add(<12:1>), PostfixOp(Unwrap(<15:1>), VariableAccess(<14:1>)))

            "#,
    )
}

#[test_log::test]
fn option_operator_if_variable() {
    check(
        r#"
         if a? {
         'this is {a}'
         } else {
         'not here'
         }
            "#,
        r#"

If(PostfixOp(Unwrap(<14:1>), VariableAccess(<13:1>)), Block([InterpolatedString([Literal(<28:8>), Interpolation(VariableAccess(<37:1>), None)])]), Some(Block([InterpolatedString([Literal(<69:8>)])])))

            "#,
    )
}

#[test_log::test]
fn option_operator_if_expression() {
    check(
        r#"
         if (b*3+99+something.call(42))? {
         'expression is something'
         } else {
         'must be none'
         }
            "#,
        r#"

If(PostfixOp(Unwrap(<40:1>), BinaryOp(BinaryOp(BinaryOp(VariableAccess(<14:1>), Multiply(<15:1>), Literal(Int(<16:1>))), Add(<17:1>), Literal(Int(<18:2>))), Add(<20:1>), MemberCall(VariableAccess(<21:9>), <31:4>, [Literal(Int(<36:2>))]))), Block([InterpolatedString([Literal(<54:23>)])]), Some(Block([InterpolatedString([Literal(<107:12>)])])))

            "#,
    )
}

#[test_log::test]
fn option_operator_assignment() {
    check(
        r"
         a = another.get_current()?
            ",
        r"

VariableAssignment(<10:1>, PostfixOp(Unwrap(<35:1>), MemberCall(VariableAccess(<14:7>), <22:11>, [])))
            ",
    );
}

#[test_log::test]
fn option_operator_assignment_chained() {
    check(
        r"
         a = another.get_current()?.another_call(b, 42)?
            ",
        r#"

VariableAssignment(<10:1>, PostfixOp(Unwrap(<56:1>), MemberCall(PostfixOp(Unwrap(<35:1>), MemberCall(VariableAccess(<14:7>), <22:11>, [])), <37:12>, [VariableAccess(<50:1>), Literal(Int(<53:2>))])))

            "#,
    );
}

#[test_log::test]
fn option_operator_if_let_expression() {
    check(
        r"
         if a = another.get_current()? {
               'this is {a}'
         } else {
            'must be none'
         }
            ",
        r"

If(VariableAssignment(<13:1>, PostfixOp(Unwrap(<38:1>), MemberCall(VariableAccess(<17:7>), <25:11>, []))), Block([InterpolatedString([Literal(<58:8>), Interpolation(VariableAccess(<67:1>), None)])]), Some(Block([InterpolatedString([Literal(<102:12>)])])))
          ",
    );
}

#[test_log::test]
fn option_operator_if_let_expression_multiple_calls() {
    check(
        r#"
         if a = another.get_current()?.another_call(b, 42)? {
               'this is {a}'
         } else {
            'must be none'
         }
            "#,
        r#"

If(VariableAssignment(<13:1>, PostfixOp(Unwrap(<59:1>), MemberCall(PostfixOp(Unwrap(<38:1>), MemberCall(VariableAccess(<17:7>), <25:11>, [])), <40:12>, [VariableAccess(<53:1>), Literal(Int(<56:2>))]))), Block([InterpolatedString([Literal(<79:8>), Interpolation(VariableAccess(<88:1>), None)])]), Some(Block([InterpolatedString([Literal(<123:12>)])])))

            "#,
    )
}

#[test_log::test]
fn none_assignment() {
    check(
        r#"
        a = none
            "#,
        r#"
VariableAssignment(<9:1>, Literal(None(<13:4>)))

            "#,
    )
}

#[test_log::test]
fn if_assignment() {
    check(
        "
    a = 3
    c = if b = a > 3 {
        b + 1
    } else {
        b + 2
    }

            ",
        "

VariableAssignment(<5:1>, Literal(Int(<9:1>)))
VariableAssignment(<15:1>, If(VariableAssignment(<22:1>, BinaryOp(VariableAccess(<26:1>), GreaterThan(<28:1>), Literal(Int(<30:1>)))), Block([BinaryOp(VariableAccess(<42:1>), Add(<44:1>), Literal(Int(<46:1>)))]), Some(Block([BinaryOp(VariableAccess(<69:1>), Add(<71:1>), Literal(Int(<73:1>)))]))))

            ",
    );
}

#[test_log::test]
fn struct_field_optional() {
    check(
        "
  struct Struct {
    some_field: Int?
   }
            ",
        r"

StructDef(StructType { identifier: LocalTypeIdentifier(<10:6>), fields: [FieldType { field_name: FieldName(<23:10>), field_type: Optional(Int(<35:3>), <38:1>) }] })

            ",
    );
}

#[test_log::test]
fn map_literal() {
    check(
        "
  a = [2: 'Hello', 3: 'World']

            ",
        r#"

VariableAssignment(<3:1>, Literal(Map([(Literal(Int(<8:1>)), InterpolatedString([Literal(<12:5>)])), (Literal(Int(<20:1>)), InterpolatedString([Literal(<24:5>)]))])))

            "#,
    );
}

#[test_log::test]
fn map_literal_no_spaces() {
    check(
        "
  a = [2:'Hello',3:'World']

            ",
        r#"

VariableAssignment(<3:1>, Literal(Map([(Literal(Int(<8:1>)), InterpolatedString([Literal(<11:5>)])), (Literal(Int(<18:1>)), InterpolatedString([Literal(<21:5>)]))])))

            "#,
    );
}

#[test_log::test]
fn map_fn() {
    check(
        "
fn map_creator() -> [Int: String] {
[2: 'hello', -1: 'world']
}

            ",
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:11>, params: [], self_parameter: None, return_type: Some(Map(Int(<22:3>), String(<27:6>))) }, body: Block([Literal(Map([(Literal(Int(<38:1>)), InterpolatedString([Literal(<42:5>)])), (UnaryOp(Negate(<50:1>), Literal(Int(<51:1>))), InterpolatedString([Literal(<55:5>)]))]))]) }))
            ",
    );
}

#[test_log::test]
fn int_fn() {
    check(
        "
fn test() -> Int { 42 }
            ",
        "

FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:4>, params: [], self_parameter: None, return_type: Some(Int(<14:3>)) }, body: Block([Literal(Int(<20:2>))]) }))


            ",
    );
}

#[test_log::test]
fn generic_type() {
    check(
        "
fn nothing() -> SomeType<Int, Float> {
}

            ",
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:7>, params: [], self_parameter: None, return_type: Some(Generic(TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<17:8>), module_path: None }), [Int(<26:3>), Float(<31:5>)])) }, body: Block([]) }))
           ",
    );
}

#[test_log::test]
fn sparse_map_static_call() {
    check(
        r"
        result = SparseMap<Int>::new()
        ",
        r"

  VariableAssignment(<9:6>, StaticCallGeneric(<18:9>, <34:3>, [], [Int(<28:3>)]))
",
    );
}

#[test_log::test]
fn assignment_op_add() {
    check(
        "a += 6",
        "VariableCompoundAssignment(<0:1>, CompoundOperator { node: <2:2>, kind: Add }, Literal(Int(<5:1>)))",
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
VariableAssignment(<9:12>, Literal(Bool(<24:5>)))
VariableAssignment(<38:1>, If(VariableAccess(<45:12>), Block([Literal(Float(<60:3>))]), Some(Block([UnaryOp(Negate(<73:1>), Literal(Float(<74:4>)))]))))

        ",
    );
}

#[test_log::test]
fn check_return_type() {
    check(
        r"
        fn x(a: Int) -> (Int, Float) {
        }
    ",
        r"
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <12:1>, params: [Parameter { variable: <14:1>, param_type: Int(<17:3>) }], self_parameter: None, return_type: Some(Tuple([Int(<26:3>), Float(<31:5>)])) }, body: Block([]) }))

    ",
    );
}

#[test_log::test]
fn check_prefix_and_generic() {
    check(
        r"
        struct Logic {
            tick_count: Int, /// how many ticks have passed
            explosions: std::Sparse<Explosion>,
         }
",
        r"
StructDef(StructType { identifier: LocalTypeIdentifier(<16:5>), fields: [FieldType { field_name: FieldName(<36:10>), field_type: Int(<48:3>) }, FieldType { field_name: FieldName(<96:10>), field_type: Generic(TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<113:6>), module_path: Some(ModulePath([ModulePathItem { node: <108:3> }])) }), [TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<120:9>), module_path: None })]) }] })

",
    );
}

#[test_log::test]
fn check_boolean_expression() {
    check(
        r"
    if !enemy.rect.intersects(shot_rect) {
    }
",
        r"
If(UnaryOp(Not(<8:1>), MemberCall(FieldAccess(VariableAccess(<9:5>), <15:4>), <20:10>, [VariableAccess(<31:9>)])), Block([]), None)
        ",
    );
}

#[test_log::test]
fn check_boolean_expression_no_block() {
    check(
        r"
    if !enemy.rect.intersects(shot_rect) 2 else 3
",
        r"
If(UnaryOp(Not(<8:1>), MemberCall(FieldAccess(VariableAccess(<9:5>), <15:4>), <20:10>, [VariableAccess(<31:9>)])), Literal(Int(<42:1>)), Some(Literal(Int(<49:1>))))
        ",
    );
}

#[test_log::test]
fn documentation_comment() {
    check(
        r"
        /// This is a doc comment
x = if true 2 else 4
",
        r"
VariableAssignment(<35:1>, If(Literal(Bool(<42:4>)), Literal(Int(<47:1>)), Some(Literal(Int(<54:1>)))))
        ",
    );
}

#[test_log::test]
fn external_function() {
    check(
        r"
        /// This is a doc comment
external fn something(i: Int) -> Float
",
        r"
FunctionDef(External(FunctionDeclaration { name: <47:9>, params: [Parameter { variable: <57:1>, param_type: Int(<60:3>) }], self_parameter: None, return_type: Some(Float(<68:5>)) }))
        ",
    );
}

#[test_log::test]
fn external_member_function() {
    check(
        r"
        struct Api {
            x : Int,
        }
        
        impl Api {
            external fn something(self, i: Int) -> Float {}
        }
",
        r"
If(UnaryOp(Not(<8:1>), MemberCall(FieldAccess(VariableAccess(<9:5>), <15:4>), <20:10>, [VariableAccess(<31:9>)])), Literal(Int(<42:1>)), Some(Literal(Int(<49:1>))))
        ",
    );
}

#[test_log::test]
fn variable() {
    check(
        r"
some_variable
",
        r"
VariableAccess(<1:13>)
        ",
    );
}

#[test_log::test]
fn block() {
    check(
        r"
if true {
    {
        a = 3
    }
}
",
        r"
If(Literal(Bool(<4:4>)), Block([Block([VariableAssignment(<25:1>, Literal(Int(<29:1>)))])]), None)
        ",
    );
}

#[test_log::test]
fn unary() {
    check(
        r"
-a
",
        r"
UnaryOp(Negate(<1:1>), VariableAccess(<2:1>))
        ",
    );
}

#[test_log::test]
fn unary_not() {
    check(
        r"
!a
",
        r"
UnaryOp(Not(<1:1>), VariableAccess(<2:1>))
        ",
    );
}

#[test_log::test]
fn mut_variable() {
    check(
        r"
mut a = 3
",
        r"
VariableAssignment(mut <1:3> <5:1>, Literal(Int(<9:1>)))
        ",
    );
}

#[test_log::test]
fn mut_variable_list() {
    check(
        r"
a, mut b = 3
",
        r"
MultiVariableAssignment([<1:1>, mut <4:3> <8:1>], Literal(Int(<12:1>)))
        ",
    );
}

#[test_log::test]
fn compound_sub() {
    check(
        r"
a -= 3
",
        r"
VariableCompoundAssignment(<1:1>, CompoundOperator { node: <3:2>, kind: Sub }, Literal(Int(<6:1>)))
        ",
    );
}

#[test_log::test]
fn compound_div() {
    check(
        r"
a /= 3
",
        r"
VariableCompoundAssignment(<1:1>, CompoundOperator { node: <3:2>, kind: Div }, Literal(Int(<6:1>)))
        ",
    );
}

#[test_log::test]
fn compound_mul() {
    check(
        r"
a *= 3
",
        r"
VariableCompoundAssignment(<1:1>, CompoundOperator { node: <3:2>, kind: Mul }, Literal(Int(<6:1>)))
        ",
    );
}

#[test_log::test]
fn compound_remainder() {
    check(
        r"
a %= 3
",
        r"
If(UnaryOp(Not(<8:1>), MemberCall(FieldAccess(VariableAccess(<9:5>), <15:4>), <20:10>, [VariableAccess(<31:9>)])), Literal(Int(<42:1>)), Some(Literal(Int(<49:1>))))
        ",
    );
}

#[test_log::test]
fn index_compound_remainder() {
    check(
        r"
a[1] %= 3
",
        r"
If(UnaryOp(Not(<8:1>), MemberCall(FieldAccess(VariableAccess(<9:5>), <15:4>), <20:10>, [VariableAccess(<31:9>)])), Literal(Int(<42:1>)), Some(Literal(Int(<49:1>))))
        ",
    );
}

#[test_log::test]
fn index_compound_add() {
    check(
        r"
a[i] += 3
",
        r"
IndexCompoundAssignment(VariableAccess(<1:1>), VariableAccess(<3:1>), CompoundOperator { node: <6:2>, kind: Add }, Literal(Int(<9:1>)))
        ",
    );
}

#[test_log::test]
fn index_compound_sub() {
    check(
        r"
a[i] -= 3
",
        r"
IndexCompoundAssignment(VariableAccess(<1:1>), VariableAccess(<3:1>), CompoundOperator { node: <6:2>, kind: Sub }, Literal(Int(<9:1>)))
        ",
    );
}

#[test_log::test]
fn index_compound_mul() {
    check(
        r"
a[i] *= 3
",
        r"
IndexCompoundAssignment(VariableAccess(<1:1>), VariableAccess(<3:1>), CompoundOperator { node: <6:2>, kind: Mul }, Literal(Int(<9:1>)))
        ",
    );
}

#[test_log::test]
fn index_compound_div() {
    check(
        r"
a[i] /= 3
",
        r"
IndexCompoundAssignment(VariableAccess(<1:1>), VariableAccess(<3:1>), CompoundOperator { node: <6:2>, kind: Div }, Literal(Int(<9:1>)))
        ",
    );
}

#[test_log::test]
fn neq() {
    check(
        r"
a != 5
",
        r"
BinaryOp(VariableAccess(<1:1>), NotEqual(<3:2>), Literal(Int(<6:1>)))
        ",
    );
}

#[test_log::test]
fn lte() {
    check(
        r"
a <= 5
",
        r"
BinaryOp(VariableAccess(<1:1>), LessEqual(<3:2>), Literal(Int(<6:1>)))
        ",
    );
}

#[test_log::test]
fn gte() {
    check(
        r"
a >= 5
",
        r"
BinaryOp(VariableAccess(<1:1>), GreaterEqual(<3:2>), Literal(Int(<6:1>)))
        ",
    );
}

#[test_log::test]
fn interpolate() {
    check(
        r"
'{a:X} {b:b}  '
",
        r"
InterpolatedString([Interpolation(VariableAccess(<3:1>), Some(UpperHex(<5:1>))), Literal(<7:1>), Interpolation(VariableAccess(<9:1>), Some(Binary(<11:1>))), Literal(<13:2>)])
        ",
    );
}

#[test_log::test]
fn generic() {
    check(
        r"
struct Something {
    sparse: Sparse<Int>
}
",
        r"
StructDef(StructType { identifier: LocalTypeIdentifier(<8:9>), fields: [FieldType { field_name: FieldName(<24:6>), field_type: Generic(TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<32:6>), module_path: None }), [Int(<39:3>)]) }] })

        ",
    );
}

#[test_log::test]
fn optional() {
    check(
        r"
struct Hello {
    first_name: String?,
}
",
        r"
StructDef(StructType { identifier: LocalTypeIdentifier(<8:5>), fields: [FieldType { field_name: FieldName(<20:10>), field_type: Optional(String(<32:6>), <38:1>) }] })

        ",
    );
}

#[test_log::test]
fn pattern_list() {
    check(
        r"
match x {
    a, b => print(a)
}
",
        r"
Match(VariableAccess(<7:1>), [MatchArm { pattern: PatternList([Variable(<15:1>), Variable(<18:1>)]), expression: FunctionCall(VariableAccess(<23:5>), [VariableAccess(<29:1>)]) }])
        ",
    );
}

#[test_log::test]
fn array_type() {
    check(
        r"
fn something(a: Int) -> [Bool] {
    [true, false]
}
",
        r"
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:9>, params: [Parameter { variable: <14:1>, param_type: Int(<17:3>) }], self_parameter: None, return_type: Some(Array(Bool(<26:4>))) }, body: Block([Literal(Array([Literal(Bool(<39:4>)), Literal(Bool(<45:5>))]))]) }))
        ",
    );
}

#[test_log::test]
fn multi_var() {
    check(
        r"
a, b = 3
",
        r"
MultiVariableAssignment([<1:1>, <4:1>], Literal(Int(<8:1>)))
        ",
    );
}

#[test_log::test]
fn multi_var_assign() {
    check(
        r"
a = b = c = 3
",
        r"
VariableAssignment(<1:1>, VariableAssignment(<5:1>, VariableAssignment(<9:1>, Literal(Int(<13:1>)))))
        ",
    );
}

#[test_log::test]
fn mut_arg() {
    check(
        r"
fn write(mut x: Int) {
    x = 4
}
",
        r"
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:5>, params: [Parameter { variable: mut <10:3> <14:1>, param_type: Int(<17:3>) }], self_parameter: None, return_type: None }, body: Block([VariableAssignment(<28:1>, Literal(Int(<32:1>)))]) }))

        ",
    );
}

#[test_log::test]
fn mut_static_call() {
    check(
        r"
Something::update(a, mut b)
",
        r"
StaticCall(<1:9>, <12:6>, [VariableAccess(<19:1>), MutRef(Variable(<26:1>))])
        ",
    );
}

#[test_log::test]
fn mut_location_field() {
    check(
        r"
something(mut field.a)
",
        r"
FunctionCall(VariableAccess(<1:9>), [MutRef(FieldAccess(VariableAccess(<15:5>), <21:1>))])
        ",
    );
}

#[test_log::test]
fn none() {
    check(
        r"
call(none)
",
        r"
FunctionCall(VariableAccess(<1:4>), [Literal(None(<6:4>))])
        ",
    );
}

#[test_log::test]
fn unit() {
    check(
        r"
fn func() -> Sparse<()> {
    ()
}
",
        r"
StaticCall(<1:9>, <12:6>, [VariableAccess(<19:1>), Mut VariableAccess(<26:1>)])
        ",
    );
}

#[test_log::test]
fn module_path_reference() {
    check(
        r"
fn func() -> module::something::TypeReference {
    ()
}
",
        r"
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:4>, params: [], self_parameter: None, return_type: Some(TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<33:13>), module_path: Some(ModulePath([ModulePathItem { node: <14:6> }, ModulePathItem { node: <22:9> }])) })) }, body: Block([Literal(Unit)]) }))

        ",
    );
}
