/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::util::check;

mod util;

/*
#[test_log::test]
fn alias() {
    let script = r#"
    alias Velocity = Int"
     "#;
    check(&script, r#""#);
}

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
    check("a = 3", "Let(VariableAssignment(a), Literal(Int(3)))");
}

#[test_log::test]
fn assignment_add() {
    check(
        "a = n + 6",
        "Let(VariableAssignment(a), BinaryOp(VariableAccess(n), Add, Literal(Int(6))))",
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
        &script,
        r#"
FunctionDef(add, [x: Int, y: Int], Int, [Expression(BinaryOp(VariableAccess(x), Add, VariableAccess(y)))])
---
Let(VariableAssignment(result), FunctionCall(VariableAccess(add), [Literal(Int(10)), Literal(Int(20))]))
    "#,
    );
}

#[test_log::test]
fn struct_def() {
    let script = "
            struct Person { first_field: Int, second_field: String }
        ";
    check(
        script,
        "StructDef(StructType { identifier: Person, fields: SeqMap(first_field: Int, second_field: String) })",
    )
}

#[test_log::test]
fn struct_init() {
    let script = "
            person = Person { first_field: 1, second_field: \"Bob\" }
        ";
    check(
        script,
        "Let(VariableAssignment(person), StructInstantiation(Person, SeqMap(first_field: Literal(Int(1)), second_field: Literal(String(Bob)))))",
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
        Let(VariableAssignment(a), Literal(Int(0)))
        WhileLoop(BinaryOp(VariableAccess(a), LessThan, Literal(Int(3))), [Expression(FunctionCall(VariableAccess(print), [VariableAccess(a)])), Let(VariableAssignment(a), BinaryOp(VariableAccess(a), Add, Literal(Int(1))))])
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
        "Let(VariableAssignment(c), IfElse(Literal(Bool(true)), VariableAccess(a), VariableAccess(b)))\n",
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
StructDef(StructType { identifier: Person, fields: SeqMap(first_field: Int, second_field: String) })
---
Let(VariableAssignment(person), StructInstantiation(Person, SeqMap(first_field: Literal(Int(1)), second_field: Literal(String(Bob)))))
    "#,
    );
}

#[test_log::test]
fn nested_function_calls() {
    check(
        "result = add(mul(2, 3), div(10, 2))",
        "Let(VariableAssignment(result), FunctionCall(VariableAccess(add), [FunctionCall(VariableAccess(mul), [Literal(Int(2)), Literal(Int(3))]), FunctionCall(VariableAccess(div), [Literal(Int(10)), Literal(Int(2))])]))",
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

        Let(VariableAssignment(result), BinaryOp(BinaryOp(BinaryOp(Literal(Int(2)), Add, Literal(Int(3))), Multiply, Literal(Int(4))), Subtract, BinaryOp(Literal(Int(10)), Divide, Literal(Int(2)))))
Let(VariableAssignment(flag), BinaryOp(BinaryOp(Literal(Bool(true)), LogicalAnd, UnaryOp(Not, Literal(Bool(false)))), LogicalOr, BinaryOp(Literal(Int(5)), GreaterThan, Literal(Int(3)))))
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
Expression(UnaryOp(Not, VariableAccess(x)))
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
        r#"
        Let(VariableAssignment(person), StructInstantiation(Person, SeqMap(first_field: Literal(Int(1)), second_field: Literal(String(Bob)))))
        Let(VariableAssignment(name), FieldAccess(VariableAccess(person), second_field))
    "#,
    );
}

#[test_log::test]
fn struct_field_access() {
    let script = "
        name = person.second_field
    ";
    check(
        script,
        "Let(VariableAssignment(name), FieldAccess(VariableAccess(person), second_field))",
    );
}

#[test_log::test]
fn struct_field_assignment_chain() {
    let script = "
        a.b.c.d = 3
    ";
    check(
        script,
        "[field_assignment [field_access [field_access [variable_access a] b] c] <- d = 3]",
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
[variable_assignment a <- [10, 20]]
[index_access [variable_access a][1]]"#,
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

[variable_assignment a <- [10, 20]]
[member_call [variable_access a].remove(99)]

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
WhileLoop(BinaryOp(BinaryOp(VariableAccess(x), GreaterThan, Literal(Int(0))), LogicalAnd, BinaryOp(VariableAccess(y), LessThan, Literal(Int(10)))), [Let(VariableAssignment(x), BinaryOp(VariableAccess(x), Subtract, Literal(Int(1)))), Let(VariableAssignment(y), BinaryOp(VariableAccess(y), Add, Literal(Int(1))))])
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
  Let(VariableAssignment(x), Literal(Int(0)))
WhileLoop(BinaryOp(VariableAccess(x), LessThan, Literal(Int(3))), [Let(VariableAssignment(y), Literal(Int(0))), WhileLoop(BinaryOp(VariableAccess(y), LessThan, Literal(Int(2))), [Expression(FunctionCall(VariableAccess(print), [BinaryOp(VariableAccess(x), Add, VariableAccess(y))])), Let(VariableAssignment(y), BinaryOp(VariableAccess(y), Add, Literal(Int(1))))]), Let(VariableAssignment(x), BinaryOp(VariableAccess(x), Add, Literal(Int(1))))])
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
        r#"
StructDef(StructType { identifier: Point, fields: SeqMap(x: Int, y: Int) })
---
Let(VariableAssignment(p1), StructInstantiation(Point, SeqMap(x: Literal(Int(5)), y: Literal(Int(10)))))
Let(VariableAssignment(dist), FunctionCall(VariableAccess(add), [FunctionCall(VariableAccess(mul), [FieldAccess(VariableAccess(p1), x), FieldAccess(VariableAccess(p1), x)]), FunctionCall(VariableAccess(mul), [FieldAccess(VariableAccess(p1), y), FieldAccess(VariableAccess(p1), y)])]))

    "#,
    );
}

#[test_log::test]
fn small_call() {
    let script = "
        mul(2)
    ";
    check(
        script,
        "Expression(FunctionCall(VariableAccess(mul), [Literal(Int(2))]))",
    );
}

#[test_log::test]
fn small_chain() {
    let script = "
        p1.x.mul(2)
    ";
    check(
        &script,
        "Expression(MemberCall(FieldAccess(VariableAccess(p1), x), mul, [Literal(Int(2))]))",
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
    Expression(MemberCall(MemberCall(FieldAccess(VariableAccess(p1), x), mul, [FieldAccess(VariableAccess(p1), x)]), add, [MemberCall(FieldAccess(VariableAccess(p1), y), mul, [FieldAccess(VariableAccess(p1), y)])]))
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
StructDef(StructType { identifier: Point, fields: SeqMap(x: Int, y: Int) })
---
Let(VariableAssignment(p1), StructInstantiation(Point, SeqMap(x: Literal(Int(5)), y: Literal(Int(10)))))
Let(VariableAssignment(dist), MemberCall(MemberCall(FieldAccess(VariableAccess(p1), x), mul, [FieldAccess(VariableAccess(p1), x)]), add, [MemberCall(FieldAccess(VariableAccess(p1), y), mul, [FieldAccess(VariableAccess(p1), y)])]))

    "#,
    );
}

#[test_log::test]
fn function_definition() {
    let script = r#"
        fn add(x: Int, y: Int) -> Int {
            x + y
        }
    "#;

    check(
        &script,
        r#"
FunctionDef(add, [x: Int, y: Int], Int, [Expression(BinaryOp(VariableAccess(x), Add, VariableAccess(y)))])
    "#,
    );
}

#[test_log::test]
fn function_with_no_parameters() {
    let script = r#"
        fn add() -> Int {
            42
        }
    "#;

    check(
        &script,
        r#"
FunctionDef(add, [], Int, [Expression(Literal(Int(42)))])
    "#,
    );
}

#[test_log::test]
fn function_with_no_parameters_return() {
    let script = r#"
        fn add() -> Int {
            return 42
        }
    "#;

    check(
        &script,
        r#"
FunctionDef(add, [], Int, [Return(Literal(Int(42)))])
    "#,
    );
}

#[test_log::test]
fn function_call_with_no_parameters() {
    let script = r#"
        fn single() -> Int {
            42
        }
        result = single()
    "#;

    check(
        &script,
        r#"
FunctionDef(single, [], Int, [Expression(Literal(Int(42)))])
---
Let(VariableAssignment(result), FunctionCall(VariableAccess(single), []))
    "#,
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
Let(VariableAssignment(x), Array([Literal(Int(1)), Literal(Int(2)), Literal(Int(3))]))
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
Let(VariableAssignment(x), Array([]))
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
Expression(Array([]))
    "#,
    );
}

#[test_log::test]
fn empty_array_expression_call() {
    let script = r#"
        [].len()
    "#;

    check(
        &script,
        r#"
Expression(MemberCall(Array([]), len, []))
        "#,
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
Expression(MemberCall(Literal(String(hello, world)), len, []))
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
Expression(MemberCall(Literal(Float(2.2)), round, []))
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
Expression(UnaryOp(Negate, Literal(Float(2.2))))
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
Expression(UnaryOp(Negate, MemberCall(Literal(Float(2.2)), round, [])))
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
Expression(Literal(Float(2.2)))
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
Expression(Literal(String(hello)))
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
Expression(Literal(Tuple([Literal(String(hello)), Literal(Int(1)), Literal(Float(2.2))])))
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
        r#"
FunctionDef(some_tuple, [], Tuple([String, Int, Float]), [Expression(Literal(Tuple([Literal(String(hello)), Literal(Int(1)), Literal(Float(2.2))])))])
        "#,
    );
}

#[test_log::test]
fn enum_type() {
    let script = r#"
    enum Custom {
        Idle,
        Running(Int, Float),
        Sleeping { hours: Int },
    }
    "#;
    check(
        &script,
        r#"
EnumDef(Custom, SeqMap(Idle: , Running: [Int, Float], Sleeping: SeqMap(hours: Int)))
        "#,
    );
}

#[test_log::test]
fn enum_type2() {
    let script = r#"
    enum Custom {
        Idle { one: Int, two: (Int, Float) },
        Running(Int, Float, String, Int),
    }
    "#;
    check(
        &script,
        r#"

EnumDef(Custom, SeqMap(Idle: SeqMap(one: Int, two: Tuple([Int, Float])), Running: [Int, Float, String, Int]))

        "#,
    );
}

#[test_log::test]
fn match_expression() {
    let script = r#"
        enum Custom {
            Idle,
            Running(Int, Float),
            Sleeping { hours: Int },
        }


        v = match state {
            Running(speed, _) => speed,
            Sleeping { hours } => hours + 10,
            _ => 0,
        }

"#;

    check(
        &script,
        r#"

EnumDef(Custom, SeqMap(Idle: , Running: [Int, Float], Sleeping: SeqMap(hours: Int)))
---
Let(VariableAssignment(v), Match(VariableAccess(state), [MatchArm { pattern: EnumTuple(Running, [speed, _]), expression: VariableAccess(speed) }, MatchArm { pattern: EnumStruct(Sleeping, [hours]), expression: BinaryOp(VariableAccess(hours), Add, Literal(Int(10))) }, MatchArm { pattern: Wildcard, expression: Literal(Int(0)) }]))

"#,
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
        r#"
Expression(Match(VariableAccess(state), [MatchArm { pattern: Wildcard, expression: Literal(Int(0)) }]))
"#,
    );
}

#[test_log::test]
fn match_expression_minimal_two_arms() {
    let script = r#"
       match state {
            EnumType(ident, _) => ident,
            _ => 0,
        }
"#;

    check(
        &script,
        r#"
Expression(Match(VariableAccess(state), [MatchArm { pattern: EnumTuple(EnumType, [ident, _]), expression: VariableAccess(ident) }, MatchArm { pattern: Wildcard, expression: Literal(Int(0)) }]))
"#,
    );
}

#[test_log::test]
fn match_expression_minimal_two_arms_enum_struct() {
    let script = r#"
       match state {
            EnumType { something, another } => another,
            _ => 0,
        }
"#;

    check(
        &script,
        r#"
Expression(Match(VariableAccess(state), [MatchArm { pattern: EnumStruct(EnumType, [something, another]), expression: VariableAccess(another) }, MatchArm { pattern: Wildcard, expression: Literal(Int(0)) }]))
"#,
    );
}

#[test_log::test]
fn deconstructing_struct() {
    let script = "{  x, y } = pos"; // No raw string literal
    check(&script, "Let(Struct([x, y]), VariableAccess(pos))");
}

#[test_log::test]
fn deconstructing_tuple() {
    let script = "(x, y) = pos"; // No raw string literal
    check(&script, "Let(Tuple([x, y]), VariableAccess(pos))");
}

#[test_log::test]
fn operator_precedence() {
    let script = "z = y * 2 - x";
    check(&script, "Let(VariableAssignment(z), BinaryOp(BinaryOp(VariableAccess(y), Multiply, Literal(Int(2))), Subtract, VariableAccess(x)))");
}

#[test_log::test]
fn operator_precedence_expression() {
    let script = "y * 2 - x";
    check(
        &script,
        "
Expression(BinaryOp(BinaryOp(VariableAccess(y), Multiply, Literal(Int(2))), Subtract, VariableAccess(x)))",
    );
}

#[test_log::test]
fn for_range() {
    let script = r#"
for x in 1..10 {
}

    "#;
    check(
        &script,
        "
ForLoop(VariableAssignment(x), ExclusiveRange(Literal(Int(1)), Literal(Int(10))), [])",
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
Expression(ExclusiveRange(Literal(Int(0)), Literal(Int(39))))",
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
ForLoop(VariableAssignment(x), Array([Literal(Int(1)), Literal(Int(2)), Literal(Int(3))]), [])",
    );
}

#[test_log::test]
fn enum_literal_basic() {
    check(
        r#"
        state = State::Running

    "#,
        r#"

Let(VariableAssignment(state), Literal(EnumVariant(State::Running)))

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

EnumDef(State, SeqMap(Running: , Stopped: ))
---
Let(VariableAssignment(state), Literal(EnumVariant(State::Running)))
Expression(FunctionCall(VariableAccess(print), [VariableAccess(state)]))

        "#,
    );
}

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
FunctionDef(increment, [mut x: Int], Int, [Let(VariableAssignment(x), BinaryOp(VariableAccess(x), Add, Literal(Int(1)))), Expression(VariableAccess(x))])",
    );
}

#[test_log::test]
fn mut_let() {
    check(
        "mut x = 3",
        "Let(VariableAssignment(mut x), Literal(Int(3)))",
    );
}

#[test_log::test]
fn import() {
    check(
        "import math",
        r#"Import(Import { module_path: ModulePath([LocalIdentifier("math")]), items: Module })"#,
    );
}

#[test_log::test]
fn impl_def() {
    check(
        r#"
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
        "#,
        r#"

ImplDef(SomeTypeName, SeqMap(something: Member(self, [], Int, [Expression(FieldAccess(VariableAccess(self), x))]), another: Member(mut self, [v: Int], Int, [Expression(FieldAssignment(VariableAccess(self), x, Literal(Int(3))))]), no_self_here: Member(self, [], Float, [Expression(Literal(Float(3.2)))])))

        "#,
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
        r#"

Expression(Match(VariableAccess(x), [MatchArm { pattern: Literal(Int(5)), expression: Literal(String(five)) }, MatchArm { pattern: Literal(String(hello)), expression: Literal(String(greeting)) }, MatchArm { pattern: Literal(Bool(true)), expression: Literal(String(yes)) }, MatchArm { pattern: Wildcard, expression: Literal(String(something else)) }]))

        "#,
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

Expression(FunctionCall(VariableAccess(print), [Literal(String(hello))]))
Expression(FunctionCall(VariableAccess(print), [Literal(String(world))]))

        "#,
    );
}

#[test_log::test]
fn multiple_assignments() {
    check(
        r#"
        x = y = z = 10
        "#,
        r#"
Let(VariableAssignment(x), VariableAssignment(VariableAccess(y), VariableAssignment(VariableAccess(z), Literal(Int(10)))))
        "#,
    );
}

#[test_log::test]
fn enum_variant_construction() {
    check(
        r#"

        shape = Shape::Rectangle { width: 10, height: 20 }

        "#,
        "Let(VariableAssignment(shape), Literal(EnumVariant(Shape::Rectangle{ width: Literal(Int(10)), height: Literal(Int(20)) })))",
    );
}

#[test_log::test]
fn enum_variant_tuple_construction() {
    check(
        r#"
        shape = Shape::Something(2, 4.4)
        "#,
        r#"
Let(VariableAssignment(shape), Literal(EnumVariant(Shape::Something([Literal(Int(2)), Literal(Float(4.4))]))))
        "#,
    );
}

#[test_log::test]
fn array_access() {
    check(
        r#"
        arr[3]
        "#,
        "Expression(ArrayAccess(VariableAccess(arr), Literal(Int(3))))",
    );
}

#[test_log::test]
fn array_set() {
    check(
        r#"
        arr[3] = 42
        "#,
        "Expression(ArrayAssignment(VariableAccess(arr), Literal(Int(3)), Literal(Int(42))))",
    );
}

#[test_log::test]
fn string_interpolation_basic_spaces() {
    check(
        "'   this is interpolated {x}   with hex  {y}  '",
        r#"
Expression(InterpolatedString([Literal("   this is interpolated "), Interpolation(VariableAccess(x), None), Literal("   with hex  "), Interpolation(VariableAccess(y), None), Literal("  ")]))
    "#,
    );
}

#[test_log::test]
fn string_interpolation() {
    check(
        "'this is interpolated {x} with hex {y:x}'",
        r#"
    Expression(InterpolatedString([Literal("this is interpolated "), Interpolation(VariableAccess(x), None), Literal(" with hex "), Interpolation(VariableAccess(y), Some(LowerHex))]))
    "#,
    );
}

#[test_log::test]
fn string_interpolation_call() {
    check(
        "'this is interpolated {x:x}    with hex  {mul(a, 2)}'",
        r#"
Expression(InterpolatedString([Literal("this is interpolated "), Interpolation(VariableAccess(x), Some(LowerHex)), Literal("    with hex  "), Interpolation(FunctionCall(VariableAccess(mul), [VariableAccess(a), Literal(Int(2))]), None)]))
    "#,
    );
}

#[test_log::test]
fn string_interpolation_call_simple() {
    check(
        "'result: {mul(a,2)}'",
        r#"
Expression(InterpolatedString([Literal("result: "), Interpolation(FunctionCall(VariableAccess(mul), [VariableAccess(a), Literal(Int(2))]), None)]))
    "#,
    );
}

#[test_log::test]
fn string_interpolation_simple() {
    check(
        "'this is interpolated {x}'",
        r#"
Expression(InterpolatedString([Literal("this is interpolated "), Interpolation(VariableAccess(x), None)]))
    "#,
    );
}

#[test_log::test]
fn string_interpolation_simple_no_space() {
    check(
        "'this is interpolated{x}'",
        r#"
Expression(InterpolatedString([Literal("this is interpolated"), Interpolation(VariableAccess(x), None)]))
    "#,
    );
}

#[test_log::test]
fn function_call_mul() {
    check(
        "mul(a, 2)",
        r#"
Expression(FunctionCall(VariableAccess(mul), [VariableAccess(a), Literal(Int(2))]))
    "#,
    );
}

#[test_log::test]
fn enum_match() {
    check(
        r#"
        msg = match result {
            Ok(value) => value,
            Err(err) => err
        }
        "#,
        "Let(VariableAssignment(msg), Match(VariableAccess(result), [MatchArm { pattern: EnumTuple(Ok, [value]), expression: VariableAccess(value) }, MatchArm { pattern: EnumTuple(Err, [err]), expression: VariableAccess(err) }]))",
    );
}

#[test_log::test]
fn enum_match_simple() {
    check(
        r#"
        msg = match result {
            Ok(value) => value,
            Err(err) => err,
            Simple => 2
        }
        "#,
        "Let(VariableAssignment(msg), Match(VariableAccess(result), [MatchArm { pattern: EnumTuple(Ok, [value]), expression: VariableAccess(value) }, MatchArm { pattern: EnumTuple(Err, [err]), expression: VariableAccess(err) }, MatchArm { pattern: EnumSimple(Simple), expression: Literal(Int(2)) }]))",
    );
}

#[test_log::test]
fn enum_match_wildcard() {
    check(
        r#"
        msg = match result {
            Ok(value) => value,
            Err(err) => err,
            Simple => 2,
            _ => 99
        }
        "#,
        "Let(VariableAssignment(msg), Match(VariableAccess(result), [MatchArm { pattern: EnumTuple(Ok, [value]), expression: VariableAccess(value) }, MatchArm { pattern: EnumTuple(Err, [err]), expression: VariableAccess(err) }, MatchArm { pattern: EnumSimple(Simple), expression: Literal(Int(2)) }, MatchArm { pattern: Wildcard, expression: Literal(Int(99)) }]))",
    );
}

#[test_log::test]
fn enum_match_with_wildcard() {
    check(
        r#"
        msg = match result {
            Ok(value) => value,
            _ => 99
        }
        "#,
        "Let(VariableAssignment(msg), Match(VariableAccess(result), [MatchArm { pattern: EnumTuple(Ok, [value]), expression: VariableAccess(value) }, MatchArm { pattern: Wildcard, expression: Literal(Int(99)) }]))",
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
        r#"

EnumDef(LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 15, line: 3, column: 14 }, end: Position { offset: 21, line: 3, column: 20 } } }, text: "Action" }, SeqMap(LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 36, line: 4, column: 13 }, end: Position { offset: 43, line: 4, column: 20 } } }, text: "Jumping" }: , LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 57, line: 5, column: 13 }, end: Position { offset: 82, line: 5, column: 38 } } }, text: "Target" }: AnonymousStruct { fields: SeqMap(IdentifierName("x"): Int, IdentifierName("y"): Int) }, LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 96, line: 6, column: 13 }, end: Position { offset: 109, line: 6, column: 26 } } }, text: "Other" }: [String]))
---
Expression(VariableAssignment(action, Literal(EnumVariant(Action::Target{ LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 139, line: 9, column: 18 }, end: Position { offset: 171, line: 9, column: 50 } } }, text: "x" }: Literal(Int(42)), LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 139, line: 9, column: 18 }, end: Position { offset: 171, line: 9, column: 50 } } }, text: "y" }: Literal(Int(-999)) }))))

Expression(Match(VariableAccess(action), [MatchArm { pattern: EnumPattern(LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 300, line: 18, column: 13 }, end: Position { offset: 308, line: 18, column: 21 } } }, text: "Jumping" }, None), expression: Literal(String(jumping)) }, MatchArm { pattern: EnumPattern(LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 334, line: 19, column: 13 }, end: Position { offset: 343, line: 19, column: 22 } } }, text: "Target" }, Some([Variable(LocalIdentifier { node: Node { span: Span { start: Position { offset: 334, line: 19, column: 13 }, end: Position { offset: 343, line: 19, column: 22 } } }, text: "y" })])), expression: VariableAccess(y) }, MatchArm { pattern: PatternList([Wildcard]), expression: Literal(String(can not find it!)) }]))

        "#,
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
        r#"

    Expression(Match(VariableAccess(v), [MatchArm { pattern: EnumTuple(Tuple, [i, s, b]), expression: Block([Expression(FunctionCall(VariableAccess(print), [Literal(String(Tuple:))])), Expression(FunctionCall(VariableAccess(print), [VariableAccess(i)])), Expression(FunctionCall(VariableAccess(print), [VariableAccess(s)])), Expression(FunctionCall(VariableAccess(print), [VariableAccess(b)]))]) }]))

    "#,
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
        "If(BinaryOp(VariableAccess(x), GreaterThan, Literal(Int(41))), [Expression(Literal(String(x is greater than 41)))], None)",
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
        r#"

ForLoop(VariableAssignment(i), ExclusiveRange(Literal(Int(1)), Literal(Int(5))), [If(BinaryOp(BinaryOp(VariableAccess(i), Modulo, Literal(Int(2))), Equal, Literal(Int(0))), [Continue], None), Expression(FunctionCall(VariableAccess(print), [Literal(String(no continue))]))])

    "#,
    )
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

            If(BinaryOp(VariableAccess(x), LessThan, Literal(Int(0))), [Return(Literal(String(negative)))], Some([If(BinaryOp(VariableAccess(x), Equal, Literal(Int(0))), [Return(Literal(String(positive)))], None)]))

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
Expression(OptionOperator(VariableAccess(a)))

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

Expression(BinaryOp(VariableAccess(b), Add, OptionOperator(VariableAccess(a))))

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

If(OptionOperator(VariableAccess(a)), [Expression(InterpolatedString([Literal("this is "), Interpolation(VariableAccess(a), None)]))], Some([Expression(InterpolatedString([Literal("not here")]))]))

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

If(OptionOperator(BinaryOp(BinaryOp(BinaryOp(VariableAccess(b), Multiply, Literal(Int(3))), Add, Literal(Int(99))), Add, MemberCall(VariableAccess(something), LocalIdentifier { node: Node { span: Span { start: Position { offset: 21, line: 2, column: 21 }, end: Position { offset: 39, line: 2, column: 39 } } }, text: "call" }, [Literal(Int(42))]))), [Expression(InterpolatedString([Literal("expression is something")]))], Some([Expression(InterpolatedString([Literal("must be none")]))]))

            "#,
    )
}

#[test_log::test]
fn option_operator_assignment() {
    check(
        r#"
         a = another.get_current()?
            "#,
        r#"

Let(VariableAssignment(a), OptionOperator(MemberCall(VariableAccess(another), LocalIdentifier { node: Node { span: Span { start: Position { offset: 14, line: 2, column: 14 }, end: Position { offset: 49, line: 3, column: 13 } } }, text: "get_current" }, [])))

            "#,
    )
}

#[test_log::test]
fn option_operator_assignment_chained() {
    check(
        r#"
         a = another.get_current()?.another_call(b, 42)?
            "#,
        r#"

Let(VariableAssignment(a), OptionOperator(MemberCall(OptionOperator(MemberCall(VariableAccess(another), LocalIdentifier { node: Node { span: Span { start: Position { offset: 14, line: 2, column: 14 }, end: Position { offset: 70, line: 3, column: 13 } } }, text: "get_current" }, [])), LocalIdentifier { node: Node { span: Span { start: Position { offset: 14, line: 2, column: 14 }, end: Position { offset: 70, line: 3, column: 13 } } }, text: "another_call" }, [VariableAccess(b), Literal(Int(42))])))

            "#,
    )
}

#[test_log::test]
fn option_operator_if_let_expression() {
    check(
        r#"
         if a = another.get_current()? {
               'this is {a}'
         } else {
            'must be none'
         }
            "#,
        r#"

If(VariableAssignment(a, OptionOperator(MemberCall(VariableAccess(another), LocalIdentifier { node: Node { span: Span { start: Position { offset: 17, line: 2, column: 17 }, end: Position { offset: 40, line: 2, column: 40 } } }, text: "get_current" }, []))), [Expression(InterpolatedString([Literal("this is "), Interpolation(VariableAccess(a), None)]))], Some([Expression(InterpolatedString([Literal("must be none")]))]))
            "#,
    )
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

If(VariableAssignment(a, OptionOperator(MemberCall(OptionOperator(MemberCall(VariableAccess(another), LocalIdentifier { node: Node { span: Span { start: Position { offset: 17, line: 2, column: 17 }, end: Position { offset: 61, line: 2, column: 61 } } }, text: "get_current" }, [])), LocalIdentifier { node: Node { span: Span { start: Position { offset: 17, line: 2, column: 17 }, end: Position { offset: 61, line: 2, column: 61 } } }, text: "another_call" }, [VariableAccess(b), Literal(Int(42))]))), [Expression(InterpolatedString([Literal("this is "), Interpolation(VariableAccess(a), None)]))], Some([Expression(InterpolatedString([Literal("must be none")]))]))

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

Let(VariableAssignment(a), Literal(none))

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

Let(VariableAssignment(a), Literal(Int(3)))
Let(VariableAssignment(c), IfElse(VariableAssignment(b, BinaryOp(VariableAccess(a), GreaterThan, Literal(Int(3)))), BinaryOp(VariableAccess(b), Add, Literal(Int(1))), BinaryOp(VariableAccess(b), Add, Literal(Int(2)))))


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
        r#"

StructDef(StructType { identifier: LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 3, line: 2, column: 3 }, end: Position { offset: 44, line: 4, column: 5 } } }, text: "Struct" }, fields: SeqMap(IdentifierName("some_field"): Optional(Int)) })


            "#,
    );
}

#[test_log::test]
fn map_literal() {
    check(
        "
  a = [2: 'Hello', 3: 'World']

            ",
        r#"

Let(VariableAssignment(a), Literal(Map([(Literal(Int(2)), InterpolatedString([Literal("Hello")])), (Literal(Int(3)), InterpolatedString([Literal("World")]))])))

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

Let(VariableAssignment(a), Literal(Map([(Literal(Int(2)), InterpolatedString([Literal("Hello")])), (Literal(Int(3)), InterpolatedString([Literal("World")]))])))

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
        r#"
FunctionDef(LocalIdentifier { node: Node { span: Span { start: Position { offset: 1, line: 2, column: 1 }, end: Position { offset: 35, line: 2, column: 35 } } }, text: "map_creator" }, Internal(FunctionSignature { name: LocalIdentifier { node: Node { span: Span { start: Position { offset: 1, line: 2, column: 1 }, end: Position { offset: 35, line: 2, column: 35 } } }, text: "map_creator" }, params: [], return_type: Map(Int, String) }, [Expression(Literal(Map([(Literal(Int(2)), InterpolatedString([Literal("hello")])), (Literal(Int(-1)), InterpolatedString([Literal("world")]))])))]))


            "#,
    );
}

#[test_log::test]
fn int_fn() {
    check(
        "
fn test() -> Int { 42 }

            ",
        r#"
FunctionDef(LocalIdentifier { node: Node { span: Span { start: Position { offset: 1, line: 2, column: 1 }, end: Position { offset: 18, line: 2, column: 18 } } }, text: "test" }, Internal(FunctionSignature { name: LocalIdentifier { node: Node { span: Span { start: Position { offset: 1, line: 2, column: 1 }, end: Position { offset: 18, line: 2, column: 18 } } }, text: "test" }, params: [], return_type: Int }, [Expression(Literal(Int(42)))]))


            "#,
    );
}

#[test_log::test]
fn generic_type() {
    check(
        "
fn nothing() -> SomeType<Int, Float> {
}

            ",
        r#"
FunctionDef(LocalIdentifier { node: Node { span: Span { start: Position { offset: 1, line: 2, column: 1 }, end: Position { offset: 38, line: 2, column: 38 } } }, text: "nothing" }, Internal(FunctionSignature { name: LocalIdentifier { node: Node { span: Span { start: Position { offset: 1, line: 2, column: 1 }, end: Position { offset: 38, line: 2, column: 38 } } }, text: "nothing" }, params: [], return_type: Generic(TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 17, line: 2, column: 17 }, end: Position { offset: 25, line: 2, column: 25 } } }, text: "SomeType" }, module_path: None }), [Int, Float]) }, []))
            "#,
    );
}

#[test_log::test]
fn sparse_map_static_call() {
    check(
        r#"
        result = SparseMap<Int>::new()
        "#,
        r#"

        Let(VariableAssignment(result), StaticCallGeneric(LocalTypeIdentifier { node: Node { span: Span { start: Position { offset: 18, line: 2, column: 18 }, end: Position { offset: 27, line: 2, column: 27 } } }, text: "SparseMap" }, LocalIdentifier { node: Node { span: Span { start: Position { offset: 34, line: 2, column: 34 }, end: Position { offset: 37, line: 2, column: 37 } } }, text: "new" }, [], [Int]))
        "#,
    );
}

#[test_log::test]
fn assignment_op_add() {
    check(
        "a += 6",
        "Expression(VariableCompoundAssignment(a, Add, Literal(Int(6))))",
    );
}
