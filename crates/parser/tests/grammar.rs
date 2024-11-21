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
    check("a = 3", "Let(Variable(a), Literal(Int(3)))");
}

#[test_log::test]
fn assignment_add() {
    check(
        "a = n + 6",
        "Let(Variable(a), BinaryOp(Variable(n), Add, Literal(Int(6))))",
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
FunctionDef(add, [x: Int, y: Int], Int, [Expression(BinaryOp(Variable(x), Add, Variable(y)))])
---
Let(Variable(result), FunctionCall(Variable(add), [Literal(Int(10)), Literal(Int(20))]))
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
        "StructDef(Person, SeqMap(first_field: Int, second_field: String))",
    )
}

#[test_log::test]
fn struct_init() {
    let script = "
            person = Person { first_field: 1, second_field: \"Bob\" }
        ";
    check(
        script,
        "Let(Variable(person), StructInstantiation(Person, SeqMap(first_field: Literal(Int(1)), second_field: Literal(String(Bob)))))",
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
        Let(Variable(a), Literal(Int(0)))
        WhileLoop(BinaryOp(Variable(a), LessThan, Literal(Int(3))), [Expression(FunctionCall(Variable(print), [Variable(a)])), Let(Variable(a), BinaryOp(Variable(a), Add, Literal(Int(1))))])
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
        "Let(Variable(c), IfElse(Literal(Bool(true)), Variable(a), Variable(b)))\n",
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
StructDef(Person, SeqMap(first_field: Int, second_field: String))
---
Let(Variable(person), StructInstantiation(Person, SeqMap(first_field: Literal(Int(1)), second_field: Literal(String(Bob)))))
    "#,
    );
}

#[test_log::test]
fn nested_function_calls() {
    check(
        "result = add(mul(2, 3), div(10, 2))",
        "Let(Variable(result), FunctionCall(Variable(add), [FunctionCall(Variable(mul), [Literal(Int(2)), Literal(Int(3))]), FunctionCall(Variable(div), [Literal(Int(10)), Literal(Int(2))])]))",
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

        Let(Variable(result), BinaryOp(BinaryOp(BinaryOp(Literal(Int(2)), Add, Literal(Int(3))), Multiply, Literal(Int(4))), Subtract, BinaryOp(Literal(Int(10)), Divide, Literal(Int(2)))))
Let(Variable(flag), BinaryOp(BinaryOp(Literal(Bool(true)), LogicalAnd, UnaryOp(Not, Literal(Bool(false)))), LogicalOr, BinaryOp(Literal(Int(5)), GreaterThan, Literal(Int(3)))))
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
        Let(Variable(person), StructInstantiation(Person, SeqMap(first_field: Literal(Int(1)), second_field: Literal(String(Bob)))))
        Let(Variable(name), FieldAccess(Variable(person), second_field))
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
        "Let(Variable(name), FieldAccess(Variable(person), second_field))",
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
WhileLoop(BinaryOp(BinaryOp(Variable(x), GreaterThan, Literal(Int(0))), LogicalAnd, BinaryOp(Variable(y), LessThan, Literal(Int(10)))), [Let(Variable(x), BinaryOp(Variable(x), Subtract, Literal(Int(1)))), Let(Variable(y), BinaryOp(Variable(y), Add, Literal(Int(1))))])
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
  Let(Variable(x), Literal(Int(0)))
WhileLoop(BinaryOp(Variable(x), LessThan, Literal(Int(3))), [Let(Variable(y), Literal(Int(0))), WhileLoop(BinaryOp(Variable(y), LessThan, Literal(Int(2))), [Expression(FunctionCall(Variable(print), [BinaryOp(Variable(x), Add, Variable(y))])), Let(Variable(y), BinaryOp(Variable(y), Add, Literal(Int(1))))]), Let(Variable(x), BinaryOp(Variable(x), Add, Literal(Int(1))))])
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
StructDef(Point, SeqMap(x: Int, y: Int))
---
Let(Variable(p1), StructInstantiation(Point, SeqMap(x: Literal(Int(5)), y: Literal(Int(10)))))
Let(Variable(dist), FunctionCall(Variable(add), [FunctionCall(Variable(mul), [FieldAccess(Variable(p1), x), FieldAccess(Variable(p1), x)]), FunctionCall(Variable(mul), [FieldAccess(Variable(p1), y), FieldAccess(Variable(p1), y)])]))

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
        "Expression(FunctionCall(Variable(mul), [Literal(Int(2))]))",
    );
}

#[test_log::test]
fn small_chain() {
    let script = "
        p1.x.mul(2)
    ";
    check(
        &script,
        "Expression(MemberCall(FieldAccess(Variable(p1), x), mul, [Literal(Int(2))]))",
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
    Expression(MemberCall(MemberCall(FieldAccess(Variable(p1), x), mul, [FieldAccess(Variable(p1), x)]), add, [MemberCall(FieldAccess(Variable(p1), y), mul, [FieldAccess(Variable(p1), y)])]))
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
StructDef(Point, SeqMap(x: Int, y: Int))
---
Let(Variable(p1), StructInstantiation(Point, SeqMap(x: Literal(Int(5)), y: Literal(Int(10)))))
Let(Variable(dist), MemberCall(MemberCall(FieldAccess(Variable(p1), x), mul, [FieldAccess(Variable(p1), x)]), add, [MemberCall(FieldAccess(Variable(p1), y), mul, [FieldAccess(Variable(p1), y)])]))

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
FunctionDef(add, [x: Int, y: Int], Int, [Expression(BinaryOp(Variable(x), Add, Variable(y)))])
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
Let(Variable(result), FunctionCall(Variable(single), []))
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
Let(Variable(x), Array([Literal(Int(1)), Literal(Int(2)), Literal(Int(3))]))
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
Let(Variable(x), Array([]))
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
Let(Variable(v), Match(Variable(state), [MatchArm { pattern: EnumTuple(Running, [speed, _]), expression: Variable(speed) }, MatchArm { pattern: EnumStruct(Sleeping, [hours]), expression: BinaryOp(Variable(hours), Add, Literal(Int(10))) }, MatchArm { pattern: Wildcard, expression: Literal(Int(0)) }]))

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
Expression(Match(Variable(state), [MatchArm { pattern: Wildcard, expression: Literal(Int(0)) }]))
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
Expression(Match(Variable(state), [MatchArm { pattern: EnumTuple(EnumType, [ident, _]), expression: Variable(ident) }, MatchArm { pattern: Wildcard, expression: Literal(Int(0)) }]))
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
Expression(Match(Variable(state), [MatchArm { pattern: EnumStruct(EnumType, [something, another]), expression: Variable(another) }, MatchArm { pattern: Wildcard, expression: Literal(Int(0)) }]))
"#,
    );
}

#[test_log::test]
fn deconstructing_struct() {
    let script = "{  x, y } = pos"; // No raw string literal
    check(&script, "Let(Struct([x, y]), Variable(pos))");
}

#[test_log::test]
fn deconstructing_tuple() {
    let script = "(x, y) = pos"; // No raw string literal
    check(&script, "Let(Tuple([x, y]), Variable(pos))");
}

#[test_log::test]
fn operator_precedence() {
    let script = "z = y * 2 - x";
    check(&script, "Let(Variable(z), BinaryOp(BinaryOp(Variable(y), Multiply, Literal(Int(2))), Subtract, Variable(x)))");
}

#[test_log::test]
fn operator_precedence_expression() {
    let script = "y * 2 - x";
    check(
        &script,
        "
Expression(BinaryOp(BinaryOp(Variable(y), Multiply, Literal(Int(2))), Subtract, Variable(x)))",
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
ForLoop(Variable(x), ExclusiveRange(Literal(Int(1)), Literal(Int(10))), [])",
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
ForLoop(Variable(x), Array([Literal(Int(1)), Literal(Int(2)), Literal(Int(3))]), [])",
    );
}

#[test_log::test]
fn enum_literal_basic() {
    check(
        r#"
        state = State::Running
    "#,
        r#"

Let(Variable(state), Literal(EnumVariant(State::Running)))

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
Let(Variable(state), Literal(EnumVariant(State::Running)))
Expression(FunctionCall(Variable(print), [Variable(state)]))

        "#,
    );
}

#[test_log::test]
fn mut_parameter() {
    let script = r#"

fn increment(mut x: Int) -> Int {
    x = x + 1
    x
}

    "#;
    check(
        &script,
        "
FunctionDef(increment, [mut x: Int], Int, [Let(Variable(x), BinaryOp(Variable(x), Add, Literal(Int(1)))), Expression(Variable(x))])",
    );
}

#[test_log::test]
fn mut_let() {
    check("mut x = 3", "Let(Variable(mut x), Literal(Int(3)))");
}

#[test_log::test]
fn import() {
    check(
        "import math",
        "Import(Import { module_path: [math], items: Module })",
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

ImplDef(SomeTypeName, SeqMap(something: Member(self, [], Int, [Expression(FieldAccess(Variable(self), x))]), another: Member(mut self, [v: Int], Int, [Expression(FieldAssignment(Variable(self), x, Literal(Int(3))))]), no_self_here: Member(self, [], Float, [Expression(Literal(Float(3.2)))])))

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

Expression(Match(Variable(x), [MatchArm { pattern: Literal(Int(5)), expression: Literal(String(five)) }, MatchArm { pattern: Literal(String(hello)), expression: Literal(String(greeting)) }, MatchArm { pattern: EnumSimple(true), expression: Literal(String(yes)) }, MatchArm { pattern: Wildcard, expression: Literal(String(something else)) }]))

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

Expression(FunctionCall(Variable(print), [Literal(String(hello))]))
Expression(FunctionCall(Variable(print), [Literal(String(world))]))

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
Let(Variable(x), Assignment(Variable(y), Assignment(Variable(z), Literal(Int(10)))))
        "#,
    );
}

#[test_log::test]
fn enum_variant_construction() {
    check(
        r#"

        shape = Shape::Rectangle { width: 10, height: 20 }

        "#,
        "Let(Variable(shape), Literal(EnumVariant(Shape::Rectangle{ width: Literal(Int(10)), height: Literal(Int(20)) })))",
    );
}

#[test_log::test]
fn enum_variant_tuple_construction() {
    check(
        r#"
        shape = Shape::Something(2, 4.4)
        "#,
        r#"
Let(Variable(shape), Literal(EnumVariant(Shape::Something([Literal(Int(2)), Literal(Float(4.4))]))))
        "#,
    );
}

#[test_log::test]
fn array_access() {
    check(
        r#"
        arr[3]
        "#,
        "Expression(ArrayAccess(Variable(arr), Literal(Int(3))))",
    );
}

#[test_log::test]
fn array_set() {
    check(
        r#"
        arr[3] = 42
        "#,
        "Expression(ArrayAssignment(Variable(arr), Literal(Int(3)), Literal(Int(42))))",
    );
}

#[test_log::test]
fn string_interpolation_basic_spaces() {
    check(
        "'   this is interpolated {x}   with hex  {y}  '",
        r#"
Expression(InterpolatedString([Literal("   this is interpolated "), Interpolation(Variable(x), None), Literal("   with hex  "), Interpolation(Variable(y), None), Literal("  ")]))
    "#,
    );
}

#[test_log::test]
fn string_interpolation() {
    check(
        "'this is interpolated {x} with hex {y:x}'",
        r#"
    Expression(InterpolatedString([Literal("this is interpolated "), Interpolation(Variable(x), None), Literal(" with hex "), Interpolation(Variable(y), Some(LowerHex))]))
    "#,
    );
}

#[test_log::test]
fn string_interpolation_call() {
    check(
        "'this is interpolated {x:x}    with hex  {mul(a, 2)}'",
        r#"
Expression(InterpolatedString([Literal("this is interpolated "), Interpolation(Variable(x), Some(LowerHex)), Literal("    with hex  "), Interpolation(FunctionCall(Variable(mul), [Variable(a), Literal(Int(2))]), None)]))
    "#,
    );
}

#[test_log::test]
fn string_interpolation_call_simple() {
    check(
        "'result: {mul(a,2)}'",
        r#"
Expression(InterpolatedString([Literal("result: "), Interpolation(FunctionCall(Variable(mul), [Variable(a), Literal(Int(2))]), None)]))
    "#,
    );
}

#[test_log::test]
fn string_interpolation_simple() {
    check(
        "'this is interpolated {x}'",
        r#"
Expression(InterpolatedString([Literal("this is interpolated "), Interpolation(Variable(x), None)]))
    "#,
    );
}

#[test_log::test]
fn string_interpolation_simple_no_space() {
    check(
        "'this is interpolated{x}'",
        r#"
Expression(InterpolatedString([Literal("this is interpolated"), Interpolation(Variable(x), None)]))
    "#,
    );
}

#[test_log::test]
fn function_call_mul() {
    check(
        "mul(a, 2)",
        r#"
Expression(FunctionCall(Variable(mul), [Variable(a), Literal(Int(2))]))
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
        "Let(Variable(msg), Match(Variable(result), [MatchArm { pattern: EnumTuple(Ok, [value]), expression: Variable(value) }, MatchArm { pattern: EnumTuple(Err, [err]), expression: Variable(err) }]))",
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
        "Let(Variable(msg), Match(Variable(result), [MatchArm { pattern: EnumTuple(Ok, [value]), expression: Variable(value) }, MatchArm { pattern: EnumTuple(Err, [err]), expression: Variable(err) }, MatchArm { pattern: EnumSimple(Simple), expression: Literal(Int(2)) }]))",
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
        "Let(Variable(msg), Match(Variable(result), [MatchArm { pattern: EnumTuple(Ok, [value]), expression: Variable(value) }, MatchArm { pattern: EnumTuple(Err, [err]), expression: Variable(err) }, MatchArm { pattern: EnumSimple(Simple), expression: Literal(Int(2)) }, MatchArm { pattern: Wildcard, expression: Literal(Int(99)) }]))",
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
        "Let(Variable(msg), Match(Variable(result), [MatchArm { pattern: EnumTuple(Ok, [value]), expression: Variable(value) }, MatchArm { pattern: Wildcard, expression: Literal(Int(99)) }]))",
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

    Expression(Match(Variable(v), [MatchArm { pattern: EnumTuple(Tuple, [i, s, b]), expression: Block([Expression(FunctionCall(Variable(print), [Literal(String(Tuple:))])), Expression(FunctionCall(Variable(print), [Variable(i)])), Expression(FunctionCall(Variable(print), [Variable(s)])), Expression(FunctionCall(Variable(print), [Variable(b)]))]) }]))

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
        "If(BinaryOp(Variable(x), GreaterThan, Literal(Int(41))), [Expression(Literal(String(x is greater than 41)))], None)",
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

ForLoop(Variable(i), ExclusiveRange(Literal(Int(1)), Literal(Int(5))), [If(BinaryOp(BinaryOp(Variable(i), Modulo, Literal(Int(2))), Equal, Literal(Int(0))), [Continue], None), Expression(FunctionCall(Variable(print), [Literal(String(no continue))]))])

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

            If(BinaryOp(Variable(x), LessThan, Literal(Int(0))), [Return(Literal(String(negative)))], Some([If(BinaryOp(Variable(x), Equal, Literal(Int(0))), [Return(Literal(String(positive)))], None)]))

            "#,
    )
}
