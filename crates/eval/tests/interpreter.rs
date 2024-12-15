/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::util::{check, check_fail};

mod util;

/*
   // Passing mutable variable to immutable parameter - passes by value
   check(
       r#"
   fn print_and_add_one(x: Int) {
       print(x + 1)
   }
   mut y = 42
   print_and_add_one(y)  // Passes by value
   print(y)  // y is still 42
   "#,
       r#"
   43
   42
   "#,
   );
*/

/*
#[test_log::test
fn set_enum_value() {
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

    "#,
    );
}
*/

#[test_log::test]
fn multiple_operations() {
    check(
        r#"
        x = 10
        y = x + 5
        z = y * 2 - x
        print(z)
    "#,
        r#"
            20
            "#,
    );
}

#[test_log::test]
fn function_call() {
    check(
        r#"
        fn add(x: Int, y: Int) -> Int {
            return x + y
        }

        a = add(5, 3)
        print(a)
        b = add(a, 2)
        print(b)
    "#,
        r#"
8
10
    "#,
    )
}

#[test_log::test]
fn function_call_implicit_return() {
    check(
        r#"
        fn add(x: Int, y: Int) -> Int {
            x + y
        }

        a = add(5, 3)
        print(a)
        b = add(a, 2)
        print(b)
    "#,
        r#"
8
10
    "#,
    )
}

#[test_log::test]
fn hello() {
    check(
        r#"
       x = 10
        print('hello {x}')

    "#,
        "hello 10",
    );
}

#[test_log::test]
fn print_different_types() {
    check(
        r#"
        x = 42
        y = true
        z = 3.14
        print(x)
        print(y)
        print(z)
        print("test")
    "#,
        r#"
        42
        true
        3.14
        test
        "#,
    );
}

#[test_log::test]
fn print_if() {
    check(
        r#"
        x = 42
        msg = if x > 41 {
            "x is greater than 41"
        } else {
            "x is not greater than 41"
        }
        print(msg)
    "#,
        "x is greater than 41",
    );
}

#[test_log::test]
fn print_range() {
    check(
        r#"
        for x in 1..10 {
            print(x)
        }
    "#,
        r#"
        1
        2
        3
        4
        5
        6
        7
        8
        9
        "#,
    );
}

#[test_log::test]
fn print_x() {
    check(
        r#"
        x = 10
        print(x)
    "#,
        "10",
    );
}

#[test_log::test]
fn iterate_over_array() {
    check(
        r#"
        for x in [1, 2, 3] {
            print(x)
        }
    "#,
        r#"
        1
        2
        3
        "#,
    );
}

#[test_log::test]
fn while_loop() {
    check(
        r#"
        mut x = 0
        while x < 5 {
            print(x)
            x = x + 1
        }
    "#,
        r#"
        0
        1
        2
        3
        4
        "#,
    );
}

#[test_log::test]
fn call_function_in_loop() {
    check(
        r#"
        fn add(another: Int) {
            print(another)
            if true { 2 } else { 3 }
            print(another * 2)
        }

        mut x = 0
        while x < 3 {
            add(x)
            x = x + 1
        }
    "#,
        r#"
        0
        0
        1
        2
        2
        4
        "#,
    );
}

#[test_log::test]
fn call_mut() {
    check_fail(
        r#"
fn increment(mut x: Int) -> Int {
    x = x + 1
    x
}
mut x = 10
increment(x)
print(x)

    "#,
        "ResolveError(ArgumentIsNotMutable)",
    );
}

#[test_log::test]
fn call_mut_2() {
    check_fail(
        r"
    fn increment(mut x: Int) -> Int {
        x = x + 1
        x
    }
    x = 10
    increment(mut x)
    print(x)

    ",
        r#"ResolveError(VariableIsNotMutable(<102:1>))"#,
    );
}

#[test_log::test]
fn test_parameter_passing() {
    // Passing mutable variable to mutable parameter with mut - modifies original
    check(
        r#"
    fn increment(mut x: Int) {
        x = x + 1
        print(x)
    }
    mut y = 42
    increment(mut y)
    print(y)  // y is now 43 since we passed by &mut
    "#,
        r#"
    43
    43
    "#,
    );
}

// Struct tests
#[test_log::test]
fn struct_basic() {
    check(
        r#"
        struct Point {
            x: Int,
            y: Int
        }

        p = Point { x: 10, y: 20 }
        print(p.x)
        print(p.y)
        "#,
        r#"
        10
        20
        "#,
    );
}

#[test_log::test]
fn struct_field_assignment() {
    check(
        r#"
        struct Point {
            x: Int,
            y: Int
        }

        mut p = Point { x: 10, y: 20 }
        p.x = 30
        print(p.x)
        "#,
        "30",
    );
}

// Enum tests
#[test_log::test]
fn enum_match() {
    check(
        r#"
        enum Result {
            Ok(Int),
            Err(String)
        }

        result = Result::Ok(42)
        msg = match result {
            Ok value => "Got value: " + value,
            Err err => "Error: " + err
        }
        print(msg)
        "#,
        "Got value: 42",
    );
}

#[test_log::test]
fn enum_instantiation_simple() {
    check(
        r"
        enum Result {
            Ok,
            Err
        }

        result = Result::Ok
        print(result)
        ",
        "Result::Ok",
    );
}

#[test_log::test]
fn enum_instantiation_simple_2() {
    check(
        r#"
        enum Result {
            Ok,
            Err
        }

        result = Result::Ok
        print(result)
        y = match result {
            Ok => 2,
            _ => 3
        }
        print(y)
        "#,
        r#"
            Result::Ok
            2
        "#,
    );
}

// Array operations
#[test_log::test]
fn array_operations() {
    check(
        r#"
        arr = [1, 2, 3]
        print(arr[0])
        print(arr[1])
        print(arr[2])

        mut nums = [10, 20, 30]
        nums[1] = 25
        print(nums[1])
        "#,
        r#"
        1
        2
        3
        25
        "#,
    );
}

// Function with multiple returns and complex logic
// Testing that it works with unnecessary returns
#[test_log::test]
fn complex_function() {
    check(
        r#"
        fn process_number(x: Int) -> String {
            if x < 0 {
                return "negative"
            } else if x == 0 {
                return "zero"
            } else if x % 2 == 0 {
                return "positive even"
            } else {
                return "positive odd"
            }
        }

        print(process_number(-5))
        print(process_number(0))
        print(process_number(4))
        print(process_number(7))
        "#,
        r#"
        negative
        zero
        positive even
        positive odd
        "#,
    );
}

// Nested control flow
#[test_log::test]
fn nested_control_flow() {
    check(
        r#"
        mut result = ""
        for i in 1..3 {
            for j in 1..3 {
                if i == j {
                    result = result + "same"
                } else {
                    result = result + "diff"
                }
            }
        }
        print(result)
        "#,
        "samediffdiffsame",
    );
}

// String operations
#[test_log::test]
fn string_operations() {
    check(
        r#"
        name = "Alice"
        age = 30
        message = "Hello, " + name + "! You are " + age + " years old."
        print(message)
        "#,
        "Hello, Alice! You are 30 years old.",
    );
}

// Struct with methods (impl)
#[test_log::test]
fn struct_with_methods() {
    check(
        r#"
        struct Rectangle {
            width: Int,
            height: Int,
        }

        impl Rectangle {
            fn area(self) -> Int {
                self.width * self.height
            }

            fn scale(mut self, factor: Int) {
                self.width = self.width * factor
                self.height = self.height * factor
            }
        }

        mut rect = Rectangle { width: 5, height: 3, }
        print(rect.area())
        rect.scale(2)
        print(rect.area())
        "#,
        r#"
        15
        60
        "#,
    );
}

#[test_log::test]
fn struct_with_methods_basic() {
    check(
        r#"
        struct Money {
            v: Int,
        }

        impl Money {
            fn get(self) -> Int {
                self.v
            }

            fn set(mut self, v: Int) {
                self.v = v
            }
        }

        mut m = Money { v: 10 }
        print(m)
        print(m.get())
        m.set(22)
        print(m.get())
        "#,
        r#"
    Money { v: 10 }
    10
    22
        "#,
    );
}

// Testing boolean operations
#[test_log::test]
fn boolean_operations() {
    check(
        r#"
        x = true
        y = false
        print(x && y)
        print(x || y)
        print(!x)
        print(x && true)
        "#,
        r#"
        false
        true
        false
        true
        "#,
    );
}

// Testing arithmetic precedence
#[test_log::test]
fn arithmetic_precedence() {
    check(
        r"
        mut result = 2 + 3 * 4
        print(result)
        result = (2 + 3) * 4
        print(result)
        result = 10 - 2 * 3
        print(result)
        ",
        r"
        14
        20
        4
        ",
    );
}

// Testing variable shadowing
#[test_log::test]
fn variable_shadowing() {
    check(
        r#"
        x = 10
        fn test() {
            x = 20
            print(x)
        }
        test()
        print(x)
        "#,
        r#"
        20
        10
        "#,
    );
}

// Testing error handling
// TODO:
//#[test_log::test]
#[allow(unused)]
fn error_handling() {
    check_fail(
        r#"
        x = 10
        x = "string"  // Type mismatch
        "#,
        "Type mismatch",
    );
}

// Testing complex match patterns
#[test_log::test]
fn complex_match_patterns() {
    check(
        r#"
        enum Shape {
            Circle(Int),
            Rectangle { width: Int, height: Int }
        }

        shape = Shape::Rectangle { width: 10, height: 20 }

        result = match shape {
            Circle radius => radius * radius,
            Rectangle width, height => width * height
        }

        print(result)
        "#,
        "200",
    );
}

#[test_log::test]
fn nested_types() {
    check(
        r#"
        struct Point { x: Int, y: Int }
        enum Container {
            Single(Point),
            Pair { first: Point, second: Point }
        }

        p1 = Point { x: 1, y: 2 }
        p2 = Point { x: 3, y: 4 }
        container = Container::Pair { first: p1, second: p2 }

        match container {
            Single p => print(p.x + p.y),
            Pair first, second => {
                print(first.x + second.x)
                print(first.y + second.y)
            }
        }
        "#,
        r#"
        4
        6
        "#,
    );
}

#[test_log::test]
fn array_literal() {
    check(
        r#"
            enum Value {
            Int(Int),
            Str(String),
            Tuple(Int, String, Bool)
        }
            values = [
            Value::Int(42),
            Value::Str("hello"),
            Value::Tuple(1, "test", true)
        ]
    "#,
        "",
    );
}

// Testing complex enum variants and pattern matching
#[test_log::test]
fn enum_pattern_matching() {
    check(
        r#"
        enum Value {
            Int(Int),
            Str(String),
            Tuple(Int, String, Bool)
        }

        values = [
            Value::Int(42),
            Value::Str("hello"),
            Value::Tuple(1, "test", true)
        ]

        for v in values {
            match v {
                Int i => print("Int: " + i),
                Str s => print("Str: " + s),
                Tuple i, s, b => {
                    print("Tuple:")
                    print(i)
                    print(s)
                    print(b)
                }
            }
        }
        "#,
        r#"
        Int: 42
        Str: hello
        Tuple:
        1
        test
        true
        "#,
    );
}

// Testing nested function calls and operator precedence
#[test_log::test]
fn nested_expressions() {
    check(
        r#"
        fn add(x: Int, y: Int) -> Int { x + y }
        fn mul(x: Int, y: Int) -> Int { x * y }

        result = add(mul(2, 3), mul(add(1, 2), 3))
        print(result)  // (2 * 3) + ((1 + 2) * 3) = 6 + 9 = 15
        "#,
        "15",
    );
}

// Testing multiple assignments and mutations
#[test_log::test]
fn multiple_assignments() {
    check(
        r#"
        mut x = 1
        mut y = 2
        mut z = 3

        x = y = z = 10

        print(x)
        print(y)
        print(z)
        "#,
        r#"
        10
        10
        10
        "#,
    );
}

// Testing complex boolean expressions
#[test_log::test]
fn complex_boolean_logic() {
    check(
        r#"
        fn check(x: Int, y: Int) -> Bool {
            (x > 0 && y > 0) || (x < 0 && y < 0)
        }

        print(check(1, 2))    // true
        print(check(-1, -2))  // true
        print(check(1, -2))   // false
        print(check(0, 0))    // false
        "#,
        r#"
        true
        true
        false
        false
        "#,
    );
}

// Testing nested scopes and shadowing
#[allow(unused)]
fn nested_scopes() {
    check(
        r#"
        x = 1
        {
            x = 2
            {
                x = 3
                print(x)
            }
            print(x)
        }
        print(x)
        "#,
        r#"
        3
        2
        1
        "#,
    );
}

// Testing early returns and control flow
#[test_log::test]
fn early_returns() {
    check(
        r#"
        fn process(x: Int) -> String {
            if x < 0 { return "negative" }

            if x == 0 {
                return "zero"
            } else {
                if x > 100 {
                    return "large"
                }
            }

            "positive"
        }

        print(process(-1))
        print(process(0))
        print(process(50))
        print(process(200))
        "#,
        r#"
        negative
        zero
        positive
        large
        "#,
    );
}

// Testing array indexing and mutations
#[test_log::test]
fn array_mutations() {
    check(
        r#"
        mut arr = [1, 2, 3, 4, 5]

        arr[0] = arr[4]
        arr[4] = arr[0] * 2

        print(arr[0])
        print(arr[4])

        // Using array elements in expressions
        sum = arr[0] + arr[1] + arr[2]
        print(sum)
        "#,
        r#"
        5
        10
        10
        "#,
    );
}

// Testing struct field mutations and method chaining
#[test_log::test]
fn struct_mutations_and_chaining() {
    check(
        r#"
        struct Counter {
            value: Int
        }

        impl Counter {
            fn increment(mut self) -> Int {
                self.value = self.value + 1
                self.value
            }

            fn double(mut self) -> Int {
                self.value = self.value * 2
                self.value
            }
        }

        mut c = Counter { value: 1 }
        print(c.increment())
        print(c.double())
        print(c.double())
        "#,
        r#"
        2
        4
        8
        "#,
    );
}

// Testing error cases
#[test_log::test]
fn array_bounds_error() {
    check_fail(
        r#"
        arr = [1, 2, 3]
        print(arr[3])  // Out of bounds
        "#,
        r#"ExecuteError(Error("Array index out of bounds: 3"))"#,
    );
}

#[allow(unused)]
fn type_mismatch_in_match() {
    check_fail(
        r#"
        x = 42
        match x {
            "string" => print("impossible"),
            _ => print("catch all")
        }
        "#,
        "Unsupported expression: Match(Variable(x), [MatchArm { pattern: Literal(String(string)), expression: FunctionCall(Variable(print), [Literal(String(impossible))]) }, MatchArm { pattern: Wildcard, expression: FunctionCall(Variable(print), [Literal(String(catch all))]) }])",
    );
}

#[test_log::test]
fn undefined_variable() {
    check_fail(
        r#"
        print(undefined_variable)
        "#,
        "ResolveError(UnknownVariable(<15:18>))",
    );
}

// Testing complex control flow combinations
#[test_log::test]
fn complex_control_flow() {
    check(
        r#"
        mut result = 0

        for i in 1..6 {
            print('i={i}')
            if i % 2 == 0 { // skips 2 and 4, lets 1 and 3 through
                print('continue')
                continue
            }

            mut j = 0
            print('before while')
            while j < i { // i is 1 first time and 3 the next time. so first time it loops once, the other it loops twice, the last loop it breaks out
                if j * i > 10 {
                    print('break out')
                    break
                }
                result = result + (i * j)
                j = j + 1
                print('last in while j={j} result={result}')
            }
        }

        print(result)
        "#,
        r#"

i=1
before while
last in while j=1 result=0
i=2
continue
i=3
before while
last in while j=1 result=0
last in while j=2 result=3
last in while j=3 result=9
i=4
continue
i=5
before while
last in while j=1 result=9
last in while j=2 result=14
last in while j=3 result=24
break out
24

        "#,
    );
}

// Testing complex control flow combinations
#[test_log::test]
fn mut_in_loop() {
    check(
        r#"
        mut result = 2

        for i in 1..3 {
            result = result + 1
            for j in 1..4 {
                result = result + 1
            }
        }

        print(result)
        "#,
        "10",
    );
}

#[test_log::test]
fn string_interpolation_basic() {
    check(
        r#"
        a = 43
        print('hex: {a:x}')

    "#,
        r#"
    hex: 2b
    "#,
    );
}

#[test_log::test]
fn string_interpolation_basic_spaces() {
    check(
        r#"
        a = 43
        print('hex: {a:x}  {a}')

    "#,
        r#"
    hex: 2b  43
    "#,
    );
}

#[test_log::test]
fn string_interpolation_with_call() {
    check(
        r#"
        fn mul(x: Int, y: Int) -> Int {
            x * y
        }
        a = 43

        print('hex: {a:x} {mul(a, 2)}')

    "#,
        r#"
    hex: 2b 86
    "#,
    );
}

#[test_log::test]
fn for_loop_continue() {
    check(
        r#"
           for i in 1..5 {
            if i % 2 == 0 {
                continue
            }
            }

   "#,
        r#"
   "#,
    )
}

#[test_log::test]
fn test_nested_mut_refs() {
    check(
        r#"
        struct Point { x: Int, y: Int }
        mut p1 = Point { x: 1, y: 2 }
        mut p2 = Point { x: 3, y: 4 }

        fn modify_points(mut a: Point, mut b: Point) {
            a.x = b.y
            b.y = a.x
        }

        modify_points(mut p1, mut p2)
        print(p1)
        print(p2)
        "#,
        r#"
        Point { x: 4, y: 2 }
        Point { x: 3, y: 4 }
        "#,
    );
}

#[test_log::test]
fn test_method_chaining() {
    check(
        r#"
        struct Counter {
            value: Int
        }

        impl Counter {
            fn increment(mut self) {
                self.value = self.value + 1
            }

            fn double(mut self) {
                self.value = self.value * 2
            }
        }

        mut c = Counter { value: 1 }
        c.increment()
        c.double()
        c.increment()
        print(c.value)
        "#,
        "5",
    );
}

#[allow(unused)]
fn test_nested_scopes() {
    check(
        r#"
        mut x = 1
        {
            mut y = 2
            {
                fn modify(mut a: Int, mut b: Int) {
                    a = a + b
                }
                modify(mut x, mut y)
            }
            print(x)
        }
        print(x)
        "#,
        r#"
        3
        3
        "#,
    );
}

#[test_log::test]
fn test_match_with_refs() {
    check(
        r#"
        enum State {
            Running { count: Int },
            Stopped
        }

        mut state = State::Running { count: 0 }

        fn increment_if_running(mut s: State) {
            match s {
                Running count => {
                    s = State::Running { count: count + 2 }
                },
                Stopped => {}
            }
        }

        increment_if_running(mut state)
        print(state)
        "#,
        "State::Running { count: 2 }",
    );
}
