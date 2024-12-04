+++
title = "Swamp Documentation"
description = "Official documentation for the Swamp programming language"
sort_by = "weight"
+++

# Welcome to Swamp

**Swamp** is a modern programming language designed for seamless embedding and rapid development cycles.

## Key Features

- Strong static typing with type inference
- First-class functions and closures
- Advanced pattern matching
- Algebraic data types (Enums and Structs)
- Built-in string interpolation
- Clear and consistent syntax
- Module system with explicit imports

## Key Goals

- **Type Safety**: Ensure robust and error-free code through strong static typing and basic type inference.

- **Embeddable**: Easily embed Swamp into existing applications, such as game engines.

- **Hot Reloading**: Support rapid development cycles with an interpreter and bytecode executor, allowing for real-time code updates and testing.

- **Rust-like syntax**: Maintain a syntax similar to Rust, facilitating easy context switching between Rust and Swamp for developers familiar with both languages.

- **Transpilation to Rust**: Enable straightforward transpilation to Rust code, allowing Swamp to be compiled alongside Rust for performance-critical applications.

- **Future Native Compilation**: Explore the potential to develop a frontend for [CraneLift](https://cranelift.dev), enabling Swamp to compile directly to native code and WebAssembly for broader platform support.

## Quick Examples

### Functions and String Interpolation

```swamp
fn say_hello(name: String) -> String {
    'Hello, {name}!'
}

print(say_hello("Ossian"))  // Outputs: "Hello, Ossian!"
```

### Pattern Matching

```swamp
enum Result {
    Ok(String),
    Err(String),
}

fn process(result: Result) {
    match result {
        Ok(message) => print('Success: {message}'),
        Err(error) => print('Error: {error}')
    }
}
```

### Structs and Methods

```swamp
struct Point {
    x: Float,
    y: Float,
}

impl Point {
    fn distance(self, other: Point) -> Float {
        ((self.x - other.x) * (self.x - other.x) + 
         (self.y - other.y) * (self.y - other.y)).sqrt()
    }
}

p1 = Point { x: 0.0, y: 0.0 }
p2 = Point { x: 3.0, y: 4.0 }

print('Distance: {p1.distance(p2)}')
```

### Mutable Variables

In Swamp Script, variables are immutable by default, meaning their values cannot be changed once assigned. Arguments are passed by value, meaning they are copied when passed to a function. Swamp encourages you to use immutable variables as long as it is possible.
This helps prevent accidental modifications and ensures safer code. To declare a mutable variable, use the `mut` keyword:

```swamp
mut x = 10
x = 20  // Now you can change the value of x
```

If you a function wants to modify a variable, it must declare the parameter as mutable. This makes it explicit that the function can change the variable's value:

```swamp
fn increment(mut x: Int) {
    x += 1
}
```

```swamp
mut a = 5 // a will be changed in the future

// even though a is already mutable, we want to make 
// it explicit that we allow and see the function might change it.

increment(mut a) 

print(a)  // Outputs: 6
```
