+++
title = "Language Specification"
description = "Detailed specification of the Swamp programming language"
+++

## Comments

```swamp
// This is a comment
```

```swamp
/*
This is a multi-line comment
*/
```

```swamp
/// This is a doc comment
```


## Definitions

### Struct

```swamp
struct MyStruct {
    a: Int,
    b: String,
}
```

### Enum

```swamp
enum MyEnum {
    Variant1,
    Variant2(String, Int),
    Variant3 {
        a: Int,
        b: String,
    },
}
```

### Function

```swamp
fn my_function(a: Int, b: String) -> Int {
    a + b.len()
}
```

### Implementation Block

```swamp
impl MyStruct {
    fn my_method(self) -> Int {
        self.a + self.b.len()
    }
}
```

### Type Alias

```swamp
type MyAlias = MyStruct
``` 

## External Function

Has no body

```swamp
external fn my_function(a: Int, b: String) -> Int
```

## Statements

### `if` expression and statement

Most things in Swamp are expressions. This means that they can be used on the right side of an assignment, or as part of a function return value.
However, the `if` statement is an exception to this rule. If it does not have an `else` statement, it is a statement.


#### If-Expression
```swamp
x = if a == 3 {
    42
} else {
    -3
}
```

#### If-Statement
```swamp
if a == 3 {
    print('a is 3')
}
```

### `while` statement

Continues executing the body of the loop as long as the condition is true.

```swamp
mut a = 0

while a < 10 {
    print(a)
    a += 1
}
```

### `for` statement

Loops over a an [iterator](#iterators). An iterator is a value that can generate a sequence of values. At the moment only exclusive ranges are supported.

```swamp
for i in 0..10 {
    print(i)
}
```


### `break` and `continue` statements

The `break` statement will exit the current "closest" loop. 
The `continue` statement will skip the rest of the current iteration and jump to the next one.


### `return` statement

The `return` statement exits a function and can be used to return a value. In Swamp, the last expression in a function is automatically returned, so `return` is not often used.

```swamp
fn add_one(x: Int) -> Int {
    if x > 3 {
        return x + 1
    }
    print('x is not greater than 3')
    x
}
```

## Expressions

### Access or Lookup Expressions

#### Field Access

Accesses a field on a struct. In Swamp, all fields are always public.

```swamp
struct Point {
    x: Int,
    y: Int,
}

p = Point { x: 1, y: 2 }

print(p.x)  // Outputs: 1
```

#### Array Access

Accesses an element in an array. The index must be an integer.

```swamp
arr = [4.2, 4.4, 4.6, 4.8, 5.0]

print(arr[0])  // Outputs: 4.2
```

#### Variable Access

Accesses a variable. Returns the value that is assigned to the variable.

```swamp
x = 10
print(x)  // Outputs: 10
```


## Assignments

### Variable Assignment

```swamp
x = 10
```

### Array Assignment

```swamp
mut arr = [1, 2, 3, 4, 5]
arr[0] = 10
```

### Struct Field Assignment

```swamp
struct Point {
    x: Int,
    y: Int,
}

mut p = Point { x: 0, y: 0 }
p.x = 10
```


## Operators

### Binary Operators

```swamp
+  -  *  /  %  && || == != < <= > >= ..
```

- Add
- Subtract
- Multiply
- Divide
- Modulo

- LogicalAnd
- LogicalOr

- Equal
- NotEqual
- LessThan
- LessEqual
- GreaterThan
- GreaterEqual

- RangeExclusive



### Unary Operators

```swamp
-  !
```

- Negate
- LogicalNot

## Function Calls

### Associated Function Calls

Associated functions are functions that are associated with a type. They are called using the type name followed by two colons and the function name. It doesn't pass in `self` as the first parameter.

```swamp
struct MyStruct {
}
impl MyStruct {
    fn add(a: Int, b: Int) -> Int {
        a + b
    }
}   

x = MyStruct::add(1,2) 
```

### Standalone Function Calls


```swamp
fn add(a: Int, b: Int) -> Int {
    a + b
}

x = add(1,2)
```

### Member Function Calls

`self` is automatically passed in as the first parameter.

```swamp
struct MyStruct {
    x: Int,
}

impl MyStruct {
    fn add(a: Int, b: Int) -> Int {
        a + b
    }
}   

x = MyStruct {
    x: 10
}

x.add(1,2)
```

## Construction

### Struct Construction

```swamp
struct Point {
    x: Int,
    y: Int,
}

p = Point { x: 1, y: 2 }
```

### Array Construction

```swamp
arr = [1, 2, 3, 4, 5]
```

### Tuple Construction

```swamp
tup = (1, 2, 3, 4, 5)
```

### Literals

* Integers. 2, 3, 42, 1000000
* Floats. 4.2, 4.4, 4.6, 4.81, 5.0
* Booleans. `true`, `false`
* Strings. "Hello, world!", "Goodbye, world!"
* Arrays. [1, 2, 3, 4, 5]
* Tuples. (1, 2, 3)
* Unit. (). This is rarely used, but useful in some cases to indicate 'no value' / 'nothing'. e.g. functions without return values return unit. `fn do_nothing(a: Int) -> () { }`
* Exclusive Range. 0..10

## Compare

### Match

Matches a value against a series of patterns and executes the corresponding block of code for the first pattern that matches.

```swamp
match x {
    1 => print('x is 1'),
    2 => print('x is 2'),
    _ => print('x is something else')
}
```

## Patterns

### To Variable

Always matches and binds the value to a variable.
```swamp
match x {
    y => print(y)
}
```

### Enum Simple Variant
An enum variant without any fields.

```swamp
match x {
    Started => print('Started'),
    Inactive => print('No value')
}
```

### Tuple

Creates a an "array" of values. Think of it as a struct with unnamed fields.

```swamp
match x {
    (a, b, _) => print(a, b)
}
```

### Enum Tuple

```swamp
match x {
    SomeEnum::Variant(a, b, _) => print(a, b)
}
```


### Struct

NOT IMPLEMENTED YET.


### Enum Variant with Struct


```swamp
match some_struct {
    SomeEnum::Variant {b, c} => print(c, b)
}
```


### Literal

```swamp
match x {
    1 => print('x is 1'),
    2 => print('x is 2'),
    _ => print('x is something else')
}
```


### Wildcard

Always matches. Useful for ignoring a value. Must be last if used.

```swamp
match x {
    _ => print('x is something else')
}
```

## Iterators

- Exclusive Range. `0..10`
- Inclusive Range. `0..=10` (not implemented yet)
- Arrays (not implemented yet)
- Maps. Key value pairs. (not implemented yet)

## Mut Modifier

Used on variables and parameters to indicate that the value can be changed.


### Variables
```swamp
mut x = 10
x = 20
``` 

### Parameters
```swamp
fn my_function(mut a: Int) -> Int {
    a = 20
    a
}
```

### Implementation Block

```swamp
impl MyStruct {
    fn my_function(mut self, a: Int, mut other: Int) -> Int {
        self.x = 20
        other = 30
        self.x + other + a
    }
}
```



