# Basics of the Swamp Script Langauge

## Primitive Types

- `Int`: Whole numbers (`42`). Represented as 32-bit integer.
- `Float`: Decimal numbers (`12.98`). Represented as a 32-bit fixed point number.
- `String`: Text (`"hello"` or `'hello'`)
- `Bool`: True/false (`true` or `false`)
- `Unit`: Nothing (`()`)

## Composite Types

- `Struct`: Named fields
- `Tuple`: Ordered values
- `Array`: Lists
- `Enum`: Variants
- `Map`: Key-value pairs _(coming soon!)_
- `_Any`: _(used internally in the compiler)_

## Literals

- Int. `42`. integer number without a dot.
- Float. `12.98`, *must* include a `.`.
- String. `'interpolation string'` or `"normal string"`
- Bool `true` or `false`
- Unit. `()`. "nothing"
- Enum: `EnumType::VariantName {x: 10, y: 20}`. Variant can hold either Unit, Tuple or Struct.
- Tuple: `(10, "hello, world", 42.19)`.


## Operators

### Binary

- **`+`**: Add
- **`-`**: Subtract
- **`*`**: Multiply
- **`/`**: Divide
- **`%`**: Modulo
- **`||`**: LogicalOr
- **`&&`**: LogicalAnd
- **`==`**: Equal
- **`!=`**: NotEqual
- **`<`**: LessThan
- **`<=`**: LessEqual
- **`>`**: GreaterThan
- **`>=`**: GreaterEqual
- **`<=`**: LessThanOrEqual
- **`>=`**: GreaterThanOrEqual
- **`..`**: RangeExclusive

### Unary

- **`!`**: Not
- **`-`**: Negate

## Expressions

### Access

- Field Access. `some_struct.field`.
- Variable Access. `just_a_var_name`.
- Array Access. `some_array[13]`

### Assignment

- Array Assignment `some_array[13] = "hello"`
- Variable Assignment. `some_var = 2.33`
- Field Assignment. `some_struct.field = 98`

### Calls

- Function Call. `one_function(param1, param2)`.
- Member Call. `some_struct.implemented_function(param1, param2)`
- String interpolation. `'some text {x}, {struct.function(param1)} {y:X}'`. With formatting: (?, x, X, b, f, precision-number-prefix)
- _Block (internal)_

### Construction

- Struct Create. `StructName { field1: 2, field2: another_value }`
- Array Create. `[value, value2]`.
- Tuple Create. `("hello world", 23.3, 2)`
- (Map Create - under development)
- Exclusive Range `-2..15`

### Compare

- If. `if x > 10 { 'high' } else {'low' }`. Mandatory else when used as an expression.
- Match. `match x { 2 => do_this(), _ => otherwise_that() }`

### Other

- _MutRef (internal)_


## Statements

- Let. `variable = 3`
- If. `if a < 0 { "value is negative" }`. `else` is optional.
- For. `for a in 0..3`.
- While. `while s < 2 { s = s + 1 }`
- Return. `return 19.49`. Return early from a scope.
- Break. `break`. Breaks current iteration.
- Continue. `continue`. Continues the current iteration, skipping the rest of the statements in the scope.
- _Block (internal)_


## Definitions

- Struct `struct Name { a: Int, b: String }`
- Enum `enum SomeName { Single, Multi(Int, Float, Int), MoreInfo { amount: Int } }`
- Function. `fn some_name(param1: Int, param2: Float) -> Int` or optional return if there are side effects: `fn some_name(param1: String)`
- Impl `impl SomeStruct { fn some_name(self) -> { self.x }  fn another(mut self, v: Int) { self.x = v} }`
- External `external fn a_name(param1: Float) -> Float`. 
- _Import. `import math` (not supported yet)_
- DocComment `/// this will be documentation`


## Patterns

- **Variable**: Matches everything, basically a `let`
- **Tuple**: `(x, _, y)`
- **Struct**:  `{ x, y }`
- **Literal**: `23`
- **Enum Tuple**: `VariantName(x, y)`
- **Enum Struct**: `VariantName {a, b, c}`
- **Enum Simple**: `VariantName`
- **Wildcard** `_`: Matches everything
  