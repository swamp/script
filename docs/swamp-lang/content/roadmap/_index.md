+++
title = "Roadmap"
description = "Future plans and development roadmap for Swamp"
+++

- [ ] `use` statement.

```swamp
use external_module
use external_module::{my_function, MyStruct}
use external_module::*
use external_module::{my_function as my_function_alias, MyStruct as MyStructAlias}
```

- [ ] `mod` statement.

```swamp
mod local_module
```

- [ ] Add support for inclusive ranges.
```swamp
0..=10
```

- [ ] Optional types.
```swamp
x: Int?

if x? {
    print(x)
}
```

- [ ] Struct pattern matching.
The variables named are fetched from the struct.

```swamp
match some_struct {
    {b, c} => print(c, b)
}
```

- [ ] Map type
- Type

```swamp
fn returns_map() -> {| String, String |} {
    {| 'key1': 'value1', 'key2': 'value2' |}
}
```

or maybe with keyword `map` or `Map`

```swamp
fn returns_map() -> Map<String, String> {
    {| 'key1': 'value1', 'key2': 'value2' |}
}
```


- Literal
```swamp
some_map = {|
    'key1': 'value1',
    'key2': 'value2'
|}
```

- Iterator
```swamp
for (key, value) in some_map {
    print(key, value)
}
```

- Assignment
```swamp
some_map['key1'] = 'new_value'
```

- Lookup
```swamp
value = some_map['key1']
if value? {
    print(value)
}
```

# Iterators
- [ ] Inclusive Range. `0..=10`
- [ ] Arrays
- [ ] Maps. Key value pairs.


- [ ] Pipes `<|` and `|>`.
- [ ] Operator implementation

```swamp
impl Add for MyOwnType {
    fn add(self, other: MyOwnType) -> MyOwnType {
        // ...
    }
}
```

- [ ] support for `Self` (as an alias for the type itself) in implementation blocks.

