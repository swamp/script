# Changelog

## 🔖 [v0.0.8](https://github.com/swamp/script/releases/tag/v0.0.8) (2024-12-30)


### General

* ✨ Keyword `use` reads and parses the specified files.
* ✨ Add `MutStructFieldRef` for location of mutable struct field reference
* 🐞 Wrapping Option values for assignments when needed
* 🐞 Do not wrap `none` with option
* 🐞 Enum variant structs, `ResolvedEnumLiteralData::Struct`, handle fields in source code order
* 🔥 Type aliases
* 🎨 More errors have `Span` information

## 🔖 [v0.0.5](https://github.com/swamp/script/releases/tag/v0.0.5) (2024-12-06)

- Array extends, push, remove and clear.
- Fixed bug where empty parameters caused return type to be `Unit`.
- Map assignment and map type.
- Optional operator in if statements and if-expressions.
- `none` keyword

## 🔖 [v0.0.4](https://github.com/swamp/script/releases/tag/v0.0.4) (2024-12-03)

- Add context to Interpreter
- Separate out external functions
- Split up resolve_definition and insert_definition

## 🔖 [v0.0.3](https://github.com/swamp/script/releases/tag/v0.0.3) (2024-12-02)

Static calls (`StructType::function(parameters)`), type alias (`type MyAlias = (Int, Float)`) and unification of external, internal and member functions.
