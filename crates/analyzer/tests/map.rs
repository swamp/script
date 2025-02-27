use crate::util::check;

mod util;

#[test_log::test]
fn map_new() {
    check(
        "

mut instantiated = Map<String, Int>::new()

         ",
        "
let instantiated: Vec<Int> = Vec<Int>::from_slice([10i,
20i
])
        ",
    );
}

#[test_log::test]
fn map_from_slice_pair_auto_coerce() {
    check(
        "

instantiated = [10: 24.4, 20: 48.4]

         ",
        "
let instantiated: Map<Int, Float> = Map<Int, Float>::new([| 10i : 24.40f, 20i : 48.40f |])
        ",
    );
}

#[test_log::test]
fn map_insert() {
    check(
        "

mut instantiated = Map<Int, Float>::new()
instantiated[42] = 4.5
         ",
        "

{
..let instantiated: Sparse<Int> = Sparse<Int>::new()
..instantiated.Sparse<Int>::add(2i)
}
        ",
    );
}

#[test_log::test]
fn vec_subscript() {
    check(
        r"

mut instantiated = Sparse<Int>::new()
key = instantiated.add(2)
value = instantiated[key]

a = with value {
    2
}
         ",
        "

{
..let instantiated: Sparse<Int> = Sparse<Int>::new()
..let key: SparseId = instantiated.Sparse<Int>::add(2i)
..let value: Int? = instantiated.Sparse<Int>::subscript(key)
..let a: Int = {
....let value: Int? = value
....{
......2i
....}
..}
}

        ",
    );
}

#[test_log::test]
fn vec_subscript_float() {
    check(
        r"

mut instantiated = Sparse<Float>::new()
key = instantiated.add(42.5)
value = instantiated[key]

a = with value {
    2
}
         ",
        "

{
..let instantiated: Sparse<Float> = Sparse<Float>::new()
..let key: SparseId = instantiated.Sparse<Float>::add(42.50f)
..let value: Float? = instantiated.Sparse<Float>::subscript(key)
..let a: Int = {
....let value: Float? = value
....{
......2i
....}
..}
}
        ",
    );
}

#[test_log::test]
fn vec_subscript_struct() {
    check(
        r"

struct Something {
   length: Int
   }

mut instantiated = Sparse<Something>::new()
key = instantiated.add(Something { length: 10 } )
value = instantiated[key]

a = with value {
    2
}
         ",
        "

Something: Something {
....length: Int, 
..}
---
{
..let instantiated: Sparse<Something> = Sparse<Something>::new()
..let key: SparseId = instantiated.Sparse<Something>::add(Something {
......length: 10i, 
....})
..let value: Something? = instantiated.Sparse<Something>::subscript(key)
..let a: Int = {
....let value: Something? = value
....{
......2i
....}
..}
}
        ",
    );
}
