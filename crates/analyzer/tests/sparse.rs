use crate::util::check;

mod util;

#[test_log::test]
fn sparse_new() {
    check(
        r"

instantiated = Sparse<Int>::new()

         ",
        "


        ",
    );
}

#[test_log::test]
fn sparse_add() {
    check(
        r"

mut instantiated = Sparse<Int>::new()
instantiated.add(2)
         ",
        "

{
..let instantiated: Sparse<[Int]> = Sparse<[Int]>::new()
..instantiated.Sparse<[Int]>::add(2i)
}
        ",
    );
}

#[test_log::test]
fn sparse_subscript() {
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
..let instantiated: Sparse<[Int]> = Sparse<[Int]>::new()
..let key: SparseId = instantiated.Sparse<[Int]>::add(2i)
..let value: Int? = instantiated.Sparse<[Int]>::subscript(key)
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
fn sparse_subscript_float() {
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
..let instantiated: Sparse<[Float]> = Sparse<[Float]>::new()
..let key: SparseId = instantiated.Sparse<[Float]>::add(42.50f)
..let value: Float? = instantiated.Sparse<[Float]>::subscript(key)
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
fn sparse_subscript_struct() {
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

{
..let instantiated: Sparse<[Float]> = Sparse<[Float]>::new()
..let key: SparseId = instantiated.Sparse<[Float]>::add(42.50f)
..let value: Float? = instantiated.Sparse<[Float]>::subscript(key)
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
