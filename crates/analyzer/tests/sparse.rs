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


        ",
    );
}
