use crate::util::check;
mod util;
#[test_log::test]
fn slice_construct() {
    check(
        r"
        fn construct(v: Slice<Int>)
        {

        }
        ",
        r"
construct: (v:Slice<Int>) -> ()
",
    );
}

#[test_log::test]
fn slice_pair_construct() {
    check(
        "
        fn construct(v: SlicePair<String, Int>) {

        }
        ",
        "
construct: (v:SlicePair<String, Int>) -> ()
",
    );
}
