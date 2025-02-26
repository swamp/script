use crate::util::check;
mod util;
#[test_log::test]
fn slice_construct() {
    check(
        r"
        fn construct(v: Slice<Int>)
        {

        }
        
        construct([10, 20])
        ",
        r"
construct: (v:Slice<Int>) -> ()
{
}
---
construct([10i, 
20i
])
",
    );
}

#[test_log::test]
fn slice_pair_construct() {
    check(
        "
        fn construct(v: SlicePair<String, Int>) {

        }

construct(['string': 23, 'another': 42])
        ",
        "
construct: (v:SlicePair<String, Int>) -> ()
{
}
---
construct([| InterpolatedString() : 23i, InterpolatedString() : 42i |])
",
    );
}
