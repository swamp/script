use crate::util::check;

mod util;
#[test_log::test]
fn basic() {
    check(
        r#"
        struct Hello {
            x: Int,
            y: Int,
        }
        "#,
        r#"

        "#,
    )
}


#[test_log::test]
fn enum_basic() {
    check(
        r#"
        struct Hello {
        x: Int,}

        enum SomeEnum {
            Simple,
            WithTuple(Int, String, Hello2),
        }
        "#,
        r#"

        "#,
    )
}
