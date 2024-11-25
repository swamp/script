use crate::util::check;

mod util;
#[test_log::test]
fn basic() {
    check(
        r#"
        struct Hello {
            x: Int,
            y: (Int, Int),
        }
        "#,
        r#"

        "#,
    )
}
