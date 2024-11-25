use crate::util::check;

mod util;
#[test_log::test]
fn test1() {
    check(
        r#"struct Hello {
            x: Int,
        }
        "#,
        r#"
          resolved program
        "#,
    )
}
