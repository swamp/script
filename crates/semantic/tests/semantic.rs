use crate::util::check;

mod util;
#[test_log::test]
fn test1() {
    check(
        r#"
        a = 33
        a = "hello"
        "#,
        r#"
          resolved program
        "#,
    )
}
