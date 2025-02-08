use util::check_doc;
mod util;

#[test]
fn generate() {
    check_doc(
        "
    struct Something {
        x: Int,
    }

    impl Something {
        /// Some comment here
        fn function(mut self, x: Int) -> Int {
            2
        }
    }
    ",
        r#"

## function
<code class="language-swamp" data-lang="swamp">
(  <span class="parameter_name">self</span>  <span class="separator">: </span>  <span class="type_name">Something</span>  <span class="parameter_name">x</span>  <span class="separator">: </span>  <span class="type_name">Int</span>)->  <span class="type_name">Int</span>
</code>

        "#,
    )
}
