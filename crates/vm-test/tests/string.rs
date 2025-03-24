use swamp_script_vm_test::util::{exec_with_assembly, exec_with_host_function};
#[test_log::test]
fn gen_string() {
    exec_with_assembly(
        r#"
result = "hello, world"

        "#,
        "
> 0000: enter 5C
> 0001: str_from_const $0000 @#0000 C
> 0002: hlt 
",
        "
00000000  E0 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
    );
}

#[test_log::test]
fn string_with_print() {
    exec_with_host_function(
        r#"

external fn print(output: String)

print("hello, world!")

        "#,
        "
> 0000: enter 50
> 0001: str_from_const $0050 @#0000 D
> 0002: host 0001 C
> 0003: hlt 
",
        "
00000000  E0 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
        "print",
        |mut args| {
            let output = args.get_str();
            eprintln!("print({output})");
        },
    );
}

#[test_log::test]
fn string_with_emoji_print() {
    exec_with_host_function(
        r#"
external fn print(output: String)

print("🐱if this works it is 🔥😎!")

        "#,
        "
> 0000: enter 50
> 0001: str_from_const $0050 @#0000 21
> 0002: host 0001 C
> 0003: hlt 
",
        "
00000000  F8 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
        "print",
        |mut args| {
            let output = args.get_str();
            eprintln!("print({output})");
        },
    );
}
