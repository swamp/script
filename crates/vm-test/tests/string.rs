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
external fn print(extra_arg: Int, output: String)

print(23, "🐱if this works it is 🔥😎!")

        "#,
        "
> 0000: enter 50
> 0001: ld32 $0050 00000017
> 0002: str_from_const $0054 @#0000 21
> 0003: host 0001 10
> 0004: hlt 
",
        "
00000000  17 00 00 00 F8 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
        "print",
        |mut args| {
            let extra_arg = args.get_i32();
            assert_eq!(extra_arg, 23);
            let output = args.get_str();
            assert_eq!("🐱if this works it is 🔥😎!", output);
            eprintln!("print({output})");
        },
    );
}

#[test_log::test]
fn string_append() {
    exec_with_host_function(
        r#"
external fn print(extra_arg: Int, output: String)

print(23, "🐱if this works it is 🔥😎!" + "extra")

        "#,
        "
> 0000: enter 50
> 0001: ld32 $0050 00000017
> 0002: str_from_const $0060 @#0000 21
> 0003: str_from_const $006C @#0021 5
> 0004: str_append $0054 $0060 $006C
> 0005: host 0001 10
> 0006: hlt 
",
        "
00000000  17 00 00 00 38 01 00 00  00 00 00 00 00 00 00 00  ....8...........

    ",
        "print",
        |mut args| {
            let extra_arg = args.get_i32();
            assert_eq!(extra_arg, 23);
            let output = args.get_str();
            assert_eq!("🐱if this works it is 🔥😎!extra", output);
            eprintln!("print({output})");
        },
    );
}

#[test_log::test]
fn string_len() {
    exec_with_assembly(
        r#"

a = "🐱if this works it is 🔥😎!"
r = a.len()

        "#,
        "
> 0000: enter 60
> 0001: str_from_const $0000 @#0000 21
> 0002: hlt 
",
        "
00000000  F8 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
    );
}
