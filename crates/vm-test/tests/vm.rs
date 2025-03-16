use swamp_script_vm_test::util::{exec, exec_vars, gen_code};

#[test]
fn variable_definition() {
    exec(
        "a = 3",
        "
00000000  03 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test]
fn var_def_add() {
    exec(
        "a = 3 + 7",
        "
00000000  0A 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test]
fn var_def_if() {
    exec(
        "a = if 3 < 7 { 128 } else { 23 }",
        "
00000000  80 00 00 00 00 00 00 00  01 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn while_loop() {
    exec_vars(
        "
        mut a = 1
        while a < 32767 {
            a += 1
        }
        ",
        "
00000000  FF 7F 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn call() {
    gen_code(
        "
        fn another_fn(a: Int, b: Int) -> Int {
            a+b
        }
        a = 20
        result = another_fn(a, 10)
        ",
        "
00000000  FF 7F 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}
