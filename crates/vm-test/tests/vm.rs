use swamp_script_vm_test::util::{exec, exec_vars};

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
00000000  80 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn while_loop() {
    exec_vars(
        "
        mut a = 1
        while a < 32000 {
            a += 1
        }
        ",
        "
00000000  00 7D 00 00 00 00 00 00  00 00 00 00 00 00 00 00  .}..............
    ",
    );
}
