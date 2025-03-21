use swamp_script_vm_test::util::exec_vars;

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
