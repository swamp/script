use swamp_script_vm_test::util::{exec_with_assembly, exec_with_host_function};

#[test_log::test]
fn host_call() {
    exec_with_host_function(
        "

external fn some_test(i: Int)

some_test(42)

        ",
        "

",
        "
00000000  17 00 00 00 31 00 00 00  02 00 00 00 00 00 00 00  ....1...........

    ",
        "some_test",
        |args| {},
    );
}
