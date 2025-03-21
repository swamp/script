use swamp_script_vm_test::util::exec_with_host_function;

#[test_log::test]
fn host_call() {
    exec_with_host_function(
        "

external fn some_test(i: Int)

some_test(-42)

        ",
        "
> 0000: enter 50
> 0001: ld32 $0050 0000002A
> 0002: sneg32 $0050 $0050
> 0003: host 0001 4
> 0004: hlt 
",
        "
00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
        "some_test",
        |mut args| {
            let argument = args.get_i32();
            eprintln!("you called me {argument}");
        },
    );
}
