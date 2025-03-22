use swamp_script_vm_test::util::{exec_with_assembly, exec_with_host_function};
#[test_log::test]
fn with() {
    exec_with_host_function(
        "

external fn print(i: Int)

with x = 3 {
    print(x)
}

        ",
        "
> 0000: enter 50
> 0001: ld32 $0050 0000002A
> 0002: sneg32 $0050 $0050
> 0003: ld32 $0054 00000017
> 0004: host 0001 8
> 0005: hlt 
",
        "
00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
        "print",
        |mut args| {
            let i = args.get_i32();
            let b = args.get_i32();
            eprintln!("you called me i:{i} b:{b}");
        },
    );
}
