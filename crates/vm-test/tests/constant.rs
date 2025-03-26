use swamp_script_vm_test::util::exec_with_assembly;
#[test_log::test]
fn gen_constant() {
    exec_with_assembly(
        "
const SOMETHING = 3

a = SOMETHING

        ",
        "
> 0000: enter 54
> 0001: ldconst $0000 @#00000000 4
> 0002: hlt 
- constant SOMETHING -
> 0003: ld32 $0000 00000003
> 0004: hlt 
",
        "
00000000  03 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
    );
}
