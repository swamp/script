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
> 0001: ldconst $0000 @0000 4
> 0002: hlt 
",
        "
00000000  0A 00 00 00 48 E1 D5 FF  B8 1E 2A 00 00 00 00 00  ....H.....*..... ; twos complement is 0xFFD5E148

    ",
    );
}
