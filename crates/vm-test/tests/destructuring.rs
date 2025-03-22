use swamp_script_vm_test::util::exec_with_assembly;
#[test_log::test]
fn gen_tuple_destructuring() {
    exec_with_assembly(
        "
result, _, fixed = (10, 20, -42.12)

        ",
        "
> 0000: enter 58
> 0001: ld32 $0058 0000000A
> 0002: ld32 $005C 00000014
> 0003: ld32 $0064 002A1EB8
> 0004: fneg $0060 $0064
> 0005: mov $0000 $0058 4
> 0006: mov $0004 $0060 4
> 0007: hlt 
",
        "
00000000  0A 00 00 00 48 E1 D5 FF  B8 1E 2A 00 00 00 00 00  ....H.....*..... ; twos complement is 0xFFD5E148

    ",
    );
}
