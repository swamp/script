use swamp_script_vm_test::util::exec_with_assembly;

#[test_log::test]
fn vec_iter() {
    exec_with_assembly(
        "

a = [8, 16, 24, 96, 32]

for i in a {
    b = 99
}
        ",
        "
> 0000: enter 5C
> 0001: ld32 $005C 00000008
> 0002: ld32 $0060 00000010
> 0003: ld32 $0064 00000018
> 0004: ld32 $0068 00000060
> 0005: ld32 $006C 00000020
> 0006: vec_from_slice $0000 $005C 4 0005
> 0007: vec_iter_init $0070 ($0000)
> 0008: vec_iter_next $0070 $0004 @B
> 0009: ld32 $0008 00000063
> 000A: jmp @8
> 000B: hlt 
",
        "
00000000  E8 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................ ; E8 00 00 00 is the allocated header pointer

    ",
    );
}
