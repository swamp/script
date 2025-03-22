use swamp_script_vm_test::util::exec_with_assembly;

#[test_log::test]
fn range() {
    exec_with_assembly(
        "
a = 2..4

        ",
        "
> 0000: enter 57
> 0001: ld32 $0000 00000002
> 0002: ld32 $0004 00000004
> 0003: ld8 $0008 00
> 0004: hlt 
",
        "
00000000  02 00 00 00 04 00 00 00  00 00 00 00 00 00 00 00  ................


    ",
    );
}

#[test_log::test]
fn range_inclusive() {
    exec_with_assembly(
        "
a = 2..=4
        ",
        "
> 0000: enter 57
> 0001: ld32 $0000 00000002
> 0002: ld32 $0004 00000004
> 0003: ld8 $0008 01        ; 01 = inclusive
> 0004: hlt 
",
        "
00000000  02 00 00 00 04 00 00 00  01 00 00 00 00 00 00 00  ................
    ",
    );
}
