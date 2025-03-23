use swamp_script_vm_test::util::exec_with_assembly;

#[test_log::test]
fn vec_iter() {
    exec_with_assembly(
        "

a = [8, 16, 24]

for i in a {
    b = 99
}

        ",
        "
> 0001: ld32  $0060 00000008 ; int literal (i32, int:8)
> 0002: ld32  $0064 00000010 ; int literal (i32, int:16)
> 0003: ld32  $0068 00000018 ; int literal (i32, int:24)
> 0004: map_iter_init $0080 @FFFFFFFF ; initialize map iterator (*b8, constant)
> 0005: map_iter_next $0080 $0016 @100 ; move to next or jump over (*b8, *b8)
> 0006: hlt   
",
        "
00000000  17 00 00 00 31 00 00 00  02 00 00 00 00 00 00 00  ....1...........

    ",
    );
}
