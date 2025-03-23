use swamp_script_vm_test::util::exec_with_assembly;

#[test_log::test]
fn match_simple() {
    exec_with_assembly(
        "

enum Test {
    Simple,
    Another,
    Third,
}

a = Test::Another

result = match a {
    Another => 2,
    Third => 3,
    _ => 0
}
        ",
        "
> 0000: enter 58
> 0001: ld8 $0000 01 ;  a = Test::Another
> 0002: eq8 $0000 01 ; match Another (index 1)
> 0003: bnz @6
> 0004: ld32 $0004 00000002 ; result = 2
> 0005: jmp @B ; exit
> 0006: eq8 $0000 02 ; match Third (index 2)
> 0007: bnz @A ; exit
> 0008: ld32 $0004 00000003
> 0009: jmp @B
> 000A: ld32 $0004 00000000
> 000B: hlt 

",
        "
00000000  01 00 00 00 03 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
    );
}

#[test_log::test]
fn match_simple_with_guard() {
    exec_with_assembly(
        "

enum Test {
    Simple,
    Another,
    Third,
}

sun_is_shining = false
a = Test::Third

result = match a {
    Another => 2,
    Third | sun_is_shining => 3,
    _ => 0
}
        ",
        "
> 0000: enter 58
> 0001: ld8 $0000 01 ;  a = Test::Another
> 0002: eq8 $0000 01 ; match Another (index 1)
> 0003: bnz @6
> 0004: ld32 $0004 00000002 ; result = 2
> 0005: jmp @9 ; exit
> 0006: eq8 $0000 02 ; match Third (index 2)
> 0007: bnz @9 ; exit
> 0008: ld32 $0004 00000003 ; r
> 0009: hlt 

",
        "
00000000  01 00 00 00 03 00 00 00  00 00 00 00 00 00 00 00  ................

    ",
    );
}
