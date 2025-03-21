use swamp_script_vm_test::util::exec_with_assembly;

#[test_log::test]
fn guard() {
    exec_with_assembly(
        "

a = 23
b = 49

result = 
  | a > 30 && b < 22 -> 1
  | b > 48 -> 2
  | _ -> 99

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
