use swamp_script_vm_test::util::{exec, exec_vars, gen_code};

#[test]
fn variable_definition() {
    exec(
        "a = 3",
        "
00000000  03 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test]
fn var_def_add() {
    exec(
        "a = 3 + 7",
        "
00000000  0A 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test]
fn var_def_if() {
    exec(
        "a = if 3 < 7 { 128 } else { 23 }",
        "
00000000  80 00 00 00 01 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn while_loop() {
    exec_vars(
        "
        mut a = 1
        while a < 32767 {
            a += 1
        }
        ",
        "
00000000  FF 7F 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn call() {
    gen_code(
        "
        fn another_fn(a: Int, b: Int) -> Int {
            a+b
        }
        a = 20
        result = another_fn(a, 10)
        ",
        "
> 0000: enter 58 # variables:
  $0000:4 a
  $0004:4 result

> 0001: ld32 $0000 00000014 # int literal
> 0002: mov $005C $0000 4 # variable access 'a' ()
> 0003: ld32 $0060 0000000A # int literal
> 0004: call @7 # calling another_fn (frame size: frame size: 0058)
> 0005: mov $0004 $0058 4 # copy the return value to destination
> 0006: hlt  #
- another_fn -
> 0007: enter 5C # variables:
  $0004:4 a
  $0008:4 b

> 0008: sadd32 $0000 $0004 $0008 # i32 add
> 0009: ret  #
    ",
    );
}

#[test_log::test]
fn call_exec() {
    exec(
        "
        fn another_fn(a: Int, b: Int) -> Int {
            a+b
        }
        a = 20
        result = another_fn(a, 10)
        ",
        "
00000000  14 00 00 00 1E 00 00 00  14 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn call_mut_argument() {
    exec(
        "
        fn add_and_overwrite(a: Int, mut b: Int) {
            b = a+b
        }

        a = 20
        mut b = 30
        add_and_overwrite(a, mut b)
        ",
        "
00000000  14 00 00 00 32 00 00 00  00 00 00 00 00 00 00 00  ....2...........
    ",
    );
}

#[test_log::test]
fn call_struct_function() {
    exec(
        "
        struct Something {
            a: Int,
            b: Int,
        }

        fn add(mut s: Something, v: Int) {
            s.b += v
        }

        mut s = Something { a: 10, b: 20 }
        add(mut s, 40)
        ",
        "
00000000  14 00 00 00 32 00 00 00  00 00 00 00 00 00 00 00  ....2...........
    ",
    );
}

#[test_log::test]
fn call_associated_function() {
    exec(
        "
        struct Something {
            a: Int,
        }

        impl Something {
            fn add(mut self, b: Int) {
                self.a += b
            }
        }

        mut s = Something { a: 10 }
        s.add(40)
        ",
        "
00000000  14 00 00 00 32 00 00 00  00 00 00 00 00 00 00 00  ....2...........
    ",
    );
}
