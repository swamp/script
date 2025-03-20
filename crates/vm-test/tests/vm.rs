use swamp_script_vm_test::util::{exec, exec_show_constants, exec_vars, gen_code};

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
> 0005: mov $0004 $0058 4 # copy the ret value to destination
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
00000000  32 00 00 00 14 00 00 00  28 00 00 00 00 00 00 00  2.......(.......
    ",
    );
}

#[test_log::test]
fn call_associated_function() {
    exec(
        "
struct Something {
    a: Int,
    b: Bool,
}

impl Something {
    fn own_add(mut self, b: Int) {
        self.a += b
    }
}

mut s = Something { a: 10, b: false }
s.own_add(40)
        ",
        "
00000000  32 00 00 00 00 00 00 00  28 00 00 00 00 00 00 00  2.......(.......
    ",
    );
}

#[test_log::test]
fn enum_literal() {
    exec(
        "
enum Something {
    First,
    Second,
}

a = Something::Second
        ",
        "
00000000  01 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn enum_literal_tuple_one() {
    exec(
        "
enum Something {
    First,
    Second(Int),
}

a = Something::First
        ",
        "
00000000  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn enum_literal_tuple_second() {
    exec(
        "
enum Something {
    First,
    Second(Int),
}

a = Something::Second(42)
        ",
        "
00000000  01 00 00 00 2A 00 00 00  00 00 00 00 00 00 00 00  ....*...........
    ",
    );
}

#[test_log::test]
fn enum_literal_struct() {
    exec(
        "
enum Something {
    First,
    Second { x: Int, y: Float },
}

a = Something::Second {
    x: 10, y: 2.4,
     }
        ",
        "
00000000  01 00 00 00 0A 00 00 00  66 66 02 00 00 00 00 00  ........ff......
    ",
    );
}

#[test_log::test]
fn enum_literal_both() {
    exec(
        "
enum Something {
    First,
    Second { x: Int, y: Float },
    Third(Int, Int, Int, Int, Float)
}

a = Something::Second {
    x: 10, y: 2.4,
     }
        ",
        "
00000000  01 00 00 00 0A 00 00 00  66 66 02 00 00 00 00 00  ........ff......
    ",
    );
}

#[test_log::test]
fn enum_literal_both_third() {
    exec(
        "
enum Something {
    First,
    Second { x: Int, y: Float },
    Third(Int, Int, Int, Int, Float)
}

a = Something::Third(1,2,3,4,7.6)
        ",
        "
00000000  02 00 00 00 01 00 00 00  02 00 00 00 03 00 00 00  ................
    ",
    );
}

#[test_log::test]
fn enum_literal_both_third_sub_struct() {
    exec(
        "
struct SubStruct {
  x: Int,
   y: Int,
}

enum Something {
    First,
    Second(SubStruct),
    Third(Int, Int, Int, Float)
}

a = Something::Second(SubStruct{x:99, y:128})
        ",
        "
00000000  01 00 00 00 63 00 00 00  80 00 00 00 00 00 00 00  ....c...........
    ",
    );
}

#[test_log::test]
fn string_literal() {
    exec_show_constants(
        r#"

a = "hello"
        "#,
        "
00000000  05 00 05 00 FA FF 00 00  00 00 00 00 00 00 00 00  ................

    ",
        "
00000000  00 00 00 00 00 00 00 00  00 00 68 65 6C 6C 6F 00  ..........hello.
    ",
    );
}

#[test_log::test]
fn string_literal_return() {
    exec_show_constants(
        r#"
fn get_string() -> String {
    "hello"
}
a = get_string()
        "#,
        "
00000000  05 00 05 00 FA FF 00 00  00 00 00 00 00 00 00 00  ................

    ",
        "
00000000  00 00 00 00 00 00 00 00  00 00 68 65 6C 6C 6F 00  ..........hello.
    ",
    );
}

#[test_log::test]
fn vec() {
    exec(
        "
result = [10, 20, 3]
b = 0x0BAD_CAFE
        ",
        "
00000000  03 00 03 00 04 00 CD 00  FE CA AD 0B 00 00 00 00  ................

    ",
    );
}

#[test_log::test]
fn map() {
    exec(
        "
struct Something {
    is_cool: Bool,
    f: Float,
}

result = [
    10 : Something { is_cool: true, f: 42.0 },
    20: Something { is_cool: false, f: 99.0 },
    3: Something { is_cool: true, f: 0.33 }
]

b = 0x0BAD_CAFE
        ",
        "
00000000  03 00 08 00 D0 00 04 00  08 00 00 00 FE CA AD 0B  ................

    ",
        // 03 00 - len() of map
        // 08 00 - number of buckets
        // 00 0D - bucket pointer
        // 04 00 - key size
        // 08 00 - value size
        // | 0              | length       | u16  | 2            | Number of entries in the map     |
        // | 2              | capacity     | u16  | 2            | Total bucket count (power of 2)  |
        // | 4              | buckets_ptr  | u16  | 2            | Pointer to bucket array          |
        // | 6              | key_size     | u16  | 2            | Key size in bytes                |
        // | 8              | value_size   | u16  | 2            | Value size in bytes              |
    );
}

#[test_log::test]
fn map_remove() {
    exec(
        "
struct Something {
    is_cool: Bool,
    f: Float,
}

mut result = [
    10 : Something { is_cool: true, f: 42.0 },
    20: Something { is_cool: false, f: 99.0 },
    3: Something { is_cool: true, f: 0.33 }
]

result.remove(10)

b = 0x0BAD_CAFE
        ",
        "
00000000  02 00 08 00 D0 00 04 00  08 00 00 00 FE CA AD 0B  ................

    ",
        // 03 00 - len() of map
        // 08 00 - number of buckets
        // 00 0D - bucket pointer
        // 04 00 - key size
        // 08 00 - value size
        // | 0              | length       | u16  | 2            | Number of entries in the map     |
        // | 2              | capacity     | u16  | 2            | Total bucket count (power of 2)  |
        // | 4              | buckets_ptr  | u16  | 2            | Pointer to bucket array          |
        // | 6              | key_size     | u16  | 2            | Key size in bytes                |
        // | 8              | value_size   | u16  | 2            | Value size in bytes              |
    );
}
