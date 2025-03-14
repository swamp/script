mod util;
use crate::util::exec;

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
        "if 3 < 7 { 128 } else { 23 }",
        "
00000000  80 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  ................
    ",
    );
}
