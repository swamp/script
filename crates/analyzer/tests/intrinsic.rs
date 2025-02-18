use crate::util::{check, check_fail};

mod util;

#[test_log::test]
fn basic_intrinsic() {
    check(
        r"

fn some_function(x: Float) -> Float {
    core::float_floor(x)
}

         ",
        r#"
some_function: Internal(Signature { parameters: [TypeForParameter { name: "x", resolved_type: Float, is_mutable: false, node: Some(Parameter) }], return_type: Float }
<38:28>Float,Block([<50:11>Float,IntrinsicCall(FloatFloor, [Expression(<62:1>Float,VariableAccess(Variable { name: <19:1>, resolved_type: Float, mutable_node: None, scope_index: 0, variable_index: 0 }))])]))
        "#,
    );
}

#[test_log::test]
fn fail_basic_intrinsic() {
    check_fail(
        r"

fn some_function(x: Float) -> Float {
    core::xfloat_floor(x)
}

         ",
        r"
Error { node: <50:12>, kind: UnknownFunction }
 ",
    );
}

#[test_log::test]
fn fail_basic_intrinsic_in_parameter() {
    check_fail(
        r"

fn some_function(x: Int) -> Float {
    core::float_floor(x)
}

         ",
        r"
Error { node: <60:1>, kind: IncompatibleTypes(Float, Int) }
 ",
    );
}

#[test_log::test]
fn fail_basic_intrinsic_in_return() {
    check_fail(
        r"

fn some_function(x: Float) -> String {
    core::float_floor(x)
}

         ",
        r"
Error { node: <45:20>, kind: IncompatibleTypes(String, Float) }
 ",
    );
}
