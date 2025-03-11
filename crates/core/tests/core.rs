use swamp_script_core::create_module;
use tiny_ver::TinyVersion;
use tracing::info;

#[test_log::test]
fn test_core() {
    let version: TinyVersion = "1.2.3".parse().unwrap();

    let module = create_module(&version);

    let x = format!("output:{module:?}");

    info!(x, "output");
}
