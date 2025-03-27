use std::path::{Path, PathBuf};
use swamp_script::prelude::SeqMap;
use swamp_script_code_gen_program::code_gen_program;
use swamp_script_dep_loader::swamp_registry_path;
use swamp_script_source_map::SourceMap;
use swamp_script_source_map_lookup::SourceMapWrapper;

#[test_log::test]
fn compile_and_run() {
    let mut mounts = SeqMap::new();
    let path_buf = Path::new("/Users/peter/external/swamp_autobattler/scripts").to_path_buf();
    mounts.insert("crate".to_string(), path_buf).unwrap();

    let registry_path = swamp_registry_path().unwrap();
    mounts
        .insert("registry".to_string(), registry_path)
        .unwrap();

    let mut source_map = SourceMap::new(&mounts).expect("source map failed");

    let crate_main_path = &["crate".to_string(), "main".to_string()];

    let program = swamp_script_compile::bootstrap_and_compile(&mut source_map, crate_main_path)
        .expect("TODO: panic message");

    let main_module = program.modules.get(crate_main_path).unwrap().clone();

    let source_map_wrapper = SourceMapWrapper {
        source_map: &source_map,
        current_dir: PathBuf::from(Path::new("")),
    };
    code_gen_program(&program, &main_module, &source_map_wrapper).expect("code gen failed");
    //info!(?program, "program");
}
