use crate::ScriptResolveError;
use crate::analyze::build_analyzer_error;
use crate::loader::build_loader_error;
use std::io::stderr;
use std::path::Path;
use swamp_script_source_map::SourceMap;

use crate::Builder;
#[must_use]
pub fn build_script_error(err: &ScriptResolveError, _source_map: &SourceMap) -> Builder<usize> {
    match err {
        ScriptResolveError::AnalyzerError(err) => build_analyzer_error(err),
        ScriptResolveError::DepLoaderError(err) => panic!("{}", format!("err: {:?}", err)),
        ScriptResolveError::DependencyError(err) => panic!("{}", format!("err: {:?}", err)),
        ScriptResolveError::LoaderError(err) => build_loader_error(err),
    }
}

/// # Panics
///
pub fn show_script_resolve_error(
    err: &ScriptResolveError,
    source_map: &SourceMap,
    current_dir: &Path,
) {
    let builder = build_script_error(err, source_map);
    let report = builder.build();
    report.print(source_map, current_dir, stderr()).unwrap();
}
