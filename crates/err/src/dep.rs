use crate::Report;
use eira::Kind;
use std::io::stderr;
use std::path::Path;
use swamp_script_dep_loader::{DependencyError, ParseRootError};
use swamp_script_node::Span;
use swamp_script_source_map::SourceMap;

use crate::Builder;
use crate::parse::build_parser_error;

#[must_use]
pub fn build_dependency_error(err: &DependencyError, _source_map: &SourceMap) -> Builder<usize> {
    let span = &Span::default();
    match err {
        DependencyError::CircularDependency(err) => {
            Report::build(Kind::Error, 10, "circular dependency", span)
        }
        DependencyError::ParseRootError(parse_root) => match parse_root {
            ParseRootError::ParserError(err) => build_parser_error(err),
        },
        DependencyError::ReadFileError(_) => {
            Report::build(Kind::Error, 10, "circular dependency", span)
        }
    }
}

/// # Panics
///
pub fn show_dependency_error(err: &DependencyError, source_map: &SourceMap, current_dir: &Path) {
    let builder = build_dependency_error(err, source_map);
    let report = builder.build();
    report.print(source_map, current_dir, stderr()).unwrap();
}
