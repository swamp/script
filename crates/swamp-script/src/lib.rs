/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_script_analyzer::prelude::Error;
use swamp_script_analyzer::prelude::Program;
use swamp_script_dep_loader::DepLoaderError;
use swamp_script_source_map::SourceMap;
pub mod prelude;
use swamp_script_eval_loader::EvalLoaderError;
use swamp_script_semantic::SemanticError;

#[derive(Debug)]
pub enum ScriptError {
    Error(Error),
    DepLoaderError(DepLoaderError),
}

impl From<Error> for ScriptError {
    fn from(err: Error) -> Self {
        Self::Error(err)
    }
}

impl From<DepLoaderError> for ScriptError {
    fn from(err: DepLoaderError) -> Self {
        Self::DepLoaderError(err)
    }
}

#[derive(Debug)]
pub enum LoaderErr {
    CouldNotLoad,
    SemanticError(SemanticError),
    AnalyzerError(Error),
}

impl From<Error> for LoaderErr {
    fn from(value: Error) -> Self {
        LoaderErr::AnalyzerError(value)
    }
}

pub fn compile_and_analyze(
    module_path: &[String],
    resolved_program: &mut Program,
    source_map: &mut SourceMap,
) -> Result<(), EvalLoaderError> {
    swamp_script_eval_loader::compile_and_analyze_all_modules(
        module_path,
        resolved_program,
        source_map,
    )
}
