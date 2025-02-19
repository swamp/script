/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use clap::{Parser, Subcommand};

use std::fmt::{Display, Formatter};
use std::io;
use std::path::{Path, PathBuf};
use swamp_script_analyzer::prelude::Error;
use swamp_script_analyzer::Program;
use swamp_script_compile::{compile_analyze_and_link_without_version, compile_and_analyze};
use swamp_script_dep_loader::{create_source_map, DepLoaderError};
use swamp_script_error_report::{show_script_resolve_error, ScriptResolveError};
use swamp_script_eval::err::ExecuteError;
use swamp_script_parser::prelude::*;
use swamp_script_pretty_print::ModulesDisplay;
use swamp_script_source_map_lookup::SourceMapWrapper;
use tracing_subscriber::EnvFilter;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compiles and build swamp script
    #[command(alias = "b")]
    Build {
        #[arg(default_value = ".")]
        path: PathBuf,

        #[arg(default_value = "main")]
        module: String,
    },
    /// Run the swamp script project
    #[command(alias = "r")]
    Run {
        #[arg(default_value = ".")]
        path: PathBuf,
    },
}

fn init_logging() {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_writer(io::stderr)
        .init();
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    init_logging();

    let cli = Cli::parse();

    command(&cli.command)?;

    Ok(())
}

fn command(command: &Commands) -> Result<(), CliError> {
    match command {
        Commands::Build { path, module } => build(path, module),
        Commands::Run { path } => run(path),
    }
}

#[derive(Debug)]
pub enum CliError {
    IoError(io::Error),
    ParseError(ParseError),
    ResolveError(Error),
    ScriptResolveError(ScriptResolveError),
    ExecuteError(ExecuteError),
    DepLoaderError(DepLoaderError),
    Other(String),
}

impl Display for CliError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for CliError {}

impl From<io::Error> for CliError {
    fn from(value: io::Error) -> Self {
        Self::IoError(value)
    }
}

impl From<ExecuteError> for CliError {
    fn from(value: ExecuteError) -> Self {
        Self::ExecuteError(value)
    }
}

impl From<Error> for CliError {
    fn from(value: Error) -> Self {
        Self::ResolveError(value)
    }
}

impl From<DepLoaderError> for CliError {
    fn from(value: DepLoaderError) -> Self {
        Self::DepLoaderError(value)
    }
}

impl From<ParseError> for CliError {
    fn from(value: ParseError) -> Self {
        Self::ParseError(value)
    }
}

impl From<String> for CliError {
    fn from(value: String) -> Self {
        Self::Other(value)
    }
}

impl From<ScriptResolveError> for CliError {
    fn from(value: ScriptResolveError) -> Self {
        Self::ScriptResolveError(value)
    }
}

pub struct CliContext;

#[allow(clippy::too_many_lines)]
fn build(root_path: &Path, root_module: &str) -> Result<(), CliError> {
    /*
       // mangrove::render
       let mangrove_render_module_path = &["mangrove-0.0.0".to_string(), "render".to_string()];
       compile_analyze_and_link_without_version(
           mangrove_render_module_path,
           &mut resolved_program,
           &mut source_map,
       )?;

       // mangrove::collection
       let mangrove_collection_module_path = &["mangrove-0.0.0".to_string(), "collection".to_string()];
       compile_analyze_and_link_without_version(
           mangrove_collection_module_path,
           &mut resolved_program,
           &mut source_map,
       )?;


       {
           //let mangrove_collection_module_path_without_version =
           //  &["mangrove".to_string(), "collection".to_string()];
           //let mangrove_collection_module = resolved_program
           //  .modules
           //.get(mangrove_collection_module_path_without_version)
           //.unwrap();

           //let md = mangrove_collection_module.borrow_mut();
           //let mut ns = md.namespace.borrow_mut();
       }

       let result = compile_and_analyze(
           &["crate".to_string(), root_module.to_string()],
           &mut resolved_program,
           &mut source_map,
       );

       let lookup = SourceMapWrapper { source_map };
       match result {
           Ok(()) => {
               eprintln!(
                   "{}",
                   ModulesDisplay {
                       resolved_modules: &resolved_program.modules,
                       source_map: &lookup
                   }
               );
           }
           Err(err) => {
               show_script_resolve_error(&err, &lookup.source_map);
           }
       }
    */
    Ok(())
}

fn run(_path: &Path) -> Result<(), CliError> {
    Ok(())
}
