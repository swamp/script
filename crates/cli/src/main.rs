/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use clap::{Parser, Subcommand};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io;
use std::path::{Path, PathBuf};
use swamp_script_analyzer::prelude::ResolveError;
use swamp_script_analyzer::ResolvedProgram;
use swamp_script_compile::{compile_and_analyze, ScriptResolveError};
use swamp_script_dep_loader::{create_source_map, DepLoaderError};
use swamp_script_error_report::show_script_resolve_error;
use swamp_script_eval::err::ExecuteError;
use swamp_script_parser::prelude::*;
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
        .with_writer(std::io::stderr)
        .init();
}

fn main() -> Result<(), Box<dyn Error>> {
    init_logging();
    let cli = Cli::parse();

    match &cli.command {
        Commands::Build { path, module } => {
            println!("Building swamp script at path: {}", path.display());
            // Call your build function here, passing the path
            if let Err(e) = build(path, module) {
                eprintln!("Error during build: {}", e);
                // Consider returning an error from main if build fails significantly
                // return Err(e); // If build failure should halt the program
            }
        }
        Commands::Run { path } => {
            println!("Running swamp script at path: {}", path.display());
            // Call your run function here, passing the path
            if let Err(e) = run(path) {
                eprintln!("Error during run: {}", e);
                // Consider returning an error from main if run fails significantly
                // return Err(e); // If run failure should halt the program
            }
        }
    }

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
    ResolveError(ResolveError),
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

impl Error for CliError {}

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

impl From<ResolveError> for CliError {
    fn from(value: ResolveError) -> Self {
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

fn build(root_path: &Path, root_module: &str) -> Result<(), CliError> {
    let mut source_map = create_source_map(root_path)?;
    let mut resolved_program = ResolvedProgram::new();

    // mangrove::render
    let mangrove_render_result = compile_and_analyze(
        &["mangrove-0.0.0".to_string(), "render".to_string()],
        &mut resolved_program,
        &mut source_map,
    );

    match mangrove_render_result {
        Ok(program) => {
            eprintln!("{program:?}");
        }
        Err(err) => {
            show_script_resolve_error(&err, &source_map);
        }
    }

    let result = compile_and_analyze(
        &["crate".to_string(), root_module.to_string()],
        &mut resolved_program,
        &mut source_map,
    );
    match result {
        Ok(program) => {
            eprintln!("{program:?}");
        }
        Err(err) => {
            show_script_resolve_error(&err, &source_map);
        }
    }

    Ok(())
}

fn module_path() -> Vec<String> {
    vec!["main".to_string()]
}

fn run(path: &Path) -> Result<(), CliError> {
    Ok(())
}
