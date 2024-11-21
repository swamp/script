/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use clap::{Parser, Subcommand};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};
use std::{fs, io};
use swamp_script_eval::value::Value::Unit;
use swamp_script_eval::{ExecuteError, Interpreter, ValueWithSignal};
use swamp_script_parser::prelude::*;
use tracing::{debug, error, info};
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    init_logging();
    let cli = Cli::parse();
    let result = command(&cli.command);

    match result {
        Ok(value_with_signal) => {
            info!("returned: {:?}", value_with_signal);
            match value_with_signal {
                ValueWithSignal::Value(v) => {
                    eprintln!("{}", v);
                }

                ValueWithSignal::Return(_) => panic!("return() should not happen"),
                ValueWithSignal::Break => panic!("break should not happen"),
                ValueWithSignal::Continue => panic!("continue should not be a value"),
            }
            Ok(())
        }
        Err(err) => {
            error!("{err:?}");
            Err(Box::new(err))
        }
    }
}

fn command(command: &Commands) -> Result<ValueWithSignal, CliError> {
    match command {
        Commands::Build { path } => {
            let _ = build(path)?;
            Ok(ValueWithSignal::Value(Unit))
        }
        Commands::Run { path } => run(path),
    }
}

#[derive(Debug)]
pub enum CliError {
    IoError(std::io::Error),
    ParseError(pest::error::Error<Rule>), // TODO: pest should not leak through here
    ExecuteError(ExecuteError),
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

impl From<pest::error::Error<Rule>> for CliError {
    fn from(value: pest::error::Error<Rule>) -> Self {
        Self::ParseError(value)
    }
}

impl From<String> for CliError {
    fn from(value: String) -> Self {
        Self::Other(value)
    }
}

// In the future we want to support directories with a project swamp.toml, but for now
// just resolve to single .swamp file
fn resolve_swamp_file(path: &Path) -> Result<PathBuf, String> {
    if !path.exists() {
        return Err(format!("Path does not exist: {}", path.display()));
    }

    if path.is_dir() {
        let main_file = path.join("main.swamp");
        if !main_file.exists() {
            return Err(format!(
                "No main.swamp found in directory: {}",
                path.display()
            ));
        }
        Ok(main_file)
    } else if path.extension().and_then(|ext| ext.to_str()) == Some("swamp") {
        Ok(path.to_path_buf())
    } else {
        Err(format!("Not a .swamp file: {}", path.display()))
    }
}

fn compile(path: &Path) -> Result<Program, CliError> {
    debug!("compile {path:?}");
    let swamp_file = resolve_swamp_file(path)?;
    let absolute_path = fs::canonicalize(&swamp_file)?;
    debug!("found_file {absolute_path:?}");

    let parser = swamp_script_parser::AstParser::new();
    debug!("reading {}", swamp_file.display());
    let file = fs::read_to_string(&swamp_file)?;
    Ok(parser.parse_script(&*file)?)
}

fn build(path: &PathBuf) -> Result<(), CliError> {
    let program = compile(path)?;
    debug!("compiled to:\n{}", program);
    Ok(())
}

fn run(path: &PathBuf) -> Result<ValueWithSignal, CliError> {
    let program = compile(path)?;
    let mut eval = Interpreter::new();

    Ok(eval.eval_program(program)?)
}
