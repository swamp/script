/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::rc::Rc;
use swamp_script_analyzer::{ResolveError, Resolver};
use swamp_script_ast::ModulePath;
use swamp_script_dep_loader::ParseModule;
use swamp_script_semantic::{ResolvedModule, ResolvedModuleRef, ResolvedProgram};

pub mod prelude;
