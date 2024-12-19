use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub mod ns;



#[derive(Debug)]
pub struct ResolvedProgram {
    pub types: ResolvedProgramTypes,
    pub state: ResolvedProgramState,
    pub modules: ResolvedModules,
}

impl ResolvedProgram {
    pub fn new() -> Self {
        Self {
            types: ResolvedProgramTypes::new(),
            state: ResolvedProgramState::new(),
            modules: ResolvedModules::new(),
        }
    }
}
