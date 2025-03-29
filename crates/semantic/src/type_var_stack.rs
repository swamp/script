use crate::SemanticError;
use crate::instantiator::TypeVariableScope;
use seq_map::SeqMap;
use swamp_script_types::Type;
use tracing::info;

#[derive(Debug, Default)]
pub struct SemanticContext {
    type_variable_scopes: Vec<TypeVariableScope>,
}

impl SemanticContext {
    pub fn push_type_scope(&mut self) {
        info!("SCOPE pushing type scope");
        self.type_variable_scopes
            .push(TypeVariableScope::new(SeqMap::new()));
    }

    pub fn push_type_scope_with_variables(
        &mut self,
        type_variables: &[String],
    ) -> Result<(), SemanticError> {
        self.push_type_scope();
        for type_variable in type_variables {
            self.declare_type_variable(type_variable)?;
        }
        Ok(())
    }
    /*
    pub fn declare_type_variable_var(&mut self, type_variable: &TypeVariable) -> Result<(), SemanticError> {
        if let Some(current_scope) = self.type_variable_scopes.last_mut() {
            current_scope
                .add_type_variable_var(type_variable)
        } else {
            panic!("Error: No active type variable scope!");
        }
    }*/

    pub fn pop_type_scope(&mut self) {
        info!("SCOPE POP");
        assert!(!self.type_variable_scopes.is_empty());
        self.type_variable_scopes.pop();
    }

    /// # Errors
    ///
    /// # Panics
    /// if no stack is present
    pub fn declare_type_variable(&mut self, name: &str) -> Result<(), SemanticError> {
        info!(?name, "SCOPE declare type");

        self.type_variable_scopes.last_mut().map_or_else(
            || {
                panic!("Error: No active type variable scope!");
            },
            |current_scope| current_scope.add_type_variable(name),
        )
    }

    #[must_use]
    pub fn resolve_type_variable(&self, name: &str) -> Option<Type> {
        for scope in self.type_variable_scopes.iter().rev() {
            if let Some(found_type) = scope.internal_get_type(&name) {
                return Some(found_type.clone());
            }
        }
        None
    }
}
