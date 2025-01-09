use crate::err::ResolveError;
use crate::{BlockScopeMode, Resolver};
use std::rc::Rc;
use swamp_script_ast::{CompoundOperator, Expression, Node, Variable};
use swamp_script_semantic::{
    ResolvedCompoundOperatorKind, ResolvedExpression, ResolvedNode, ResolvedType, ResolvedVariable,
    ResolvedVariableAssignment, ResolvedVariableCompoundAssignment, ResolvedVariableRef, Spanned,
};

impl<'a> Resolver<'a> {
    fn try_find_local_variable(&self, node: &ResolvedNode) -> Option<&ResolvedVariableRef> {
        let current_scope = self
            .scope
            .block_scope_stack
            .iter()
            .last()
            .expect("no scope stack available");

        let variable_text = self.get_text_resolved(node).to_string();

        current_scope.variables.get(&variable_text)
    }
    #[allow(unused)]
    pub(crate) fn find_variable(
        &self,
        variable: &Variable,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        self.try_find_variable(&variable.name).map_or_else(
            || Err(ResolveError::UnknownVariable(self.to_node(&variable.name))),
            Ok,
        )
    }

    pub(crate) fn find_variable_from_node(
        &self,
        node: &Node,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        self.try_find_variable(node).map_or_else(
            || Err(ResolveError::UnknownVariable(self.to_node(node))),
            Ok,
        )
    }

    pub(crate) fn try_find_variable(&self, node: &Node) -> Option<ResolvedVariableRef> {
        let variable_text = self.get_text(node);

        for scope in self.scope.block_scope_stack.iter().rev() {
            if let Some(value) = scope.variables.get(&variable_text.to_string()) {
                return Some(value.clone());
            }
            if scope.mode == BlockScopeMode::Closed {
                break;
            }
        }

        None
    }

    fn set_or_overwrite_variable_with_type(
        &mut self,
        variable: &Variable,
        variable_type_ref: &ResolvedType,
    ) -> Result<(ResolvedVariableRef, bool), ResolveError> {
        if let Some(existing_variable) = self.try_find_variable(&variable.name) {
            // Check type compatibility
            if !&existing_variable.resolved_type.same_type(variable_type_ref) {
                return Err(ResolveError::OverwriteVariableWithAnotherType(
                    self.to_node(&variable.name),
                ));
            }

            // For reassignment, check if the EXISTING variable is mutable
            if !existing_variable.is_mutable() {
                return Err(ResolveError::CanOnlyOverwriteVariableWithMut(
                    self.to_node(&variable.name),
                ));
            }

            return Ok((existing_variable, true));
        }

        // For first assignment, create new variable with the mutability from the assignment
        let scope_index = self.scope.block_scope_stack.len() - 1;
        let name = self.to_node(&variable.name);
        let mutable_node = self.to_node_option(&variable.is_mutable);
        let variable_name_str = self.get_text_resolved(&name).to_string();

        let variables = &mut self
            .scope
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables;
        let variable_index = variables.len();

        let resolved_variable = ResolvedVariable {
            name,
            resolved_type: variable_type_ref.clone(),
            mutable_node,
            scope_index,
            variable_index,
        };

        let variable_ref = Rc::new(resolved_variable);

        {
            variables
                .insert(variable_name_str, variable_ref.clone())
                .expect("should have checked earlier for variable");
        }

        Ok((variable_ref, false))
    }

    pub(crate) fn create_local_variable(
        &mut self,
        variable: &Node,
        is_mutable: &Option<Node>,
        variable_type_ref: &ResolvedType,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        self.create_local_variable_resolved(
            &self.to_node(variable),
            &self.to_node_option(is_mutable),
            variable_type_ref,
        )
    }

    pub(crate) fn create_local_variable_resolved(
        &mut self,
        variable: &ResolvedNode,
        is_mutable: &Option<ResolvedNode>,
        variable_type_ref: &ResolvedType,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        if let Some(_existing_variable) = self.try_find_local_variable(variable) {
            return Err(ResolveError::OverwriteVariableNotAllowedHere(
                variable.clone(),
            ));
        }
        let variable_str = self.get_text_resolved(variable).to_string();

        let scope_index = self.scope.block_scope_stack.len() - 1;

        let variables = &mut self
            .scope
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables;

        let resolved_variable = ResolvedVariable {
            name: variable.clone(),
            resolved_type: variable_type_ref.clone(),
            mutable_node: is_mutable.clone(),
            scope_index,
            variable_index: variables.len(),
        };

        let variable_ref = Rc::new(resolved_variable);

        variables
            .insert(variable_str, variable_ref.clone())
            .expect("should have checked earlier for variable");

        Ok(variable_ref)
    }

    pub(crate) fn link_local_variable_resolved(
        &mut self,
        variable_ref: ResolvedVariableRef,
    ) -> Result<(), ResolveError> {
        let variable_name = self.get_text_resolved(&variable_ref.name).to_string();
        let variables = &mut self
            .scope
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables;

        variables
            .insert(variable_name, variable_ref.clone())
            .expect("should have checked earlier for variable");

        Ok(())
    }

    pub(crate) fn create_local_variable_generated(
        &mut self,
        variable_str: &str,
        is_mutable: bool,
        variable_type_ref: &ResolvedType,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        /*


        if let Some(_existing_variable) = self.try_find_local_variable(variable) {
            return Err(ResolveError::OverwriteVariableNotAllowedHere(
                variable.clone(),
            ));
        }
        let variable_str = self.get_text_resolved(variable).to_string();

         */

        let scope_index = self.scope.block_scope_stack.len() - 1;

        let variables = &mut self
            .scope
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables;

        let resolved_variable = ResolvedVariable {
            name: ResolvedNode::default(),
            resolved_type: variable_type_ref.clone(),
            mutable_node: if is_mutable {
                Some(ResolvedNode::default())
            } else {
                None
            },
            scope_index,
            variable_index: variables.len(),
        };

        let variable_ref = Rc::new(resolved_variable);

        variables
            .insert(variable_str.to_string(), variable_ref.clone())
            .expect("should have checked earlier for variable");

        Ok(variable_ref)
    }

    pub(crate) fn resolve_compound_assignment_variable(
        &mut self,
        target: &Node,
        operator: &CompoundOperator,
        source: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_variable = self.find_variable_from_node(target)?;
        let resolved_source = self.resolve_expression(source)?;
        let source_type = resolved_source.resolution();

        let target_type = &resolved_variable.resolved_type;

        let resolved_compound_operator = self.resolve_compound_operator(operator);

        match &target_type {
            ResolvedType::Int => {
                if source_type != ResolvedType::Int {
                    return Err(ResolveError::IncompatibleTypes(
                        resolved_compound_operator.node.span().clone(),
                        source_type,
                    ));
                }
                let compound_assignment = ResolvedVariableCompoundAssignment {
                    variable_ref: resolved_variable,
                    expression: Box::from(resolved_source),
                    compound_operator: resolved_compound_operator,
                };
                Ok(ResolvedExpression::VariableCompoundAssignment(
                    compound_assignment,
                ))
            }
            ResolvedType::Array(array_type_ref) => {
                match resolved_compound_operator.kind {
                    ResolvedCompoundOperatorKind::Add => {
                        let source_type = resolved_source.resolution();
                        if let ResolvedType::Array(_source_array_type) = &source_type {
                            // Concatenating two arrays
                            if !target_type.same_type(&source_type) {
                                return Err(ResolveError::IncompatibleTypes(
                                    resolved_source.span(),
                                    target_type.clone(),
                                ));
                            }
                            Ok(ResolvedExpression::ArrayExtend(
                                resolved_variable,
                                Box::new(resolved_source),
                            ))
                        } else {
                            // Appending a single item
                            if !source_type.same_type(&array_type_ref.item_type) {
                                return Err(ResolveError::IncompatibleTypes(
                                    resolved_source.span(),
                                    target_type.clone(),
                                ));
                            }
                            Ok(ResolvedExpression::ArrayPush(
                                resolved_variable,
                                Box::new(resolved_source),
                            ))
                        }
                    }
                    _ => Err(ResolveError::InvalidOperatorForArray(
                        self.to_node(&operator.node),
                    )),
                }
            }
            _ => Err(ResolveError::ExpectedArray(target_type.clone())),
        }
    }

    pub(crate) fn resolve_variable_assignment(
        &mut self,
        ast_variable: &Variable,
        ast_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let converted_expression = self.resolve_expression(ast_expression)?;
        let expression_type = converted_expression.resolution();
        let (variable_ref, is_reassignment) =
            self.set_or_overwrite_variable_with_type(ast_variable, &expression_type)?;

        let assignment = ResolvedVariableAssignment {
            variable_refs: vec![variable_ref],
            expression: Box::from(converted_expression),
        };

        if is_reassignment {
            Ok(ResolvedExpression::ReassignVariable(assignment))
        } else {
            Ok(ResolvedExpression::InitializeVariable(assignment))
        }
    }

    pub(crate) fn resolve_variable_assignment_resolved(
        &mut self,
        ast_variable: &Variable,
        converted_expression: ResolvedExpression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let expression_type = converted_expression.resolution();
        let (variable_ref, is_reassignment) =
            self.set_or_overwrite_variable_with_type(ast_variable, &expression_type)?;

        let assignment = ResolvedVariableAssignment {
            variable_refs: vec![variable_ref],
            expression: Box::from(converted_expression),
        };

        if is_reassignment {
            Ok(ResolvedExpression::ReassignVariable(assignment))
        } else {
            Ok(ResolvedExpression::InitializeVariable(assignment))
        }
    }

    pub(crate) fn resolve_multi_variable_assignment(
        &mut self,
        ast_variables: &[Variable],
        ast_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let converted_expression = self.resolve_expression(ast_expression)?;
        let expression_type = converted_expression.resolution();

        let mut variable_refs = Vec::new();
        let mut all_reassignment = true;

        if ast_variables.len() > 1 {
            if let ResolvedType::Tuple(tuple) = expression_type {
                if ast_variables.len() > tuple.0.len() {
                    return Err(ResolveError::TooManyDestructureVariables);
                }
                for (variable_ref, tuple_type) in ast_variables.iter().zip(tuple.0.clone()) {
                    let (variable_ref, is_reassignment) =
                        self.set_or_overwrite_variable_with_type(&variable_ref, &tuple_type)?;
                    variable_refs.push(variable_ref);
                }
                return Ok(ResolvedExpression::TupleDestructuring(
                    variable_refs,
                    tuple,
                    Box::from(converted_expression),
                ));
            } else {
                return Err(ResolveError::CanNotDestructure(
                    self.to_node(&ast_variables[0].name).span,
                ));
            }
        } else {
            let ast_variable = &ast_variables[0];
            let (variable_ref, is_reassignment) =
                self.set_or_overwrite_variable_with_type(&ast_variable, &expression_type)?;
            variable_refs.push(variable_ref);
            if !is_reassignment {
                all_reassignment = false;
            }
        }

        let assignment = ResolvedVariableAssignment {
            variable_refs,
            expression: Box::from(converted_expression),
        };

        if all_reassignment {
            Ok(ResolvedExpression::ReassignVariable(assignment))
        } else {
            Ok(ResolvedExpression::InitializeVariable(assignment))
        }
    }
}
