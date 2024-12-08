/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::ConversionError;
use crate::extra::{SparseValueId, SparseValueMap};
use crate::value::{to_rust_value, Value};
use err::ExecuteError;
use seq_map::SeqMap;
use std::fmt::Debug;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
pub use swamp_script_semantic::ns::ResolvedModuleNamespace;
use swamp_script_semantic::prelude::*;
use swamp_script_semantic::{
    ResolvedForPattern, ResolvedFunction, ResolvedPatternElement, ResolvedStaticCall,
};
use tracing::{debug, error, info, trace};
use value::format_value;

pub mod err;
mod extra;
mod idx_gen;
pub mod prelude;
pub mod value;

type RawFunctionFn<C> = dyn FnMut(&[Value], &mut C) -> Result<Value, ExecuteError>;

type FunctionFn<C> = Box<RawFunctionFn<C>>;

pub struct EvalExternalFunction<C> {
    pub name: String,
    pub func: FunctionFn<C>,
    pub id: ExternalFunctionId,
}

impl<C> Debug for EvalExternalFunction<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "external_fn {} {}", self.id, self.name)
    }
}

pub type EvalExternalFunctionRef<C> = Rc<RefCell<EvalExternalFunction<C>>>;

#[derive(Debug)]
pub enum ValueWithSignal {
    Value(Value),
    Return(Value),
    Break,    // Value is not defined
    Continue, // No value, it is a signal
}

impl From<String> for ExecuteError {
    fn from(err: String) -> Self {
        Self::Error(err)
    }
}

impl From<ConversionError> for ExecuteError {
    fn from(err: ConversionError) -> Self {
        Self::ConversionError(err)
    }
}

#[derive(Debug, Clone)]
struct BlockScope {
    variables: Vec<Value>,
}

#[derive(Default)]
struct FunctionScope {
    saved_block_scope: Vec<BlockScope>,
}

fn create_fixed_vec(capacity: usize) -> Vec<Value> {
    let mut items = Vec::with_capacity(capacity);
    items.extend((0..capacity).map(|_| Value::Unit));
    items
}
impl Default for BlockScope {
    fn default() -> Self {
        Self {
            variables: create_fixed_vec(64),
        }
    }
}

#[derive(Default)]
pub struct ExternalFunctions<C> {
    external_functions: HashMap<String, EvalExternalFunctionRef<C>>,
    external_functions_by_id: HashMap<ExternalFunctionId, EvalExternalFunctionRef<C>>,
}

impl<C> ExternalFunctions<C> {
    pub fn new() -> Self {
        Self {
            external_functions: HashMap::new(),
            external_functions_by_id: HashMap::new(),
        }
    }

    pub fn register_external_function(
        &mut self,
        name: &str,
        function_id: ExternalFunctionId,
        handler: impl FnMut(&[Value], &mut C) -> Result<Value, ExecuteError> + 'static,
    ) -> Result<(), String> {
        let external_func = EvalExternalFunction {
            name: name.to_string().clone(),
            func: Box::new(handler),
            id: function_id,
        };

        let external_func_ref = Rc::new(RefCell::new(external_func));

        self.external_functions_by_id
            .insert(function_id, external_func_ref.clone());
        self.external_functions
            .insert(name.to_string(), external_func_ref);

        Ok(())
    }
}

pub fn eval_module<C>(
    externals: &ExternalFunctions<C>,
    module: &ResolvedModule,
    context: &mut C,
) -> Result<Value, ExecuteError> {
    let mut interpreter = Interpreter::<C>::new(externals, context);
    let signal = interpreter.execute_statements(&module.statements)?;
    Ok(signal.try_into()?)
}

pub fn util_execute_function<C>(
    //&mut self,
    externals: &ExternalFunctions<C>,
    func: &ResolvedInternalFunctionDefinitionRef,
    arguments: &[Value],
    context: &mut C,
) -> Result<Value, ExecuteError> {
    let mut interpreter = Interpreter::<C>::new(externals, context);
    interpreter.bind_parameters(&func.signature.parameters, arguments)?;
    let with_signal = interpreter.execute_statements(&func.statements)?;
    interpreter.current_block_scopes.clear();
    interpreter.function_scope_stack.clear();
    Ok(Value::try_from(with_signal)?)
}

pub struct Interpreter<'a, C> {
    function_scope_stack: Vec<FunctionScope>,
    current_block_scopes: Vec<BlockScope>,
    externals: &'a ExternalFunctions<C>,
    context: &'a mut C,
}

impl<'a, C> Interpreter<'a, C> {
    pub fn new(externals: &'a ExternalFunctions<C>, context: &'a mut C) -> Self {
        Self {
            function_scope_stack: vec![FunctionScope::default()],
            current_block_scopes: vec![BlockScope::default()],
            externals,
            context,
        }
    }

    fn push_function_scope(&mut self, debug_str: &str) {
        debug!(debug_str=%debug_str, "push function scope");
        self.function_scope_stack.push(FunctionScope {
            saved_block_scope: self.current_block_scopes.clone(),
        });
        trace!(len=%self.current_block_scopes.len(), "saved block len");

        self.current_block_scopes.clear();
        self.push_block_scope("default function scope");
    }

    fn push_block_scope(&mut self, debug_str: &str) {
        debug!(debug_str = %debug_str, "push block scope");
        info!(
            "EVAL: pushing scope '{}', current depth: {}",
            debug_str,
            self.current_block_scopes.len()
        );
        self.current_block_scopes.push(BlockScope::default());
    }

    fn pop_block_scope(&mut self, debug_str: &str) {
        debug!(debug_str=%debug_str, "pop block scope");
        let old_len = self.current_block_scopes.len();
        info!(
            "EVAL: popping scope '{}', new depth: {}",
            debug_str,
            old_len - 1
        );
        self.current_block_scopes.pop();
    }

    fn pop_function_scope(&mut self, debug_str: &str) {
        debug!(debug_str=%debug_str, "pop function scope");
        if self.function_scope_stack.len() == 1 {
            error!("you popped too far");
            panic!("you popped too far");
        }
        let last_one = self.function_scope_stack.pop().expect("pop function scope");
        self.current_block_scopes = last_one.saved_block_scope;

        trace!(
            len = self.current_block_scopes.len(),
            "restored block scope"
        );
    }

    fn bind_parameters(
        &mut self,
        params: &[ResolvedParameter],
        args: &[Value],
    ) -> Result<(), ExecuteError> {
        for (index, (param, arg)) in params.iter().zip(args).enumerate() {
            let value = if param.is_mutable {
                match arg {
                    Value::Reference(r) => {
                        // For mutable parameters, use the SAME reference
                        Value::Reference(r.clone())
                    }
                    _ => return Err(ExecuteError::ArgumentIsNotMutable), //v => Value::Reference(Rc::new(RefCell::new(v))),
                }
            } else {
                match arg {
                    Value::Reference(r) => r.borrow().clone(),
                    v => v.clone(),
                }
            };

            self.set_local_var(index, value, param.is_mutable, &param.resolved_type)?;
        }

        Ok(())
    }

    fn evaluate_static_function_call(
        &mut self,
        static_call: &ResolvedStaticCall,
    ) -> Result<Value, ExecuteError> {
        // First evaluate the arguments
        let evaluated_args = self.evaluate_args(&static_call.arguments)?;

        match &*static_call.function {
            ResolvedFunction::Internal(function_data) => {
                self.push_function_scope("static function call");
                self.bind_parameters(&function_data.signature.parameters, &evaluated_args)?;
                let result = self.execute_statements(&function_data.statements)?;

                let v = match result {
                    ValueWithSignal::Value(v) => v,
                    ValueWithSignal::Return(v) => v,
                    ValueWithSignal::Break => Value::Unit,
                    ValueWithSignal::Continue => Value::Unit,
                };
                self.pop_function_scope("static function call");
                Ok(v)
            }
            ResolvedFunction::External(external) => {
                let mut func = self
                    .externals
                    .external_functions_by_id
                    .get(&external.id)
                    .expect("external function missing")
                    .borrow_mut();
                (func.func)(&evaluated_args, &mut self.context)
            }
        }
    }

    fn evaluate_external_function_call(
        &mut self,
        call: &ResolvedExternalFunctionCall,
    ) -> Result<Value, ExecuteError> {
        let evaluated_args = self.evaluate_args(&call.arguments)?;
        let mut func = self
            .externals
            .external_functions_by_id
            .get(&call.function_definition.id)
            .expect("external function missing")
            .borrow_mut();
        let v = (func.func)(&evaluated_args, &mut self.context)?;
        Ok(v)
    }

    fn evaluate_internal_function_call(
        &mut self,
        call: &ResolvedInternalFunctionCall,
    ) -> Result<Value, ExecuteError> {
        let func_val = self.evaluate_expression(&call.function_expression)?;
        match &func_val {
            Value::InternalFunction(internal_func_ref) => {
                info!("internal func: {internal_func_ref}")
            }
            _ => {
                return Err(ExecuteError::Error(
                    "internal error, can only execute internal function".to_owned(),
                ))
            }
        }

        let evaluated_args = self.evaluate_args(&call.arguments)?;
        debug!("call {:?}", func_val);

        self.push_function_scope(&format!("{func_val}"));

        // Bind parameters before executing body
        self.bind_parameters(
            &call.function_definition.signature.parameters,
            &evaluated_args,
        )?;
        let result = self.execute_statements(&call.function_definition.statements)?;

        self.pop_function_scope(&format!("{func_val}"));

        // Since signals can not propagate from the function call, we just return a normal Value
        let v = match result {
            ValueWithSignal::Value(v) => v,
            ValueWithSignal::Return(v) => v,
            ValueWithSignal::Break => Value::Unit,
            ValueWithSignal::Continue => Value::Unit,
        };

        Ok(v)
    }

    fn tabs(&self) -> String {
        "..".repeat(self.function_scope_stack.len() - 1)
    }

    fn evaluate_args(&mut self, args: &[ResolvedExpression]) -> Result<Vec<Value>, ExecuteError> {
        let mut evaluated = Vec::with_capacity(args.len());

        for arg in args {
            match arg {
                ResolvedExpression::MutRef(var_ref) => {
                    match self.lookup_var(
                        var_ref.variable_ref.scope_index,
                        var_ref.variable_ref.variable_index,
                    )? {
                        Value::Reference(r) => evaluated.push(Value::Reference(r.clone())),
                        _ => {
                            Err("Can only take mutable reference of mutable variable".to_string())?
                        }
                    }
                }
                expr => {
                    let value = self.evaluate_expression(expr)?;
                    evaluated.push(value);
                }
            }
        }

        Ok(evaluated)
    }

    fn evaluate_expressions(
        &mut self,
        exprs: &[ResolvedExpression],
    ) -> Result<Vec<Value>, ExecuteError> {
        let mut values = vec![];
        for expr in exprs {
            let value = self.evaluate_expression(expr)?;
            values.push(value);
        }

        Ok(values)
    }

    // Variables / Registers ----------------------------------------
    #[inline]
    fn overwrite_existing_var(
        &mut self,
        relative_scope_index: usize,
        variable_index: usize,
        new_value: Value,
    ) -> Result<(), ExecuteError> {
        trace!(
            "VAR: overwrite_existing_var {relative_scope_index}:{variable_index} = {new_value:?}"
        );
        let existing_var =
            &mut self.current_block_scopes[relative_scope_index].variables[variable_index];

        match existing_var {
            Value::Reference(r) => {
                *r.borrow_mut() = new_value.clone();
                Ok(())
            }
            _ => Err(format!("Cannot assign to immutable variable: {:?}", variable_index).into()),
        }
    }

    /// Initializes a variable for the first time
    #[inline]
    fn initialize_var(
        &mut self,
        relative_scope_index: usize,
        variable_index: usize,
        value: Value,
        is_mutable: bool,
    ) -> Result<(), ExecuteError> {
        trace!("VAR: set var mut:{is_mutable} {relative_scope_index}:{variable_index} = {value:?}");

        if is_mutable {
            // TODO: Check that we are not overwriting an existing used variables (debug)
            self.current_block_scopes[relative_scope_index].variables[variable_index] =
                Value::Reference(Rc::new(RefCell::new(value)));
        } else {
            // If it is immutable, just store normal values
            self.current_block_scopes[relative_scope_index].variables[variable_index] = value
        }

        Ok(())
    }

    fn lookup_variable(&self, variable: &ResolvedVariableRef) -> Result<&Value, ExecuteError> {
        self.lookup_var(variable.scope_index, variable.variable_index)
    }

    #[inline]
    fn lookup_var(
        &self,
        relative_scope_index: usize,
        variable_index: usize,
    ) -> Result<&Value, ExecuteError> {
        if relative_scope_index >= self.current_block_scopes.len() {
            panic!(
                "illegal scope index {relative_scope_index} of {}",
                self.current_block_scopes.len()
            );
        }

        let variables = &self.current_block_scopes[relative_scope_index].variables;
        if variable_index >= variables.len() {
            panic!("illegal index");
        }
        let existing_var = &variables[variable_index];

        trace!("VAR: lookup {relative_scope_index}:{variable_index} > {existing_var:?}");
        Ok(existing_var)
    }

    fn assign_value(target_type: &ResolvedType, value: Value) -> Value {
        match value {
            Value::Struct(struct_ref, fields, _) => {
                // Use the target's display type instead of the value's type
                Value::Struct(struct_ref, fields, target_type.clone())
            }
            _ => value,
        }
    }

    #[inline]
    fn set_local_var(
        &mut self,
        variable_index: usize,
        value: Value,
        _is_mutable: bool,
        var_type: &ResolvedType,
    ) -> Result<(), ExecuteError> {
        let last_scope_index = self.current_block_scopes.len() - 1;
        let assigned = Self::assign_value(var_type, value);
        trace!("VAR: set_local_var {last_scope_index}:{variable_index} = {assigned:?}");

        self.current_block_scopes[last_scope_index].variables[variable_index] = assigned;
        Ok(())
    }

    // ---------------
    fn evaluate_expression(&mut self, expr: &ResolvedExpression) -> Result<Value, ExecuteError> {
        debug!("evaluate expression {expr:?}");
        let value = match expr {
            // Constructing
            ResolvedExpression::Literal(lit) => match lit {
                ResolvedLiteral::IntLiteral(n, _) => Value::Int(*n),
                ResolvedLiteral::FloatLiteral(f, _) => Value::Float(*f),
                ResolvedLiteral::StringLiteral(s, _) => Value::String(s.0.clone()),
                ResolvedLiteral::BoolLiteral(b, _) => Value::Bool(*b),
                ResolvedLiteral::EnumVariantLiteral(enum_variant_type, data) => {
                    let variant_container_value: Value = match &enum_variant_type.data {
                        ResolvedEnumVariantContainerType::Tuple(tuple_type) => match data {
                            ResolvedEnumLiteralData::Tuple(tuple_expressions) => {
                                let eval_expressions =
                                    self.evaluate_expressions(tuple_expressions)?;
                                Value::EnumVariantTuple(tuple_type.clone(), eval_expressions)
                            }
                            _ => return Err("wrong container type".to_string())?,
                        },

                        ResolvedEnumVariantContainerType::Struct(struct_type_ref) => match data {
                            ResolvedEnumLiteralData::Struct(resolved_field_values) => {
                                let mut values = Vec::with_capacity(resolved_field_values.len());
                                for resolved_expression in resolved_field_values {
                                    let value = self.evaluate_expression(resolved_expression)?;
                                    values.push(value);
                                }
                                Value::EnumVariantStruct(struct_type_ref.clone(), values)
                            }
                            _ => return Err("wrong container type".to_string())?,
                        },

                        ResolvedEnumVariantContainerType::Nothing => {
                            Value::EnumVariantSimple(enum_variant_type.clone())
                        }
                    };
                    variant_container_value
                }

                ResolvedLiteral::TupleLiteral(tuple_type, resolved_expressions) => {
                    let values = self.evaluate_expressions(resolved_expressions)?;
                    Value::Tuple(tuple_type.clone(), values)
                }

                ResolvedLiteral::UnitLiteral(_) => Value::Unit,
                ResolvedLiteral::Array(array_type, expressions) => {
                    let values = self.evaluate_expressions(expressions)?;
                    Value::Array(array_type.clone(), values)
                }
                ResolvedLiteral::Map(map_type_ref, expressions) => {
                    let mut items = SeqMap::new();
                    for (key, value) in expressions {
                        let key_val = self.evaluate_expression(key)?;
                        let value_val = self.evaluate_expression(value)?;
                        items
                            .insert(key_val, value_val)
                            .map_err(|_err| ExecuteError::NonUniqueKeysInMapLiteralDetected)?;
                    }
                    Value::Map(map_type_ref.clone(), items)
                }
                ResolvedLiteral::NoneLiteral => Value::Option(None),
            },

            ResolvedExpression::Array(array_instantiation) => {
                let mut values = Vec::new();
                for element in &array_instantiation.expressions {
                    values.push(self.evaluate_expression(element)?);
                }

                Value::Array(array_instantiation.array_type_ref.clone(), values)
            }

            ResolvedExpression::StructInstantiation(struct_instantiation) => {
                // Evaluate all field expressions and validate types
                let mut field_values = Vec::new();
                for field_expr in &struct_instantiation.expressions_in_order {
                    let value = self.evaluate_expression(field_expr)?;
                    field_values.push(value);
                }

                Value::Struct(
                    struct_instantiation.struct_type_ref.clone(),
                    field_values,
                    struct_instantiation.display_type_ref.clone(),
                )
            }

            ResolvedExpression::ExclusiveRange(_resolved_type_ref, start, end) => {
                let start_val = self.evaluate_expression(start)?;
                let end_val = self.evaluate_expression(end)?;
                match (start_val, end_val) {
                    (Value::Int(s), Value::Int(e)) => {
                        Value::ExclusiveRange(Box::new(s), Box::new(e))
                    }
                    _ => Err("Range bounds must be integers".to_string())?,
                }
            }

            // ==================== ASSIGNMENT ====================
            ResolvedExpression::InitializeVariable(var_assignment) => {
                let new_value = self.evaluate_expression(&var_assignment.expression)?;

                // Initialize all variables (single or multiple)
                for variable_ref in &var_assignment.variable_refs {
                    self.initialize_var(
                        variable_ref.scope_index,
                        variable_ref.variable_index,
                        new_value.clone(),
                        variable_ref.is_mutable(),
                    )?;
                }

                new_value
            }

            ResolvedExpression::ReassignVariable(var_assignment) => {
                let new_value = self.evaluate_expression(&var_assignment.expression)?;

                // Reassign all variables (single or multiple)
                for variable_ref in &var_assignment.variable_refs {
                    self.overwrite_existing_var(
                        variable_ref.scope_index,
                        variable_ref.variable_index,
                        new_value.clone(),
                    )?;
                }

                new_value
            }

            ResolvedExpression::ArrayExtend(variable_ref, source_expression) => {
                let source_val = self.evaluate_expression(source_expression)?;

                let array_val = self.lookup_variable(variable_ref)?;
                match array_val {
                    Value::Reference(r) => {
                        if let Value::Array(_type_id, ref mut vector) = &mut *r.borrow_mut() {
                            if let Value::Array(_, items) = source_val {
                                vector.extend(items);
                            } else {
                                Err("Cannot extend non-array reference".to_string())?
                            }
                        } else {
                            Err("Cannot extend non-array reference".to_string())?
                        }
                    }
                    _ => Err(ExecuteError::NotAnArray)?,
                }
                array_val.clone()
            }

            ResolvedExpression::ArrayPush(variable_ref, source_expression) => {
                let source_val = self.evaluate_expression(source_expression)?;
                let array_val = self.lookup_variable(variable_ref)?;
                match &array_val {
                    Value::Reference(r) => {
                        if let Value::Array(_type_id, ref mut vector) = &mut *r.borrow_mut() {
                            vector.push(source_val);
                        } else {
                            Err("Cannot extend non-array reference".to_string())?
                        }
                    }
                    _ => Err(ExecuteError::NotAnArray)?,
                }
                array_val.clone()
            }

            ResolvedExpression::ArrayRemoveIndex(variable_ref, usize_index_expression) => {
                let index_val = self.evaluate_expression(usize_index_expression)?;
                let index = match index_val {
                    Value::Int(x) => x,
                    _ => return Err(ExecuteError::ArgumentIsNotMutable),
                };
                let array_val = self.lookup_variable(variable_ref)?;

                match &array_val {
                    Value::Reference(r) => {
                        if let Value::Array(_type_id, ref mut vector) = &mut *r.borrow_mut() {
                            vector.remove(index as usize);
                        } else {
                            Err("Cannot extend non-array reference".to_string())?
                        }
                    }
                    _ => Err(ExecuteError::NotAnArray)?,
                }
                array_val.clone()
            }

            ResolvedExpression::ArrayClear(variable_ref) => {
                let array_val = self.lookup_variable(variable_ref)?;
                match &array_val {
                    Value::Reference(r) => {
                        if let Value::Array(_type_id, ref mut vector) = &mut *r.borrow_mut() {
                            vector.clear();
                        } else {
                            Err("Cannot extend non-array reference".to_string())?
                        }
                    }
                    _ => Err(ExecuteError::NotAnArray)?,
                }
                array_val.clone()
            }

            ResolvedExpression::ArrayAssignment(array, index, value) => {
                let array_val = self.evaluate_expression(&array.expression)?;
                let index_val = self.evaluate_expression(&index.expression)?;
                let new_val = self.evaluate_expression(value)?;

                match (array_val, index_val) {
                    (Value::Reference(r), Value::Int(i)) => {
                        if let Value::Array(_type_id, ref mut elements) = &mut *r.borrow_mut() {
                            if i < 0 || i >= elements.len() as i32 {
                                return Err(format!("Array index out of bounds: {}", i))?;
                            }
                            elements[i as usize] = new_val.clone();
                        } else {
                            Err("Cannot index into non-array reference".to_string())?
                        }
                    }
                    (arr, idx) => Err(format!(
                        "Invalid array assignment: cannot index {:?} with {:?}",
                        arr, idx
                    ))?,
                }

                new_val
            }

            ResolvedExpression::MapAssignment(array, index, value) => {
                let map_val = self.evaluate_expression(&array.expression)?;
                let index_val = self.evaluate_expression(&index.expression)?;
                let new_val = self.evaluate_expression(value)?;

                match map_val {
                    Value::Reference(r) => {
                        if let Value::Map(_type_id, ref mut elements) = &mut *r.borrow_mut() {
                            elements
                                .insert(index_val, new_val.clone())
                                .expect("todo: improve error handling");
                        } else {
                            Err("Cannot index into non-array reference".to_string())?
                        }
                    }
                    _ => Err(format!("Invalid map assignment: must be mutable"))?,
                }

                new_val
            }

            ResolvedExpression::StructFieldAssignment(
                resolved_struct_field_ref,
                source_expression,
            ) => {
                let target_struct_value =
                    self.evaluate_expression(&resolved_struct_field_ref.inner.struct_expression)?;
                let value = self.evaluate_expression(source_expression)?;

                match target_struct_value {
                    Value::Reference(r) => {
                        let mut borrowed = r.borrow_mut();
                        // We know it must be a struct because references can only point to structs
                        match &mut *borrowed {
                            Value::Struct(struct_type, fields, _) => {
                                if let Some(field) =
                                    fields.get_mut(resolved_struct_field_ref.inner.index)
                                {
                                    let struct_ref = struct_type.borrow();
                                    let field_type = struct_ref
                                        .fields
                                        .values()
                                        .nth(resolved_struct_field_ref.inner.index)
                                        .ok_or_else(|| "Field index out of bounds".to_string())?
                                        .clone(); // Clone the type to extend its lifetime

                                    let assign = Self::assign_value(&field_type, value);
                                    *field = assign.clone();
                                    assign
                                } else {
                                    Err(format!(
                                        "Field '{}' not found in struct '{:?}'",
                                        resolved_struct_field_ref.inner.index, struct_type
                                    ))?
                                }
                            }
                            _ => {
                                Err("Internal error: reference contains non-struct value"
                                    .to_string())?
                            }
                        }
                    }
                    Value::Struct(_, _, _) => {
                        Err("Cannot assign to field of non-mutable struct".to_string())?
                    }
                    _ => Err(format!(
                        "Cannot access field '{}' on non-struct value",
                        resolved_struct_field_ref.inner.index
                    ))?,
                }
            }

            // ------------- LOOKUP ---------------------
            ResolvedExpression::VariableAccess(var) => {
                let value = self.lookup_var(var.scope_index, var.variable_index)?;
                value.clone()
            }

            ResolvedExpression::ArrayAccess(array_item_ref) => {
                let array_val = self.evaluate_expression(&array_item_ref.array_expression)?;
                let index_val = self.evaluate_expression(&array_item_ref.int_expression)?;

                match (array_val, index_val) {
                    (Value::Array(_type_id, elements), Value::Int(i)) => {
                        if i < 0 || i >= elements.len() as i32 {
                            return Err(format!("Array index out of bounds: {}", i))?;
                        }
                        elements[i as usize].clone()
                    }
                    (Value::Reference(r), Value::Int(i)) => {
                        if let Value::Array(_type_id, elements) = &*r.borrow() {
                            if i < 0 || i >= elements.len() as i32 {
                                return Err(format!("Array index out of bounds: {}", i))?;
                            }
                            elements[i as usize].clone()
                        } else {
                            Err("Cannot index into non-array reference".to_string())?
                        }
                    }
                    (arr, idx) => Err(format!(
                        "Invalid array access: cannot index {:?} with {:?}",
                        arr, idx
                    ))?,
                }
            }

            ResolvedExpression::MapIndexAccess(ref map_lookup) => {
                let map_val = self.evaluate_expression(&map_lookup.map_expression)?;
                let index_val = self.evaluate_expression(&map_lookup.index_expression)?;

                match (map_val, index_val) {
                    (Value::Map(_type_id, elements), v) => {
                        let x = elements.get(&v);
                        match x {
                            None => Value::Option(None),
                            Some(v) => Value::Option(Some(Box::from(v.clone()))),
                        }
                    }
                    (Value::Reference(r), v) => {
                        if let Value::Map(_type_id, elements) = &*r.borrow() {
                            let x = elements.get(&v);
                            match x {
                                None => Value::Option(None),
                                Some(v) => Value::Option(Some(Box::from(v.clone()))),
                            }
                        } else {
                            Err("Cannot index into non-map reference".to_string())?
                        }
                    }
                    (arr, idx) => Err(format!(
                        "Invalid map access: cannot index {:?} with {:?}",
                        arr, idx
                    ))?,
                }
            }

            ResolvedExpression::FieldAccess(struct_field_access) => {
                let struct_expression =
                    self.evaluate_expression(&struct_field_access.struct_expression)?;

                match struct_expression {
                    Value::Struct(_struct_type, fields, _) => {
                        fields[struct_field_access.index].clone()
                    }
                    Value::Reference(r) => {
                        // If it's a reference, dereference and try field access
                        let value = r.borrow();
                        match &*value {
                            Value::Struct(_struct_type, fields, _) => {
                                fields[struct_field_access.index].clone()
                            }
                            _ => Err(format!(
                                "Cannot access field '{}' on non-struct value",
                                struct_field_access.index
                            ))?,
                        }
                    }
                    _ => Err(format!(
                        "Cannot access field '{}' on non-struct value",
                        struct_field_access.index
                    ))?,
                }
            }

            ResolvedExpression::MutRef(var_ref) => {
                let value = self.lookup_var(
                    var_ref.variable_ref.scope_index,
                    var_ref.variable_ref.variable_index,
                )?;

                match value {
                    Value::Reference(r) => Value::Reference(r.clone()),
                    _ => Err("Can only take mutable reference of mutable variable".to_string())?,
                }
            }

            // Operators
            ResolvedExpression::BinaryOp(binary_operator) => {
                let left_val = self.evaluate_expression(&binary_operator.left)?;
                let right_val = self.evaluate_expression(&binary_operator.right)?;
                self.evaluate_binary_op(left_val, &binary_operator.ast_operator_type, right_val)?
            }

            ResolvedExpression::UnaryOp(unary_operator) => {
                let left_val = self.evaluate_expression(&unary_operator.left)?;
                self.evaluate_unary_op(&unary_operator.ast_operator_type, left_val)?
            }

            ResolvedExpression::PostfixOp(postfix_operator) => {
                let left_val = self.evaluate_expression(&postfix_operator.left)?;
                self.evaluate_postfix_op(&postfix_operator.ast_operator_type, left_val)?
            }

            // Calling
            ResolvedExpression::FunctionInternalCall(resolved_internal_call) => {
                self.evaluate_internal_function_call(resolved_internal_call)?
            }

            ResolvedExpression::FunctionExternalCall(resolved_external_call) => {
                self.evaluate_external_function_call(resolved_external_call)?
            }

            ResolvedExpression::StaticCall(static_call) => {
                self.evaluate_static_function_call(static_call)?
            }

            ResolvedExpression::StaticCallGeneric(static_call_generic) => {
                let evaluated_args = self.evaluate_args(&static_call_generic.arguments)?;
                match &*static_call_generic.function {
                    ResolvedFunction::Internal(function_data) => {
                        self.push_function_scope("static generic function call");
                        self.bind_parameters(&function_data.signature.parameters, &evaluated_args)?;
                        let result = self.execute_statements(&function_data.statements)?;
                        self.pop_function_scope("static generic function call");
                        match result {
                            ValueWithSignal::Value(v) | ValueWithSignal::Return(v) => Ok(v),
                            _ => Ok(Value::Unit),
                        }
                    }
                    ResolvedFunction::External(external) => {
                        let mut func = self
                            .externals
                            .external_functions_by_id
                            .get(&external.id)
                            .expect("external function missing")
                            .borrow_mut();
                        (func.func)(&evaluated_args, &mut self.context)
                    }
                }?
            }

            ResolvedExpression::MemberCall(resolved_member_call) => {
                let member_value =
                    self.evaluate_expression(&resolved_member_call.self_expression)?;

                trace!("{} > member call {:?}", self.tabs(), member_value);

                let parameters = match &*resolved_member_call.function {
                    ResolvedFunction::Internal(function_data) => {
                        &function_data.signature.parameters
                    }
                    ResolvedFunction::External(external_data) => {
                        &external_data.signature.parameters
                    }
                };

                let mut member_call_arguments = Vec::new();
                member_call_arguments.push(member_value.clone()); // Add self as first argument
                for arg in &resolved_member_call.arguments {
                    member_call_arguments.push(self.evaluate_expression(arg)?);
                }

                // Check total number of parameters (including self)
                if member_call_arguments.len() != parameters.len() {
                    return Err(ExecuteError::Error(format!(
                        "wrong number of arguments: expected {}, got {}",
                        parameters.len(),
                        member_call_arguments.len()
                    )));
                }

                match &*resolved_member_call.function {
                    ResolvedFunction::Internal(internal_function) => {
                        self.push_function_scope(&format!("member_call {member_value}"));
                        self.bind_parameters(parameters, &member_call_arguments)?;
                        let result = self.execute_statements(&internal_function.statements)?;
                        self.pop_function_scope(&format!("member_call {resolved_member_call}"));

                        match result {
                            ValueWithSignal::Value(v) => v,
                            ValueWithSignal::Return(v) => v,
                            ValueWithSignal::Break => {
                                Err("break not allowed in member calls".to_string())?
                            }
                            ValueWithSignal::Continue => {
                                Err("continue not allowed in member calls".to_string())?
                            }
                        }
                    }
                    ResolvedFunction::External(external_func) => {
                        let mut func = self
                            .externals
                            .external_functions_by_id
                            .get(&external_func.id)
                            .expect("external function missing")
                            .borrow_mut();
                        (func.func)(&member_call_arguments, &mut self.context)?
                    }
                }
            }

            ResolvedExpression::Block(statements) => {
                self.push_block_scope("block statements");
                let result = self.execute_statements(statements)?;
                self.pop_block_scope("block_statements");
                match result {
                    ValueWithSignal::Value(v) => v,
                    ValueWithSignal::Return(_) => {
                        Err("return is not allowed in expressions".to_string())?
                    }
                    ValueWithSignal::Break => {
                        Err("break is not allowed in expressions".to_string())?
                    }
                    ValueWithSignal::Continue => {
                        Err("continue is not allowed in expressions".to_string())?
                    }
                }
            }

            ResolvedExpression::InterpolatedString(_string_type_ref, parts) => {
                let mut result = String::new();

                for part in parts {
                    match part {
                        ResolvedStringPart::Literal(text) => {
                            result.push_str(text);
                        }
                        ResolvedStringPart::Interpolation(expr, format_spec) => {
                            let value = self.evaluate_expression(expr)?;
                            let formatted = match format_spec {
                                Some(spec) => format_value(&value, spec)?,
                                None => value.to_string(),
                            };
                            result.push_str(&formatted);
                        }
                    }
                }

                Value::String(result)
            }

            // Comparing
            ResolvedExpression::IfElse(condition, then_expr, else_expr) => {
                self.push_block_scope("if_else");
                let cond_value = self.evaluate_expression(&condition.expression)?;
                let result = if cond_value.is_truthy()? {
                    self.evaluate_expression(then_expr)?
                } else {
                    self.evaluate_expression(else_expr)?
                };

                self.pop_block_scope("if_else");
                result
            }

            ResolvedExpression::IfElseOnlyVariable {
                variable,
                optional_expr,
                true_block,
                false_block,
            } => {
                let value = self.evaluate_expression(optional_expr)?;
                match value {
                    Value::Option(Some(inner_value)) => {
                        self.push_block_scope("if else only variable");
                        self.initialize_var(
                            variable.scope_index,
                            variable.variable_index,
                            *inner_value,
                            variable.is_mutable(),
                        )?;
                        let result = self.evaluate_expression(true_block)?;
                        self.pop_block_scope("if else only variable");
                        result
                    }
                    Value::Option(None) => self.evaluate_expression(false_block)?,
                    _ => return Err(ExecuteError::ExpectedOptional),
                }
            }

            ResolvedExpression::IfElseAssignExpression {
                variable,
                optional_expr,
                true_block,
                false_block,
            } => {
                let value = self.evaluate_expression(optional_expr)?;
                match value {
                    Value::Option(Some(inner_value)) => {
                        self.push_block_scope("if else assign expression");
                        self.initialize_var(
                            variable.scope_index,
                            variable.variable_index,
                            *inner_value,
                            variable.is_mutable(),
                        )?;
                        let result = self.evaluate_expression(true_block)?;
                        self.pop_block_scope("if else assign expression");
                        result
                    }
                    Value::Option(None) => self.evaluate_expression(false_block)?,
                    _ => return Err(ExecuteError::ExpectedOptional),
                }
            }

            ResolvedExpression::Match(resolved_match) => self.eval_match(resolved_match)?,
            ResolvedExpression::InternalFunctionAccess(fetch_function) => {
                Value::InternalFunction(fetch_function.clone())
            }
            ResolvedExpression::ExternalFunctionAccess(fetch_function) => {
                let external_ref = self
                    .externals
                    .external_functions_by_id
                    .get(&fetch_function.id)
                    .expect("should have external function ref");
                Value::ExternalFunction(external_ref.borrow().id)
            }
            ResolvedExpression::MutMemberCall(_, _) => todo!(),
            ResolvedExpression::Tuple(_) => todo!(),
            ResolvedExpression::LetVar(_, _) => todo!(),
            ResolvedExpression::Option(inner) => match inner {
                None => Value::Option(None),
                Some(expression) => {
                    Value::Option(Some(Box::from(self.evaluate_expression(expression)?)))
                }
            },

            // --------------- SPECIAL FUNCTIONS
            ResolvedExpression::SparseNew(rust_type_ref, generic_type) => {
                let sparse_value_map = SparseValueMap::new(generic_type.clone());
                to_rust_value(rust_type_ref.clone(), sparse_value_map)
            }

            ResolvedExpression::SparseAdd(sparse_rust, value_expression) => {
                let resolved_sparse_value = self.evaluate_expression(sparse_rust)?;

                let sparse_value_map = resolved_sparse_value.downcast_rust_mut::<SparseValueMap>();
                if let Some(found) = sparse_value_map {
                    let resolved_value = self.evaluate_expression(value_expression)?;
                    let id_value = found.borrow_mut().add(resolved_value);

                    id_value
                } else {
                    return Err(ExecuteError::NotSparseValue);
                }
            }
            ResolvedExpression::SparseRemove(sparse_rust, id_expression) => {
                let resolved_sparse_value = self.evaluate_expression(sparse_rust)?;
                let sparse_value_map = resolved_sparse_value.downcast_rust_mut::<SparseValueMap>();
                if let Some(found) = sparse_value_map {
                    let id_value = self.evaluate_expression(id_expression)?;
                    if let Some(found_id) = id_value.downcast_rust::<SparseValueId>() {
                        found.borrow_mut().remove(&**found_id.borrow());
                    } else {
                        return Err(ExecuteError::Error(
                            "was not a sparse slot. can not remove".to_string(),
                        ));
                    }
                }

                resolved_sparse_value
            }
            ResolvedExpression::CoerceOptionToBool(expression) => {
                let value = self.evaluate_expression(expression)?;
                match value {
                    Value::Option(inner) => Value::Bool(inner.is_some()),
                    _ => return Err(ExecuteError::CoerceOptionToBoolFailed),
                }
            }
        };

        Ok(value)
    }

    #[inline]
    pub fn execute_statements(
        &mut self,
        statements: &Vec<ResolvedStatement>,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let mut value = Value::Unit;

        for statement in statements {
            trace!("{} exec {statement:?}", self.tabs());

            // First handle signal aware statements
            match statement {
                ResolvedStatement::Continue => {
                    return Ok(ValueWithSignal::Continue);
                }
                ResolvedStatement::Break => return Ok(ValueWithSignal::Break),
                ResolvedStatement::Return(expr) => {
                    return Ok(ValueWithSignal::Return(self.evaluate_expression(expr)?));
                }

                ResolvedStatement::WhileLoop(condition, body) => {
                    while self
                        .evaluate_expression(&condition.expression)?
                        .is_truthy()?
                    {
                        match self.execute_statements(body) {
                            Err(e) => return Err(e),
                            Ok(signal) => match signal {
                                ValueWithSignal::Value(_v) => {} // Just discard normal values
                                ValueWithSignal::Break => {
                                    break;
                                }
                                ValueWithSignal::Return(v) => {
                                    return Ok(ValueWithSignal::Return(v))
                                }
                                ValueWithSignal::Continue => {}
                            },
                        }
                    }
                    continue;
                }

                ResolvedStatement::If(condition, consequences, optional_alternative) => {
                    let cond_value = self.evaluate_expression(&condition.expression)?;
                    if cond_value.is_truthy()? {
                        match self.execute_statements(consequences)? {
                            ValueWithSignal::Value(v) => value = v, // Store the value
                            ValueWithSignal::Break => return Ok(ValueWithSignal::Break),
                            ValueWithSignal::Return(v) => return Ok(ValueWithSignal::Return(v)),
                            ValueWithSignal::Continue => return Ok(ValueWithSignal::Continue),
                        }
                    } else {
                        if let Some(alternative) = optional_alternative {
                            match self.execute_statements(alternative)? {
                                ValueWithSignal::Value(v) => value = v, // Store the value
                                ValueWithSignal::Break => return Ok(ValueWithSignal::Break),
                                ValueWithSignal::Return(v) => {
                                    return Ok(ValueWithSignal::Return(v))
                                }
                                ValueWithSignal::Continue => return Ok(ValueWithSignal::Continue),
                            }
                        }
                    }
                    continue;
                }

                ResolvedStatement::IfOnlyVariable {
                    variable,
                    optional_expr,
                    true_block,
                    false_block,
                } => {
                    let value = self.evaluate_expression(optional_expr)?;
                    match value {
                        Value::Option(Some(inner_value)) => {
                            self.push_block_scope("if only variable");
                            self.initialize_var(
                                variable.scope_index,
                                variable.variable_index,
                                *inner_value,
                                variable.is_mutable(),
                            )?;

                            let result = self.execute_statements(&true_block)?;
                            self.pop_block_scope("if only variable");

                            match result {
                                ValueWithSignal::Value(_) => {}
                                signal => return Ok(signal),
                            }
                        }
                        Value::Option(None) => {
                            if let Some(else_block) = false_block {
                                match self.execute_statements(&else_block)? {
                                    ValueWithSignal::Value(_) => {}
                                    signal => return Ok(signal),
                                }
                            }
                        }
                        _ => return Err(ExecuteError::ExpectedOptional),
                    }
                    continue;
                }

                ResolvedStatement::IfAssignExpression {
                    variable,
                    optional_expr,
                    true_block,
                    false_block,
                } => {
                    let value = self.evaluate_expression(optional_expr)?;
                    match value {
                        Value::Option(Some(inner_value)) => {
                            self.push_block_scope("if assign expression");
                            self.initialize_var(
                                variable.scope_index,
                                variable.variable_index,
                                *inner_value,
                                variable.is_mutable(),
                            )?;

                            let result = self.execute_statements(&true_block)?;
                            self.pop_block_scope("if assign expression");

                            match result {
                                ValueWithSignal::Value(_) => {}
                                signal => return Ok(signal),
                            }
                        }
                        Value::Option(None) => {
                            if let Some(else_block) = false_block {
                                match self.execute_statements(&else_block)? {
                                    ValueWithSignal::Value(_) => {}
                                    signal => return Ok(signal),
                                }
                            }
                        }
                        _ => return Err(ExecuteError::ExpectedOptional),
                    }
                    continue;
                }

                ResolvedStatement::ForLoop(pattern, iterator_expr, body) => {
                    let iterator_value =
                        self.evaluate_expression(&iterator_expr.resolved_expression)?;

                    match pattern {
                        ResolvedForPattern::Single(var_ref) => {
                            self.push_block_scope(&format!("for_loop single {:?}", var_ref));

                            for value in iterator_value.into_iter()? {
                                self.initialize_var(
                                    var_ref.scope_index,
                                    var_ref.variable_index,
                                    value,
                                    false,
                                )?;

                                match self.execute_statements(body)? {
                                    ValueWithSignal::Value(_) => {}
                                    ValueWithSignal::Return(v) => {
                                        return Ok(ValueWithSignal::Return(v))
                                    }
                                    ValueWithSignal::Break => break,
                                    ValueWithSignal::Continue => continue,
                                }
                            }

                            self.pop_block_scope("for loop single");
                        }

                        ResolvedForPattern::Pair(first_ref, second_ref) => {
                            self.push_block_scope("for_loop pair");

                            let pair_iterator = iterator_value.pairs_iterator()?;
                            for (key, value) in pair_iterator {
                                // Set both variables
                                self.initialize_var(
                                    first_ref.scope_index,
                                    first_ref.variable_index,
                                    key,
                                    false,
                                )?;
                                self.initialize_var(
                                    second_ref.scope_index,
                                    second_ref.variable_index,
                                    value,
                                    false,
                                )?;

                                match self.execute_statements(body)? {
                                    ValueWithSignal::Value(_) => {}
                                    ValueWithSignal::Return(v) => {
                                        return Ok(ValueWithSignal::Return(v))
                                    }
                                    ValueWithSignal::Break => break,
                                    ValueWithSignal::Continue => continue,
                                }
                            }

                            self.pop_block_scope("for loop pair");
                        }
                    }

                    continue;
                }

                ResolvedStatement::Block(body) => {
                    match self.execute_statements(body)? {
                        ValueWithSignal::Value(_v) => {} // ignore normal values
                        ValueWithSignal::Return(v) => return Ok(ValueWithSignal::Return(v)), //  Value::Void?
                        ValueWithSignal::Break => return Ok(ValueWithSignal::Break),
                        ValueWithSignal::Continue => return Ok(ValueWithSignal::Continue),
                    }
                }
                _ => {}
            }

            value = match statement {
                ResolvedStatement::Expression(expr) => {
                    let result = self.evaluate_expression(expr); // since it is statement_expression, the value is intentionally discarded
                    if result.is_err() {
                        return Err(result.unwrap_err());
                    }
                    result?
                }

                // destructuring
                //TODO: ResolvedStatement::Let(ResolvedPattern::Tuple(_), _) => Value::Unit,
                //TODO: ResolvedStatement::Let(ResolvedPattern::Struct(_), _) => Value::Unit,
                //TODO: ResolvedStatement::Let(ResolvedPattern::EnumTuple(_, _), _) => Value::Unit,
                //TODO: ResolvedStatement::Let(ResolvedPattern::EnumStruct(_, _), _) => Value::Unit,

                // ignore the let
                //TODO: ResolvedStatement::Let(ResolvedPattern::EnumSimple(_), _) => Value::Unit,

                // Ignore signal aware statements, they have been handled earlier
                ResolvedStatement::Return(_) => panic!("return should have been handled earlier"),
                ResolvedStatement::ForLoop(_, _, _) => {
                    panic!("for_loop should have been handled earlier")
                }
                ResolvedStatement::WhileLoop(_, _) => {
                    panic!("while_loop should have been handled earlier")
                }
                ResolvedStatement::Break => panic!("break should have been handled earlier"),
                ResolvedStatement::Continue => panic!("continue should have been handled earlier"),
                ResolvedStatement::Block(_) => panic!("block should have been handled earlier"),
                ResolvedStatement::If(_, _, _) => panic!("if should have been handled earlier"),
                ResolvedStatement::IfOnlyVariable { .. } => {
                    panic!("if should have been handled earlier")
                }
                ResolvedStatement::IfAssignExpression { .. } => {
                    panic!("if should have been handled earlier")
                }
            }
        }

        Ok(ValueWithSignal::Value(value))
    }

    #[inline(always)]
    fn eval_match(&mut self, resolved_match: &ResolvedMatch) -> Result<Value, ExecuteError> {
        let cond_value = self.evaluate_expression(&resolved_match.expression)?;
        // Dereference if we got a reference
        let actual_value = match &cond_value {
            Value::Reference(r) => r.borrow().clone(),
            _ => cond_value.clone(),
        };

        for arm in &resolved_match.arms {
            match &arm.pattern {
                ResolvedPattern::PatternList(elements) => {
                    // Handle single variable/wildcard patterns that match any value
                    if elements.len() == 1 {
                        match &elements[0] {
                            ResolvedPatternElement::Variable(var_ref)
                            | ResolvedPatternElement::VariableWithFieldIndex(var_ref, _) => {
                                self.push_block_scope("pattern variable");
                                self.set_local_var(
                                    var_ref.variable_index,
                                    actual_value.clone(),
                                    false,
                                    &var_ref.resolved_type,
                                )?;
                                let result = self.evaluate_expression(&arm.expression);
                                self.pop_block_scope("pattern variable");
                                return result;
                            }
                            ResolvedPatternElement::Wildcard => {
                                // Wildcard matches anything
                                return self.evaluate_expression(&arm.expression);
                            }
                        }
                    }

                    match &actual_value {
                        Value::Tuple(_tuple_type_ref, values) => {
                            if elements.len() == values.len() {
                                self.push_block_scope("pattern list");

                                for (element, value) in elements.iter().zip(values.iter()) {
                                    match element {
                                        ResolvedPatternElement::Variable(var_ref) => {
                                            self.set_local_var(
                                                var_ref.variable_index,
                                                value.clone(),
                                                false,
                                                &var_ref.resolved_type,
                                            )?;
                                        }
                                        ResolvedPatternElement::VariableWithFieldIndex(
                                            var_ref,
                                            _,
                                        ) => {
                                            self.set_local_var(
                                                var_ref.variable_index,
                                                value.clone(),
                                                false,
                                                &var_ref.resolved_type,
                                            )?;
                                        }
                                        ResolvedPatternElement::Wildcard => {
                                            // Skip wildcards
                                            continue;
                                        }
                                    }
                                }

                                let result = self.evaluate_expression(&arm.expression);
                                self.pop_block_scope("pattern list");
                                return result;
                            }
                        }
                        _ => continue,
                    }
                }

                ResolvedPattern::EnumPattern(variant_ref, maybe_elements) => {
                    let value_to_match = match &actual_value {
                        Value::Reference(value_ref) => value_ref.borrow().clone(),
                        _ => actual_value.clone(),
                    };

                    match &value_to_match {
                        Value::EnumVariantTuple(value_tuple_type, values) => {
                            // First check if the variant types match
                            if variant_ref.number != value_tuple_type.common.number {
                                continue; // Try next pattern
                            }

                            if let Some(elements) = maybe_elements {
                                if elements.len() == values.len() {
                                    self.push_block_scope("enum tuple pattern");

                                    for (element, value) in elements.iter().zip(values.iter()) {
                                        match element {
                                            ResolvedPatternElement::Variable(var_ref) => {
                                                self.set_local_var(
                                                    var_ref.variable_index,
                                                    value.clone(),
                                                    false,
                                                    &var_ref.resolved_type,
                                                )?;
                                            }
                                            ResolvedPatternElement::VariableWithFieldIndex(
                                                var_ref,
                                                _,
                                            ) => {
                                                self.set_local_var(
                                                    var_ref.variable_index,
                                                    value.clone(),
                                                    false,
                                                    &var_ref.resolved_type,
                                                )?;
                                            }
                                            ResolvedPatternElement::Wildcard => continue,
                                        }
                                    }

                                    let result = self.evaluate_expression(&arm.expression);
                                    self.pop_block_scope("enum tuple pattern");
                                    return result;
                                }
                            }
                        }
                        Value::EnumVariantStruct(value_struct_type, values) => {
                            if value_struct_type.common.number == variant_ref.number {
                                if let Some(elements) = maybe_elements {
                                    self.push_block_scope("enum struct pattern");

                                    for element in elements {
                                        if let ResolvedPatternElement::VariableWithFieldIndex(
                                            var_ref,
                                            field_index,
                                        ) = element
                                        {
                                            let value = &values[*field_index];
                                            self.set_local_var(
                                                var_ref.variable_index,
                                                value.clone(),
                                                false,
                                                &var_ref.resolved_type,
                                            )?;
                                        }
                                    }

                                    let result = self.evaluate_expression(&arm.expression);
                                    self.pop_block_scope("enum struct pattern");
                                    return result;
                                }
                            }
                        }

                        Value::EnumVariantSimple(value_variant_ref) => {
                            if value_variant_ref.number == variant_ref.number
                                && maybe_elements.is_none()
                            {
                                return self.evaluate_expression(&arm.expression);
                            }
                        }
                        _ => continue,
                    }
                }

                ResolvedPattern::Literal(lit) => match (lit, &actual_value) {
                    (ResolvedLiteral::IntLiteral(a, _), Value::Int(b)) if a == b => {
                        return self.evaluate_expression(&arm.expression);
                    }
                    (ResolvedLiteral::FloatLiteral(a, _), Value::Float(b)) if a == b => {
                        return self.evaluate_expression(&arm.expression);
                    }
                    (ResolvedLiteral::StringLiteral(a, _), Value::String(b)) if a.0 == *b => {
                        return self.evaluate_expression(&arm.expression);
                    }
                    (ResolvedLiteral::BoolLiteral(a, _), Value::Bool(b)) if a == b => {
                        return self.evaluate_expression(&arm.expression);
                    }
                    _ => continue,
                },
            }
        }

        Err(ExecuteError::Error(
            "must match one of the match arms!".to_string(),
        ))
    }

    fn evaluate_binary_op(
        &self,
        left: Value,
        op: &BinaryOperator,
        right: Value,
    ) -> Result<Value, ExecuteError> {
        // Get the actual values, but keep track if left was a reference
        let left_val = match left {
            Value::Reference(r) => r.borrow().clone(),
            v => v,
        };

        let right_val = match right {
            Value::Reference(r) => r.borrow().clone(),
            v => v,
        };

        trace!(
            "{} > binary op {left_val:?} {op:?} {right_val:?}",
            self.tabs()
        );

        let result: Value = match (left_val, op, right_val) {
            // Integer operations
            (Value::Int(a), BinaryOperator::Add, Value::Int(b)) => Value::Int(a + b),
            (Value::Int(a), BinaryOperator::Subtract, Value::Int(b)) => Value::Int(a - b),
            (Value::Int(a), BinaryOperator::Multiply, Value::Int(b)) => Value::Int(a * b),
            (Value::Int(a), BinaryOperator::Divide, Value::Int(b)) => {
                if b == 0 {
                    return Err("Division by zero".to_string())?;
                }
                Value::Int(a / b)
            }
            (Value::Int(a), BinaryOperator::Modulo, Value::Int(b)) => Value::Int(a % b),

            // Float operations
            (Value::Float(a), BinaryOperator::Add, Value::Float(b)) => Value::Float(a + b),
            (Value::Float(a), BinaryOperator::Subtract, Value::Float(b)) => Value::Float(a - b),
            (Value::Float(a), BinaryOperator::Multiply, Value::Float(b)) => Value::Float(a * b),
            (Value::Float(a), BinaryOperator::Divide, Value::Float(b)) => {
                if b.abs().inner() <= 400 {
                    return Err("Division by zero".to_string())?;
                }
                Value::Float(a / b)
            }
            (Value::Float(a), BinaryOperator::Modulo, Value::Float(b)) => Value::Float(a % b),

            // Boolean operations
            (Value::Bool(a), BinaryOperator::LogicalAnd, Value::Bool(b)) => Value::Bool(a && b),
            (Value::Bool(a), BinaryOperator::LogicalOr, Value::Bool(b)) => Value::Bool(a || b),

            // Comparison operations
            (Value::Int(a), BinaryOperator::Equal, Value::Int(b)) => Value::Bool(a == b),
            (Value::Int(a), BinaryOperator::NotEqual, Value::Int(b)) => Value::Bool(a != b),
            (Value::Int(a), BinaryOperator::LessThan, Value::Int(b)) => Value::Bool(a < b),
            (Value::Int(a), BinaryOperator::GreaterThan, Value::Int(b)) => Value::Bool(a > b),
            (Value::Int(a), BinaryOperator::LessThanOrEqual, Value::Int(b)) => Value::Bool(a <= b),
            (Value::Int(a), BinaryOperator::GreaterThanOrEqual, Value::Int(b)) => {
                Value::Bool(a >= b)
            }

            // String operations
            (Value::String(a), BinaryOperator::Add, Value::String(b)) => Value::String(a + &b),
            (Value::String(a), BinaryOperator::Equal, Value::String(b)) => Value::Bool(a == b),

            (Value::String(a), BinaryOperator::Add, Value::Int(b)) => {
                Value::String(a + &b.to_string())
            }
            (Value::Int(a), BinaryOperator::Add, Value::String(b)) => {
                Value::String(a.to_string() + &b)
            }

            (Value::Bool(a), BinaryOperator::Equal, Value::Bool(b)) => Value::Bool(a == b),
            (Value::Bool(a), BinaryOperator::NotEqual, Value::Bool(b)) => Value::Bool(a != b),

            _ => return Err(format!("Invalid binary operation {op:?} ").into()),
        };

        Ok(result)
    }

    fn evaluate_unary_op(&self, op: &UnaryOperator, val: Value) -> Result<Value, ExecuteError> {
        match (op, val) {
            (UnaryOperator::Negate, Value::Int(n)) => Ok(Value::Int(-n)),
            (UnaryOperator::Negate, Value::Float(n)) => Ok(Value::Float(-n)),
            (UnaryOperator::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            _ => Err(format!("Invalid unary operation"))?,
        }
    }

    fn evaluate_postfix_op(&self, op: &PostfixOperator, val: Value) -> Result<Value, ExecuteError> {
        match op {
            PostfixOperator::Unwrap => self.evaluate_unwrap_op(val),
        }
    }

    #[inline]
    fn evaluate_unwrap_op(&self, val: Value) -> Result<Value, ExecuteError> {
        match val {
            Value::Option(ref unwrapped_boxed_opt) => match unwrapped_boxed_opt {
                Some(value) => Ok(*value.clone()),
                None => Ok(val),
            },
            _ => Err(ExecuteError::CanNotUnwrap),
        }
    }
}
