/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::value::Value;
use std::fmt::Debug;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
pub use swamp_script_semantic::ns::ResolvedModuleNamespace;
use swamp_script_semantic::prelude::*;
use swamp_script_semantic::ResolvedImplMemberRef;
use tracing::{debug, error, info, trace};
use value::format_value;

pub mod prelude;
pub mod value;

type RawFunctionFn = dyn FnMut(&[Value]) -> Result<Value, ExecuteError>;

type FunctionFn = Box<RawFunctionFn>;

pub struct EvalExternalFunction {
    pub name: String,
    pub func: FunctionFn,
    pub id: ExternalFunctionId,
}

impl Debug for EvalExternalFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "external_fn {} {}", self.id, self.name)
    }
}

pub type EvalExternalFunctionRef = Rc<RefCell<EvalExternalFunction>>;

#[derive(Debug, PartialEq, Eq)]
pub enum ExecuteError {
    Error(String),
    ArgumentIsNotMutable,
}

#[derive(Debug)]
pub enum ValueWithSignal {
    Value(Value),
    Return(Value),
    Break,    // Value is not defined
    Continue, // No value, it is a signal
}

impl From<String> for ExecuteError {
    fn from(err: String) -> Self {
        ExecuteError::Error(err)
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

pub struct Interpreter {
    function_scope_stack: Vec<FunctionScope>,
    current_block_scopes: Vec<BlockScope>,
    external_functions: HashMap<String, EvalExternalFunctionRef>,
    external_functions_by_id: HashMap<ExternalFunctionId, EvalExternalFunctionRef>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            function_scope_stack: vec![FunctionScope::default()],
            current_block_scopes: vec![BlockScope::default()],
            external_functions: HashMap::new(),
            external_functions_by_id: HashMap::new(),
        }
    }

    pub fn eval_module(&mut self, module: &ResolvedModule) -> Result<Value, ExecuteError> {
        let signal = self.execute_statements(&module.statements)?;
        Ok(signal.try_into()?)
    }

    pub fn util_execute_function(
        &mut self,
        func: &ResolvedInternalFunctionDefinitionRef,
        arguments: &[Value],
    ) -> Result<Value, ExecuteError> {
        self.bind_parameters(&func.parameters, arguments)?;
        let with_signal = self.execute_statements(&func.statements)?;
        Ok(Value::try_from(with_signal)?)
    }

    pub fn util_execute_member(
        &mut self,
        impl_member: &ResolvedImplMemberRef,
        arguments: &[Value],
    ) -> Result<Value, ExecuteError> {
        self.bind_parameters(&impl_member.parameters, arguments)?;
        let with_signal = self.execute_statements(&impl_member.body)?;
        Ok(with_signal.try_into()?)
    }

    fn push_function_scope(&mut self, debug_str: String) {
        trace!(debug_str=%debug_str, "push function scope");
        self.function_scope_stack.push(FunctionScope {
            saved_block_scope: self.current_block_scopes.clone(),
        });
        trace!(len=%self.current_block_scopes.len(), "saved block len");

        self.current_block_scopes.clear();
        self.push_block_scope("default function scope".to_string());
    }

    fn push_block_scope(&mut self, debug_str: String) {
        trace!(debug_str = %debug_str, "push block scope");
        self.current_block_scopes.push(BlockScope::default());
    }

    fn pop_block_scope(&mut self, debug_str: String) {
        trace!(debug_str=%debug_str, "pop block scope");
        self.current_block_scopes.pop();
    }

    fn pop_function_scope(&mut self, debug_str: String) {
        trace!(debug_str=%debug_str, "pop function scope");
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

            self.set_local_var(index, value, param.is_mutable)?;
        }

        Ok(())
    }

    pub fn register_external_function(
        &mut self,
        name: String,
        function_id: ExternalFunctionId,
        handler: impl FnMut(&[Value]) -> Result<Value, ExecuteError> + 'static,
    ) -> Result<(), String> {
        let external_func = EvalExternalFunction {
            name: name.clone(),
            func: Box::new(handler),
            id: function_id,
        };

        let external_func_ref = Rc::new(RefCell::new(external_func));

        self.external_functions_by_id
            .insert(function_id, external_func_ref.clone());
        self.external_functions.insert(name, external_func_ref);

        Ok(())
    }

    fn evaluate_external_function_call(
        &mut self,
        call: &ResolvedExternalFunctionCall,
    ) -> Result<Value, ExecuteError> {
        let evaluated_args = self.evaluate_args(&call.arguments)?;
        let mut func = self
            .external_functions_by_id
            .get_mut(&call.function_definition.id)
            .expect("external function missing")
            .borrow_mut();
        let v = (func.func)(&evaluated_args)?;
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

        self.push_function_scope(format!("{func_val}"));

        // Bind parameters before executing body
        self.bind_parameters(&call.function_definition.parameters, &evaluated_args)?;
        let result = self.execute_statements(&call.function_definition.statements)?;

        self.pop_function_scope(format!("{func_val}"));

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
                    // When we get a mut ref, return the reference directly
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
                // For all other expressions, evaluate normally
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

    #[inline]
    fn set_var(
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

    #[inline]
    fn lookup_var(
        &self,
        relative_scope_index: usize,
        variable_index: usize,
    ) -> Result<&Value, ExecuteError> {
        let existing_var =
            &self.current_block_scopes[relative_scope_index].variables[variable_index];
        trace!("VAR: lookup {relative_scope_index}:{variable_index} > {existing_var:?}");
        Ok(existing_var)
    }

    #[inline]
    fn set_local_var(
        &mut self,
        variable_index: usize,
        value: Value,
        _is_mutable: bool,
    ) -> Result<(), ExecuteError> {
        let last_scope_index = self.current_block_scopes.len() - 1;

        trace!("VAR: set_local_var {last_scope_index}:{variable_index} = {value:?}");

        self.current_block_scopes[last_scope_index].variables[variable_index] = value;
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

                Value::Struct(struct_instantiation.struct_type_ref.clone(), field_values)
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
            ResolvedExpression::VariableAssignment(resolved_var_assignment) => {
                let new_value = self.evaluate_expression(&resolved_var_assignment.expression)?;
                self.overwrite_existing_var(
                    resolved_var_assignment.variable_ref.scope_index,
                    resolved_var_assignment.variable_ref.variable_index,
                    new_value.clone(),
                )?;

                new_value
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
                            Value::Struct(struct_type, fields) => {
                                if let Some(field) =
                                    fields.get_mut(resolved_struct_field_ref.inner.index)
                                {
                                    *field = value.clone();
                                    value
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
                    Value::Struct(_, _) => {
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
                        // Handle array access through a reference
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

            ResolvedExpression::FieldAccess(struct_field_access) => {
                let struct_expression =
                    self.evaluate_expression(&struct_field_access.struct_expression)?;

                match struct_expression {
                    Value::Struct(_struct_type, fields) => {
                        fields[struct_field_access.index].clone()
                    }
                    Value::Reference(r) => {
                        // If it's a reference, dereference and try field access
                        let value = r.borrow();
                        match &*value {
                            Value::Struct(_struct_type, fields) => {
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

            // Calling
            ResolvedExpression::FunctionInternalCall(resolved_internal_call) => {
                self.evaluate_internal_function_call(resolved_internal_call)?
            }

            ResolvedExpression::FunctionExternalCall(resolved_external_call) => {
                self.evaluate_external_function_call(resolved_external_call)?
            }

            ResolvedExpression::MemberCall(resolved_member_call) => {
                let member_value =
                    self.evaluate_expression(&resolved_member_call.self_expression)?;

                trace!("{} > member call {:?}", self.tabs(), member_value);

                self.push_function_scope(format!("member_call {member_value}")); // TODO: maybe have automatic pop on Drop

                let mut member_call_arguments = Vec::new();
                member_call_arguments.push(member_value);
                for arg in &resolved_member_call.arguments {
                    member_call_arguments.push(self.evaluate_expression(arg)?);
                }

                let expected_parameter_count = resolved_member_call.impl_member.parameters.len();
                if member_call_arguments.len() != expected_parameter_count {
                    return Err(ExecuteError::Error("wrong number of arguments".to_string()))?;
                }

                self.bind_parameters(
                    &resolved_member_call.impl_member.parameters,
                    &member_call_arguments,
                )?;

                let result = self.execute_statements(&resolved_member_call.impl_member.body)?;

                self.pop_function_scope(format!("member_call {resolved_member_call}"));

                match result {
                    ValueWithSignal::Value(v) => v,
                    ValueWithSignal::Return(_) => {
                        Err("not allowed with return in member calls".to_string())?
                    }
                    ValueWithSignal::Break => {
                        Err("not allowed with break in member calls".to_string())?
                    }
                    ValueWithSignal::Continue => {
                        Err("not allowed with continue in member calls".to_string())?
                    }
                }
            }

            ResolvedExpression::Block(statements) => {
                self.push_block_scope("block statements".to_string());
                let result = self.execute_statements(statements)?;
                self.pop_block_scope("block_statements".to_string());
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
                let cond_value = self.evaluate_expression(&condition.expression)?;
                match cond_value {
                    Value::Bool(true) => self.evaluate_expression(then_expr)?,
                    Value::Bool(false) => self.evaluate_expression(else_expr)?,
                    _ => Err("If condition must evaluate to a boolean".to_string())?,
                }
            }

            ResolvedExpression::Match(resolved_match) => self.eval_match(resolved_match)?,
            ResolvedExpression::InternalFunctionAccess(fetch_function) => {
                Value::InternalFunction(fetch_function.clone())
            }
            ResolvedExpression::ExternalFunctionAccess(fetch_function) => {
                let external_ref = self
                    .external_functions_by_id
                    .get(&fetch_function.id)
                    .expect("should have external function ref");
                Value::ExternalFunction(external_ref.clone())
            }
            ResolvedExpression::TupleFieldAssignment(_, _) => todo!(),
            ResolvedExpression::MutMemberCall(_, _) => todo!(),
            ResolvedExpression::Tuple(_) => todo!(),
            ResolvedExpression::LetVar(_, _) => todo!(),
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
                    while self.evaluate_expression(&condition.expression)?.as_bool()? {
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
                    match cond_value {
                        Value::Bool(true) => {
                            match self.execute_statements(consequences)? {
                                ValueWithSignal::Value(_v) => {} // Just discard normal values
                                ValueWithSignal::Break => return Ok(ValueWithSignal::Break),
                                ValueWithSignal::Return(v) => {
                                    return Ok(ValueWithSignal::Return(v))
                                }
                                ValueWithSignal::Continue => return Ok(ValueWithSignal::Continue),
                            }
                        }
                        Value::Bool(false) => {
                            if let Some(alternative) = optional_alternative {
                                match self.execute_statements(alternative)? {
                                    ValueWithSignal::Value(_v) => {} // Just discard normal values
                                    ValueWithSignal::Break => return Ok(ValueWithSignal::Break),
                                    ValueWithSignal::Return(v) => {
                                        return Ok(ValueWithSignal::Return(v))
                                    }
                                    ValueWithSignal::Continue => {
                                        return Ok(ValueWithSignal::Continue)
                                    }
                                }
                            }
                        }
                        _ => return Err("If statement must evaluate to a boolean".to_string())?,
                    };
                    continue; // no need for the switch
                }

                ResolvedStatement::ForLoop(var_pattern, iterator_expr, body) => {
                    let iterator = self.evaluate_expression(&iterator_expr.resolved_expression)?;
                    match iterator {
                        Value::ExclusiveRange(start, end) => {
                            self.push_block_scope(format!(
                                "for_loop {:?} {:?}",
                                var_pattern, iterator_expr
                            ));

                            for i in *start..*end {
                                match var_pattern {
                                    ResolvedPattern::VariableAssignment(ident) => self
                                        .set_local_var(
                                            ident.variable_index,
                                            Value::Int(i),
                                            false,
                                        )?,
                                    _ => return Err("Expected identifier in for loop".to_string())?,
                                }

                                let signal = self.execute_statements(body)?;
                                debug!(signal=?signal, "signal in loop");

                                match signal {
                                    ValueWithSignal::Value(_v) => {} // ignore normal values
                                    ValueWithSignal::Return(v) => {
                                        return Ok(ValueWithSignal::Return(v))
                                    }
                                    ValueWithSignal::Break => {
                                        break;
                                    }
                                    ValueWithSignal::Continue => {}
                                }
                            }

                            // Pop the loop scope
                            self.pop_block_scope("for loop".to_string());
                        }

                        Value::Array(_item_swamp_type, elements) => {
                            // Push a new scope for the loop
                            self.push_block_scope("array".to_string());

                            for element in elements {
                                match var_pattern {
                                    ResolvedPattern::VariableAssignment(ident) => {
                                        self.set_var(
                                            ident.scope_index,
                                            ident.variable_index,
                                            element,
                                            false,
                                        )
                                        .expect("can not set array element");
                                    }
                                    _ => return Err("Expected identifier in for loop".to_string())?,
                                }

                                self.execute_statements(body)?;
                            }

                            self.pop_block_scope("array".to_string());
                        }
                        Value::Int(_) => todo!(),
                        Value::Float(_) => todo!(),
                        Value::String(_) => todo!(),
                        Value::Bool(_) => todo!(),
                        Value::Tuple(_, _) => todo!(),
                        Value::Struct(_, _) => todo!(),
                        Value::Unit => todo!(),
                        Value::Reference(_) => todo!(),
                        Value::InternalFunction(_) => todo!(),
                        Value::ExternalFunction(_) => {}
                        Value::EnumVariantTuple(_, _) => todo!(),
                        Value::EnumVariantStruct(_, _) => todo!(),
                        Value::EnumVariantSimple(_) => todo!(),
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
                ResolvedStatement::Let(ResolvedPattern::VariableAssignment(var), expr) => {
                    let value = self.evaluate_expression(expr)?;
                    self.set_var(var.scope_index, var.variable_index, value, var.is_mutable())
                        .expect("could not set variable");
                    Value::Unit
                }

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
                ResolvedStatement::Let(ResolvedPattern::Literal(_), _) => Value::Unit,
                //TODO: ResolvedStatement::Let(ResolvedPattern::EnumTuple(_, _), _) => Value::Unit,
                //TODO: ResolvedStatement::Let(ResolvedPattern::EnumStruct(_, _), _) => Value::Unit,

                // ignore the let
                ResolvedStatement::Let(ResolvedPattern::Wildcard, _) => Value::Unit,
                //TODO: ResolvedStatement::Let(ResolvedPattern::EnumSimple(_), _) => Value::Unit,
                ResolvedStatement::LetVar(variable_ref, expression) => {
                    let value = self.evaluate_expression(expression)?;
                    self.set_var(
                        variable_ref.scope_index,
                        variable_ref.variable_index,
                        value.clone(),
                        variable_ref.is_mutable(),
                    )?;
                    value
                }
                ResolvedStatement::SetVar(variable_ref, expression) => {
                    let value = self.evaluate_expression(expression)?;
                    self.overwrite_existing_var(
                        variable_ref.scope_index,
                        variable_ref.variable_index,
                        value.clone(),
                    )?;
                    value
                }

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
                ResolvedStatement::Let(ResolvedPattern::Tuple(_), _) => todo!(),
                ResolvedStatement::Let(ResolvedPattern::EnumTuple(_, _), _) => todo!(),
                ResolvedStatement::Let(ResolvedPattern::EnumStruct(_, _), _) => todo!(),
                ResolvedStatement::Let(ResolvedPattern::Struct(_), _) => todo!(),
                ResolvedStatement::Let(ResolvedPattern::EnumSimple(_), _) => todo!(),
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
                ResolvedPattern::VariableAssignment(var) => {
                    // Variable pattern matches anything, so it is basically a let expression
                    self.push_block_scope("variable assignment".to_string());
                    self.set_local_var(var.variable_index, cond_value.clone(), false)
                        .expect("could not set local variable in arm.pattern");
                    let result = self.evaluate_expression(&arm.expression);
                    self.pop_block_scope("variable assignment".to_string());
                    return result;
                }

                ResolvedPattern::Tuple(resolved_tuple_type_ref) => {
                    if let Value::Tuple(_tuple_type_ref, values) = &actual_value {
                        if resolved_tuple_type_ref.0.len() == values.len() {
                            self.push_block_scope("tuple".to_string());
                            for (field_index, (_field, value)) in resolved_tuple_type_ref
                                .0
                                .iter()
                                .zip(values.iter())
                                .enumerate()
                            {
                                self.set_local_var(field_index, value.clone(), false)
                                    .expect("failed to set tuple local var");
                            }
                            let result = self.evaluate_expression(&arm.expression);
                            self.pop_block_scope("tuple".to_string());
                            return result;
                        }
                    }
                }

                ResolvedPattern::Struct(_fields) => { // TODO: PBJ
                     /* TODO:
                     if let Value::Struct(_struct_type_ref, values) = &actual_value {
                         if fields.len() == values.len() {
                             self.push_scope(ScopeType::Block);
                             for (field, value) in fields.iter().zip(values.iter()) {
                                 self.set_existing_var_or_create_new_one(
                                     field.0.clone(),
                                     value.clone(),
                                     false,
                                 );
                             }
                             let result = self.evaluate_expression(&arm.expression);
                             self.pop_scope();
                             return result;
                         }
                     }

                      */
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

                ResolvedPattern::EnumTuple(pattern_enum_variant_tuple_type_ref, variable_names) => {
                    if let Value::EnumVariantTuple(ref value_tuple_type_ref, values_in_order) =
                        &actual_value
                    {
                        if same_tuple(
                            &pattern_enum_variant_tuple_type_ref.fields_in_order,
                            &value_tuple_type_ref.fields_in_order,
                        ) {
                            self.push_block_scope("enum tuple".to_string());

                            for (index, tuple_type) in variable_names.iter().enumerate() {
                                self.set_local_var(
                                    index,
                                    values_in_order[tuple_type.field_index].clone(),
                                    false,
                                )?;
                            }

                            let result = self.evaluate_expression(&arm.expression);

                            self.pop_block_scope("enum tuple end".to_string());

                            return result;
                        }
                    }
                }

                ResolvedPattern::EnumStruct(_enum_struct_ref, variable_names) => {
                    self.push_block_scope("enum struct".to_string());

                    if let Value::EnumVariantStruct(ref _container_struct_ref, values_in_order) =
                        &actual_value
                    {
                        for (index, struct_field) in variable_names.iter().enumerate() {
                            self.set_local_var(
                                index,
                                values_in_order[struct_field.field_index].clone(),
                                false,
                            )?;
                        }

                        let result = self.evaluate_expression(&arm.expression);

                        self.pop_block_scope("enum struct end".to_string());

                        return result;
                    }
                }

                ResolvedPattern::EnumSimple(enum_variant_type_ref) => {
                    if let Value::EnumVariantSimple(ref value_enum_variant_ref) = &actual_value {
                        if value_enum_variant_ref.number == enum_variant_type_ref.number {
                            return self.evaluate_expression(&arm.expression);
                        }
                    }
                }

                ResolvedPattern::Wildcard => {
                    return Ok(self.evaluate_expression(&arm.expression)?);
                }
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
}

fn same_tuple(p0: &[ResolvedType], p1: &[ResolvedType]) -> bool {
    if p0.len() != p1.len() {
        return false;
    }

    for (p0_type, p1_type) in p0.iter().zip(p1.iter()) {
        if p0_type != p1_type {
            return false;
        }
    }

    true
}
