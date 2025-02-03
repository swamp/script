/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::block::BlockScopes;
use crate::err::ExecuteErrorKind;
use crate::prelude::{ValueReference, VariableValue};
use err::ExecuteError;
use seq_map::SeqMap;
use std::fmt::Debug;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use swamp_script_core::extra::{SparseValueId, SparseValueMap};
use swamp_script_core::prelude::ValueError;
use swamp_script_core::value::ValueRef;
use swamp_script_core::value::{
    convert_vec_to_rc_refcell, format_value, to_rust_value, SourceMapLookup, Value,
};
use swamp_script_semantic::modules::ResolvedModules;
use swamp_script_semantic::prelude::*;
use swamp_script_semantic::{same_array_ref, ResolvedPostfix};
use swamp_script_semantic::{
    same_struct_ref, ConstantId, ResolvedBinaryOperatorKind, ResolvedCompoundOperatorKind,
    ResolvedForPattern, ResolvedFunction, ResolvedMutOrImmutableExpression, ResolvedNormalPattern,
    ResolvedPatternElement, ResolvedPostfixKind, ResolvedSingleLocationExpression,
    ResolvedSingleLocationExpressionKind, ResolvedUnaryOperatorKind,
};
use swamp_script_semantic::{
    ResolvedArgumentExpressionOrLocation, ResolvedLocationAccess, ResolvedLocationAccessKind,
};
use tracing::{error, info};

pub mod err;

mod block;
pub mod prelude;
pub mod value_both;
pub mod value_ref;

impl From<ValueError> for ExecuteError {
    fn from(value: ValueError) -> Self {
        Self {
            kind: ExecuteErrorKind::ValueError(value),
            node: Default::default(),
        }
    }
}

type RawFunctionFn<C> = dyn FnMut(&[VariableValue], &mut C) -> Result<Value, ExecuteError>;

type FunctionFn<C> = Box<RawFunctionFn<C>>;

#[derive(Debug)]
pub enum FunctionData {
    Internal(ResolvedInternalFunctionDefinitionRef),
    External(ExternalFunctionId),
}

pub struct EvalExternalFunction<C> {
    pub func: FunctionFn<C>,
    pub id: ExternalFunctionId,
}

impl<C> Debug for EvalExternalFunction<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "external_fn {}", self.id)
    }
}

pub type EvalExternalFunctionRef<C> = Rc<RefCell<EvalExternalFunction<C>>>;

#[derive(Debug)]
pub enum ValueWithSignal {
    Value(Value),
    Return(Value),
    Break,
    Continue,
}

impl TryFrom<ValueWithSignal> for Value {
    type Error = String;

    fn try_from(value: ValueWithSignal) -> Result<Self, Self::Error> {
        match value {
            ValueWithSignal::Value(v) => Ok(v),
            ValueWithSignal::Return(v) => Ok(v),
            ValueWithSignal::Break => Err("break can not be converted".to_string()),
            ValueWithSignal::Continue => Err("continue can not be converted".to_string()),
        }
    }
}

#[derive(Default)]
struct FunctionScope {
    saved_block_scope: BlockScopes,
}

#[derive(Debug, Default)]
pub struct ExternalFunctions<C> {
    //    external_functions: HashMap<String, EvalExternalFunctionRef<C>>,
    external_functions_by_id: HashMap<ExternalFunctionId, EvalExternalFunctionRef<C>>,
}

#[derive(Debug)]
pub struct Constants {
    pub values: Vec<Value>,
}

impl Default for Constants {
    fn default() -> Self {
        Self::new()
    }
}

impl Constants {
    #[must_use]
    pub fn lookup_constant_value(&self, id: ConstantId) -> &Value {
        let x = &self.values[id as usize];
        assert_ne!(*x, Value::Unit, "illegal constant");
        x
    }

    pub fn set(&mut self, id: ConstantId, value: Value) {
        self.values[id as usize] = value;
    }

    #[must_use]
    pub fn new() -> Self {
        let arr: [Value; 1024] = core::array::from_fn(|_| Value::Unit);
        Self {
            values: arr.to_vec(),
        }
    }
}

impl<C> ExternalFunctions<C> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            external_functions_by_id: HashMap::new(),
        }
    }

    pub fn register_external_function(
        &mut self,
        function_id: ExternalFunctionId,
        handler: impl FnMut(&[VariableValue], &mut C) -> Result<Value, ExecuteError> + 'static,
    ) -> Result<(), String> {
        let external_func = EvalExternalFunction {
            func: Box::new(handler),
            id: function_id,
        };

        let external_func_ref = Rc::new(RefCell::new(external_func));

        self.external_functions_by_id
            .insert(function_id, external_func_ref.clone());

        Ok(())
    }
}

pub fn eval_module<C>(
    externals: &ExternalFunctions<C>,
    constants: &Constants,
    root_expression: &ResolvedExpression,
    debug_source_map: Option<&dyn SourceMapLookup>,
    context: &mut C,
) -> Result<Value, ExecuteError> {
    let mut interpreter = Interpreter::<C>::new(externals, constants, context);
    interpreter.debug_source_map = debug_source_map;
    let value_with_signal = interpreter.evaluate_expression_with_signal(root_expression)?;
    Ok(value_with_signal.try_into().map_err(|_| ExecuteError {
        node: Default::default(),
        kind: ExecuteErrorKind::CouldNotConvertFromSignal,
    })?)
}

pub fn eval_constants<C>(
    externals: &ExternalFunctions<C>,
    constants: &mut Constants,
    modules: &ResolvedModules,
    context: &mut C,
) -> Result<(), ExecuteError> {
    for constant in &modules.constants {
        let mut interpreter = Interpreter::<C>::new(externals, constants, context);
        let value = interpreter.evaluate_expression(&constant.expr)?;
        constants.set(constant.id, value);
    }

    Ok(())
}

pub fn util_execute_function<C>(
    externals: &ExternalFunctions<C>,
    constants: &Constants,
    func: &ResolvedInternalFunctionDefinitionRef,
    arguments: &[VariableValue],
    context: &mut C,
    debug_source_map: Option<&dyn SourceMapLookup>,
) -> Result<Value, ExecuteError> {
    let mut interpreter = Interpreter::<C>::new(externals, constants, context);
    interpreter.debug_source_map = debug_source_map;
    interpreter.bind_parameters(&func.body.node, &func.signature.parameters, &arguments)?;
    let value = interpreter.evaluate_expression(&func.body)?;
    interpreter.current_block_scopes.clear();
    interpreter.function_scope_stack.clear();
    Ok(value)
}

pub struct Interpreter<'a, C> {
    function_scope_stack: Vec<FunctionScope>,
    current_block_scopes: BlockScopes,
    constants: &'a Constants,
    externals: &'a ExternalFunctions<C>,
    context: &'a mut C,
    debug_source_map: Option<&'a dyn SourceMapLookup>,
    depth: usize,
}

impl<'a, C> Interpreter<'a, C> {
    pub fn new(
        externals: &'a ExternalFunctions<C>,
        constants: &'a Constants,
        context: &'a mut C,
    ) -> Self {
        Self {
            function_scope_stack: vec![FunctionScope::default()],
            current_block_scopes: BlockScopes::default(),
            externals,
            context,
            debug_source_map: None,
            constants,
            depth: 0,
        }
    }

    #[inline]
    fn push_function_scope(&mut self) {
        self.function_scope_stack.push(FunctionScope {
            saved_block_scope: self.current_block_scopes.clone(),
        });

        self.current_block_scopes.clear();
        self.push_block_scope();
    }

    #[inline]
    fn push_block_scope(&mut self) {
        self.current_block_scopes.push();
    }

    #[inline]
    fn pop_block_scope(&mut self) {
        self.current_block_scopes.pop();
    }

    #[inline]
    fn pop_function_scope(&mut self) {
        assert_ne!(self.function_scope_stack.len(), 1, "you popped too far");
        let last_one = self.function_scope_stack.pop().expect("pop function scope");
        self.current_block_scopes = last_one.saved_block_scope;
    }

    fn bind_parameters(
        &mut self,
        node: &ResolvedNode,
        params: &[ResolvedTypeForParameter],
        args: &[VariableValue],
    ) -> Result<(), ExecuteError> {
        for (index, (param, arg)) in params.iter().zip(args).enumerate() {
            let complete_value = if param.is_mutable {
                match arg {
                    VariableValue::Reference(_r) => {
                        // For mutable parameters, use the SAME reference
                        arg.clone()
                    }
                    _ => return Err(self.create_err(ExecuteErrorKind::ArgumentIsNotMutable, &node)),
                }
            } else {
                match arg {
                    VariableValue::Reference(r) => VariableValue::Value(r.borrow().clone()),
                    VariableValue::Value(v) => VariableValue::Value(v.clone()),
                }
            };

            self.current_block_scopes
                .set_local_var_ex(index, complete_value, param.is_mutable)?;
        }

        Ok(())
    }
    fn evaluate_function_call(
        &mut self,
        function_expression: &ResolvedExpression,
        arguments: &[ResolvedArgumentExpressionOrLocation],
    ) -> Result<Value, ExecuteError> {
        let func_val = self.evaluate_expression(function_expression)?;
        let evaluated_args = self.evaluate_args(arguments)?;

        match &func_val {
            Value::InternalFunction(internal_func_ref) => {
                self.push_function_scope();

                self.bind_parameters(
                    &internal_func_ref.body.node,
                    &internal_func_ref.signature.parameters,
                    &evaluated_args,
                )?;

                let result = self.evaluate_expression(&internal_func_ref.body)?;

                self.pop_function_scope();

                Ok(result)
            }

            Value::ExternalFunction(external_function_ref) => {
                let external_function_id = &external_function_ref.id;
                let mut func = self
                    .externals
                    .external_functions_by_id
                    .get(&external_function_id)
                    .ok_or(self.create_err(
                        ExecuteErrorKind::MissingExternalFunction(*external_function_id),
                        &function_expression.node,
                    ))?
                    .borrow_mut();

                (func.func)(&evaluated_args, self.context)
            }
            _ => Err(self.create_err(
                ExecuteErrorKind::ExpectedFunction,
                &function_expression.node,
            )),
        }
    }

    fn evaluate_location_chain(
        &mut self,
        node: &ResolvedNode,
        start_value_reference: ValueRef,
        chain_items: &Vec<ResolvedLocationAccess>,
    ) -> Result<ValueRef, ExecuteError> {
        let mut value_ref = start_value_reference;
        for chain in chain_items {
            value_ref = {
                match &chain.kind {
                    ResolvedLocationAccessKind::FieldIndex(_resolved_node, index) => {
                        let borrowed = value_ref.borrow();

                        let (_struct_ref, fields) = borrowed
                            .expect_struct()
                            .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedStruct, node))?;
                        fields[*index].clone()
                    }
                    ResolvedLocationAccessKind::ArrayIndex(_array_type_ref, index_expr) => {
                        let index = self
                            .evaluate_expression(index_expr)?
                            .expect_int()
                            .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedArray, node))?;

                        let borrowed = value_ref.borrow();

                        let (_array_ref, fields) = borrowed
                            .expect_array()
                            .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedArray, node))?;
                        fields[index as usize].clone()
                    }

                    ResolvedLocationAccessKind::ArrayRange(_, _) => todo!(),
                    ResolvedLocationAccessKind::StringIndex(_) => todo!(),
                    ResolvedLocationAccessKind::StringRange(_) => todo!(),

                    ResolvedLocationAccessKind::MapIndex(_map_type_ref, key_expr) => {
                        let key_expr_value = self.evaluate_expression(key_expr)?;

                        let borrowed = value_ref.borrow();

                        let (_map_type_ref, seq_map) = borrowed
                            .expect_map()
                            .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedMap, node))?;

                        let maybe_found = seq_map.get(&key_expr_value);

                        wrap_in_option(maybe_found)
                    }

                    ResolvedLocationAccessKind::MapIndexInsertIfNonExisting(
                        _map_type_ref,
                        key_expr,
                    ) => {
                        let key_expr_value = self.evaluate_expression(key_expr)?;
                        let key = key_expr_value.clone();
                        let mut borrowed_mut = value_ref.borrow_mut();
                        let found_memory = {
                            let (_, seq_map_mutable) =
                                borrowed_mut.expect_map_mut().map_err(|_| {
                                    self.create_err(ExecuteErrorKind::ExpectedMap, node)
                                })?;
                            seq_map_mutable.get(&key).cloned()
                        };
                        if let Some(found) = found_memory {
                            found
                        } else {
                            let (_, seq_map_mutable) =
                                borrowed_mut.expect_map_mut().map_err(|_| {
                                    self.create_err(ExecuteErrorKind::ExpectedMap, node)
                                })?;
                            let default_value = Rc::new(RefCell::new(Value::Unit));
                            seq_map_mutable
                                .insert(key, default_value.clone())
                                .expect("insert should work");
                            default_value
                        }
                    }

                    ResolvedLocationAccessKind::RustTypeIndex(_rust_type_ref, key_expr) => {
                        let key_expr_value = self.evaluate_expression(key_expr)?;
                        if let Some(found_sparse_id) =
                            key_expr_value.downcast_rust::<SparseValueId>()
                        {
                            if let Some(sparse_value_map) =
                                value_ref.borrow_mut().downcast_rust::<SparseValueMap>()
                            {
                                let inner_map = sparse_value_map.borrow_mut();
                                wrap_in_option(inner_map.get(&found_sparse_id.borrow()).clone())
                            } else {
                                return Err(self.create_err(ExecuteErrorKind::ExpectedArray, node))?;
                            }
                        } else {
                            return Err(self.create_err(ExecuteErrorKind::ExpectedArray, node))?;
                        }
                    }
                }
            };
        }

        Ok(value_ref)
    }

    fn evaluate_location(
        &mut self,
        found_location_expr: &ResolvedSingleLocationExpression,
    ) -> Result<ValueRef, ExecuteError> {
        let variable_ref = self
            .current_block_scopes
            .lookup_variable_mut_ref(&found_location_expr.starting_variable)?;

        let value_ref = self.evaluate_location_chain(
            &found_location_expr.node,
            variable_ref.clone(),
            &found_location_expr.access_chain,
        )?;

        let converted_value_ref = match &found_location_expr.kind {
            ResolvedSingleLocationExpressionKind::MutVariableRef => value_ref,
            ResolvedSingleLocationExpressionKind::MutStructFieldRef(
                _base_expression,
                _resolved_access,
            ) => value_ref,
            ResolvedSingleLocationExpressionKind::MutArrayIndexRef(_resolved_array_ref) => {
                //info!(?base_expression, "base expression for field access");
                let (_struct, _values) = value_ref.borrow().expect_array().map_err(|_| {
                    self.create_err(ExecuteErrorKind::ExpectedArray, &found_location_expr.node)
                })?;
                value_ref
            }
            _ => {
                panic!("not sure what this is")
            }
        };

        Ok(converted_value_ref)
    }

    fn evaluate_mut_or_immutable_expression(
        &mut self,
        expr: &ResolvedMutOrImmutableExpression,
    ) -> Result<VariableValue, ExecuteError> {
        let var_value = match &expr.expression_or_location {
            ResolvedArgumentExpressionOrLocation::Location(loc) => {
                VariableValue::Reference(self.evaluate_location(loc)?)
            }
            ResolvedArgumentExpressionOrLocation::Expression(expr) => {
                VariableValue::Value(self.evaluate_expression(expr)?)
            }
        };
        Ok(var_value)
    }

    fn evaluate_argument(
        &mut self,
        expr: &ResolvedArgumentExpressionOrLocation,
    ) -> Result<VariableValue, ExecuteError> {
        let var_value = match expr {
            ResolvedArgumentExpressionOrLocation::Location(mutable_location) => {
                VariableValue::Reference(self.evaluate_location(mutable_location)?)
            }
            ResolvedArgumentExpressionOrLocation::Expression(expr) => {
                let value = self.evaluate_expression(expr)?;
                VariableValue::Value(value)
            }
        };

        Ok(var_value)
    }

    fn evaluate_args(
        &mut self,
        args: &[ResolvedArgumentExpressionOrLocation],
    ) -> Result<Vec<VariableValue>, ExecuteError> {
        let mut evaluated = Vec::with_capacity(args.len());

        for argument_expression in args {
            let mem_value = self.evaluate_argument(argument_expression)?;
            evaluated.push(mem_value);
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

    fn evaluate_while_loop(
        &mut self,
        condition: &ResolvedBooleanExpression,
        body: &ResolvedExpression,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let mut result = Value::Unit;
        while self
            .evaluate_expression(&condition.expression)?
            .is_truthy()
            .unwrap()
        // TODO: improve error handling
        {
            match self.evaluate_expression_with_signal(body) {
                Err(e) => return Err(e),
                Ok(signal) => match signal {
                    ValueWithSignal::Value(v) => result = v,
                    ValueWithSignal::Break => {
                        break;
                    }
                    ValueWithSignal::Return(v) => return Ok(ValueWithSignal::Return(v)),
                    ValueWithSignal::Continue => {}
                },
            }
        }

        Ok(ValueWithSignal::Value(result))
    }

    fn evaluate_block(
        &mut self,
        expressions: &Vec<ResolvedExpression>,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let mut result = Value::Unit;

        self.push_block_scope();
        for expression in expressions {
            match self.evaluate_expression_with_signal(&expression)? {
                ValueWithSignal::Value(v) => result = v,
                ValueWithSignal::Return(v) => return Ok(ValueWithSignal::Return(v)),
                ValueWithSignal::Break => return Ok(ValueWithSignal::Break),
                ValueWithSignal::Continue => return Ok(ValueWithSignal::Continue),
            }
        }
        self.pop_block_scope();
        Ok(ValueWithSignal::Value(result))
    }

    fn evaluate_for_loop_mutable(
        &mut self,
        pattern: &ResolvedForPattern,
        iterator_expr: &ResolvedIterable,
        body: &Box<ResolvedExpression>,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let mut result = Value::Unit;

        let iterator_value_mem =
            self.evaluate_mut_or_immutable_expression(&iterator_expr.resolved_expression)?;
        let iterator_value = match iterator_value_mem {
            VariableValue::Value(_) => {
                return Err(self.create_err(ExecuteErrorKind::ArgumentIsNotMutable, &body.node))
            }
            VariableValue::Reference(value_ref) => value_ref,
        };

        match pattern {
            ResolvedForPattern::Single(var_ref) => {
                self.push_block_scope();

                for value in ValueReference(iterator_value).into_iter_mut().unwrap() {
                    // TODO: Improve error handling
                    self.current_block_scopes.initialize_var_mut(var_ref, value);

                    match self.evaluate_expression_with_signal(body)? {
                        ValueWithSignal::Value(v) => result = v,
                        ValueWithSignal::Return(v) => return Ok(ValueWithSignal::Return(v)),
                        ValueWithSignal::Break => break,
                        ValueWithSignal::Continue => continue,
                    }
                }

                self.pop_block_scope();
            }

            ResolvedForPattern::Pair(first_ref, second_ref) => {
                self.push_block_scope();

                for (key, value_reference) in ValueReference(iterator_value)
                    .into_iter_mut_pairs()
                    .unwrap()
                {
                    // TODO: error handling
                    // Set both variables
                    self.current_block_scopes.initialize_var(
                        first_ref.scope_index,
                        first_ref.variable_index,
                        key,
                        false,
                    );
                    self.current_block_scopes
                        .initialize_var_mut(second_ref, value_reference.0);

                    match self.evaluate_expression_with_signal(body)? {
                        ValueWithSignal::Value(v) => result = v,
                        ValueWithSignal::Return(v) => return Ok(ValueWithSignal::Return(v)),
                        ValueWithSignal::Break => break,
                        ValueWithSignal::Continue => continue,
                    }
                }

                self.pop_block_scope();
            }
        }

        Ok(ValueWithSignal::Value(result))
    }

    fn evaluate_for_loop(
        &mut self,
        pattern: &ResolvedForPattern,
        iterator_expr: &ResolvedIterable,
        body: &Box<ResolvedExpression>,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let mut result = Value::Unit;

        let iterator_value =
            self.evaluate_mut_or_immutable_expression(&iterator_expr.resolved_expression)?;

        match pattern {
            ResolvedForPattern::Single(var_ref) => {
                self.push_block_scope();

                for value in iterator_value.into_iter().unwrap() {
                    // TODO: Error handling
                    self.current_block_scopes.initialize_var(
                        var_ref.scope_index,
                        var_ref.variable_index,
                        value,
                        var_ref.is_mutable(),
                    );

                    match self.evaluate_expression_with_signal(body)? {
                        ValueWithSignal::Value(v) => result = v,
                        ValueWithSignal::Return(v) => {
                            self.pop_block_scope();
                            return Ok(ValueWithSignal::Return(v));
                        }
                        ValueWithSignal::Break => break,
                        ValueWithSignal::Continue => continue,
                    }
                }

                self.pop_block_scope();
            }

            ResolvedForPattern::Pair(first_ref, second_ref) => {
                self.push_block_scope();

                // iterator_expr.is_mutable() should select reference

                for (key, value) in iterator_value.into_iter_pairs().unwrap() {
                    // TODO: Error handling
                    // Set both variables
                    self.current_block_scopes.initialize_var(
                        first_ref.scope_index,
                        first_ref.variable_index,
                        key,
                        false,
                    );
                    self.current_block_scopes.initialize_var(
                        second_ref.scope_index,
                        second_ref.variable_index,
                        value,
                        false,
                    );

                    match self.evaluate_expression_with_signal(body)? {
                        ValueWithSignal::Value(v) => result = v,
                        ValueWithSignal::Return(v) => return Ok(ValueWithSignal::Return(v)),
                        ValueWithSignal::Break => break,
                        ValueWithSignal::Continue => continue,
                    }
                }

                self.pop_block_scope();
            }
        }

        Ok(ValueWithSignal::Value(result))
    }

    fn debug_expr(&self, expr: &ResolvedExpression) {
        if let Some(debug_source_map) = self.debug_source_map {
            let source_line = debug_source_map.get_text(&expr.node);
            eprintln!("{:?}:\n  {}", expr.kind, source_line);
            //io::stderr().flush().expect("Failed to flush stdout");
        }
    }

    #[inline]
    #[allow(clippy::too_many_lines)]
    fn evaluate_expression_with_signal(
        &mut self,
        expr: &ResolvedExpression,
    ) -> Result<ValueWithSignal, ExecuteError> {
        match &expr.kind {
            ResolvedExpressionKind::Break => Ok(ValueWithSignal::Break),
            ResolvedExpressionKind::Continue => Ok(ValueWithSignal::Continue),

            ResolvedExpressionKind::Return(maybe_expr) => {
                let value = match maybe_expr {
                    None => Value::Unit,
                    Some(expr) => self.evaluate_expression(expr)?,
                };
                Ok(ValueWithSignal::Return(value))
            }

            ResolvedExpressionKind::WhileLoop(condition, body) => {
                self.evaluate_while_loop(condition, body)
            }

            ResolvedExpressionKind::ForLoop(pattern, iterator_expr, body) => {
                if pattern.is_mutable() {
                    self.evaluate_for_loop_mutable(pattern, iterator_expr, body)
                } else {
                    self.evaluate_for_loop(pattern, iterator_expr, body)
                }
            }

            ResolvedExpressionKind::If(condition, consequences, optional_alternative) => {
                let cond_value = self.evaluate_expression(&condition.expression)?;
                if cond_value.is_truthy().unwrap() {
                    // TODO: error handling
                    self.evaluate_expression_with_signal(consequences)
                } else if let Some(alternative) = optional_alternative {
                    self.evaluate_expression_with_signal(alternative)
                } else {
                    Ok(ValueWithSignal::Value(Value::Unit))
                }
            }

            ResolvedExpressionKind::Block(expressions) => self.evaluate_block(expressions),

            ResolvedExpressionKind::When(bindings, true_block, maybe_else_block) => {
                let mut all_are_some = true;
                let mut all_expressions = Vec::new();
                for binding in bindings {
                    let source = self.evaluate_mut_or_immutable_expression(&binding.expr)?;
                    if let Value::Option(boxed_val) = source.to_value() {
                        if let Some(found_val) = boxed_val {
                            let variable_value = match source {
                                VariableValue::Value(_) => {
                                    // Source was not created mut, so copy value
                                    VariableValue::Value(found_val.borrow().clone())
                                }
                                VariableValue::Reference(_var_ref) => {
                                    // It was `mut`
                                    VariableValue::Reference(found_val)
                                }
                            };
                            all_expressions.push(variable_value);
                        } else {
                            all_are_some = false;
                            //warn!(?source, "Not ALL ARE SOME!");
                            break;
                        }
                    } else {
                        return Err(
                            self.create_err(ExecuteErrorKind::ExpectedOptional, &true_block.node)
                        );
                    }
                }

                if all_are_some {
                    self.push_block_scope();

                    for (binding, value) in bindings.iter().zip(all_expressions) {
                        self.current_block_scopes
                            .initialize_var_mem(&binding.variable, value)?;
                    }

                    let result = self.evaluate_expression_with_signal(true_block)?;
                    self.pop_block_scope();

                    Ok(result)
                } else if let Some(else_block) = maybe_else_block {
                    self.evaluate_expression_with_signal(else_block)
                } else {
                    Ok(ValueWithSignal::Value(Value::Unit))
                }
            }

            _ => Ok(ValueWithSignal::Value(self.evaluate_expression(expr)?)),
        }
    }

    // ---------------
    #[allow(clippy::too_many_lines)]
    #[inline]
    fn evaluate_expression(&mut self, expr: &ResolvedExpression) -> Result<Value, ExecuteError> {
        self.depth += 1;
        self.debug_expr(expr);
        let value = match &expr.kind {
            // Illegal in this context
            ResolvedExpressionKind::Continue => {
                return Err(self.create_err(ExecuteErrorKind::ContinueNotAllowedHere, &expr.node));
            }
            ResolvedExpressionKind::Break => {
                return Err(self.create_err(ExecuteErrorKind::BreakNotAllowedHere, &expr.node));
            }
            ResolvedExpressionKind::Return(_maybe_expr) => {
                return Err(self.create_err(ExecuteErrorKind::ReturnNotAllowedHere, &expr.node));
            }

            ResolvedExpressionKind::WhileLoop(_condition, _body) => {
                panic!("should have been handled earlier")
            }

            ResolvedExpressionKind::ForLoop(_pattern, _iterator_expr, _body) => {
                panic!("should have been handled earlier")
            }

            // Constructing
            ResolvedExpressionKind::Literal(lit) => self.evaluate_literal(&expr.node, lit)?,

            ResolvedExpressionKind::Array(array_instantiation) => {
                let mut values = Vec::new();
                for element in &array_instantiation.expressions {
                    values.push(self.evaluate_expression(element)?);
                }

                Value::Array(
                    array_instantiation.array_type_ref.clone(),
                    convert_vec_to_rc_refcell(values),
                )
            }

            ResolvedExpressionKind::StructInstantiation(struct_instantiation) => {
                // Evaluate all field expressions and validate types
                let mut field_values =
                    Vec::with_capacity(struct_instantiation.source_order_expressions.len());
                field_values.resize_with(
                    struct_instantiation.source_order_expressions.len(),
                    Default::default,
                );

                // They are evaluated in source order, but an array_index is provided for the definition order
                for (array_index, field_expr) in &struct_instantiation.source_order_expressions {
                    let value = self.evaluate_expression(field_expr)?;
                    field_values[*array_index] = value;
                }

                Value::Struct(
                    struct_instantiation.struct_type_ref.clone(),
                    convert_vec_to_rc_refcell(field_values),
                )
            }

            ResolvedExpressionKind::Range(start, end, range_mode) => {
                let start_val = self.evaluate_expression(start)?;
                let end_val = self.evaluate_expression(end)?;
                match (start_val, end_val) {
                    (Value::Int(s), Value::Int(e)) => {
                        Value::Range(Box::new(s), Box::new(e), range_mode.clone())
                    }
                    _ => Err(self.create_err(ExecuteErrorKind::RangeItemMustBeInt, &expr.node))?,
                }
            }

            // ==================== ASSIGNMENT ====================
            ResolvedExpressionKind::VariableDefinition(target_var, source_expr) => {
                let source_value_or_reference =
                    self.evaluate_mut_or_immutable_expression(source_expr)?;

                self.current_block_scopes
                    .initialize_var_mem(target_var, source_value_or_reference.clone())?;

                source_value_or_reference.to_value().clone()
            }

            ResolvedExpressionKind::VariableReassignment(variable_ref, source_expr) => {
                let new_value = self.evaluate_mut_or_immutable_expression(source_expr)?;

                let value_ref = self
                    .current_block_scopes
                    .lookup_variable_mut_ref(variable_ref)?;

                let mut was_assigned = false;
                if let Value::Option(inner_value) = &*value_ref.borrow() {
                    if let Some(inner) = inner_value {
                        *inner.borrow_mut() = new_value.to_value();
                        was_assigned = true;
                    }
                }

                if !was_assigned {
                    self.current_block_scopes
                        .overwrite_existing_var_mem(variable_ref, new_value.clone())?;
                }

                Value::Unit
            }
            ResolvedExpressionKind::ArrayExtend(location, source_expression) => {
                let source_val = self.evaluate_expression(source_expression)?;

                let array_val_ref = self.evaluate_location(&location.0)?;
                if let Value::Array(_type_id, ref mut vector) = &mut *array_val_ref.borrow_mut() {
                    if let Value::Array(_, items) = source_val {
                        vector.extend(items);
                    } else {
                        Err(self.create_err(ExecuteErrorKind::OperationRequiresArray, &expr.node))?;
                    }
                } else {
                    todo!("handle error")
                }

                // array_val_ref.borrow().clone()
                Value::Unit
            }

            ResolvedExpressionKind::ArrayPush(location, source_expression) => {
                let source_val = self.evaluate_expression(source_expression)?;
                let array_val_ref = self.evaluate_location(&location.0)?;

                if let Value::Array(_type_id, ref mut vector) = &mut *array_val_ref.borrow_mut() {
                    vector.push(Rc::new(RefCell::new(source_val)));
                } else {
                    Err(self.create_err(ExecuteErrorKind::OperationRequiresArray, &expr.node))?;
                }
                //array_val_ref.borrow().clone()
                Value::Unit
            }

            ResolvedExpressionKind::MapAssignment(map, index, value) => {
                let map_val = self.evaluate_location(&map.0)?;
                let index_val = self.evaluate_expression(index)?;
                let new_val = self.evaluate_expression(value)?;

                if let Value::Map(_type_id, ref mut elements) = &mut *map_val.borrow_mut() {
                    elements
                        .insert(index_val, Rc::new(RefCell::new(new_val)))
                        .map_err(|_| {
                            self.create_err(ExecuteErrorKind::MapKeyAlreadyExists, &expr.node)
                        })?;
                } else {
                    Err(self.create_err(ExecuteErrorKind::OperationRequiresArray, &expr.node))?;
                }

                Value::Unit
            }

            // ------------- LOOKUP ---------------------
            ResolvedExpressionKind::ConstantAccess(constant) => {
                self.constants.lookup_constant_value(constant.id).clone()
            }

            ResolvedExpressionKind::AssignmentSlice(_mut_location, _source) => todo!(),

            ResolvedExpressionKind::Assignment(mut_location_expr, source_expr) => {
                let value_ref = self.evaluate_location(&mut_location_expr.0)?;
                let source_value = self.evaluate_expression(source_expr)?;

                *value_ref.borrow_mut() = source_value;

                Value::Unit
            }

            ResolvedExpressionKind::CompoundAssignment(mut_location_expr, op, source_expr) => {
                let value_ref = self.evaluate_location(&mut_location_expr.0)?;
                let source_value = self.evaluate_expression(source_expr)?;

                self.apply_compound_operator(
                    &expr.node,
                    &mut value_ref.borrow_mut(),
                    op,
                    &source_value,
                )?;

                Value::Unit
            }

            // Operators
            ResolvedExpressionKind::BinaryOp(binary_operator) => {
                let left_val = self.evaluate_expression(&binary_operator.left)?;
                let right_val = self.evaluate_expression(&binary_operator.right)?;
                self.evaluate_binary_op(&expr.node, left_val, &binary_operator.kind, right_val)?
            }

            ResolvedExpressionKind::UnaryOp(unary_operator) => {
                let left_val = self.evaluate_expression(&unary_operator.left)?;
                self.evaluate_unary_op(&expr.node, &unary_operator.kind, left_val)?
            }

            // Calling
            ResolvedExpressionKind::FunctionCall(_signature, expr, arguments) => {
                self.evaluate_function_call(expr, arguments)?
            }

            ResolvedExpressionKind::MemberCall(resolved_member_call) => {
                let parameters = match &*resolved_member_call.function {
                    ResolvedFunction::Internal(function_data) => {
                        &function_data.signature.parameters
                    }
                    ResolvedFunction::External(external_data) => {
                        &external_data.signature.parameters
                    }
                };

                let mut member_call_arguments = Vec::new();
                member_call_arguments.extend(self.evaluate_args(&resolved_member_call.arguments)?);

                // Check total number of parameters (including self)
                if member_call_arguments.len() != parameters.len() {
                    return Err(self.create_err(
                        ExecuteErrorKind::WrongNumberOfArguments(
                            parameters.len(),
                            member_call_arguments.len(),
                        ),
                        &expr.node,
                    ));
                }

                match &*resolved_member_call.function {
                    ResolvedFunction::Internal(internal_function) => {
                        self.push_function_scope();
                        self.bind_parameters(
                            &expr.node,
                            &internal_function.signature.parameters,
                            &member_call_arguments,
                        )?;
                        let result = self.evaluate_expression(&internal_function.body)?;
                        self.pop_function_scope();

                        result
                    }
                    ResolvedFunction::External(external_func) => {
                        let mut func = self
                            .externals
                            .external_functions_by_id
                            .get(&external_func.id)
                            .expect("member call: external function missing")
                            .borrow_mut();
                        (func.func)(&member_call_arguments, self.context)?
                    }
                }
            }

            ResolvedExpressionKind::Block(statements) => {
                self.evaluate_block(statements)?.try_into().unwrap() // TODO: Error handling
            }

            ResolvedExpressionKind::InterpolatedString(parts) => {
                let mut result = String::new();

                for part in parts {
                    match part {
                        ResolvedStringPart::Literal(_resolved_node, text) => {
                            result.push_str(text);
                        }
                        ResolvedStringPart::Interpolation(expr, format_spec) => {
                            let value = self.evaluate_expression(expr)?;
                            let formatted = match format_spec {
                                Some(spec) => format_value(&value, &spec.kind).unwrap(), // TODO: Error handling
                                None => value.convert_to_string_if_needed(),
                            };
                            result.push_str(&formatted);
                        }
                    }
                }

                Value::String(result)
            }

            ResolvedExpressionKind::Match(resolved_match) => self.eval_match(resolved_match)?,
            ResolvedExpressionKind::Guard(guards) => self.eval_guard(&expr.node, guards)?,

            ResolvedExpressionKind::InternalFunctionAccess(fetch_function) => {
                Value::InternalFunction(fetch_function.clone())
            }

            ResolvedExpressionKind::ExternalFunctionAccess(fetch_function) => {
                self.externals
                    .external_functions_by_id
                    .get(&fetch_function.id)
                    .expect("should have external function ref");
                Value::ExternalFunction(fetch_function.clone())
            }

            //ResolvedExpressionKind::MutMemberCall(_, _) => todo!(),
            ResolvedExpressionKind::Tuple(_) => todo!(),
            ResolvedExpressionKind::Option(inner) => match inner {
                None => Value::Option(None),
                Some(expression) => {
                    let v = self.evaluate_expression(expression)?;
                    match v {
                        Value::Option(_) => {
                            panic!("unnecessary wrap!, should be investigated");
                        }
                        _ => Value::Option(Some(Rc::new(RefCell::new(v)))),
                    }
                }
            },

            // --------------- SPECIAL FUNCTIONS
            ResolvedExpressionKind::SparseNew(
                sparse_id_rust_type_ref,
                resolved_value_item_type,
            ) => {
                let sparse_value_map = SparseValueMap::new(
                    sparse_id_rust_type_ref.clone(),
                    resolved_value_item_type.clone(),
                );
                to_rust_value(sparse_id_rust_type_ref.clone(), sparse_value_map)
            }

            ResolvedExpressionKind::CoerceOptionToBool(expression) => {
                let value = self.evaluate_expression(expression)?;
                match value {
                    Value::Option(inner) => Value::Bool(inner.is_some()),
                    _ => {
                        return Err(
                            self.create_err(ExecuteErrorKind::CoerceOptionToBoolFailed, &expr.node)
                        )
                    }
                }
            }

            ResolvedExpressionKind::If(condition, consequences, optional_alternative) => {
                let cond_value = self.evaluate_expression(&condition.expression)?;
                if cond_value.is_truthy().unwrap() {
                    // TODO: ERROR HANDLING
                    self.evaluate_expression(consequences)?
                } else if let Some(alternative) = optional_alternative {
                    self.evaluate_expression(alternative)?
                } else {
                    Value::Unit
                }
            }

            ResolvedExpressionKind::When(bindings, true_block, maybe_else_block) => {
                let mut all_are_some = true;
                let mut all_expressions = Vec::new();
                for binding in bindings {
                    let source = self.evaluate_mut_or_immutable_expression(&binding.expr)?;
                    if let Value::Option(boxed_val) = source.to_value() {
                        if let Some(found_val) = boxed_val {
                            all_expressions.push(found_val.borrow().clone());
                        } else {
                            all_are_some = false;
                            break;
                        }
                    } else {
                        return Err(
                            self.create_err(ExecuteErrorKind::ExpectedOptional, &true_block.node)
                        );
                    }
                }

                if all_are_some {
                    self.push_block_scope();

                    for (binding, value) in bindings.iter().zip(all_expressions) {
                        info!(var=?binding.variable, "binding as mutable");
                        self.current_block_scopes.initialize_var(
                            binding.variable.scope_index,
                            binding.variable.variable_index,
                            value,
                            binding.variable.is_mutable(),
                        );
                    }

                    let result = self.evaluate_expression(true_block)?;
                    self.pop_block_scope();

                    result
                } else if let Some(else_block) = maybe_else_block {
                    self.evaluate_expression(else_block)?
                } else {
                    Value::Unit
                }
            }

            ResolvedExpressionKind::TupleDestructuring(variable_refs, _, expr) => {
                let value = self.evaluate_expression(expr)?;
                if let Value::Tuple(_tuple_ref, values) = value {
                    if variable_refs.len() > values.len() {
                        return Err(self.create_err(ExecuteErrorKind::NotAnArray, &expr.node));
                    }
                    for (index, variable_ref) in variable_refs.iter().enumerate() {
                        let value = &values[index].borrow().clone();
                        self.current_block_scopes.initialize_var(
                            variable_ref.scope_index,
                            variable_ref.variable_index,
                            value.clone(),
                            false,
                        );
                    }
                }
                Value::Unit
            }
            ResolvedExpressionKind::VariableAccess(variable_ref) => {
                self.current_block_scopes.lookup_var_value(variable_ref)
            }
            ResolvedExpressionKind::FieldAccess(expr, struct_field) => {
                let resolved_expr = self.evaluate_expression(expr)?;
                let (_struct_type, values) = resolved_expr
                    .expect_struct()
                    .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedStruct, &expr.node))?;
                let x = values[struct_field.index].borrow().clone();
                x
            }

            ResolvedExpressionKind::ArrayAccess(expr, _array, index_expr) => {
                let resolved_expr = self.evaluate_expression(expr)?;
                let (_array_type, values) = resolved_expr
                    .expect_array()
                    .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedArray, &expr.node))?;

                let index = self
                    .evaluate_expression(index_expr)?
                    .expect_int()
                    .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedInt, &expr.node))?
                    as usize;

                let x = values[index].borrow().clone();
                x
            }

            ResolvedExpressionKind::MapIndexAccess(expr, _map_type_ref, key_expr) => {
                let resolved_expr = self.evaluate_expression(expr)?;
                let (_map_type, seq_map) = resolved_expr
                    .expect_map()
                    .map_err(|_| self.create_err(ExecuteErrorKind::ExpectedMap, &expr.node))?;

                let key_val = self.evaluate_expression(key_expr)?;

                let value_val_maybe = seq_map.get(&key_val);
                Value::Option(value_val_maybe.cloned())
            }
            ResolvedExpressionKind::StringRangeAccess(_, _) => todo!(),
            ResolvedExpressionKind::ArrayRangeAccess(_, _) => todo!(),
            ResolvedExpressionKind::PostfixChain(start, parts) => {
                let value_ref = self.eval_chain(&expr.node, start, parts)?;
                let x = value_ref.borrow().clone();
                x
            }
        };

        self.depth -= 1;
        Ok(value)
    }

    fn evaluate_literal(
        &mut self,
        node: &ResolvedNode,
        lit: &ResolvedLiteral,
    ) -> Result<Value, ExecuteError> {
        let v = match lit {
            ResolvedLiteral::IntLiteral(n) => Value::Int(*n),
            ResolvedLiteral::FloatLiteral(f) => Value::Float(*f),
            ResolvedLiteral::StringLiteral(s) => Value::String(s.clone()),
            ResolvedLiteral::BoolLiteral(b) => Value::Bool(*b),

            ResolvedLiteral::EnumVariantLiteral(enum_variant_type, data) => {
                let variant_container_value: Value = match &**enum_variant_type {
                    ResolvedEnumVariantType::Tuple(tuple_type) => match data {
                        ResolvedEnumLiteralData::Tuple(tuple_expressions) => {
                            let eval_expressions = self.evaluate_expressions(tuple_expressions)?;
                            let value_refs = values_to_value_refs_owned(eval_expressions);
                            Value::EnumVariantTuple(tuple_type.clone(), value_refs)
                        }
                        _ => panic!("wrong container type"),
                    },

                    ResolvedEnumVariantType::Struct(struct_type_ref) => match data {
                        ResolvedEnumLiteralData::Struct(source_order_field_values) => {
                            let mut field_values =
                                Vec::with_capacity(source_order_field_values.len());
                            field_values
                                .resize_with(source_order_field_values.len(), Default::default);
                            for (index, resolved_expression) in source_order_field_values {
                                let value = self.evaluate_expression(resolved_expression)?;
                                field_values[*index] = Rc::new(RefCell::new(value));
                            }
                            Value::EnumVariantStruct(struct_type_ref.clone(), field_values)
                        }
                        _ => panic!("wrong container type"),
                    },

                    ResolvedEnumVariantType::Nothing(data) => {
                        Value::EnumVariantSimple(data.clone())
                    }
                };
                variant_container_value
            }

            ResolvedLiteral::TupleLiteral(tuple_type, resolved_expressions) => {
                let values = self.evaluate_expressions(resolved_expressions)?;
                Value::Tuple(tuple_type.clone(), convert_vec_to_rc_refcell(values))
            }

            ResolvedLiteral::Array(array_type, expressions) => {
                let values = self.evaluate_expressions(expressions)?;
                Value::Array(array_type.clone(), convert_vec_to_rc_refcell(values))
            }
            ResolvedLiteral::Map(map_type_ref, expressions) => {
                let mut items = SeqMap::new();
                for (key, value) in expressions {
                    let key_val = self.evaluate_expression(key)?;
                    let value_val = self.evaluate_expression(value)?;
                    items
                        .insert(key_val, Rc::new(RefCell::new(value_val)))
                        .map_err(|_err| {
                            self.create_err(
                                ExecuteErrorKind::NonUniqueKeysInMapLiteralDetected,
                                &node,
                            )
                        })?;
                }
                Value::Map(map_type_ref.clone(), items)
            }
            ResolvedLiteral::NoneLiteral => Value::Option(None),
        };
        Ok(v)
    }

    #[allow(clippy::too_many_lines)]
    fn eval_internal_postfix(
        &mut self,
        value_ref: &ValueRef,
        resolved_postfix: &ResolvedPostfix,
    ) -> Result<Value, ExecuteError> {
        let node = &resolved_postfix.node;
        let val = match &resolved_postfix.kind {
            ResolvedPostfixKind::ArrayRemoveIndex(usize_index_expression) => {
                let index_val = self.evaluate_expression(usize_index_expression)?;
                let Value::Int(index) = index_val else {
                    return Err(self.create_err(ExecuteErrorKind::ArgumentIsNotMutable, node));
                };

                if let Value::Array(_type_id, ref mut vector) = &mut *value_ref.borrow_mut() {
                    vector.remove(index as usize);
                } else {
                    Err(self.create_err(ExecuteErrorKind::OperationRequiresArray, node))?;
                }

                value_ref.borrow().clone()
            }

            ResolvedPostfixKind::ArrayClear => {
                if let Value::Array(_type_id, ref mut vector) = &mut *value_ref.borrow_mut() {
                    vector.clear();
                } else {
                    Err(self.create_err(ExecuteErrorKind::OperationRequiresArray, node))?;
                }
                Value::Unit
            }

            ResolvedPostfixKind::MapHas(index_expr) => {
                let index_val = self.evaluate_expression(index_expr)?;

                if let Value::Map(_type_id, ref seq_map) = value_ref.borrow().clone() {
                    let has_key = seq_map.contains_key(&index_val);
                    Value::Bool(has_key)
                } else {
                    return Err(self.create_err(ExecuteErrorKind::NotAMap, node));
                }
            }

            ResolvedPostfixKind::MapRemove(index_expr, _map_type_ref) => {
                let index_val = self.evaluate_expression(&index_expr)?;

                let result = {
                    let mut borrowed = value_ref.borrow_mut();
                    if let Value::Map(_type_id, ref mut seq_map) = &mut *borrowed {
                        let x = seq_map.remove(&index_val);
                        x.map_or_else(|| Value::Option(None), |v| Value::Option(Some(v.clone())))
                    } else {
                        return Err(self.create_err(ExecuteErrorKind::NotAMap, node));
                    }
                };
                result
            }

            ResolvedPostfixKind::SparseAdd(value_expression) => {
                let borrowed = value_ref.borrow();

                let sparse_value_map = borrowed.downcast_rust::<SparseValueMap>();
                if let Some(found) = sparse_value_map {
                    let resolved_value = self.evaluate_expression(value_expression)?;
                    let id_value = found.borrow_mut().add(resolved_value);

                    id_value
                } else {
                    return Err(self.create_err(ExecuteErrorKind::NotSparseValue, node));
                }
            }

            ResolvedPostfixKind::SparseRemove(id_expression) => {
                let borrowed = value_ref.borrow();

                let sparse_value_map = borrowed.downcast_rust::<SparseValueMap>();
                if let Some(found) = sparse_value_map {
                    let id_value = self.evaluate_expression(id_expression)?;
                    if let Some(found_id) = id_value.downcast_rust::<SparseValueId>() {
                        found.borrow_mut().remove(&found_id.borrow());
                    } else {
                        return Err(self.create_err(ExecuteErrorKind::NotSparseValue, node));
                    }
                }

                Value::Unit
            }
            ResolvedPostfixKind::SparseAccess(id_expression) => {
                let sparse_value_map = value_ref.borrow_mut().downcast_rust::<SparseValueMap>();
                if let Some(found) = sparse_value_map {
                    let id_value = self.evaluate_expression(id_expression)?;
                    if let Some(found_id) = id_value.downcast_rust::<SparseValueId>() {
                        if let Some(found_value) = found.borrow_mut().get(&found_id.borrow()) {
                            Value::Option(Some(found_value.clone()))
                        } else {
                            Value::Option(None)
                        }
                    } else {
                        return Err(self.create_err(ExecuteErrorKind::NotSparseId, node));
                    }
                } else {
                    return Err(self.create_err(ExecuteErrorKind::NotSparseId, node));
                }
            }

            ResolvedPostfixKind::FloatRound => {
                if let Value::Float(f) = value_ref.borrow().clone() {
                    Value::Int(f.round().into())
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }
            ResolvedPostfixKind::FloatFloor => {
                if let Value::Float(f) = value_ref.borrow().clone() {
                    Value::Int(f.floor().into())
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }

            ResolvedPostfixKind::FloatSign => {
                if let Value::Float(f) = value_ref.borrow().clone() {
                    let signum = if f.inner() < 0 {
                        -1
                    } else if f.inner() > 0 {
                        1
                    } else {
                        0
                    };
                    Value::Float(Fp::from(signum as i16))
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }
            ResolvedPostfixKind::FloatAbs => {
                if let Value::Float(f) = value_ref.borrow().clone() {
                    Value::Float(f.abs())
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }

            ResolvedPostfixKind::FloatCos => {
                if let Value::Float(f) = value_ref.borrow().clone() {
                    Value::Float(f.cos())
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }

            ResolvedPostfixKind::FloatAcos => {
                if let Value::Float(f) = value_ref.borrow().clone() {
                    Value::Float(f.acos())
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }

            ResolvedPostfixKind::FloatSin => {
                if let Value::Float(f) = value_ref.borrow().clone() {
                    Value::Float(f.sin())
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }

            ResolvedPostfixKind::FloatAsin => {
                if let Value::Float(f) = value_ref.borrow().clone() {
                    Value::Float(f.asin())
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }

            ResolvedPostfixKind::FloatSqrt => {
                if let Value::Float(f) = value_ref.borrow().clone() {
                    Value::Float(f.sqrt())
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }

            ResolvedPostfixKind::FloatMin(min) => {
                let min_value = self.evaluate_expression(min)?;
                if let (Value::Float(f), Value::Float(min_f)) =
                    (value_ref.borrow().clone(), min_value)
                {
                    Value::Float(f.min(min_f))
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }

            ResolvedPostfixKind::FloatMax(max) => {
                let max_value = self.evaluate_expression(max)?;
                if let (Value::Float(f), Value::Float(max_f)) =
                    (value_ref.borrow().clone(), max_value)
                {
                    Value::Float(f.max(max_f))
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }

            ResolvedPostfixKind::FloatAtan2(x) => {
                let x_value = self.evaluate_expression(x)?;
                if let (Value::Float(_y_f), Value::Float(_x_f)) =
                    (value_ref.borrow().clone(), x_value)
                {
                    Value::Float(Fp::from(-9999)) //y_f.atan2(x_f)) // TODO: Implement atan2
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }

            ResolvedPostfixKind::FloatClamp(min, max) => {
                let min_value = self.evaluate_expression(min)?;
                let max_value = self.evaluate_expression(max)?;
                if let (Value::Float(f), Value::Float(min_f), Value::Float(max_f)) =
                    (value_ref.borrow().clone(), min_value, max_value)
                {
                    Value::Float(f.clamp(min_f, max_f))
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }

            ResolvedPostfixKind::FloatRnd => {
                if let Value::Float(f) = value_ref.borrow().clone() {
                    let new_raw = squirrel_prng::squirrel_noise5(f.inner() as u32, 0);
                    Value::Int(new_raw as i32)
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }

            ResolvedPostfixKind::IntAbs => {
                if let Value::Int(i) = value_ref.borrow().clone() {
                    Value::Int(i.abs())
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedFloat, node));
                }
            }

            ResolvedPostfixKind::IntClamp(min, max) => {
                let min_value = self.evaluate_expression(min)?;
                let max_value = self.evaluate_expression(max)?;
                if let (Value::Int(i), Value::Int(min_i), Value::Int(max_i)) =
                    (value_ref.borrow().clone(), min_value, max_value)
                {
                    Value::Int(i.clamp(min_i, max_i))
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedInt, node));
                }
            }

            ResolvedPostfixKind::IntMin(max) => {
                let max_value = self.evaluate_expression(max)?;
                if let (Value::Int(i), Value::Int(min_i)) = (value_ref.borrow().clone(), max_value)
                {
                    Value::Int(i.min(min_i))
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedInt, node));
                }
            }

            ResolvedPostfixKind::IntMax(max) => {
                let max_value = self.evaluate_expression(max)?;
                if let (Value::Int(i), Value::Int(max_i)) = (value_ref.borrow().clone(), max_value)
                {
                    Value::Int(i.max(max_i))
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedInt, node));
                }
            }

            ResolvedPostfixKind::IntRnd => {
                if let Value::Int(i) = value_ref.borrow().clone() {
                    Value::Int(squirrel_prng::squirrel_noise5(i as u32, 0) as i32)
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedInt, node));
                }
            }

            ResolvedPostfixKind::IntToFloat => {
                if let Value::Int(i) = value_ref.borrow().clone() {
                    Value::Float(Fp::from(i as i16))
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedInt, node));
                }
            }

            ResolvedPostfixKind::StringLen => {
                if let Value::String(s) = value_ref.borrow().clone() {
                    Value::Int(s.len().try_into().expect("string len overflow"))
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedString, node));
                }
            }

            ResolvedPostfixKind::Tuple2FloatMagnitude => {
                if let Value::Tuple(_tuple_ref, values) = value_ref.borrow().clone() {
                    if values.len() != 2 {
                        return Err(self.create_err(
                            ExecuteErrorKind::WrongNumberOfArguments(2, values.len()),
                            &node,
                        ));
                    }
                    match (
                        values[0].as_ref().borrow().clone(),
                        values[1].as_ref().borrow().clone(),
                    ) {
                        (Value::Float(a), Value::Float(b)) => {
                            let a_raw: i64 = a.inner() as i64;
                            let b_raw: i64 = b.inner() as i64;

                            let i64_magnitude = i64_sqrt(a_raw * a_raw + b_raw * b_raw);

                            let new_fp = Fp::from_raw(
                                i32::try_from(i64_magnitude).expect("wrong with i64_sqrt"),
                            );
                            Value::Float(new_fp)
                        }
                        _ => {
                            return Err(
                                self.create_err(ExecuteErrorKind::ExpectedTwoFloatTuple, node)
                            );
                        }
                    }
                } else {
                    return Err(self.create_err(ExecuteErrorKind::ExpectedTwoFloatTuple, node));
                }
            }

            _ => todo!(),
        };

        Ok(val)
    }

    #[allow(clippy::too_many_lines)]
    fn eval_chain(
        &mut self,
        node: &ResolvedNode,
        start: &ResolvedExpression,
        parts: &[ResolvedPostfix],
    ) -> Result<ValueRef, ExecuteError> {
        let (mut val_ref, mut is_mutable) = match &start.kind {
            ResolvedExpressionKind::VariableAccess(start_var) => {
                let start_variable_value = self.current_block_scopes.get_var(&start_var);

                match start_variable_value {
                    VariableValue::Value(value) => (Rc::new(RefCell::new(value.clone())), false),
                    VariableValue::Reference(value_ref) => (value_ref.clone(), true),
                }
            }
            _ => (
                Rc::new(RefCell::new(self.evaluate_expression(start)?)),
                false,
            ),
        };

        let mut is_uncertain = false;
        let mut is_undefined = false;

        for part in parts {
            if let ResolvedPostfixKind::NoneCoalesce(default_expression) = &part.kind {
                val_ref = {
                    let borrowed = val_ref.borrow();

                    if let Value::Option(found_option) = borrowed.clone() {
                        if let Some(some_value) = found_option {
                            some_value
                        } else {
                            let default_value = self.evaluate_expression(default_expression)?;
                            Rc::new(RefCell::new(default_value))
                        }
                    } else {
                        return Err(self.create_err(ExecuteErrorKind::ExpectedOptional, &part.node));
                    }
                };

                is_mutable = false;
                is_uncertain = false;
                is_undefined = false;
            } else if is_undefined {
                continue;
            }
            match &part.kind {
                ResolvedPostfixKind::NoneCoalesce(_default_expression) => {
                    // Handled earlier
                }
                ResolvedPostfixKind::StructField(expected_struct_type, index) => {
                    let (encountered_struct_type, fields) = {
                        let brw = val_ref.borrow();
                        let (struct_ref, fields_ref) = brw.expect_struct().map_err(|_| {
                            self.create_err(ExecuteErrorKind::PostfixChainError, &part.node)
                        })?;
                        (struct_ref.clone(), fields_ref.clone())
                    };

                    assert!(same_struct_ref(
                        &encountered_struct_type,
                        expected_struct_type
                    ));
                    val_ref = fields[*index].clone();
                }
                ResolvedPostfixKind::ArrayIndex(expected_array_type, index_expr) => {
                    let (encountered_array_type, fields) = {
                        let brw = val_ref.borrow();
                        let (array_ref, fields_ref) = brw.expect_array().map_err(|_| {
                            self.create_err(ExecuteErrorKind::PostfixChainError, &part.node)
                        })?;
                        (array_ref.clone(), fields_ref.clone())
                    };
                    assert!(same_array_ref(&encountered_array_type, expected_array_type));

                    let index =
                        self.evaluate_expression(index_expr)?
                            .expect_int()
                            .map_err(|_| {
                                self.create_err(ExecuteErrorKind::PostfixChainError, &part.node)
                            })? as usize;
                    if index >= fields.len() {
                        return Err(self.create_err(ExecuteErrorKind::IndexOutOfBounds, &part.node));
                    }
                    val_ref = fields[index].clone();
                }
                ResolvedPostfixKind::MapIndex(_expected_map_type_ref, key_expr) => {
                    let (_encountered_map_type, seq_map) = {
                        let brw = val_ref.borrow();
                        let (array_ref, seq_map) = brw.expect_map().map_err(|_| {
                            self.create_err(ExecuteErrorKind::PostfixChainError, &part.node)
                        })?;
                        (array_ref.clone(), seq_map.clone())
                    };
                    let key_val = self.evaluate_expression(key_expr)?;

                    val_ref = Rc::new(RefCell::new(Value::Option(seq_map.get(&key_val).cloned())));
                }
                ResolvedPostfixKind::RustTypeIndexRef(_rust_type_ref, map_expr) => {
                    let key_expr_value = self.evaluate_expression(map_expr)?;
                    val_ref = {
                        if let Some(found_sparse_id) =
                            key_expr_value.downcast_rust::<SparseValueId>()
                        {
                            if let Some(sparse_value_map) =
                                val_ref.borrow_mut().downcast_rust::<SparseValueMap>()
                            {
                                wrap_in_option(
                                    sparse_value_map.borrow_mut().get(&found_sparse_id.borrow()),
                                )
                            } else {
                                panic!("internal error");
                            }
                        } else {
                            panic!("todo");
                        }
                    };
                }
                ResolvedPostfixKind::MemberCall(function_ref, arguments) => {
                    let val =
                        self.eval_member_call(node, &val_ref, is_mutable, function_ref, arguments)?;

                    val_ref = Rc::new(RefCell::new(val));
                    is_mutable = false;
                }
                ResolvedPostfixKind::FunctionCall(arguments) => {
                    let val = self.eval_function_call(node, &val_ref, arguments)?;

                    val_ref = Rc::new(RefCell::new(val));
                    is_mutable = false;
                }
                ResolvedPostfixKind::OptionUnwrap => {
                    val_ref = {
                        let borrowed = val_ref.borrow();

                        if let Value::Option(found_option) = borrowed.clone() {
                            if let Some(some_value) = found_option {
                                some_value
                            } else {
                                is_undefined = true;

                                Rc::new(RefCell::new(Value::Option(None)))
                            }
                        } else {
                            return Err(
                                self.create_err(ExecuteErrorKind::ExpectedOptional, &part.node)
                            );
                        }
                    };

                    is_mutable = false;
                    is_uncertain = true;
                }
                _ => {
                    val_ref = Rc::new(RefCell::new(self.eval_internal_postfix(&val_ref, part)?));
                    is_mutable = false;
                }
            }
        }

        if is_uncertain {
            let binding = val_ref.borrow().clone();
            if let Value::Option(_) = binding {
            } else {
                val_ref = Rc::new(RefCell::new(Value::Option(Some(val_ref))));
            }
        }

        Ok(val_ref)
    }

    fn eval_function_call(
        &mut self,
        node: &ResolvedNode,
        function_val: &ValueRef,
        arguments: &[ResolvedArgumentExpressionOrLocation],
    ) -> Result<Value, ExecuteError> {
        let resolved_fn = match function_val.borrow().clone() {
            Value::InternalFunction(x) => ResolvedFunction::Internal(x.clone()),
            Value::ExternalFunction(external_fn) => ResolvedFunction::External(external_fn.clone()),
            _ => panic!("no function to call"),
        };

        let parameters = &resolved_fn.signature().parameters;
        // Check total number of parameters (including self)
        if arguments.len() != parameters.len() {
            panic!("wrong number of arguments")
        }

        let resolved_arguments = self.evaluate_args(&arguments)?;

        let result_val = match &resolved_fn {
            ResolvedFunction::Internal(internal_function) => {
                self.push_function_scope();

                self.bind_parameters(node, &parameters, &resolved_arguments)?;
                let result = self.evaluate_expression(&internal_function.body)?;
                self.pop_function_scope();

                result
            }
            ResolvedFunction::External(external_func) => {
                let mut func = self
                    .externals
                    .external_functions_by_id
                    .get(&external_func.id)
                    .expect("member call: external function missing")
                    .borrow_mut();
                (func.func)(&resolved_arguments, self.context)?
            }
        };

        Ok(result_val)
    }

    #[inline]
    fn eval_member_call(
        &mut self,
        node: &ResolvedNode,
        self_value_ref: &ValueRef,
        is_mutable: bool,
        function_ref: &ResolvedFunctionRef,
        arguments: &[ResolvedArgumentExpressionOrLocation],
    ) -> Result<Value, ExecuteError> {
        let parameters = &function_ref.signature().parameters;

        let self_var_value = if parameters[0].is_mutable {
            if !is_mutable {
                return Err(self.create_err(ExecuteErrorKind::ArgumentIsNotMutable, &node));
            }
            VariableValue::Reference(self_value_ref.clone())
        } else {
            VariableValue::Value(self_value_ref.borrow().clone())
        };

        let mut member_call_arguments = Vec::new();
        member_call_arguments.push(self_var_value); // Add self as first argument
        member_call_arguments.extend(self.evaluate_args(&arguments)?);

        // Check total number of parameters (including self)
        if member_call_arguments.len() != parameters.len() {
            panic!("wrong number of arguments")
        }

        let result_val = match &**function_ref {
            ResolvedFunction::Internal(internal_function) => {
                self.push_function_scope();
                self.bind_parameters(node, &parameters, &member_call_arguments)?;
                let result = self.evaluate_expression(&internal_function.body)?;
                self.pop_function_scope();

                result
            }
            ResolvedFunction::External(external_func) => {
                let mut func = self
                    .externals
                    .external_functions_by_id
                    .get(&external_func.id)
                    .expect("member call: external function missing")
                    .borrow_mut();
                (func.func)(&member_call_arguments, self.context)?
            }
        };

        Ok(result_val)
    }

    fn eval_guard(
        &mut self,
        node: &ResolvedNode,
        guards: &[ResolvedGuard],
    ) -> Result<Value, ExecuteError> {
        for guard in guards {
            let should_evaluate = if let Some(found_clause) = &guard.condition {
                self.evaluate_expression(&found_clause.expression)?
                    .is_truthy()?
            } else {
                true
            };

            if should_evaluate {
                return self.evaluate_expression(&guard.result);
            }
        }

        Err(self.create_err(ExecuteErrorKind::MustHaveGuardArmThatMatches, &node))
    }

    #[inline(always)]
    #[allow(clippy::too_many_lines)]
    fn eval_match(&mut self, resolved_match: &ResolvedMatch) -> Result<Value, ExecuteError> {
        let actual_value = self.evaluate_expression(&resolved_match.expression)?;

        for arm in &resolved_match.arms {
            match &arm.pattern {
                ResolvedPattern::Wildcard(_node) => {
                    return self.evaluate_expression(&arm.expression)
                }
                ResolvedPattern::Normal(normal_pattern, maybe_guard) => {
                    if let Some(found_guard) = maybe_guard {
                        if !self
                            .evaluate_expression(&found_guard.expression)?
                            .is_truthy()
                            .unwrap()
                        // TODO: ERROR HANDLING
                        {
                            continue;
                        }
                    }
                    match &normal_pattern {
                        ResolvedNormalPattern::PatternList(elements) => {
                            // Handle single variable/wildcard patterns that match any value
                            if elements.len() == 1 {
                                return match &elements[0] {
                                    ResolvedPatternElement::Variable(var_ref)
                                    | ResolvedPatternElement::VariableWithFieldIndex(var_ref, _) => {
                                        self.push_block_scope();
                                        self.current_block_scopes.init_var(var_ref, &actual_value);
                                        let result = self.evaluate_expression(&arm.expression);
                                        self.pop_block_scope();
                                        result
                                    }
                                    ResolvedPatternElement::Wildcard(_) => {
                                        // Wildcard matches anything
                                        self.evaluate_expression(&arm.expression)
                                    }
                                };
                            }

                            if let Value::Tuple(_tuple_type_ref, values) = &actual_value {
                                if elements.len() == values.len() {
                                    self.push_block_scope();

                                    for (element, value) in elements.iter().zip(values.iter()) {
                                        match element {
                                            ResolvedPatternElement::Variable(var_ref) => {
                                                self.current_block_scopes.set_local_var_value(
                                                    var_ref,
                                                    value.borrow().clone(),
                                                );
                                            }
                                            ResolvedPatternElement::VariableWithFieldIndex(
                                                var_ref,
                                                _,
                                            ) => {
                                                self.current_block_scopes.set_local_var_value(
                                                    var_ref,
                                                    value.borrow().clone(),
                                                );
                                            }
                                            ResolvedPatternElement::Wildcard(_) => {
                                                // Skip wildcards
                                                continue;
                                            }
                                        }
                                    }

                                    let result = self.evaluate_expression(&arm.expression);
                                    self.pop_block_scope();
                                    return result;
                                }
                            }
                        }

                        ResolvedNormalPattern::EnumPattern(variant_ref, maybe_elements) => {
                            match &actual_value {
                                Value::EnumVariantTuple(value_tuple_type, values) => {
                                    // First check if the variant types match
                                    if variant_ref.common().number != value_tuple_type.common.number
                                    {
                                        continue; // Try next pattern
                                    }

                                    if let Some(elements) = maybe_elements {
                                        if elements.len() == values.len() {
                                            self.push_block_scope();

                                            for (element, value) in
                                                elements.iter().zip(values.iter())
                                            {
                                                match element {
                                                    ResolvedPatternElement::Variable(var_ref) => {
                                                        self.current_block_scopes
                                                            .set_local_var_value(var_ref, value.borrow().clone());
                                                    }
                                                    ResolvedPatternElement::VariableWithFieldIndex(
                                                        var_ref,
                                                        _,
                                                    ) => {
                                                        self.current_block_scopes
                                                            .set_local_var_value(var_ref, value.borrow().clone());
                                                    }
                                                    ResolvedPatternElement::Wildcard(_) => continue,
                                                }
                                            }

                                            let result = self.evaluate_expression(&arm.expression);
                                            self.pop_block_scope();
                                            return result;
                                        }
                                    }
                                }
                                Value::EnumVariantStruct(value_enum_struct_type, values) => {
                                    info!(
                                        ?value_enum_struct_type,
                                        ?variant_ref,
                                        "comparing enum variant struct match arm"
                                    );
                                    if value_enum_struct_type.common.number
                                        == variant_ref.common().number
                                    {
                                        info!(?value_enum_struct_type, ?variant_ref, "FOUND!");
                                        if let Some(elements) = maybe_elements {
                                            self.push_block_scope();

                                            for element in elements {
                                                if let ResolvedPatternElement::VariableWithFieldIndex(
                                                    var_ref,
                                                    field_index,
                                                ) = element
                                                {
                                                    let value = &values[*field_index];
                                                    info!(?value, "setting match arm variable");
                                                    self.current_block_scopes
                                                        .init_var_ref(var_ref, value);
                                                }
                                            }

                                            let result = self.evaluate_expression(&arm.expression);
                                            self.pop_block_scope();
                                            return result;
                                        }
                                    }
                                }

                                Value::EnumVariantSimple(value_variant_ref) => {
                                    if value_variant_ref.common.number
                                        == variant_ref.common().number
                                        && maybe_elements.is_none()
                                    {
                                        return self.evaluate_expression(&arm.expression);
                                    }
                                }
                                _ => {}
                            }
                        }

                        ResolvedNormalPattern::Literal(lit) => match (lit, &actual_value) {
                            (ResolvedLiteral::IntLiteral(a), Value::Int(b)) if a == b => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (ResolvedLiteral::FloatLiteral(a), Value::Float(b)) if a == b => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (ResolvedLiteral::StringLiteral(a), Value::String(b)) if *a == *b => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (ResolvedLiteral::BoolLiteral(a), Value::Bool(b)) if a == b => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (
                                ResolvedLiteral::TupleLiteral(_a_type_ref, a_values),
                                Value::Tuple(_b_type_ref, b_values),
                            ) if self.expressions_equal_to_values(&a_values, &b_values)? => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            _ => {}
                        },
                    }
                }
            }
        }

        panic!("must match one of the match arms!");
    }

    #[inline(always)]
    const fn modulo(a: i32, b: i32) -> i32 {
        ((a % b) + b) % b
    }

    #[inline(always)]
    const fn modulo_fp(a: Fp, b: Fp) -> Fp {
        let raw = ((a.inner() % b.inner()) + b.inner()) % b.inner();
        Fp::from_raw(raw)
    }

    #[allow(clippy::too_many_lines)]
    fn evaluate_binary_op(
        &self,
        node: &ResolvedNode,
        left_val: Value,
        op: &ResolvedBinaryOperatorKind,
        right_val: Value,
    ) -> Result<Value, ExecuteError> {
        let result: Value = match (&left_val, op, &right_val) {
            // Integer operations
            (Value::Int(a), ResolvedBinaryOperatorKind::Add, Value::Int(b)) => Value::Int(a + b),
            (Value::Int(a), ResolvedBinaryOperatorKind::Subtract, Value::Int(b)) => {
                Value::Int(a - b)
            }
            (Value::Int(a), ResolvedBinaryOperatorKind::Multiply, Value::Int(b)) => {
                Value::Int(a * b)
            }
            (Value::Int(a), ResolvedBinaryOperatorKind::Divide, Value::Int(b)) => {
                if *b == 0 {
                    return Err(self.create_err(ExecuteErrorKind::DivideByZero, node));
                }
                Value::Int(a / b)
            }
            (Value::Int(a), ResolvedBinaryOperatorKind::Modulo, Value::Int(b)) => {
                Value::Int(Self::modulo(*a, *b))
            }
            (Value::Int(a), ResolvedBinaryOperatorKind::Equal, Value::Int(b)) => {
                Value::Bool(a == b)
            }
            (Value::Int(a), ResolvedBinaryOperatorKind::NotEqual, Value::Int(b)) => {
                Value::Bool(a != b)
            }
            (Value::Int(a), ResolvedBinaryOperatorKind::LessThan, Value::Int(b)) => {
                Value::Bool(a < b)
            }
            (Value::Int(a), ResolvedBinaryOperatorKind::GreaterThan, Value::Int(b)) => {
                Value::Bool(a > b)
            }
            (Value::Int(a), ResolvedBinaryOperatorKind::LessEqual, Value::Int(b)) => {
                Value::Bool(a <= b)
            }
            (Value::Int(a), ResolvedBinaryOperatorKind::GreaterEqual, Value::Int(b)) => {
                Value::Bool(a >= b)
            }

            // Float operations
            (Value::Float(a), ResolvedBinaryOperatorKind::Equal, Value::Float(b)) => {
                Value::Bool(a == b)
            }
            (Value::Float(a), ResolvedBinaryOperatorKind::NotEqual, Value::Float(b)) => {
                Value::Bool(a != b)
            }

            (Value::Float(a), ResolvedBinaryOperatorKind::Add, Value::Float(b)) => {
                Value::Float(*a + *b)
            }
            (Value::Float(a), ResolvedBinaryOperatorKind::Subtract, Value::Float(b)) => {
                Value::Float(*a - *b)
            }
            (Value::Float(a), ResolvedBinaryOperatorKind::Multiply, Value::Float(b)) => {
                Value::Float(*a * *b)
            }
            (Value::Float(a), ResolvedBinaryOperatorKind::Divide, Value::Float(b)) => {
                if b.abs().inner() <= 400 {
                    return Err(self.create_err(ExecuteErrorKind::DivideByZero, node));
                }
                Value::Float(*a / *b)
            }
            (Value::Float(a), ResolvedBinaryOperatorKind::Modulo, Value::Float(b)) => {
                Value::Float(Self::modulo_fp(*a, *b))
            }

            (Value::Float(a), ResolvedBinaryOperatorKind::GreaterThan, Value::Float(b)) => {
                Value::Bool(a > b)
            }
            (Value::Float(a), ResolvedBinaryOperatorKind::GreaterEqual, Value::Float(b)) => {
                Value::Bool(a >= b)
            }
            (Value::Float(a), ResolvedBinaryOperatorKind::LessThan, Value::Float(b)) => {
                Value::Bool(a < b)
            }
            (Value::Float(a), ResolvedBinaryOperatorKind::LessEqual, Value::Float(b)) => {
                Value::Bool(a <= b)
            }

            // Boolean operations
            (Value::Bool(a), ResolvedBinaryOperatorKind::LogicalAnd, Value::Bool(b)) => {
                Value::Bool(*a && *b)
            }
            (Value::Bool(a), ResolvedBinaryOperatorKind::LogicalOr, Value::Bool(b)) => {
                Value::Bool(*a || *b)
            }

            // Comparison operations

            // RustType
            (
                Value::RustValue(_, left),
                ResolvedBinaryOperatorKind::Equal,
                Value::RustValue(_, right),
            ) => {
                let left_borrow = left.borrow();
                let right_borrow = right.borrow();
                let equal = left_borrow.eq_dyn(&**right_borrow);
                Value::Bool(equal)
            }
            (
                Value::RustValue(_, left),
                ResolvedBinaryOperatorKind::NotEqual,
                Value::RustValue(_, right),
            ) => {
                let left_borrow = left.borrow();
                let right_borrow = right.borrow();
                let equal = left_borrow.eq_dyn(&**right_borrow);
                Value::Bool(!equal)
            }

            // String operations
            (Value::String(a), ResolvedBinaryOperatorKind::Add, Value::String(b)) => {
                Value::String(a.to_owned() + b)
            }
            (Value::String(a), ResolvedBinaryOperatorKind::Equal, Value::String(b)) => {
                Value::Bool(a == b)
            }

            (Value::String(a), ResolvedBinaryOperatorKind::Add, Value::Int(b)) => {
                Value::String(a.to_owned() + &(*b).to_string())
            }
            (Value::Int(a), ResolvedBinaryOperatorKind::Add, Value::String(b)) => {
                Value::String(a.to_string() + b)
            }

            // Enum
            (
                Value::EnumVariantSimple(a),
                ResolvedBinaryOperatorKind::Equal,
                Value::EnumVariantSimple(b),
            ) => Value::Bool(a == b),
            (
                Value::EnumVariantSimple(a),
                ResolvedBinaryOperatorKind::NotEqual,
                Value::EnumVariantSimple(b),
            ) => Value::Bool(a != b),

            // Bool
            (Value::Bool(a), ResolvedBinaryOperatorKind::Equal, Value::Bool(b)) => {
                Value::Bool(a == b)
            }
            (Value::Bool(a), ResolvedBinaryOperatorKind::NotEqual, Value::Bool(b)) => {
                Value::Bool(a != b)
            }

            (Value::Option(a), ResolvedBinaryOperatorKind::Equal, Value::Option(b)) => {
                Value::Bool(a == b)
            }

            _ => {
                error!(?op, "invalid binary operation!!");
                panic!("invalid binary operation"); // TODO: improve error handling
            }
        };

        Ok(result)
    }

    fn evaluate_unary_op(
        &self,
        node: &ResolvedNode,
        op: &ResolvedUnaryOperatorKind,
        val: Value,
    ) -> Result<Value, ExecuteError> {
        match (op, val) {
            (ResolvedUnaryOperatorKind::Negate, Value::Int(n)) => Ok(Value::Int(-n)),
            (ResolvedUnaryOperatorKind::Negate, Value::Float(n)) => Ok(Value::Float(-n)),
            (ResolvedUnaryOperatorKind::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            _ => Err(self.create_err(ExecuteErrorKind::DivideByZero, node)),
        }
    }

    fn expressions_equal_to_values(
        &mut self,
        p0: &[ResolvedExpression],
        p1: &[ValueRef],
    ) -> Result<bool, ExecuteError> {
        for (a, b_value) in p0.iter().zip(p1.iter()) {
            let a_value = self.evaluate_expression(a)?;

            if a_value != *b_value.borrow() {
                return Ok(false);
            }
        }

        Ok(true)
    }

    #[inline(always)]
    fn apply_compound_operator(
        &self,
        node: &ResolvedNode,
        target: &mut Value,
        operator: &ResolvedCompoundOperatorKind,
        source: &Value,
    ) -> Result<(), ExecuteError> {
        match operator {
            ResolvedCompoundOperatorKind::Mul => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &ResolvedBinaryOperatorKind::Multiply,
                    source.clone(),
                )?;
            }
            ResolvedCompoundOperatorKind::Div => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &ResolvedBinaryOperatorKind::Divide,
                    source.clone(),
                )?;
            }
            ResolvedCompoundOperatorKind::Add => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &ResolvedBinaryOperatorKind::Add,
                    source.clone(),
                )?;
            }
            ResolvedCompoundOperatorKind::Sub => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &ResolvedBinaryOperatorKind::Subtract,
                    source.clone(),
                )?;
            }
            ResolvedCompoundOperatorKind::Modulo => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &ResolvedBinaryOperatorKind::Modulo,
                    source.clone(),
                )?;
            }
        }
        Ok(())
    }

    /*
        fn evaluate_range(
            &mut self,
            min_expr: &ResolvedExpression,
            max_expr: &ResolvedExpression,
        ) -> Result<(i32, i32), ExecuteError> {
            let min_value = self.evaluate_expression_int(&min_expr)?;
            let max_value = self.evaluate_expression_int(&max_expr)?;

            Ok((min_value, max_value))
        }

        fn calculate_range(
            start_val: i32,
            end_val: i32,
            len: usize,
            mode: &ResolvedRangeMode,
        ) -> (usize, usize) {
            let adjusted_min = if start_val < 0 {
                len + start_val as usize
            } else {
                start_val as usize
            };

            let mut adjusted_max = if end_val < 0 {
                len + end_val as usize
            } else {
                end_val as usize
            };
            if ResolvedRangeMode::Inclusive == mode.clone() {
                adjusted_max += 1;
            }

            (adjusted_min, adjusted_max)
        }

        fn evaluate_and_calculate_range(
            &mut self,
            min_expr: &ResolvedExpression,
            max_expr: &ResolvedExpression,
            mode: &ResolvedRangeMode,
            len: usize,
        ) -> Result<(usize, usize), ExecuteError> {
            let (start_val, end_val) = self.evaluate_range(min_expr, max_expr)?;

            Ok(Self::calculate_range(start_val, end_val, len, mode))
        }

        #[inline]
        fn evaluate_array_range_access(
            &mut self,
            base_expr: &ResolvedExpression,
            array_type_ref: &ResolvedArrayTypeRef,
            min_expr: &ResolvedExpression,
            max_expr: &ResolvedExpression,
            mode: &ResolvedRangeMode,
        ) -> Result<Value, ExecuteError> {
            let array_value = self.evaluate_expression(base_expr)?;

            if let Value::Array(_, values) = array_value {
                let (adjusted_start, adjusted_end) =
                    self.evaluate_and_calculate_range(min_expr, max_expr, mode, values.len())?;

                let slice = &values.as_slice()[adjusted_start..adjusted_end];
                Ok(Value::Array(array_type_ref.clone(), Vec::from(slice)))
            } else {
                Err(self.create_err(ExecuteErrorKind::NotAnArray, &base_expr.node))
            }
        }

        fn evaluate_expression_int(
            &mut self,
            int_expr: &ResolvedExpression,
        ) -> Result<i32, ExecuteError> {
            let v = self.evaluate_expression(&int_expr)?;

            if let Value::Int(i) = v {
                Ok(i)
            } else {
                Err(self.create_err(ExecuteErrorKind::ExpectedInt, &int_expr.node))
            }
        }
    */

    /*
    fn evaluate_string_range_access(
        &mut self,
        string_expr: &ResolvedExpression,
        start_expr: &ResolvedExpression,
        end_expr: &ResolvedExpression,
        mode: &ResolvedRangeMode,
    ) -> Result<Value, ExecuteError> {
        let string_value = self.evaluate_expression(string_expr)?;

        if let Value::String(string) = string_value {
            let (adjusted_start, adjusted_end) =
                self.evaluate_and_calculate_range(start_expr, end_expr, mode, string.len())?;
            Ok(Value::String(
                string[adjusted_start..adjusted_end].to_string(),
            ))
        } else {
            Err(self.create_err(ExecuteErrorKind::ExpectedString, &string_expr.node))
        }
    }

     */

    fn create_err(&self, kind: ExecuteErrorKind, node: &ResolvedNode) -> ExecuteError {
        ExecuteError {
            node: node.clone(),
            kind,
        }
    }
}

#[inline]
#[must_use]
pub fn i64_sqrt(v: i64) -> i64 {
    assert!(v >= 0, "negative numbers are undefined for sqrt() {v}");

    if v == 0 {
        return v;
    }

    const MAX_ITERATIONS: usize = 40;
    const TOLERANCE: i64 = 2;

    let mut guess = v / 2;

    for _ in 0..MAX_ITERATIONS {
        let next_guess = (guess + v / guess) / 2;

        // Check if the change is within the tolerance level
        if (next_guess - guess).abs() <= TOLERANCE {
            return next_guess;
        }

        guess = next_guess;
    }

    guess // Return the last guess if convergence wasn't fully reached
}

#[allow(unused)]
pub fn values_to_value_refs(values: &[Value]) -> Vec<ValueRef> {
    let mut items = Vec::new();

    for x in values.iter().cloned() {
        items.push(Rc::new(RefCell::new(x)));
    }

    items
}

pub fn values_to_value_refs_owned(values: Vec<Value>) -> Vec<ValueRef> {
    values
        .into_iter()
        .map(|x| Rc::new(RefCell::new(x)))
        .collect()
}

pub fn wrap_in_option(maybe: Option<&ValueRef>) -> ValueRef {
    match maybe {
        None => Rc::new(RefCell::new(Value::Option(None))),
        Some(x) => Rc::new(RefCell::new(Value::Option(Some(x.clone())))),
    }
}
