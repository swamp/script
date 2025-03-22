/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::block::BlockScopes;
use crate::err::RuntimeErrorKind;
use crate::prelude::RuntimeError;
use crate::prelude::{ValueReference, VariableValue};
use seq_map::SeqMap;
use std::fmt::Debug;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use swamp_script_core_extra::extra::{SparseValueId, SparseValueMap};
use swamp_script_core_extra::prelude::ValueError;
use swamp_script_core_extra::value::ValueRef;
use swamp_script_core_extra::value::{
    SourceMapLookup, Value, convert_vec_to_rc_refcell, format_value, to_rust_value,
};
use swamp_script_node::Node;
use swamp_script_semantic::prelude::*;
use swamp_script_semantic::{ArgumentExpressionOrLocation, LocationAccess, LocationAccessKind};
use swamp_script_semantic::{
    BinaryOperatorKind, CompoundOperatorKind, ConstantId, ForPattern, Function,
    MutOrImmutableExpression, NormalPattern, PatternElement, PostfixKind, SingleLocationExpression,
    SingleLocationExpressionKind, UnaryOperatorKind,
};
use swamp_script_semantic::{ExternalFunctionId, Postfix, SingleMutLocationExpression};
use swamp_script_types::{EnumVariantType, TypeForParameter, same_anon_struct_ref};
use tracing::{error, info};
pub mod err;

mod block;
pub mod prelude;
pub mod value_both;
pub mod value_ref;

impl From<ValueError> for RuntimeError {
    fn from(value: ValueError) -> Self {
        Self {
            kind: RuntimeErrorKind::ValueError(value),
            node: Default::default(),
        }
    }
}

type RawFunctionFn<C> = dyn FnMut(&[VariableValue], &mut C) -> Result<Value, RuntimeError>;

type FunctionFn<C> = Box<RawFunctionFn<C>>;

#[derive(Debug)]
pub enum FunctionData {
    Internal(InternalFunctionDefinitionRef),
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
        handler: impl FnMut(&[VariableValue], &mut C) -> Result<Value, RuntimeError> + 'static,
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
    root_expression: &Expression,
    debug_source_map: Option<&dyn SourceMapLookup>,
    context: &mut C,
) -> Result<Value, RuntimeError> {
    let mut interpreter = Interpreter::<C>::new(externals, constants, context);
    interpreter.debug_source_map = debug_source_map;
    let value_with_signal = interpreter.evaluate_expression_with_signal(root_expression)?;
    value_with_signal.try_into().map_err(|_| RuntimeError {
        node: Node::default(),
        kind: RuntimeErrorKind::CouldNotConvertFromSignal,
    })
}

pub fn eval_constants<C>(
    externals: &ExternalFunctions<C>,
    eval_constants: &mut Constants,
    program_state: &ProgramState,
    context: &mut C,
) -> Result<(), RuntimeError> {
    for constant in &program_state.constants_in_dependency_order {
        let mut interpreter = Interpreter::<C>::new(externals, eval_constants, context);
        let value = interpreter.evaluate_expression(&constant.expr)?;
        eval_constants.set(constant.id, value);
    }

    Ok(())
}

pub fn util_execute_function<C>(
    externals: &ExternalFunctions<C>,
    constants: &Constants,
    func: &InternalFunctionDefinitionRef,
    arguments: &[VariableValue],
    context: &mut C,
    debug_source_map: Option<&dyn SourceMapLookup>,
) -> Result<Value, RuntimeError> {
    let mut interpreter = Interpreter::<C>::new(externals, constants, context);
    interpreter.debug_source_map = debug_source_map;
    interpreter.bind_parameters(&func.body.node, &func.signature.parameters, &arguments)?;
    let value = interpreter.evaluate_expression(&func.body)?;
    interpreter.current_block_scopes.clear();
    interpreter.function_scope_stack.clear();
    Ok(value)
}

pub fn util_execute_expression<C>(
    externals: &ExternalFunctions<C>,
    constants: &Constants,
    expr: &Expression,
    context: &mut C,
    debug_source_map: Option<&dyn SourceMapLookup>,
) -> Result<Value, RuntimeError> {
    let mut interpreter = Interpreter::<C>::new(externals, constants, context);
    interpreter.debug_source_map = debug_source_map;
    let value = interpreter.evaluate_expression(expr)?;
    interpreter.current_block_scopes.clear();
    interpreter.function_scope_stack.clear();
    Ok(value)
}

pub fn util_execute_member_function_mut<C>(
    externals: &ExternalFunctions<C>,
    constants: &Constants,
    fn_def: &InternalFunctionDefinitionRef,
    self_value_ref: ValueRef,
    arguments: &[Value],
    context: &mut C,
    debug_source_map: Option<&dyn SourceMapLookup>,
) -> Result<Value, RuntimeError> {
    let mut complete_arguments = Vec::new();
    complete_arguments.push(VariableValue::Reference(self_value_ref));
    for arg in arguments {
        complete_arguments.push(VariableValue::Value(arg.clone()));
    }

    let value = util_execute_function(
        externals,
        constants,
        fn_def,
        &complete_arguments,
        context,
        debug_source_map,
    )?;

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
        debug_assert_ne!(self.function_scope_stack.len(), 1, "you popped too far");
        let last_one = self.function_scope_stack.pop().expect("pop function scope");
        self.current_block_scopes = last_one.saved_block_scope;
    }

    fn bind_parameters(
        &mut self,
        node: &Node,
        params: &[TypeForParameter],
        args: &[VariableValue],
    ) -> Result<(), RuntimeError> {
        for (index, (param, arg)) in params.iter().zip(args).enumerate() {
            let complete_value = if param.is_mutable {
                match arg {
                    VariableValue::Reference(_r) => {
                        // For mutable parameters, use the SAME reference
                        arg.clone()
                    }
                    _ => return Err(self.create_err(RuntimeErrorKind::ArgumentIsNotMutable, node)),
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
        function_expression: &Expression,
        arguments: &[ArgumentExpressionOrLocation],
    ) -> Result<Value, RuntimeError> {
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
                        RuntimeErrorKind::MissingExternalFunction(*external_function_id),
                        &function_expression.node,
                    ))?
                    .borrow_mut();

                (func.func)(&evaluated_args, self.context)
            }
            _ => Err(self.create_err(
                RuntimeErrorKind::ExpectedFunction,
                &function_expression.node,
            )),
        }
    }

    fn evaluate_location_chain(
        &mut self,
        node: &Node,
        start_value_reference: ValueRef,
        chain_items: &Vec<LocationAccess>,
    ) -> Result<ValueRef, RuntimeError> {
        let mut value_ref = start_value_reference;
        for chain in chain_items {
            value_ref = {
                match &chain.kind {
                    LocationAccessKind::FieldIndex(_resolved_node, index) => {
                        let borrowed = value_ref.borrow();

                        let (_struct_ref, fields) = borrowed
                            .expect_anon_struct()
                            .map_err(|_| self.create_err(RuntimeErrorKind::ExpectedStruct, node))?;
                        fields[*index].clone()
                    }
                    LocationAccessKind::IntrinsicCallMut(intrinsic_fn, arguments) => self
                        .eval_intrinsic_postfix_mut_return(
                            node,
                            &value_ref,
                            intrinsic_fn,
                            arguments,
                        )?,
                }
            };
        }

        Ok(value_ref)
    }

    fn evaluate_location(
        &mut self,
        found_location_expr: &SingleLocationExpression,
    ) -> Result<ValueRef, RuntimeError> {
        let variable_ref = self
            .current_block_scopes
            .lookup_variable_mut_ref(&found_location_expr.starting_variable)?;

        let value_ref = self.evaluate_location_chain(
            &found_location_expr.node,
            variable_ref.clone(),
            &found_location_expr.access_chain,
        )?;

        let converted_value_ref = match &found_location_expr.kind {
            SingleLocationExpressionKind::MutVariableRef => value_ref,
            SingleLocationExpressionKind::MutStructFieldRef(_base_expression, _resolved_access) => {
                value_ref
            }
        };

        Ok(converted_value_ref)
    }

    fn evaluate_mut_or_immutable_expression(
        &mut self,
        expr: &MutOrImmutableExpression,
    ) -> Result<VariableValue, RuntimeError> {
        let var_value = match &expr.expression_or_location {
            ArgumentExpressionOrLocation::Location(loc) => {
                VariableValue::Reference(self.evaluate_location(loc)?)
            }
            ArgumentExpressionOrLocation::Expression(expr) => {
                VariableValue::Value(self.evaluate_expression(expr)?)
            }
        };
        Ok(var_value)
    }

    fn evaluate_argument(
        &mut self,
        expr: &ArgumentExpressionOrLocation,
    ) -> Result<VariableValue, RuntimeError> {
        let var_value = match expr {
            ArgumentExpressionOrLocation::Location(mutable_location) => {
                VariableValue::Reference(self.evaluate_location(mutable_location)?)
            }
            ArgumentExpressionOrLocation::Expression(expr) => {
                let value = self.evaluate_expression(expr)?;
                VariableValue::Value(value)
            }
        };

        Ok(var_value)
    }

    fn evaluate_args(
        &mut self,
        args: &[ArgumentExpressionOrLocation],
    ) -> Result<Vec<VariableValue>, RuntimeError> {
        let mut evaluated = Vec::with_capacity(args.len());

        for argument_expression in args {
            let mem_value = self.evaluate_argument(argument_expression)?;
            evaluated.push(mem_value);
        }

        Ok(evaluated)
    }

    fn evaluate_expressions(&mut self, exprs: &[Expression]) -> Result<Vec<Value>, RuntimeError> {
        let mut values = vec![];
        for expr in exprs {
            let value = self.evaluate_expression(expr)?;
            values.push(value);
        }

        Ok(values)
    }

    fn evaluate_while_loop(
        &mut self,
        condition: &BooleanExpression,
        body: &Expression,
    ) -> Result<ValueWithSignal, RuntimeError> {
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
        expressions: &Vec<Expression>,
    ) -> Result<ValueWithSignal, RuntimeError> {
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
        pattern: &ForPattern,
        iterator_expr: &Iterable,
        body: &Box<Expression>,
    ) -> Result<ValueWithSignal, RuntimeError> {
        let mut result = Value::Unit;

        let iterator_value_mem =
            self.evaluate_mut_or_immutable_expression(&iterator_expr.resolved_expression)?;
        let iterator_value = match iterator_value_mem {
            VariableValue::Value(_) => {
                return Err(self.create_err(RuntimeErrorKind::ArgumentIsNotMutable, &body.node));
            }
            VariableValue::Reference(value_ref) => value_ref,
        };

        match pattern {
            ForPattern::Single(var_ref) => {
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

            ForPattern::Pair(first_ref, second_ref) => {
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
        pattern: &ForPattern,
        iterator_expr: &Iterable,
        body: &Box<Expression>,
    ) -> Result<ValueWithSignal, RuntimeError> {
        let mut result = Value::Unit;

        let iterator_value =
            self.evaluate_mut_or_immutable_expression(&iterator_expr.resolved_expression)?;

        match pattern {
            ForPattern::Single(var_ref) => {
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

            ForPattern::Pair(first_ref, second_ref) => {
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

    #[allow(unused)]
    fn debug_expr(&self, expr: &Expression) {
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
        expr: &Expression,
    ) -> Result<ValueWithSignal, RuntimeError> {
        match &expr.kind {
            ExpressionKind::WhileLoop(condition, body) => self.evaluate_while_loop(condition, body),

            ExpressionKind::ForLoop(pattern, iterator_expr, body) => {
                if pattern.is_mutable() {
                    self.evaluate_for_loop_mutable(pattern, iterator_expr, body)
                } else {
                    self.evaluate_for_loop(pattern, iterator_expr, body)
                }
            }

            ExpressionKind::If(condition, consequences, optional_alternative) => {
                let cond_value = self.evaluate_expression(&condition.expression)?;
                if cond_value.is_truthy()? {
                    self.evaluate_expression_with_signal(consequences)
                } else if let Some(alternative) = optional_alternative {
                    self.evaluate_expression_with_signal(alternative)
                } else {
                    Ok(ValueWithSignal::Value(Value::Unit))
                }
            }

            ExpressionKind::Block(expressions) => self.evaluate_block(expressions),

            ExpressionKind::When(bindings, true_block, maybe_else_block) => {
                let mut all_are_some = true;
                let mut all_expressions = Vec::new();
                for binding in bindings {
                    let source = self.evaluate_mut_or_immutable_expression(&binding.expr)?;
                    match source.to_value() {
                        Value::Option(boxed_val) => {
                            match boxed_val {
                                Some(found_val) => {
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
                                }
                                _ => {
                                    all_are_some = false;
                                    //warn!(?source, "Not ALL ARE SOME!");
                                    break;
                                }
                            }
                        }
                        _ => {
                            return Err(self
                                .create_err(RuntimeErrorKind::ExpectedOptional, &true_block.node));
                        }
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
    fn evaluate_expression(&mut self, expr: &Expression) -> Result<Value, RuntimeError> {
        self.depth += 1;
        let value = match &expr.kind {
            // Illegal in this context
            ExpressionKind::WhileLoop(_condition, _body) => {
                panic!("should have been handled earlier")
            }

            ExpressionKind::ForLoop(_pattern, _iterator_expr, _body) => {
                panic!("should have been handled earlier")
            }

            // Constructing
            ExpressionKind::Literal(lit) => self.evaluate_literal(&expr.node, lit)?,

            ExpressionKind::StructInstantiation(struct_instantiation) => {
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

                Value::NamedStruct(
                    struct_instantiation.struct_type_ref.clone(),
                    convert_vec_to_rc_refcell(field_values),
                )
            }

            ExpressionKind::AnonymousStructLiteral(struct_instantiation) => {
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

                Value::AnonymousStruct(
                    struct_instantiation.anonymous_struct_type.clone(),
                    convert_vec_to_rc_refcell(field_values),
                )
            }

            ExpressionKind::Range(start, end, range_mode) => {
                let start_val = self.evaluate_expression(start)?;
                let end_val = self.evaluate_expression(end)?;
                match (start_val, end_val) {
                    (Value::Int(s), Value::Int(e)) => {
                        Value::Range(Box::new(s), Box::new(e), range_mode.clone())
                    }
                    _ => Err(self.create_err(RuntimeErrorKind::RangeItemMustBeInt, &expr.node))?,
                }
            }

            // ==================== ASSIGNMENT ====================
            ExpressionKind::VariableDefinition(target_var, source_expr) => {
                let source_value_or_reference =
                    self.evaluate_mut_or_immutable_expression(source_expr)?;

                self.current_block_scopes
                    .initialize_var_mem(target_var, source_value_or_reference.clone())?;

                source_value_or_reference.to_value().clone()
            }

            ExpressionKind::VariableReassignment(variable_ref, source_expr) => {
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

            /*
            ExpressionKind::IntrinsicCallMut(intrinsic, location, arguments) => {
               self.evaluate_intrinsic_mut(&expr.node, intrinsic, location, arguments)?
            }

             */
            // ------------- LOOKUP ---------------------
            ExpressionKind::ConstantAccess(constant) => {
                self.constants.lookup_constant_value(constant.id).clone()
            }

            ExpressionKind::Assignment(mut_location_expr, source_expr) => {
                let value_ref = self.evaluate_location(&mut_location_expr.0)?;
                let source_value = self.evaluate_expression(source_expr)?;

                *value_ref.borrow_mut() = source_value;

                Value::Unit
            }

            ExpressionKind::CompoundAssignment(mut_location_expr, op, source_expr) => {
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
            ExpressionKind::BinaryOp(binary_operator) => {
                let left_val = self.evaluate_expression(&binary_operator.left)?;
                let right_val = self.evaluate_expression(&binary_operator.right)?;
                self.evaluate_binary_op(&expr.node, left_val, &binary_operator.kind, right_val)?
            }

            ExpressionKind::UnaryOp(unary_operator) => {
                let left_val = self.evaluate_expression(&unary_operator.left)?;
                self.evaluate_unary_op(&expr.node, &unary_operator.kind, left_val)?
            }

            // Calling
            ExpressionKind::FunctionValueCall(_signature, expr, arguments) => {
                self.evaluate_function_call(expr, arguments)?
            }

            ExpressionKind::IntrinsicCallEx(intrinsic, arguments) => {
                self.eval_intrinsic(&expr.node, intrinsic, arguments)?
            }

            ExpressionKind::Block(statements) => {
                self.evaluate_block(statements)?.try_into().unwrap() // TODO: Error handling
            }

            ExpressionKind::InterpolatedString(parts) => {
                let mut result = String::new();

                for part in parts {
                    match part {
                        StringPart::Literal(_resolved_node, text) => {
                            result.push_str(text);
                        }
                        StringPart::Interpolation(expr, format_spec) => {
                            let value = self.evaluate_expression(expr)?;
                            let formatted = format_spec.as_ref().map_or_else(
                                || value.convert_to_string_if_needed(),
                                |spec| format_value(&value, &spec.kind).unwrap(),
                            );
                            result.push_str(&formatted);
                        }
                    }
                }

                Value::String(result)
            }

            ExpressionKind::Match(resolved_match) => self.eval_match(resolved_match)?,
            ExpressionKind::Guard(guards) => self.eval_guard(&expr.node, guards)?,

            ExpressionKind::InternalFunctionAccess(fetch_function) => {
                Value::InternalFunction(fetch_function.clone())
            }

            ExpressionKind::ExternalFunctionAccess(fetch_function) => {
                self.externals
                    .external_functions_by_id
                    .get(&fetch_function.id)
                    .expect("should have external function ref");
                Value::ExternalFunction(fetch_function.clone())
            }

            ExpressionKind::Option(inner) => match inner {
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
            ExpressionKind::CoerceOptionToBool(expression) => {
                let value = self.evaluate_expression(expression)?;
                match value {
                    Value::Option(inner) => Value::Bool(inner.is_some()),
                    _ => {
                        return Err(
                            self.create_err(RuntimeErrorKind::CoerceOptionToBoolFailed, &expr.node)
                        );
                    }
                }
            }

            ExpressionKind::If(condition, consequences, optional_alternative) => {
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

            ExpressionKind::When(bindings, true_block, maybe_else_block) => {
                let mut all_are_some = true;
                let mut all_expressions = Vec::new();
                for binding in bindings {
                    let source = self.evaluate_mut_or_immutable_expression(&binding.expr)?;
                    match source.to_value() {
                        Value::Option(boxed_val) => match boxed_val {
                            Some(found_val) => {
                                all_expressions.push(found_val.borrow().clone());
                            }
                            _ => {
                                all_are_some = false;
                                break;
                            }
                        },
                        _ => {
                            return Err(self
                                .create_err(RuntimeErrorKind::ExpectedOptional, &true_block.node));
                        }
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

            ExpressionKind::TupleDestructuring(variable_refs, _, expr) => {
                let value = self.evaluate_expression(expr)?;
                if let Value::Tuple(_tuple_ref, values) = value {
                    if variable_refs.len() > values.len() {
                        return Err(self.create_err(RuntimeErrorKind::NotAnArray, &expr.node));
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
            ExpressionKind::VariableAccess(variable_ref) => {
                let temp = self.current_block_scopes.lookup_var_value(variable_ref);
                assert_ne!(temp, Value::Unit);
                temp
            }
            ExpressionKind::PostfixChain(start, parts) => {
                let value_ref = self.eval_chain(&expr.node, start, parts)?;
                let x = value_ref.borrow().clone();
                x
            }
            ExpressionKind::IntrinsicFunctionAccess(_) => panic!(
                "Intrinsic Function Access should have been converted to IntrinsicFunctionCalls before eval"
            ),
        };

        self.depth -= 1;

        //self.debug_expr(expr);
        //eprintln!("{value:?} resulted in value");
        Ok(value)
    }

    fn evaluate_literal(&mut self, node: &Node, lit: &Literal) -> Result<Value, RuntimeError> {
        let v = match lit {
            Literal::IntLiteral(n) => Value::Int(*n),
            Literal::FloatLiteral(f) => Value::Float(*f),
            Literal::StringLiteral(s) => Value::String(s.clone()),
            Literal::BoolLiteral(b) => Value::Bool(*b),

            Literal::EnumVariantLiteral(enum_type, enum_variant_type, data) => {
                let variant_container_value: Value = match enum_variant_type {
                    EnumVariantType::Tuple(tuple_type) => match data {
                        EnumLiteralData::Tuple(tuple_expressions) => {
                            let eval_expressions = self.evaluate_expressions(tuple_expressions)?;
                            let value_refs = values_to_value_refs_owned(eval_expressions);
                            Value::EnumVariantTuple(
                                enum_type.clone(),
                                tuple_type.clone(),
                                value_refs,
                            )
                        }
                        _ => panic!("wrong container type {data:?}"),
                    },

                    EnumVariantType::Struct(struct_type_ref) => match data {
                        EnumLiteralData::Struct(source_order_field_values) => {
                            let mut field_values =
                                Vec::with_capacity(source_order_field_values.len());
                            field_values
                                .resize_with(source_order_field_values.len(), Default::default);
                            for (index, resolved_expression) in source_order_field_values {
                                let value = self.evaluate_expression(resolved_expression)?;
                                field_values[*index] = Rc::new(RefCell::new(value));
                            }
                            Value::EnumVariantStruct(
                                enum_type.clone(),
                                struct_type_ref.clone(),
                                field_values,
                            )
                        }
                        _ => panic!("wrong container type"),
                    },

                    EnumVariantType::Nothing(data) => {
                        Value::EnumVariantSimple(enum_type.clone(), data.clone())
                    }
                };
                variant_container_value
            }

            Literal::TupleLiteral(tuple_type, resolved_expressions) => {
                let values = self.evaluate_expressions(resolved_expressions)?;
                Value::Tuple(tuple_type.clone(), convert_vec_to_rc_refcell(values))
            }

            Literal::Slice(element_type, expressions) => {
                let values = self.evaluate_expressions(expressions)?;
                Value::Slice(element_type.clone(), convert_vec_to_rc_refcell(values))
            }

            Literal::SlicePair(slice_pair_type, expressions) => {
                let mut items = SeqMap::new();
                for (key, value) in expressions {
                    let key_val = self.evaluate_expression(key)?;
                    let value_val = self.evaluate_expression(value)?;
                    items
                        .insert(key_val, Rc::new(RefCell::new(value_val)))
                        .map_err(|_err| {
                            self.create_err(
                                RuntimeErrorKind::NonUniqueKeysInMapLiteralDetected,
                                node,
                            )
                        })?;
                }
                Value::SlicePair(slice_pair_type.clone(), items)
            }

            Literal::NoneLiteral => Value::Option(None),
        };
        Ok(v)
    }

    #[allow(clippy::too_many_lines)]
    fn eval_intrinsic_postfix_mut_return(
        &mut self,
        node: &Node,
        value_ref: &ValueRef,
        //        resolved_postfix: &Postfix,
        intrinsic_function: &IntrinsicFunction,
        arguments: &[Expression],
    ) -> Result<ValueRef, RuntimeError> {
        //let node = &resolved_postfix.node;
        let val = match &intrinsic_function {
            IntrinsicFunction::VecSubscriptMut => match &mut *value_ref.borrow_mut() {
                Value::Vec(_type_id, vector) => {
                    let index = self.evaluate_expression(&arguments[0])?;
                    let index_int = index.expect_int()?;
                    vector[index_int as usize].clone()
                }
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::OperationRequiresArray, node))?;
                }
            },
            IntrinsicFunction::MapSubscriptMut => match &mut *value_ref.borrow_mut() {
                Value::Map(_type_id, seq_map) => {
                    let key_value = self.evaluate_expression(&arguments[0])?;
                    let maybe_value = seq_map.get_mut(&key_value);
                    maybe_value.unwrap().clone()
                }
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::OperationRequiresArray, node))?;
                }
            },

            IntrinsicFunction::MapSubscriptMutCreateIfNeeded => {
                match &mut *value_ref.borrow_mut() {
                    Value::Map(_type_id, seq_map) => {
                        let key_value = self.evaluate_expression(&arguments[0])?;
                        let maybe_value = seq_map.get_mut(&key_value);
                        if let Some(ref found) = maybe_value {
                            maybe_value.unwrap().clone()
                        } else {
                            let empty_value = Rc::new(RefCell::new(Value::Int(0)));
                            seq_map.insert(key_value, empty_value.clone()).unwrap();
                            empty_value.clone()
                        }
                    }
                    _ => {
                        return Err(
                            self.create_err(RuntimeErrorKind::OperationRequiresArray, node)
                        )?;
                    }
                }
            }
            _ => panic!(
                "{}",
                format!("missing intrinsic {intrinsic_function:?} that returns mut. please add it")
            ),
        };

        Ok(val)
    }

    #[allow(clippy::too_many_lines)]
    fn eval_intrinsic(
        &mut self,
        node: &Node,
        intrinsic_function: &IntrinsicFunction,
        arguments: &[ArgumentExpressionOrLocation],
    ) -> Result<Value, RuntimeError> {
        let self_value = self.evaluate_argument(&arguments[0])?;

        let mut expressions = Vec::new();
        for arg in &arguments[1..] {
            match arg {
                ArgumentExpressionOrLocation::Location(_loc) => panic!("not supported"),
                ArgumentExpressionOrLocation::Expression(expr) => expressions.push(expr),
            }
        }

        self.eval_intrinsic_internal(
            node,
            intrinsic_function,
            self_value.to_value_ref(),
            &expressions,
        )
    }

    #[allow(clippy::too_many_lines)]
    fn eval_intrinsic_internal(
        &mut self,
        node: &Node,
        intrinsic_function: &IntrinsicFunction,
        value_ref: ValueRef,
        arguments: &[&Expression],
    ) -> Result<Value, RuntimeError> {
        let val = match &intrinsic_function {
            IntrinsicFunction::VecRemoveIndex => {
                let index_val = self.evaluate_expression(&arguments[0])?;
                let Value::Int(index) = index_val else {
                    return Err(self.create_err(RuntimeErrorKind::ArgumentIsNotMutable, node));
                };

                match &mut *value_ref.borrow_mut() {
                    Value::Vec(_type_id, vector) => {
                        vector.remove(index as usize);
                    }
                    _ => {
                        Err(self.create_err(RuntimeErrorKind::OperationRequiresArray, node))?;
                    }
                }

                Value::Unit
            }

            IntrinsicFunction::VecClear => {
                match &mut *value_ref.borrow_mut() {
                    Value::Vec(_type_id, vector) => {
                        vector.clear();
                    }
                    _ => {
                        Err(self.create_err(RuntimeErrorKind::OperationRequiresArray, node))?;
                    }
                }
                Value::Unit
            }

            IntrinsicFunction::VecFromSlice => {
                let (slice_type, values) = value_ref.borrow().expect_slice()?;
                Value::Vec(slice_type, values)
            }

            IntrinsicFunction::VecPush => {
                match &mut *value_ref.borrow_mut() {
                    Value::Vec(_type_id, vector) => {
                        let value_to_add = self.evaluate_expression(&arguments[0])?;
                        vector.push(Rc::new(RefCell::new(value_to_add)));
                    }
                    _ => {
                        Err(self.create_err(RuntimeErrorKind::OperationRequiresArray, node))?;
                    }
                }
                Value::Unit
            }

            IntrinsicFunction::VecSubscript => match &mut *value_ref.borrow_mut() {
                Value::Vec(_type_id, vector) => {
                    let index = self.evaluate_expression(&arguments[0])?;
                    let index_int = index.expect_int()?;
                    let maybe_value = vector.get(index_int as usize);
                    if let Some(found_value) = maybe_value {
                        found_value.borrow().clone()
                    } else {
                        return Err(
                            self.create_err(RuntimeErrorKind::VecSubscriptNonExisting, node)
                        );
                    }
                }
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::OperationRequiresArray, node))?;
                }
            },

            IntrinsicFunction::VecLen => match &mut *value_ref.borrow_mut() {
                Value::Vec(_type_id, vector) => {
                    let length = vector.len();
                    Value::Int(length as i32)
                }
                _ => Err(self.create_err(RuntimeErrorKind::OperationRequiresArray, node))?,
            },

            IntrinsicFunction::VecIsEmpty => match &mut *value_ref.borrow_mut() {
                Value::Vec(_type_id, vector) => Value::Bool(vector.len() == 0),
                _ => Err(self.create_err(RuntimeErrorKind::OperationRequiresArray, node))?,
            },

            IntrinsicFunction::VecPop => match &mut *value_ref.borrow_mut() {
                Value::Vec(_type_id, vector) => {
                    let maybe_val = vector.pop();
                    if let Some(found_value) = maybe_val {
                        found_value.borrow().clone()
                    } else {
                        return Err(self.create_err(RuntimeErrorKind::StackCouldNotBePopped, node));
                    }
                }
                _ => Err(self.create_err(RuntimeErrorKind::OperationRequiresArray, node))?,
            },

            IntrinsicFunction::MapFromSlicePair => {
                let borrow = value_ref.borrow();
                let (slice_pair_type, seq_map) = borrow.expect_slice_pair()?;
                Value::Map(slice_pair_type, seq_map.clone())
            }

            IntrinsicFunction::MapHas => {
                let index_val = self.evaluate_expression(&arguments[0])?;

                match value_ref.borrow().clone() {
                    Value::Map(_key_type, ref seq_map) => {
                        let has_key = seq_map.contains_key(&index_val);
                        Value::Bool(has_key)
                    }
                    _ => {
                        return Err(self.create_err(RuntimeErrorKind::NotAMap, node));
                    }
                }
            }

            IntrinsicFunction::MapLen => match value_ref.borrow().clone() {
                Value::Map(_key_type, ref seq_map) => Value::Int(seq_map.len() as i32),
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::NotAMap, node));
                }
            },

            IntrinsicFunction::MapIsEmpty => match &mut *value_ref.borrow_mut() {
                Value::Vec(_type_id, seq_map) => Value::Bool(seq_map.len() == 0),
                _ => Err(self.create_err(RuntimeErrorKind::OperationRequiresArray, node))?,
            },

            IntrinsicFunction::MapSubscript => match value_ref.borrow().clone() {
                Value::Map(_type_id, seq_map) => {
                    let key_value = self.evaluate_expression(&arguments[0])?;
                    let maybe_value = seq_map.get(&key_value);
                    if let Some(found_value) = maybe_value {
                        found_value.borrow().clone()
                    } else {
                        return Err(self.create_err(RuntimeErrorKind::MapKeyNonExisting, node));
                    }
                }
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::OperationRequiresArray, node))?;
                }
            },

            IntrinsicFunction::MapRemove => {
                let index_val = self.evaluate_expression(&arguments[0])?;

                let result = {
                    let mut borrowed = value_ref.borrow_mut();
                    match &mut *borrowed {
                        Value::Map(_key_type, seq_map) => {
                            seq_map.remove(&index_val);
                            /*
                            let x =
                            x.map_or_else(
                                || Value::Option(None),
                                |v| Value::Option(Some(v.clone())),
                            )

                             */
                            Value::Unit
                        }
                        _ => {
                            return Err(self.create_err(RuntimeErrorKind::NotAMap, node));
                        }
                    }
                };
                result
            }

            IntrinsicFunction::SparseAdd => {
                let mut borrowed = value_ref.borrow_mut();

                match &mut *borrowed {
                    Value::Sparse(_type, found) => {
                        let resolved_value = self.evaluate_expression(&arguments[0])?;
                        let id_value = found.add(resolved_value);

                        id_value
                    }
                    _ => {
                        return Err(self.create_err(RuntimeErrorKind::NotSparseValue, node));
                    }
                }
            }

            IntrinsicFunction::SparseRemove => {
                let mut borrowed = value_ref.borrow_mut();

                match &mut *borrowed {
                    Value::Sparse(_type, found) => {
                        let id_value = self.evaluate_expression(&arguments[0])?;
                        match id_value.downcast_rust::<SparseValueId>() {
                            Some(found_id) => {
                                found.remove(&found_id.borrow());
                            }
                            _ => {
                                return Err(self.create_err(RuntimeErrorKind::NotSparseValue, node));
                            }
                        }
                    }
                    _ => {
                        return Err(self.create_err(RuntimeErrorKind::NotSparseValue, node));
                    }
                }

                Value::Unit
            }
            IntrinsicFunction::SparseSubscript => {
                let borrowed = value_ref.borrow();

                match &*borrowed {
                    Value::Sparse(_type, found) => {
                        let id_value = self.evaluate_expression(&arguments[0])?; // id
                        match id_value.downcast_rust::<SparseValueId>() {
                            Some(found_id) => match found.get(&found_id.borrow()) {
                                Some(found_value) => Value::Option(Some(found_value.clone())),
                                _ => Value::Option(None),
                            },
                            _ => {
                                return Err(self.create_err(RuntimeErrorKind::NotSparseId, node));
                            }
                        }
                    }
                    _ => {
                        return Err(self.create_err(RuntimeErrorKind::NotSparseId, node));
                    }
                }
            }

            IntrinsicFunction::FloatRound => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Int(f.round().into()),
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                }
            },
            IntrinsicFunction::FloatFloor => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Int(f.floor().into()),
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatSign => match value_ref.borrow().clone() {
                Value::Float(f) => {
                    let signum = if f.inner() < 0 {
                        -1
                    } else if f.inner() > 0 {
                        1
                    } else {
                        0
                    };
                    Value::Float(Fp::from(signum as i16))
                }
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                }
            },
            IntrinsicFunction::FloatAbs => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Float(f.abs()),
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatCos => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Float(f.cos()),
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatAcos => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Float(f.acos()),
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatSin => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Float(f.sin()),
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatAsin => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Float(f.asin()),
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatSqrt => match value_ref.borrow().clone() {
                Value::Float(f) => Value::Float(f.sqrt()),
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::FloatMin => {
                let min_value = self.evaluate_expression(&arguments[0])?;
                match (value_ref.borrow().clone(), min_value) {
                    (Value::Float(f), Value::Float(min_f)) => Value::Float(f.min(min_f)),
                    _ => {
                        return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                    }
                }
            }

            IntrinsicFunction::FloatMax => {
                let max_value = self.evaluate_expression(&arguments[0])?;
                match (value_ref.borrow().clone(), max_value) {
                    (Value::Float(f), Value::Float(max_f)) => Value::Float(f.max(max_f)),
                    _ => {
                        return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                    }
                }
            }

            IntrinsicFunction::FloatAtan2 => {
                let x_value = self.evaluate_expression(&arguments[0])?;
                match (value_ref.borrow().clone(), x_value) {
                    (Value::Float(_y_f), Value::Float(_x_f)) => {
                        Value::Float(Fp::from(-9999)) //y_f.atan2(x_f)) // TODO: Implement atan2
                    }
                    _ => {
                        return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                    }
                }
            }

            IntrinsicFunction::FloatClamp => {
                let min_value = self.evaluate_expression(&arguments[0])?;
                let max_value = self.evaluate_expression(&arguments[1])?;
                match (value_ref.borrow().clone(), min_value, max_value) {
                    (Value::Float(f), Value::Float(min_f), Value::Float(max_f)) => {
                        Value::Float(f.clamp(min_f, max_f))
                    }
                    _ => {
                        return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                    }
                }
            }

            IntrinsicFunction::FloatRnd => match value_ref.borrow().clone() {
                Value::Float(f) => {
                    let new_raw = squirrel_prng::squirrel_noise5(f.inner() as u32, 0);
                    Value::Int(new_raw as i32)
                }
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::IntAbs => match value_ref.borrow().clone() {
                Value::Int(i) => Value::Int(i.abs()),
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedFloat, node));
                }
            },

            IntrinsicFunction::IntClamp => {
                let min_value = self.evaluate_expression(arguments[0])?;
                let max_value = self.evaluate_expression(arguments[1])?;
                match (value_ref.borrow().clone(), min_value, max_value) {
                    (Value::Int(i), Value::Int(min_i), Value::Int(max_i)) => {
                        Value::Int(i.clamp(min_i, max_i))
                    }
                    _ => {
                        return Err(self.create_err(RuntimeErrorKind::ExpectedInt, node));
                    }
                }
            }

            IntrinsicFunction::IntMin => {
                let max_value = self.evaluate_expression(arguments[0])?;
                match (value_ref.borrow().clone(), max_value) {
                    (Value::Int(i), Value::Int(min_i)) => Value::Int(i.min(min_i)),
                    _ => {
                        return Err(self.create_err(RuntimeErrorKind::ExpectedInt, node));
                    }
                }
            }

            IntrinsicFunction::IntMax => {
                let max_value = self.evaluate_expression(&arguments[0])?;
                match (value_ref.borrow().clone(), max_value) {
                    (Value::Int(i), Value::Int(max_i)) => Value::Int(i.max(max_i)),
                    _ => {
                        return Err(self.create_err(RuntimeErrorKind::ExpectedInt, node));
                    }
                }
            }

            IntrinsicFunction::IntRnd => match value_ref.borrow().clone() {
                Value::Int(i) => Value::Int(squirrel_prng::squirrel_noise5(i as u32, 0) as i32),
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedInt, node));
                }
            },

            IntrinsicFunction::IntToFloat => match value_ref.borrow().clone() {
                Value::Int(i) => Value::Float(Fp::from(i as i16)),
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedInt, node));
                }
            },

            IntrinsicFunction::StringLen => match value_ref.borrow().clone() {
                Value::String(s) => Value::Int(s.len().try_into().expect("string len overflow")),
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedString, node));
                }
            },

            IntrinsicFunction::Float2Magnitude => match value_ref.borrow().clone() {
                Value::Tuple(_tuple_ref, values) => {
                    if values.len() != 2 {
                        return Err(self.create_err(
                            RuntimeErrorKind::WrongNumberOfArguments(2, values.len()),
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
                                self.create_err(RuntimeErrorKind::ExpectedTwoFloatTuple, node)
                            );
                        }
                    }
                }
                _ => {
                    return Err(self.create_err(RuntimeErrorKind::ExpectedTwoFloatTuple, node));
                }
            },

            _ => todo!("{intrinsic_function:?} not implemented"),
        };

        Ok(val)
    }

    #[allow(clippy::too_many_lines)]
    fn eval_chain(
        &mut self,
        node: &Node,
        start: &Expression,
        parts: &[Postfix],
    ) -> Result<ValueRef, RuntimeError> {
        let (mut val_ref, mut is_mutable) = match &start.kind {
            ExpressionKind::VariableAccess(start_var) => {
                let start_variable_value = self.current_block_scopes.get_var(&start_var);

                match start_variable_value {
                    VariableValue::Value(value) => {
                        assert_ne!(*value, Value::Unit);
                        (Rc::new(RefCell::new(value.clone())), false)
                    }
                    VariableValue::Reference(value_ref) => {
                        assert_ne!(value_ref.borrow().clone(), Value::Unit);
                        (value_ref.clone(), true)
                    }
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
            if let PostfixKind::NoneCoalesce(default_expression) = &part.kind {
                val_ref = {
                    let borrowed = val_ref.borrow();

                    match borrowed.clone() {
                        Value::Option(found_option) => match found_option {
                            Some(some_value) => some_value,
                            _ => {
                                let default_value = self.evaluate_expression(default_expression)?;
                                Rc::new(RefCell::new(default_value))
                            }
                        },
                        _ => {
                            return Err(
                                self.create_err(RuntimeErrorKind::ExpectedOptional, &part.node)
                            );
                        }
                    }
                };

                is_mutable = false;
                is_uncertain = false;
                is_undefined = false;
            } else if is_undefined {
                continue;
            }
            match &part.kind {
                PostfixKind::NoneCoalesce(_default_expression) => {
                    // Handled earlier
                }

                PostfixKind::StructField(expected_struct_type, index) => {
                    let (encountered_struct_type, fields) = {
                        let brw = val_ref.borrow();
                        let (struct_ref, fields_ref) = brw.expect_anon_struct().map_err(|_| {
                            self.create_err(RuntimeErrorKind::PostfixChainError, &part.node)
                        })?;
                        (struct_ref.clone(), fields_ref.clone())
                    };

                    if !same_anon_struct_ref(&encountered_struct_type, expected_struct_type) {
                        error!(
                            ?expected_struct_type,
                            ?encountered_struct_type,
                            "difference"
                        );
                    }

                    debug_assert!(same_anon_struct_ref(
                        &encountered_struct_type,
                        expected_struct_type
                    ));
                    val_ref = fields[*index].clone();
                }

                PostfixKind::MemberCall(function_ref, arguments) => {
                    let val =
                        self.eval_member_call(node, &val_ref, is_mutable, function_ref, arguments)?;

                    val_ref = Rc::new(RefCell::new(val));
                    is_mutable = false;
                }
                PostfixKind::FunctionCall(arguments) => {
                    let val = self.eval_function_call(node, &val_ref, arguments)?;

                    val_ref = Rc::new(RefCell::new(val));
                    is_mutable = false;
                }

                PostfixKind::OptionUnwrap => {
                    val_ref = {
                        let borrowed = val_ref.borrow();

                        match borrowed.clone() {
                            Value::Option(found_option) => match found_option {
                                Some(some_value) => some_value,
                                _ => {
                                    is_undefined = true;

                                    Rc::new(RefCell::new(Value::Option(None)))
                                }
                            },
                            _ => {
                                return Err(
                                    self.create_err(RuntimeErrorKind::ExpectedOptional, &part.node)
                                );
                            }
                        }
                    };

                    is_mutable = false;
                    is_uncertain = true;
                }
                _ => {}
            }
        }

        if is_uncertain {
            let binding = val_ref.borrow().clone();
            match binding {
                Value::Option(_) => {}
                _ => {
                    val_ref = Rc::new(RefCell::new(Value::Option(Some(val_ref))));
                }
            }
        }

        Ok(val_ref)
    }

    fn eval_function_call(
        &mut self,
        node: &Node,
        function_val: &ValueRef,
        arguments: &[ArgumentExpressionOrLocation],
    ) -> Result<Value, RuntimeError> {
        let resolved_fn = match function_val.borrow().clone() {
            Value::InternalFunction(x) => Function::Internal(x.clone()),
            Value::ExternalFunction(external_fn) => Function::External(external_fn.clone()),
            _ => panic!("no function to call"),
        };

        let parameters = &resolved_fn.signature().parameters;
        // Check total number of parameters (including self)
        debug_assert_eq!(
            arguments.len(),
            parameters.len(),
            "wrong number of arguments"
        );

        let resolved_arguments = self.evaluate_args(&arguments)?;

        let result_val = match &resolved_fn {
            Function::Internal(internal_function) => {
                self.push_function_scope();

                self.bind_parameters(node, &parameters, &resolved_arguments)?;
                let result = self.evaluate_expression(&internal_function.body)?;
                self.pop_function_scope();

                result
            }
            Function::External(external_func) => {
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
        node: &Node,
        self_value_ref: &ValueRef,
        is_mutable: bool,
        function_ref: &FunctionRef,
        arguments: &[ArgumentExpressionOrLocation],
    ) -> Result<Value, RuntimeError> {
        let parameters = &function_ref.signature().parameters;

        let self_var_value = if parameters[0].is_mutable {
            if !is_mutable {
                return Err(self.create_err(RuntimeErrorKind::ArgumentIsNotMutable, &node));
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
            Function::Internal(internal_function) => {
                self.push_function_scope();
                self.bind_parameters(node, &parameters, &member_call_arguments)?;
                let result = self.evaluate_expression(&internal_function.body)?;
                self.pop_function_scope();

                result
            }
            Function::External(external_func) => {
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

    fn eval_guard(&mut self, node: &Node, guards: &[Guard]) -> Result<Value, RuntimeError> {
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

        Err(self.create_err(RuntimeErrorKind::MustHaveGuardArmThatMatches, &node))
    }

    #[inline(always)]
    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    fn eval_match(&mut self, resolved_match: &Match) -> Result<Value, RuntimeError> {
        let actual_value = self.evaluate_mut_or_immutable_expression(&resolved_match.expression)?;
        let value_ref = actual_value.to_value_ref();

        for arm in &resolved_match.arms {
            match &arm.pattern {
                Pattern::Wildcard(_node) => return self.evaluate_expression(&arm.expression),
                Pattern::Normal(normal_pattern, maybe_guard) => {
                    if let Some(found_guard) = maybe_guard {
                        if !self
                            .evaluate_expression(&found_guard.expression)?
                            .is_truthy()?
                        // TODO: ERROR HANDLING
                        {
                            continue;
                        }
                    }

                    let immutable_value = actual_value.to_value();

                    match &normal_pattern {
                        NormalPattern::PatternList(elements) => {
                            return Ok(self.eval_normal_pattern_list(
                                elements,
                                &arm.expression,
                                value_ref.clone(),
                            )?);
                        }
                        NormalPattern::EnumPattern(enum_variant_ref, pattern_elements) => {
                            let maybe_found_match = self.eval_normal_pattern_enum(
                                pattern_elements.as_ref(),
                                &arm.expression,
                                enum_variant_ref,
                                value_ref.clone(),
                            )?;

                            if let Some(found_match) = maybe_found_match {
                                return Ok(found_match);
                            }
                        }

                        NormalPattern::Literal(lit) => match (lit, &immutable_value) {
                            (Literal::IntLiteral(a), Value::Int(b)) if a == b => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (Literal::FloatLiteral(a), Value::Float(b)) if a == b => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (Literal::StringLiteral(a), Value::String(b)) if *a == *b => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (Literal::BoolLiteral(a), Value::Bool(b)) if a == b => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (
                                Literal::TupleLiteral(_a_type_ref, a_values),
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

    fn eval_normal_pattern_list(
        &mut self,
        elements: &[PatternElement],
        expression_to_evaluate: &Expression,
        value_ref: ValueRef,
    ) -> Result<Value, RuntimeError> {
        // Handle single variable/wildcard patterns that match any value
        if elements.len() == 1 {
            return match &elements[0] {
                PatternElement::Variable(var_ref)
                | PatternElement::VariableWithFieldIndex(var_ref, _) => {
                    self.push_block_scope();
                    self.current_block_scopes
                        .initialize_var_mut(var_ref, value_ref);
                    let result = self.evaluate_expression(expression_to_evaluate);
                    self.pop_block_scope();
                    result
                }
                PatternElement::Wildcard(_) => {
                    // Wildcard matches anything
                    self.evaluate_expression(expression_to_evaluate)
                }
            };
        }

        if let Value::Tuple(_tuple_type_ref, values) = value_ref.borrow_mut().clone() {
            debug_assert_eq!(
                elements.len(),
                values.len(),
                "must use all elements in tuple"
            );
            self.push_block_scope();

            for (element, _inside_value) in elements.iter().zip(values.iter()) {
                match element {
                    PatternElement::Variable(var_ref) => {
                        self.current_block_scopes
                            .initialize_var_mut(var_ref, value_ref.clone());
                    }
                    PatternElement::VariableWithFieldIndex(var_ref, _) => {
                        self.current_block_scopes
                            .initialize_var_mut(var_ref, value_ref.clone());
                    }
                    PatternElement::Wildcard(_) => {
                        // Skip wildcards
                        continue;
                    }
                }
            }

            let result = self.evaluate_expression(expression_to_evaluate);
            self.pop_block_scope();

            return result;
        }
        panic!("should not get here")
    }

    fn eval_normal_pattern_enum(
        &mut self,
        maybe_elements: Option<&Vec<PatternElement>>,
        expression_to_evaluate: &Expression,
        variant_ref: &EnumVariantType,
        value_ref: ValueRef,
    ) -> Result<Option<Value>, RuntimeError> {
        match value_ref.borrow_mut().clone() {
            Value::EnumVariantTuple(enum_type, value_tuple_type, values) => {
                // First check if the variant types match
                if variant_ref.common().container_index != value_tuple_type.common.container_index {
                    return Ok(None); // Try next pattern
                }

                if let Some(elements) = maybe_elements {
                    debug_assert_eq!(elements.len(), values.len());
                    self.push_block_scope();

                    for (element, value) in elements.iter().zip(values.iter()) {
                        match element {
                            PatternElement::Variable(var_ref) => {
                                self.current_block_scopes
                                    .initialize_var_mut(var_ref, value.clone());
                            }
                            PatternElement::VariableWithFieldIndex(var_ref, _) => {
                                self.current_block_scopes
                                    .initialize_var_mut(var_ref, value.clone());
                            }
                            PatternElement::Wildcard(_) => continue,
                        }
                    }

                    let result = self.evaluate_expression(&expression_to_evaluate);
                    self.pop_block_scope();
                    return Ok(Option::from(result?));
                } else {
                    panic!("not work");
                }
            }
            Value::EnumVariantStruct(enum_type, value_enum_struct_type, values) => {
                info!(
                    ?value_enum_struct_type,
                    ?variant_ref,
                    "comparing enum variant struct match arm"
                );
                if value_enum_struct_type.common.container_index
                    == variant_ref.common().container_index
                {
                    info!(?value_enum_struct_type, ?variant_ref, "FOUND!");
                    if let Some(elements) = maybe_elements {
                        self.push_block_scope();

                        for element in elements {
                            if let PatternElement::VariableWithFieldIndex(var_ref, field_index) =
                                element
                            {
                                let value = &values[*field_index];
                                info!(?value, "setting match arm variable");
                                self.current_block_scopes.init_var_ref(var_ref, value);
                            }
                        }

                        let result = self.evaluate_expression(&expression_to_evaluate);
                        self.pop_block_scope();
                        return Ok(Some(result?));
                    }
                }
            }

            Value::EnumVariantSimple(_enum_type, value_variant_ref) => {
                if value_variant_ref.common.container_index == variant_ref.common().container_index
                    && maybe_elements.is_none()
                {
                    return Ok(Some(self.evaluate_expression(expression_to_evaluate)?));
                }
            }
            _ => {
                panic!("could not find enum variant, serious error")
            }
        }

        Ok(None)
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
        node: &Node,
        left_val: Value,
        op: &BinaryOperatorKind,
        right_val: Value,
    ) -> Result<Value, RuntimeError> {
        let result: Value = match (&left_val, op, &right_val) {
            // Integer operations
            (Value::Int(a), BinaryOperatorKind::Add, Value::Int(b)) => Value::Int(a + b),
            (Value::Int(a), BinaryOperatorKind::Subtract, Value::Int(b)) => Value::Int(a - b),
            (Value::Int(a), BinaryOperatorKind::Multiply, Value::Int(b)) => Value::Int(a * b),
            (Value::Int(a), BinaryOperatorKind::Divide, Value::Int(b)) => {
                if *b == 0 {
                    return Err(self.create_err(RuntimeErrorKind::DivideByZero, node));
                }
                Value::Int(a / b)
            }
            (Value::Int(a), BinaryOperatorKind::Modulo, Value::Int(b)) => {
                Value::Int(Self::modulo(*a, *b))
            }
            (Value::Int(a), BinaryOperatorKind::Equal, Value::Int(b)) => Value::Bool(a == b),
            (Value::Int(a), BinaryOperatorKind::NotEqual, Value::Int(b)) => Value::Bool(a != b),
            (Value::Int(a), BinaryOperatorKind::LessThan, Value::Int(b)) => Value::Bool(a < b),
            (Value::Int(a), BinaryOperatorKind::GreaterThan, Value::Int(b)) => Value::Bool(a > b),
            (Value::Int(a), BinaryOperatorKind::LessEqual, Value::Int(b)) => Value::Bool(a <= b),
            (Value::Int(a), BinaryOperatorKind::GreaterEqual, Value::Int(b)) => Value::Bool(a >= b),

            // Float operations
            (Value::Float(a), BinaryOperatorKind::Equal, Value::Float(b)) => Value::Bool(a == b),
            (Value::Float(a), BinaryOperatorKind::NotEqual, Value::Float(b)) => Value::Bool(a != b),

            (Value::Float(a), BinaryOperatorKind::Add, Value::Float(b)) => Value::Float(*a + *b),
            (Value::Float(a), BinaryOperatorKind::Subtract, Value::Float(b)) => {
                Value::Float(*a - *b)
            }
            (Value::Float(a), BinaryOperatorKind::Multiply, Value::Float(b)) => {
                Value::Float(*a * *b)
            }
            (Value::Float(a), BinaryOperatorKind::Divide, Value::Float(b)) => {
                if b.abs().inner() <= 400 {
                    return Err(self.create_err(RuntimeErrorKind::DivideByZero, node));
                }
                Value::Float(*a / *b)
            }
            (Value::Float(a), BinaryOperatorKind::Modulo, Value::Float(b)) => {
                Value::Float(Self::modulo_fp(*a, *b))
            }

            (Value::Float(a), BinaryOperatorKind::GreaterThan, Value::Float(b)) => {
                Value::Bool(a > b)
            }
            (Value::Float(a), BinaryOperatorKind::GreaterEqual, Value::Float(b)) => {
                Value::Bool(a >= b)
            }
            (Value::Float(a), BinaryOperatorKind::LessThan, Value::Float(b)) => Value::Bool(a < b),
            (Value::Float(a), BinaryOperatorKind::LessEqual, Value::Float(b)) => {
                Value::Bool(a <= b)
            }

            // Boolean operations
            (Value::Bool(a), BinaryOperatorKind::LogicalAnd, Value::Bool(b)) => {
                Value::Bool(*a && *b)
            }
            (Value::Bool(a), BinaryOperatorKind::LogicalOr, Value::Bool(b)) => {
                Value::Bool(*a || *b)
            }

            // Comparison operations

            // RustType
            (Value::RustValue(_, left), BinaryOperatorKind::Equal, Value::RustValue(_, right)) => {
                let left_borrow = left.borrow();
                let right_borrow = right.borrow();
                let equal = left_borrow.eq_dyn(&**right_borrow);
                Value::Bool(equal)
            }
            (
                Value::RustValue(_, left),
                BinaryOperatorKind::NotEqual,
                Value::RustValue(_, right),
            ) => {
                let left_borrow = left.borrow();
                let right_borrow = right.borrow();
                let equal = left_borrow.eq_dyn(&**right_borrow);
                Value::Bool(!equal)
            }

            // String operations
            (Value::String(a), BinaryOperatorKind::Add, Value::String(b)) => {
                Value::String(a.to_owned() + b)
            }
            (Value::String(a), BinaryOperatorKind::Equal, Value::String(b)) => Value::Bool(a == b),

            (Value::String(a), BinaryOperatorKind::Add, Value::Int(b)) => {
                Value::String(a.to_owned() + &(*b).to_string())
            }
            (Value::Int(a), BinaryOperatorKind::Add, Value::String(b)) => {
                Value::String(a.to_string() + b)
            }

            // Enum
            (
                Value::EnumVariantSimple(_a_enum_type, a),
                BinaryOperatorKind::Equal,
                Value::EnumVariantSimple(_b_enum_type, b),
            ) => Value::Bool(a == b),
            (
                Value::EnumVariantSimple(_a_enum_type, a),
                BinaryOperatorKind::NotEqual,
                Value::EnumVariantSimple(_b_enum_type, b),
            ) => Value::Bool(a != b),

            // Bool
            (Value::Bool(a), BinaryOperatorKind::Equal, Value::Bool(b)) => Value::Bool(a == b),
            (Value::Bool(a), BinaryOperatorKind::NotEqual, Value::Bool(b)) => Value::Bool(a != b),

            (Value::Option(a), BinaryOperatorKind::Equal, Value::Option(b)) => Value::Bool(a == b),

            _ => {
                error!(?op, "invalid binary operation!!");
                panic!("invalid binary operation"); // TODO: improve error handling
            }
        };

        Ok(result)
    }

    fn evaluate_unary_op(
        &self,
        node: &Node,
        op: &UnaryOperatorKind,
        val: Value,
    ) -> Result<Value, RuntimeError> {
        match (op, val) {
            (UnaryOperatorKind::Negate, Value::Int(n)) => Ok(Value::Int(-n)),
            (UnaryOperatorKind::Negate, Value::Float(n)) => Ok(Value::Float(-n)),
            (UnaryOperatorKind::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            _ => Err(self.create_err(RuntimeErrorKind::DivideByZero, node)),
        }
    }

    fn expressions_equal_to_values(
        &mut self,
        p0: &[Expression],
        p1: &[ValueRef],
    ) -> Result<bool, RuntimeError> {
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
        node: &Node,
        target: &mut Value,
        operator: &CompoundOperatorKind,
        source: &Value,
    ) -> Result<(), RuntimeError> {
        match operator {
            CompoundOperatorKind::Mul => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &BinaryOperatorKind::Multiply,
                    source.clone(),
                )?;
            }
            CompoundOperatorKind::Div => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &BinaryOperatorKind::Divide,
                    source.clone(),
                )?;
            }
            CompoundOperatorKind::Add => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &BinaryOperatorKind::Add,
                    source.clone(),
                )?;
            }
            CompoundOperatorKind::Sub => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &BinaryOperatorKind::Subtract,
                    source.clone(),
                )?;
            }
            CompoundOperatorKind::Modulo => {
                *target = self.evaluate_binary_op(
                    node,
                    target.clone(),
                    &BinaryOperatorKind::Modulo,
                    source.clone(),
                )?;
            }
        }
        Ok(())
    }

    fn create_err(&self, kind: RuntimeErrorKind, node: &Node) -> RuntimeError {
        RuntimeError {
            node: node.clone(),
            kind,
        }
    }

    /*
    fn evaluate_intrinsic_mut(
        &mut self,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        location: &SingleMutLocationExpression,
        arguments: &Vec<Expression>,
    ) -> Result<Value, RuntimeError> {
        let val = match intrinsic_fn {
            IntrinsicFunction::VecSelfPush => {
                let source_val = self.evaluate_expression(&arguments[0])?;
                let array_val_ref = self.evaluate_location(&location.0)?;

                match &mut *array_val_ref.borrow_mut() {
                    Value::Vec(_type_id, vector) => {
                        vector.push(Rc::new(RefCell::new(source_val)));
                    }
                    _ => {
                        Err(self.create_err(RuntimeErrorKind::OperationRequiresArray, &node))?;
                    }
                }
                //array_val_ref.borrow().clone()
                Value::Unit
            }
            IntrinsicFunction::VecSelfExtend => {
                let source_val = self.evaluate_expression(&arguments[0])?;

                let array_val_ref = self.evaluate_location(&location.0)?;
                match &mut *array_val_ref.borrow_mut() {
                    Value::Vec(_type_id, vector) => match source_val {
                        Value::Vec(_, items) => {
                            vector.extend(items);
                        }
                        _ => {
                            Err(self.create_err(RuntimeErrorKind::OperationRequiresArray, &node))?;
                        }
                    },
                    _ => {
                        todo!("handle error")
                    }
                }

                // array_val_ref.borrow().clone()
                Value::Unit
            }
            _ => return Err(self.create_err(RuntimeErrorKind::UnknownMutIntrinsic, &node)),
        };

        Ok(val)
    }

     */
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
