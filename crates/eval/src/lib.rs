/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::block::BlockScopes;
use crate::err::ConversionError;
use crate::prelude::{ValueReference, VariableValue};
use err::ExecuteError;
use seq_map::SeqMap;
use std::fmt::Debug;
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use swamp_script_core::extra::{SparseValueId, SparseValueMap};
use swamp_script_core::value::{
    convert_vec_to_rc_refcell, format_value, to_rust_value, SourceMapLookup, Value, ValueError,
};
use swamp_script_core::value::{RustType, ValueRef, SPARSE_ID_TYPE_ID, SPARSE_TYPE_ID};
use swamp_script_semantic::modules::ResolvedModules;
use swamp_script_semantic::prelude::*;
use swamp_script_semantic::{
    ConstantId, ResolvedAccess, ResolvedBinaryOperatorKind, ResolvedCompoundOperatorKind,
    ResolvedForPattern, ResolvedFunction, ResolvedNormalPattern, ResolvedPatternElement,
    ResolvedPostfixOperatorKind, ResolvedRangeMode, ResolvedStaticCall, ResolvedUnaryOperatorKind,
};

pub mod err;

mod block;
pub mod prelude;
pub mod value_both;
pub mod value_ref;

type RawFunctionFn<C> = dyn FnMut(&[VariableValue], &mut C) -> Result<Value, ExecuteError>;

type FunctionFn<C> = Box<RawFunctionFn<C>>;

pub struct EvalExternalFunction<C> {
    pub func: FunctionFn<C>,
    pub id: ExternalFunctionId,
}

impl<C> Debug for EvalExternalFunction<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "external_fn {}", self.id)
    }
}

#[inline]
#[allow(clippy::too_many_lines)]
pub fn quick_deserialize(resolved_type: &ResolvedType, buf: &[u8]) -> (Value, usize) {
    match resolved_type {
        ResolvedType::Int => {
            let i = i32::from_ne_bytes(buf[0..4].try_into().expect("REASON"));
            (Value::Int(i), 4)
        }

        ResolvedType::Float => {
            let i = i32::from_ne_bytes(buf[0..4].try_into().expect("couldn't convert to Fp"));
            (Value::Float(Fp::from_raw(i)), 4)
        }
        ResolvedType::String => {
            let octet_len =
                u16::from_ne_bytes(buf[0..2].try_into().expect("could not convert strlen"));
            let str =
                String::from_utf8(buf[2..2 + octet_len as usize].to_owned()).expect("utf8 error");
            (Value::String(str), (octet_len + 2) as usize)
        }
        ResolvedType::Bool => (Value::Bool(buf[0] != 0), 1),
        ResolvedType::Unit => (Value::Unit, 0),
        ResolvedType::Array(array_type_ref) => {
            let mut offset = 0;
            let count = u16::from_ne_bytes(
                buf[offset..offset + 2]
                    .try_into()
                    .expect("should work with u16"),
            );
            let item_ref = &array_type_ref.item_type;
            offset += 2;
            let mut values = Vec::new();
            for _index in 0..count {
                let (value, item_octet_size) = quick_deserialize(item_ref, &buf[offset..]);
                offset += item_octet_size;
                values.push(Rc::new(RefCell::new(value)));
            }
            (Value::Array(array_type_ref.clone(), values), offset)
        }
        ResolvedType::Tuple(tuple_type_ref) => {
            let mut offset = 0;
            let mut values = Vec::new();
            for tuple_item_type in &tuple_type_ref.0 {
                let (value, item_octet_size) = quick_deserialize(tuple_item_type, &buf[offset..]);
                values.push(Rc::new(RefCell::new(value)));
                offset += item_octet_size;
            }
            (Value::Tuple(tuple_type_ref.clone(), values), offset)
        }
        ResolvedType::Struct(struct_type_ref) => {
            let mut values = Vec::new();
            let mut offset = 0;
            for struct_field_type in struct_type_ref
                .borrow()
                .anon_struct_type
                .defined_fields
                .values()
            {
                let (value, octet_size) =
                    quick_deserialize(&struct_field_type.field_type, &buf[offset..]);
                values.push(Rc::new(RefCell::new(value)));
                offset += octet_size;
            }
            (Value::Struct(struct_type_ref.clone(), values), offset)
        }
        ResolvedType::Map(map_type_ref) => {
            let mut offset = 0;
            let key_type = &map_type_ref.key_type;
            let value_type = &map_type_ref.value_type;
            let count = u16::from_ne_bytes(
                buf[offset..offset + 2]
                    .try_into()
                    .expect("should work with u16"),
            );
            offset += 2;
            let mut seq_map = SeqMap::new(); //SeqMap<Value, ValueRef>
            for _map_index in 0..count {
                let (key_val, key_octet_size) = quick_deserialize(&key_type, &buf[offset..]);
                offset += key_octet_size;

                let (value_val, value_octet_size) = quick_deserialize(&value_type, &buf[offset..]);
                offset += value_octet_size;

                let value_ref = Rc::new(RefCell::new(value_val));

                seq_map
                    .insert(key_val, value_ref)
                    .expect("should work to insert");
            }
            (Value::Map(map_type_ref.clone(), seq_map), offset)
        }
        ResolvedType::Enum(_resolved_type) => todo!(),
        ResolvedType::Generic(base_type, type_parameters) => {
            if let ResolvedType::Struct(found_struct) = &**base_type {
                if found_struct.borrow().assigned_name == "Sparse" {
                    let sparse_type_id_rust_type = Rc::new(ResolvedRustType {
                        type_name: "SparseId".to_string(),
                        number: SPARSE_ID_TYPE_ID, // TODO: FIX hardcoded number
                    });

                    let value_type = &type_parameters[0];

                    let internal_map =
                        SparseValueMap::new(sparse_type_id_rust_type, value_type.clone());
                    let wrapped_internal_map: Rc<RefCell<Box<dyn RustType>>> =
                        Rc::new(RefCell::new(Box::new(internal_map)));

                    let sparse_collection_rust_type = Rc::new(ResolvedRustType {
                        type_name: "Sparse".to_string(),
                        number: SPARSE_TYPE_ID, // TODO: FIX hardcoded number
                    });

                    (
                        Value::RustValue(sparse_collection_rust_type, wrapped_internal_map),
                        0,
                    )
                } else {
                    panic!("unknown generic type");
                }
            } else {
                panic!("unknown generic type")
            }
        }
        ResolvedType::Function(_) => {
            panic!("can not serialize function")
        }
        ResolvedType::Iterable(_) => {
            panic!("can not serialize iterables")
        }
        ResolvedType::Optional(optional_type_ref) => {
            let mut offset = 0;
            let has_some = buf[0] != 0;
            offset += 1;
            if has_some {
                let (v, octet_size) = quick_deserialize(optional_type_ref, &buf[1..]);
                offset += octet_size;
                (
                    Value::Option(Some(Box::from(Rc::new(RefCell::new(v))))),
                    offset,
                )
            } else {
                (Value::Option(None), offset)
            }
        }
        ResolvedType::RustType(rust_type_ref) => {
            panic!("can not deserialize rust types {}", rust_type_ref.type_name)
        }
        ResolvedType::Any => {
            panic!("can not deserialize any");
        }
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

impl From<ValueError> for ExecuteError {
    fn from(value: ValueError) -> Self {
        Self::ValueError(value)
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
            //          external_functions: HashMap::new(),
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
        //    self.external_functions
        //      .insert(name.to_string(), external_func_ref);

        Ok(())
    }
}

pub fn eval_module<C>(
    externals: &ExternalFunctions<C>,
    constants: &Constants,
    root_expression: &ResolvedExpression,
    context: &mut C,
) -> Result<Value, ExecuteError> {
    let mut interpreter = Interpreter::<C>::new(externals, constants, context);
    let value = interpreter.evaluate_expression(root_expression)?;
    Ok(value)
}

pub fn eval_constants<C>(
    externals: &ExternalFunctions<C>,
    constants: &mut Constants,
    modules: &ResolvedModules,
    context: &mut C,
) -> Result<(), ExecuteError> {
    for constant in &modules.constants_in_eval_order {
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
    interpreter.bind_parameters(&func.signature.parameters, &arguments)?;
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
                    _ => {
                        return Err(ExecuteError::ArgumentIsNotMutable(
                            param.node.as_ref().unwrap().name.clone(),
                        ))
                    }
                }
            } else {
                match arg {
                    VariableValue::Reference(r) => VariableValue::Value(r.unref().clone()),
                    VariableValue::Value(v) => VariableValue::Value(v.clone()),
                }
            };

            self.current_block_scopes
                .set_local_var_ex(index, complete_value, param.is_mutable)?;
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
                self.push_function_scope();
                self.bind_parameters(&function_data.signature.parameters, &evaluated_args)?;
                let result = self.evaluate_expression(&function_data.body)?;

                self.pop_function_scope();
                Ok(result)
            }
            ResolvedFunction::External(external) => {
                let mut func = self
                    .externals
                    .external_functions_by_id
                    .get(&external.id)
                    .expect("static call: external function missing")
                    .borrow_mut();
                (func.func)(&evaluated_args, self.context)
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
            .expect("function call: external function missing")
            .borrow_mut();
        let v = (func.func)(&evaluated_args, self.context)?;
        Ok(v)
    }

    fn evaluate_function_call(
        &mut self,
        function_expression: &ResolvedExpression,
        arguments: &[ResolvedExpression],
    ) -> Result<Value, ExecuteError> {
        let func_val = self.evaluate_expression(function_expression)?;
        let evaluated_args = self.evaluate_args(arguments)?;

        match &func_val {
            Value::InternalFunction(internal_func_ref) => {
                self.push_function_scope();

                self.bind_parameters(&internal_func_ref.signature.parameters, &evaluated_args)?;

                let result = self.evaluate_expression(&internal_func_ref.body)?;

                self.pop_function_scope();

                Ok(result)
            }

            Value::ExternalFunction(external_function_id) => {
                let mut func = self
                    .externals
                    .external_functions_by_id
                    .get(external_function_id)
                    .ok_or(ExecuteError::MissingExternalFunction(*external_function_id))?
                    .borrow_mut();

                (func.func)(&evaluated_args, self.context)
            }
            _ => Err(ExecuteError::Error(format!(
                "internal error, can only execute internal or external function {func_val:?}"
            ))),
        }
    }

    fn evaluate_internal_function_call(
        &mut self,
        call: &ResolvedInternalFunctionCall,
    ) -> Result<Value, ExecuteError> {
        let func_val = self.evaluate_expression(&call.function_expression)?;
        match &func_val {
            Value::InternalFunction(_internal_func_ref) => {}
            _ => {
                return Err(ExecuteError::Error(
                    "internal error, can only execute internal function".to_owned(),
                ))
            }
        }

        let evaluated_args = self.evaluate_args(&call.arguments)?;

        self.push_function_scope();

        self.bind_parameters(
            &call.function_definition.signature.parameters,
            &evaluated_args,
        )?;

        let result = self.evaluate_expression(&call.function_definition.body)?;

        self.pop_function_scope();

        Ok(result)
    }

    fn evaluate_mut_expression(
        &mut self,
        expr: &ResolvedExpression,
    ) -> Result<VariableValue, ExecuteError> {
        let mem_value = match expr {
            ResolvedExpression::MutVariableRef(var_ref) => {
                let found_var = self
                    .current_block_scopes
                    .lookup_variable(&var_ref.variable_ref);
                match found_var {
                    VariableValue::Reference(_) => found_var.clone(),
                    _ => {
                        return Err(
                            "Can only take mutable reference of mutable variable".to_string()
                        )?;
                    }
                }
            }

            ResolvedExpression::MutStructFieldRef(base_expr, _type, access_chain) => {
                VariableValue::Reference(ValueReference(
                    self.evaluate_location(base_expr, access_chain)?.clone(),
                ))
            }

            ResolvedExpression::MutArrayIndexRef(base_expr, _, access_chain) => {
                VariableValue::Reference(ValueReference(
                    self.evaluate_location(base_expr, access_chain)?.clone(),
                ))
            }

            ResolvedExpression::MutMapIndexRef(map_expression, _map_type, key_expression) => {
                let map_ref = self.evaluate_expression_mut_location_start(&map_expression)?;
                let key_val = self.evaluate_expression(&key_expression)?;

                let option_val = {
                    if let Value::Map(_type_id, ref seq_map) = &*map_ref.borrow_mut() {
                        let x = seq_map.get(&key_val);
                        x.map_or_else(
                            || Value::Option(None),
                            |v| Value::Option(Some(Box::from(v.clone()))),
                        )
                    } else {
                        return Err(ExecuteError::NotAMap);
                    }
                };

                let option_val_ref = Rc::new(RefCell::new(option_val));

                VariableValue::Reference(ValueReference(option_val_ref))
            }

            // If it is accessing a variable, we want the full access of it
            ResolvedExpression::VariableAccess(variable_ref) => self
                .current_block_scopes
                .lookup_variable(variable_ref)
                .clone(),

            _ => {
                let value = self.evaluate_expression(expr)?;
                VariableValue::Value(value)
            }
        };

        Ok(mem_value)
    }

    fn evaluate_args(
        &mut self,
        args: &[ResolvedExpression],
    ) -> Result<Vec<VariableValue>, ExecuteError> {
        let mut evaluated = Vec::with_capacity(args.len());

        for argument_expression in args {
            let mem_value = self.evaluate_mut_expression(argument_expression)?;
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
            .is_truthy()?
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

    /*
                   self.push_block_scope();
               let result = {
                   let mut last_value = Value::Unit;

                   for expr in statements {
                       last_value = self.evaluate_expression(expr)?;
                   }
                   last_value
               };
               self.pop_block_scope();

    */

    fn evaluate_for_loop_mutable(
        &mut self,
        pattern: &ResolvedForPattern,
        iterator_expr: &ResolvedIterator,
        body: &Box<ResolvedExpression>,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let mut result = Value::Unit;

        let iterator_value = ValueReference(
            self.evaluate_expression_mut_location_start(&iterator_expr.resolved_expression)?,
        );

        match pattern {
            ResolvedForPattern::Single(var_ref) => {
                self.push_block_scope();

                for value in iterator_value.into_iter_mut()? {
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

                for (key, value_reference) in iterator_value.into_iter_mut_pairs()? {
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
        iterator_expr: &ResolvedIterator,
        body: &Box<ResolvedExpression>,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let mut result = Value::Unit;

        let iterator_value = self.evaluate_expression(&iterator_expr.resolved_expression)?;

        match pattern {
            ResolvedForPattern::Single(var_ref) => {
                self.push_block_scope();

                for value in iterator_value.into_iter()? {
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

                for (key, value) in iterator_value.into_iter_pairs()? {
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

    /*
    fn debug_expr(&self, expr: &ResolvedExpression, debug_str: &str) {
        if let Some(debug_source_map) = self.debug_source_map {
            let wrapped = ResolvedExpressionDisplay::new(expr, debug_source_map);
            info!(expr = %wrapped, "{}", format!("{debug_str}"));
            //eprintln!("{}: {}", debug_str, wrapped);
            //io::stderr().flush().expect("Failed to flush stdout");
        }
    }

     */

    #[inline]
    #[allow(clippy::too_many_lines)]
    fn evaluate_expression_with_signal(
        &mut self,
        expr: &ResolvedExpression,
    ) -> Result<ValueWithSignal, ExecuteError> {
        match expr {
            ResolvedExpression::Break(_) => Ok(ValueWithSignal::Break),
            ResolvedExpression::Continue(_) => Ok(ValueWithSignal::Continue),

            ResolvedExpression::Return(maybe_expr) => {
                let value = match maybe_expr {
                    None => Value::Unit,
                    Some(expr) => self.evaluate_expression(expr)?,
                };
                Ok(ValueWithSignal::Return(value))
            }

            ResolvedExpression::WhileLoop(condition, body) => {
                self.evaluate_while_loop(condition, body)
            }

            ResolvedExpression::ForLoop(pattern, iterator_expr, body) => {
                if pattern.is_mutable() {
                    self.evaluate_for_loop_mutable(pattern, iterator_expr, body)
                } else {
                    self.evaluate_for_loop(pattern, iterator_expr, body)
                }
            }

            ResolvedExpression::If(condition, consequences, optional_alternative) => {
                let cond_value = self.evaluate_expression(&condition.expression)?;
                if cond_value.is_truthy()? {
                    self.evaluate_expression_with_signal(consequences)
                } else if let Some(alternative) = optional_alternative {
                    self.evaluate_expression_with_signal(alternative)
                } else {
                    Ok(ValueWithSignal::Value(Value::Unit))
                }
            }

            ResolvedExpression::Block(expressions) => self.evaluate_block(expressions),

            ResolvedExpression::IfOnlyVariable {
                variable,
                optional_expr,
                true_block,
                false_block,
            } => {
                let condition_value = self.evaluate_expression(optional_expr)?;
                match condition_value {
                    Value::Option(Some(inner_value)) => {
                        self.push_block_scope();
                        self.current_block_scopes
                            .initialize_var_mut(variable, *inner_value);

                        let result = self.evaluate_expression_with_signal(true_block);
                        self.pop_block_scope();
                        result
                    }

                    Value::Option(None) => false_block.as_ref().map_or_else(
                        || Ok(ValueWithSignal::Value(Value::Unit)),
                        |else_block| self.evaluate_expression_with_signal(else_block),
                    ),
                    _ => Err(ExecuteError::ExpectedOptional),
                }
            }

            ResolvedExpression::IfAssignExpression {
                variable,
                optional_expr,
                true_block,
                false_block,
            } => {
                let value_mem = self.evaluate_mut_expression(optional_expr)?;
                let was_some = match value_mem {
                    VariableValue::Value(v) => match v {
                        Value::Option(maybe_value) => match maybe_value {
                            None => false,
                            Some(unwrapped_value) => {
                                self.push_block_scope();
                                self.current_block_scopes
                                    .initialize_var_mut(variable, *unwrapped_value.clone());
                                true
                            }
                        },
                        _ => return Err(ExecuteError::ExpectedOptional),
                    },
                    VariableValue::Reference(reference) => match &*reference.0.borrow() {
                        Value::Option(maybe_value) => match maybe_value {
                            None => false,
                            Some(unwrapped_value) => {
                                self.push_block_scope();
                                self.current_block_scopes
                                    .initialize_var_mut(variable, *unwrapped_value.clone());
                                true
                            }
                        },
                        _ => return Err(ExecuteError::ExpectedOptional),
                    },
                };

                if was_some {
                    let result = self.evaluate_expression_with_signal(true_block)?;
                    self.pop_block_scope();

                    Ok(result)
                } else if let Some(else_block) = false_block {
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
        let value = match expr {
            // Illegal in this context
            ResolvedExpression::Continue(_) => {
                return Err(ExecuteError::ContinueNotAllowedHere);
            }
            ResolvedExpression::Break(_) => {
                return Err(ExecuteError::BreakNotAllowedHere);
            }
            ResolvedExpression::Return(_maybe_expr) => {
                return Err(ExecuteError::BreakNotAllowedHere);
            }

            ResolvedExpression::WhileLoop(_condition, _body) => {
                panic!("should have been handled earlier")
            }

            ResolvedExpression::ForLoop(_pattern, _iterator_expr, _body) => {
                panic!("should have been handled earlier")
            }
            ResolvedExpression::IfOnlyVariable { .. } => {
                panic!("should have been handled earlier");
                /*
                let condition_value = self.evaluate_expression(optional_expr)?;
                match condition_value {
                    Value::Option(Some(inner_value)) => {
                        self.push_block_scope();
                        self.current_block_scopes.initialize_var(
                            variable.scope_index,
                            variable.variable_index,
                            *inner_value,
                            variable.is_mutable(),
                        );

                        let result = self.evaluate_expression(true_block)?;

                        self.pop_block_scope();

                        result
                    }
                    Value::Option(None) => {
                        if let Some(else_block) = false_block {
                            self.evaluate_expression(else_block)?
                        } else {
                            Value::Unit
                        }
                    }
                    _ => return Err(ExecuteError::ExpectedOptional),
                }

                 */
            }

            ResolvedExpression::IfAssignExpression { .. } => {
                panic!("should have been handled earlier");
                /*
                let value = self.evaluate_expression(optional_expr)?;
                match value {
                    Value::Option(Some(inner_value)) => {
                        self.push_block_scope();
                        self.current_block_scopes.initialize_var(
                            variable.scope_index,
                            variable.variable_index,
                            *inner_value,
                            variable.is_mutable(),
                        );

                        let result = self.evaluate_expression(true_block)?;
                        self.pop_block_scope();

                        result
                    }
                    Value::Option(None) => {
                        if let Some(else_block) = false_block {
                            self.evaluate_expression(else_block)?
                        } else {
                            Value::Unit
                        }
                    }
                    _ => return Err(ExecuteError::ExpectedOptional),
                }

                 */
            }

            // Constructing
            ResolvedExpression::Literal(lit) => match lit {
                ResolvedLiteral::IntLiteral(n, _resolved_node) => Value::Int(*n),
                ResolvedLiteral::FloatLiteral(f, _resolved_node) => Value::Float(*f),
                ResolvedLiteral::StringLiteral(s, _resolved_node) => Value::String(s.clone()),
                ResolvedLiteral::BoolLiteral(b, _resolved_node) => Value::Bool(*b),

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
                            ResolvedEnumLiteralData::Struct(source_order_field_values) => {
                                let mut field_values =
                                    Vec::with_capacity(source_order_field_values.len());
                                field_values
                                    .resize_with(source_order_field_values.len(), Default::default);
                                for (index, resolved_expression) in source_order_field_values {
                                    let value = self.evaluate_expression(resolved_expression)?;
                                    field_values[*index] = value;
                                }
                                Value::EnumVariantStruct(struct_type_ref.clone(), field_values)
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
                    Value::Tuple(tuple_type.clone(), convert_vec_to_rc_refcell(values))
                }

                ResolvedLiteral::UnitLiteral(_) => Value::Unit,
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
                            .map_err(|_err| ExecuteError::NonUniqueKeysInMapLiteralDetected)?;
                    }
                    Value::Map(map_type_ref.clone(), items)
                }
                ResolvedLiteral::NoneLiteral(_) => Value::Option(None),
            },

            ResolvedExpression::Array(array_instantiation) => {
                let mut values = Vec::new();
                for element in &array_instantiation.expressions {
                    values.push(self.evaluate_expression(element)?);
                }

                Value::Array(
                    array_instantiation.array_type_ref.clone(),
                    convert_vec_to_rc_refcell(values),
                )
            }

            ResolvedExpression::StructInstantiation(struct_instantiation) => {
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

            ResolvedExpression::ExclusiveRange(start, end) => {
                let start_val = self.evaluate_expression(start)?;
                let end_val = self.evaluate_expression(end)?;
                match (start_val, end_val) {
                    (Value::Int(s), Value::Int(e)) => {
                        Value::ExclusiveRange(Box::new(s), Box::new(e))
                    }
                    _ => Err("Range bounds must be integers".to_string())?,
                }
            }

            ResolvedExpression::InclusiveRange(start, end) => {
                let start_val = self.evaluate_expression(start)?;
                let end_val = self.evaluate_expression(end)?;
                match (start_val, end_val) {
                    (Value::Int(s), Value::Int(e)) => {
                        Value::InclusiveRange(Box::new(s), Box::new(e))
                    }
                    _ => Err("Range bounds must be integers".to_string())?,
                }
            }

            // ==================== ASSIGNMENT ====================
            ResolvedExpression::InitializeVariable(var_assignment) => {
                let target_var = &var_assignment.variable_refs;
                let source_value_or_reference =
                    self.evaluate_mut_expression(&var_assignment.expression)?;

                self.current_block_scopes
                    .initialize_var_mem(target_var, source_value_or_reference)?;

                Value::Unit
            }

            ResolvedExpression::ReassignVariable(var_assignment) => {
                let new_value = self.evaluate_mut_expression(&var_assignment.expression)?;

                let variable_ref = &var_assignment.variable_refs;
                self.current_block_scopes
                    .overwrite_existing_var_mem(variable_ref, new_value.clone())?;

                //new_value.into()
                Value::Unit
            }

            ResolvedExpression::VariableCompoundAssignment(var_assignment) => {
                let modifier_value = self.evaluate_expression(&var_assignment.expression)?;
                let current_value = self
                    .current_block_scopes
                    .lookup_mut_variable(&var_assignment.variable_ref)?;

                // Only int first
                let int_mod = modifier_value.expect_int()?;
                let current_int = current_value.borrow().expect_int()?;

                let new_result = match var_assignment.compound_operator.kind {
                    ResolvedCompoundOperatorKind::Add => current_int + int_mod,
                    ResolvedCompoundOperatorKind::Sub => current_int - int_mod,
                    ResolvedCompoundOperatorKind::Mul => current_int * int_mod,
                    ResolvedCompoundOperatorKind::Div => current_int / int_mod,
                    ResolvedCompoundOperatorKind::Modulo => Self::modulo(current_int, int_mod),
                };

                let new_value = Value::Int(new_result);
                {
                    let mut mutated = current_value.borrow_mut();
                    *mutated = new_value.clone();
                }
                new_value
            }

            ResolvedExpression::ArrayExtend(variable_ref, source_expression) => {
                let source_val = self.evaluate_expression(source_expression)?;

                let array_val_ref = self
                    .current_block_scopes
                    .lookup_variable_mut_ref(variable_ref)?;
                if let Value::Array(_type_id, ref mut vector) = &mut *array_val_ref.borrow_mut() {
                    if let Value::Array(_, items) = source_val {
                        vector.extend(items);
                    } else {
                        Err("Cannot extend non-array reference".to_string())?;
                    }
                } else {
                    Err("Cannot extend non-array reference".to_string())?;
                }

                array_val_ref.borrow().clone()
            }

            ResolvedExpression::ArrayPush(variable_ref, source_expression) => {
                let source_val = self.evaluate_expression(source_expression)?;
                let array_val = self
                    .current_block_scopes
                    .lookup_variable_mut_ref(variable_ref)?;

                if let Value::Array(_type_id, ref mut vector) = &mut *array_val.borrow_mut() {
                    vector.push(Rc::new(RefCell::new(source_val)));
                } else {
                    Err("Cannot extend non-array reference".to_string())?;
                }
                array_val.borrow().clone()
            }

            ResolvedExpression::ArrayRemoveIndex(variable_ref, usize_index_expression) => {
                let index_val = self.evaluate_expression(usize_index_expression)?;
                let Value::Int(index) = index_val else {
                    return Err(ExecuteError::ArgumentIsNotMutable(
                        variable_ref.name.clone(),
                    ));
                };
                let array_ref = self
                    .current_block_scopes
                    .lookup_variable_mut_ref(variable_ref)?;

                if let Value::Array(_type_id, ref mut vector) = &mut *array_ref.borrow_mut() {
                    vector.remove(index as usize);
                } else {
                    Err("Cannot extend non-array reference".to_string())?;
                }

                array_ref.borrow().clone()
            }

            ResolvedExpression::ArrayClear(variable_ref) => {
                let array_ref = self
                    .current_block_scopes
                    .lookup_variable_mut_ref(variable_ref)?;
                if let Value::Array(_type_id, ref mut vector) = &mut *array_ref.borrow_mut() {
                    vector.clear();
                } else {
                    Err("Cannot extend non-array reference".to_string())?;
                }
                array_ref.borrow().clone()
            }

            ResolvedExpression::ArrayAssignment(array, index, value) => {
                let array_ref = self.evaluate_expression_mut_location_start(&array.expression)?;
                let index_val = self.evaluate_expression(&index.expression)?;
                let new_val = self.evaluate_expression(value)?;
                let Value::Int(i) = index_val else {
                    return Err(ExecuteError::IndexWasNotInteger);
                };

                if let Value::Array(_type_id, ref mut elements) = &mut *array_ref.borrow_mut() {
                    if i < 0 || i >= elements.len() as i32 {
                        return Err(format!("Array index out of bounds: {i}"))?;
                    }
                    elements[i as usize] = Rc::new(RefCell::new(new_val.clone()));
                } else {
                    Err("Cannot index into non-array reference".to_string())?;
                }

                new_val
            }

            ResolvedExpression::MapAssignment(array, index, value) => {
                let map_val = self.evaluate_expression_mut_location_start(&array.expression)?;
                let index_val = self.evaluate_expression(&index.expression)?;
                let new_val = self.evaluate_expression(value)?;

                if let Value::Map(_type_id, ref mut elements) = &mut *map_val.borrow_mut() {
                    elements
                        .insert(index_val, Rc::new(RefCell::new(new_val.clone())))
                        .expect("todo: improve error handling");
                } else {
                    Err("Cannot index into non-array reference".to_string())?;
                }

                new_val
            }

            ResolvedExpression::StructFieldAssignment(
                resolved_struct_field_ref,
                lookups,
                source_expression,
            ) => self.location_assignment(resolved_struct_field_ref, lookups, source_expression)?,

            ResolvedExpression::FieldCompoundAssignment(
                resolved_struct_field_ref,
                lookups,
                compound_operator,
                source_expression,
            ) => self.location_assignment_compound(
                resolved_struct_field_ref,
                lookups,
                &compound_operator.kind,
                source_expression,
            )?,

            ResolvedExpression::AssignArrayRange(
                base_expr,
                _resolved_type,
                start_expr,
                end_expr,
                mode,
                assign,
            ) => {
                let start = self.evaluate_expression_mut_location_start(base_expr)?;

                let rhs_value = self.evaluate_expression(assign)?;
                if let Value::Array(source_array_type, rhs_values) = rhs_value {
                    let mut borrow = start.borrow_mut();
                    if let Value::Array(target_array_type, ref mut values) = &mut *borrow {
                        if !source_array_type
                            .item_type
                            .same_type(&target_array_type.item_type)
                        {
                            return Err(ExecuteError::IncompatiableTypes);
                        }
                        let (start, end) = self.evaluate_and_calculate_range(
                            start_expr,
                            end_expr,
                            mode,
                            values.len(),
                        )?;
                        let mut index = start;
                        for value_ref in &mut values[start..end] {
                            *value_ref = rhs_values[index].clone();
                            index += 1;
                        }
                        Value::Unit
                    } else {
                        return Err(ExecuteError::NotAnArray);
                    }
                } else {
                    return Err(ExecuteError::NotAnArray);
                }
            }

            ResolvedExpression::AssignStringRange(
                base_expr,
                start_expr,
                end_expr,
                mode,
                assign,
            ) => {
                let start = self.evaluate_expression_mut_location_start(base_expr)?;

                let rhs_value = self.evaluate_expression(assign)?;
                if let Value::String(source_string) = rhs_value {
                    let mut borrow = start.borrow_mut();
                    if let Value::String(ref mut target_string) = &mut *borrow {
                        let (start, end) = self.evaluate_and_calculate_range(
                            start_expr,
                            end_expr,
                            mode,
                            target_string.len(),
                        )?;
                        target_string.replace_range(start..end, &source_string);
                        Value::Unit
                    } else {
                        return Err(ExecuteError::NotAnArray);
                    }
                } else {
                    return Err(ExecuteError::NotAnArray);
                }
            }

            // ------------- LOOKUP ---------------------
            ResolvedExpression::VariableAccess(var) => {
                self.current_block_scopes.lookup_var_value(var)
            }

            ResolvedExpression::ConstantAccess(constant) => {
                self.constants.lookup_constant_value(constant.id).clone()
            }

            ResolvedExpression::MapIndexAccess(ref map_lookup) => {
                let map_ref = self.evaluate_expression(&map_lookup.map_expression)?;
                let index_val = self.evaluate_expression(&map_lookup.index_expression)?;

                let result = {
                    if let Value::Map(_type_id, ref seq_map) = map_ref {
                        let x = seq_map.get(&index_val);
                        x.map_or_else(
                            || Value::Option(None),
                            |v| Value::Option(Some(Box::from(v.clone()))),
                        )
                    } else {
                        return Err(ExecuteError::NotAMap);
                    }
                };
                result
            }

            ResolvedExpression::MapHas(box_expr, index_expr) => {
                let map_ref = self.evaluate_expression(box_expr)?;
                let index_val = self.evaluate_expression(index_expr)?;

                if let Value::Map(_type_id, ref seq_map) = map_ref {
                    let has_key = seq_map.contains_key(&index_val);
                    Value::Bool(has_key)
                } else {
                    return Err(ExecuteError::NotAMap);
                }
            }

            ResolvedExpression::MapRemove(map_expr, index_expr, _map_type_ref) => {
                let map_ref = self.evaluate_location(&map_expr, &vec![])?;
                let index_val = self.evaluate_expression(&index_expr)?;

                let result = {
                    let mut borrowed = map_ref.borrow_mut();
                    if let Value::Map(_type_id, ref mut seq_map) = &mut *borrowed {
                        let x = seq_map.remove(&index_val);
                        x.map_or_else(
                            || Value::Option(None),
                            |v| Value::Option(Some(Box::from(v))),
                        )
                    } else {
                        return Err(ExecuteError::NotAMap);
                    }
                };
                result
            }

            ResolvedExpression::FieldAccess(struct_field_access, _field_ref, access_list) => {
                self.evaluate_lookups(struct_field_access, access_list)?
            }

            ResolvedExpression::ArrayRangeAccess(
                base_expr,
                array_type_ref,
                start_expr,
                end_expr,
                mode,
            ) => self.evaluate_array_range_access(
                base_expr,
                array_type_ref,
                start_expr,
                end_expr,
                mode,
            )?,

            ResolvedExpression::StringRangeAccess(base_expr, start_expr, end_expr, mode) => {
                self.evaluate_string_range_access(base_expr, start_expr, end_expr, mode)?
            }

            ResolvedExpression::ArrayAccess(base_expr, _, access_list) => {
                self.evaluate_lookups(base_expr, access_list)?
                /*
                let array_ref =
                    self.evaluate_expression_mut_location_start(&array_item_ref.array_expression)?;
                let index_val = self.evaluate_expression(&array_item_ref.int_expression)?;
                let i = if let Value::Int(v) = index_val {
                    v
                } else {
                    return Err(ExecuteError::IndexWasNotInteger);
                };
                let result = {
                    if let Value::Array(_type_id, ref mut vector) = &mut *array_ref.borrow_mut() {
                        if i < 0 || i >= vector.len() as i32 {
                            return Err(format!("Array index out of bounds: {i}"))?;
                        }
                        vector[i as usize].borrow().clone()
                    } else {
                        return Err("Cannot extend non-array reference".to_string())?;
                    }
                };

                result

                     */
            }
            ResolvedExpression::MutVariableRef(var_ref) => self
                .current_block_scopes
                .lookup_var_value(&var_ref.variable_ref),

            ResolvedExpression::MutStructFieldRef(base_expr, _type, access_chain) => self
                .evaluate_location(base_expr, access_chain)?
                .borrow()
                .clone(),

            ResolvedExpression::MutArrayIndexRef(base_expr, _resolved_type, access_chain) => self
                .evaluate_location(base_expr, access_chain)?
                .borrow()
                .clone(),

            ResolvedExpression::MutMapIndexRef(map_expression, _map_type_ref, key_expression) => {
                let map_ref = self.evaluate_expression(&map_expression)?;
                let key_val = self.evaluate_expression(&key_expression)?;

                let result = {
                    if let Value::Map(_type_id, ref seq_map) = map_ref {
                        let x = seq_map.get(&key_val);
                        x.map_or_else(
                            || Value::Option(None),
                            |v| Value::Option(Some(Box::from(v.clone()))),
                        )
                    } else {
                        return Err(ExecuteError::NotAMap);
                    }
                };
                result
            }

            ResolvedExpression::MutRustTypeIndexRef(
                rust_type_expression,
                _rust_type_ref,
                _key_type,
                id_expression,
            ) => {
                let resolved_sparse_value = self.evaluate_expression(rust_type_expression)?;
                let sparse_value_map = resolved_sparse_value.downcast_rust::<SparseValueMap>();
                if let Some(found) = sparse_value_map {
                    let id_value = self.evaluate_expression(id_expression)?;
                    if let Some(found_id) = id_value.downcast_rust::<SparseValueId>() {
                        found.borrow_mut().get(&found_id.borrow()).map_or_else(
                            || Value::Option(None),
                            |found_value| Value::Option(Some(Box::new(found_value.clone()))),
                        )
                    } else {
                        return Err(ExecuteError::Error(
                            "not a SparseId, can not access".to_string(),
                        ));
                    }
                } else {
                    return Err(ExecuteError::Error(
                        "not a SparseId, can not access".to_string(),
                    ));
                }
            }

            // Operators
            ResolvedExpression::BinaryOp(binary_operator) => {
                let left_val = self.evaluate_expression(&binary_operator.left)?;
                let right_val = self.evaluate_expression(&binary_operator.right)?;
                Self::evaluate_binary_op(left_val, &binary_operator.kind, right_val)?
            }

            ResolvedExpression::UnaryOp(unary_operator) => {
                let left_val = self.evaluate_expression(&unary_operator.left)?;
                Self::evaluate_unary_op(&unary_operator.kind, left_val)?
            }

            ResolvedExpression::PostfixOp(postfix_operator) => {
                let left_val = self.evaluate_expression(&postfix_operator.left)?;
                Self::evaluate_postfix_op(&postfix_operator.kind, left_val)?
            }

            // Calling
            ResolvedExpression::FunctionCall(_signature, expr, arguments) => {
                self.evaluate_function_call(expr, arguments)?
            }

            ResolvedExpression::MemberCall(resolved_member_call) => {
                let mem_self_value = if resolved_member_call.self_is_mutable {
                    VariableValue::Reference(ValueReference(
                        self.evaluate_expression_mut_location_start(
                            &resolved_member_call.self_expression,
                        )?,
                    ))
                } else {
                    VariableValue::Value(
                        self.evaluate_expression(&resolved_member_call.self_expression)?,
                    )
                };

                let parameters = match &*resolved_member_call.function {
                    ResolvedFunction::Internal(function_data) => {
                        &function_data.signature.parameters
                    }
                    ResolvedFunction::External(external_data) => {
                        &external_data.signature.parameters
                    }
                };

                let mut member_call_arguments = Vec::new();
                member_call_arguments.push(mem_self_value); // Add self as first argument
                member_call_arguments.extend(self.evaluate_args(&resolved_member_call.arguments)?);

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
                        self.push_function_scope();
                        self.bind_parameters(
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
                        self.push_function_scope();
                        self.bind_parameters(&function_data.signature.parameters, &evaluated_args)?;
                        let result = self.evaluate_expression(&function_data.body)?;
                        self.pop_function_scope();
                        Ok(result)
                    }
                    ResolvedFunction::External(external) => {
                        let mut func = self
                            .externals
                            .external_functions_by_id
                            .get(&external.id)
                            .expect("call_generic: external function missing")
                            .borrow_mut();
                        (func.func)(&evaluated_args, self.context)
                    }
                }?
            }

            ResolvedExpression::Block(statements) => self.evaluate_block(statements)?.try_into()?,

            ResolvedExpression::InterpolatedString(parts) => {
                let mut result = String::new();

                for part in parts {
                    match part {
                        ResolvedStringPart::Literal(_resolved_node, text) => {
                            result.push_str(text);
                        }
                        ResolvedStringPart::Interpolation(expr, format_spec) => {
                            let value = self.evaluate_expression(expr)?;
                            let formatted = match format_spec {
                                Some(spec) => format_value(&value, &spec.kind)?,
                                None => value.convert_to_string_if_needed(),
                            };
                            result.push_str(&formatted);
                        }
                    }
                }

                Value::String(result)
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
                        self.push_block_scope();
                        self.current_block_scopes
                            .initialize_var_mut(variable, *inner_value);
                        let result = self.evaluate_expression(true_block)?;
                        self.pop_block_scope();
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
                if variable.is_mutable() {}
                let value = self.evaluate_expression(optional_expr)?;
                match value {
                    Value::Option(Some(inner_value)) => {
                        self.push_block_scope();
                        self.current_block_scopes
                            .initialize_var_mut(variable, *inner_value);
                        let result = self.evaluate_expression(true_block)?;
                        self.pop_block_scope();
                        result
                    }
                    Value::Option(None) => self.evaluate_expression(false_block)?,
                    _ => return Err(ExecuteError::ExpectedOptional),
                }
            }

            ResolvedExpression::Match(resolved_match) => self.eval_match(resolved_match)?,

            ResolvedExpression::Guard(guards, maybe_wildcard) => {
                self.eval_guard(guards, maybe_wildcard.as_deref())?
            }

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

            //ResolvedExpression::MutMemberCall(_, _) => todo!(),
            ResolvedExpression::Tuple(_) => todo!(),
            ResolvedExpression::LetVar(_, _) => todo!(),
            ResolvedExpression::Option(inner) => match inner {
                None => Value::Option(None),
                Some(expression) => {
                    let v = self.evaluate_expression(expression)?;
                    match v {
                        Value::Option(_) => {
                            panic!("unnecessary wrap!, should be investigated");
                        }
                        _ => Value::Option(Some(Box::from(Rc::new(RefCell::new(v))))),
                    }
                }
            },

            ResolvedExpression::NoneCoalesceOperator(base_expr, default_expr) => {
                let value = self.evaluate_expression(base_expr)?;
                if let Value::Option(found_option) = value {
                    match found_option {
                        None => self.evaluate_expression(default_expr)?,
                        Some(some_value) => {
                            // TODO: Verify type of value
                            some_value.borrow().clone()
                        }
                    }
                } else {
                    return Err(ExecuteError::ExpectedOptional);
                }
            }

            // --------------- SPECIAL FUNCTIONS
            ResolvedExpression::SparseNew(rust_type_ref, resolved_type) => {
                let sparse_value_map =
                    SparseValueMap::new(rust_type_ref.clone(), resolved_type.clone());
                to_rust_value(rust_type_ref.clone(), sparse_value_map)
            }

            ResolvedExpression::SparseAdd(sparse_rust, value_expression, _return_type_ref) => {
                let resolved_sparse_value = self.evaluate_expression(sparse_rust)?;

                let sparse_value_map = resolved_sparse_value.downcast_rust::<SparseValueMap>();
                if let Some(found) = sparse_value_map {
                    let resolved_value = self.evaluate_expression(value_expression)?;
                    let id_value = found.borrow_mut().add(resolved_value);

                    id_value
                } else {
                    return Err(ExecuteError::NotSparseValue);
                }
            }
            ResolvedExpression::SparseRemove(sparse_rust, id_expression, _) => {
                let resolved_sparse_value = self.evaluate_expression(sparse_rust)?;
                let sparse_value_map = resolved_sparse_value.downcast_rust::<SparseValueMap>();
                if let Some(found) = sparse_value_map {
                    let id_value = self.evaluate_expression(id_expression)?;
                    if let Some(found_id) = id_value.downcast_rust::<SparseValueId>() {
                        found.borrow_mut().remove(&found_id.borrow());
                    } else {
                        return Err(ExecuteError::Error(
                            "was not a sparse slot. can not remove".to_string(),
                        ));
                    }
                }

                resolved_sparse_value
            }
            ResolvedExpression::SparseAccess(sparse_rust, id_expression, _expected_type) => {
                let resolved_sparse_value = self.evaluate_expression(sparse_rust)?;
                let sparse_value_map = resolved_sparse_value.downcast_rust::<SparseValueMap>();
                if let Some(found) = sparse_value_map {
                    let id_value = self.evaluate_expression(id_expression)?;
                    if let Some(found_id) = id_value.downcast_rust::<SparseValueId>() {
                        if let Some(found_value) = found.borrow_mut().get(&found_id.borrow()) {
                            Value::Option(Some(Box::new(found_value.clone())))
                        } else {
                            Value::Option(None)
                        }
                    } else {
                        return Err(ExecuteError::Error(
                            "not a SparseId, can not access".to_string(),
                        ));
                    }
                } else {
                    return Err(ExecuteError::Error(
                        "not a SparseId, can not access".to_string(),
                    ));
                }
            }

            ResolvedExpression::CoerceOptionToBool(expression) => {
                let value = self.evaluate_expression(expression)?;
                match value {
                    Value::Option(inner) => Value::Bool(inner.is_some()),
                    _ => return Err(ExecuteError::CoerceOptionToBoolFailed),
                }
            }

            ResolvedExpression::FloatRound(expr) => {
                let value = self.evaluate_expression(expr)?;
                if let Value::Float(f) = value {
                    Value::Int(f.round().into())
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }
            ResolvedExpression::FloatFloor(expr) => {
                let value = self.evaluate_expression(expr)?;
                if let Value::Float(f) = value {
                    Value::Int(f.floor().into())
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }

            ResolvedExpression::FloatSign(expr) => {
                let value = self.evaluate_expression(expr)?;
                if let Value::Float(f) = value {
                    let signum = if f.inner() < 0 {
                        -1
                    } else if f.inner() > 0 {
                        1
                    } else {
                        0
                    };
                    Value::Float(Fp::from(signum as i16))
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }
            ResolvedExpression::FloatAbs(expr) => {
                let value = self.evaluate_expression(expr)?;
                if let Value::Float(f) = value {
                    Value::Float(f.abs())
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }

            ResolvedExpression::FloatCos(expr) => {
                let value = self.evaluate_expression(expr)?;
                if let Value::Float(f) = value {
                    Value::Float(f.cos())
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }

            ResolvedExpression::FloatAcos(expr) => {
                let value = self.evaluate_expression(expr)?;
                if let Value::Float(f) = value {
                    Value::Float(f.acos())
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }

            ResolvedExpression::FloatSin(expr) => {
                let value = self.evaluate_expression(expr)?;
                if let Value::Float(f) = value {
                    Value::Float(f.sin())
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }

            ResolvedExpression::FloatAsin(expr) => {
                let value = self.evaluate_expression(expr)?;
                if let Value::Float(f) = value {
                    Value::Float(f.asin())
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }

            ResolvedExpression::FloatSqrt(expr) => {
                let value = self.evaluate_expression(expr)?;
                if let Value::Float(f) = value {
                    Value::Float(f.sqrt())
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }

            ResolvedExpression::FloatMin(expr, min) => {
                let value = self.evaluate_expression(expr)?;
                let min_value = self.evaluate_expression(min)?;
                if let (Value::Float(f), Value::Float(min_f)) = (value, min_value) {
                    Value::Float(f.min(min_f))
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }

            ResolvedExpression::FloatMax(expr, max) => {
                let value = self.evaluate_expression(expr)?;
                let max_value = self.evaluate_expression(max)?;
                if let (Value::Float(f), Value::Float(max_f)) = (value, max_value) {
                    Value::Float(f.max(max_f))
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }

            ResolvedExpression::FloatAtan2(y, x) => {
                let y_value = self.evaluate_expression(y)?;
                let x_value = self.evaluate_expression(x)?;
                if let (Value::Float(_y_f), Value::Float(_x_f)) = (y_value, x_value) {
                    Value::Float(Fp::from(-9999)) //y_f.atan2(x_f)) // TODO: Implement atan2
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }

            ResolvedExpression::FloatClamp(v, min, max) => {
                let v_value = self.evaluate_expression(v)?;
                let min_value = self.evaluate_expression(min)?;
                let max_value = self.evaluate_expression(max)?;
                if let (Value::Float(f), Value::Float(min_f), Value::Float(max_f)) =
                    (v_value, min_value, max_value)
                {
                    Value::Float(f.clamp(min_f, max_f))
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }

            ResolvedExpression::FloatRnd(float_expr) => {
                let value = self.evaluate_expression(float_expr)?;
                if let Value::Float(f) = value {
                    let new_raw = squirrel_prng::squirrel_noise5(f.inner() as u32, 0);
                    Value::Int(new_raw as i32)
                } else {
                    return Err(ExecuteError::TypeError("Expected float".to_string()));
                }
            }

            ResolvedExpression::IntAbs(int_expr) => {
                let value = self.evaluate_expression(int_expr)?;
                if let Value::Int(i) = value {
                    Value::Int(i.abs())
                } else {
                    return Err(ExecuteError::TypeError("Expected int".to_string()));
                }
            }

            ResolvedExpression::IntClamp(v, min, max) => {
                let v_value = self.evaluate_expression(v)?;
                let min_value = self.evaluate_expression(min)?;
                let max_value = self.evaluate_expression(max)?;
                if let (Value::Int(i), Value::Int(min_i), Value::Int(max_i)) =
                    (v_value, min_value, max_value)
                {
                    Value::Int(i.clamp(min_i, max_i))
                } else {
                    return Err(ExecuteError::TypeError("Expected int".to_string()));
                }
            }

            ResolvedExpression::IntMin(expr, max) => {
                let value = self.evaluate_expression(expr)?;
                let max_value = self.evaluate_expression(max)?;
                if let (Value::Int(i), Value::Int(min_i)) = (value, max_value) {
                    Value::Int(i.min(min_i))
                } else {
                    return Err(ExecuteError::TypeError("Expected int".to_string()));
                }
            }

            ResolvedExpression::IntMax(expr, max) => {
                let value = self.evaluate_expression(expr)?;
                let max_value = self.evaluate_expression(max)?;
                if let (Value::Int(i), Value::Int(max_i)) = (value, max_value) {
                    Value::Int(i.max(max_i))
                } else {
                    return Err(ExecuteError::TypeError("Expected int".to_string()));
                }
            }

            ResolvedExpression::IntRnd(int_expr) => {
                let value = self.evaluate_expression(int_expr)?;
                if let Value::Int(i) = value {
                    Value::Int(squirrel_prng::squirrel_noise5(i as u32, 0) as i32)
                } else {
                    return Err(ExecuteError::TypeError("Expected int".to_string()));
                }
            }

            ResolvedExpression::IntToFloat(int_expr) => {
                let value = self.evaluate_expression(int_expr)?;
                if let Value::Int(i) = value {
                    Value::Float(Fp::from(i as i16))
                } else {
                    return Err(ExecuteError::TypeError("Expected int".to_string()));
                }
            }

            ResolvedExpression::StringLen(string_expr) => {
                let value = self.evaluate_expression(string_expr)?;
                if let Value::String(s) = value {
                    Value::Int(s.len().try_into().expect("string len overflow"))
                } else {
                    return Err(ExecuteError::TypeError("Expected string".to_string()));
                }
            }

            ResolvedExpression::If(condition, consequences, optional_alternative) => {
                let cond_value = self.evaluate_expression(&condition.expression)?;
                if cond_value.is_truthy()? {
                    self.evaluate_expression(consequences)?
                } else if let Some(alternative) = optional_alternative {
                    self.evaluate_expression(alternative)?
                } else {
                    Value::Unit
                }
            }

            ResolvedExpression::TupleDestructuring(variable_refs, _, expr) => {
                let value = self.evaluate_expression(expr)?;
                if let Value::Tuple(_tuple_ref, values) = value {
                    if variable_refs.len() > values.len() {
                        return Err(ExecuteError::NotAnArray);
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
        };

        Ok(value)
    }

    fn eval_guard(
        &mut self,
        guards: &[ResolvedGuard],
        maybe_wildcard: Option<&ResolvedExpression>,
    ) -> Result<Value, ExecuteError> {
        for guard in guards {
            if self
                .evaluate_expression(&guard.condition.expression)?
                .is_truthy()?
            {
                return Ok(self.evaluate_expression(&guard.result)?);
            }
        }

        self.evaluate_expression(
            &maybe_wildcard.expect("must have wildcard '_' if nothing matches in the guard"),
        )
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
                            .is_truthy()?
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
                                        self.current_block_scopes
                                            .set_local_var_value(var_ref, actual_value.clone());
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
                                    if variant_ref.number != value_tuple_type.common.number {
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
                                                            .set_local_var_value(var_ref, value.clone());
                                                    }
                                                    ResolvedPatternElement::VariableWithFieldIndex(
                                                        var_ref,
                                                        _,
                                                    ) => {
                                                        self.current_block_scopes
                                                            .set_local_var_value(var_ref, value.clone());
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
                                Value::EnumVariantStruct(value_struct_type, values) => {
                                    if value_struct_type.common.number == variant_ref.number {
                                        if let Some(elements) = maybe_elements {
                                            self.push_block_scope();

                                            for element in elements {
                                                if let ResolvedPatternElement::VariableWithFieldIndex(
                                                    var_ref,
                                                    field_index,
                                                ) = element
                                                {
                                                    let value = &values[*field_index];
                                                    self.current_block_scopes
                                                        .set_local_var_value(var_ref, value.clone());
                                                }
                                            }

                                            let result = self.evaluate_expression(&arm.expression);
                                            self.pop_block_scope();
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
                                _ => {}
                            }
                        }

                        ResolvedNormalPattern::Literal(lit) => match (lit, &actual_value) {
                            (ResolvedLiteral::IntLiteral(a, _resolved_node), Value::Int(b))
                                if a == b =>
                            {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (ResolvedLiteral::FloatLiteral(a, _resolved_node), Value::Float(b))
                                if a == b =>
                            {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (
                                ResolvedLiteral::StringLiteral(a, _resolved_node),
                                Value::String(b),
                            ) if *a == *b => {
                                return self.evaluate_expression(&arm.expression);
                            }
                            (ResolvedLiteral::BoolLiteral(a, _resolved_node), Value::Bool(b))
                                if a == b =>
                            {
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

        Err(ExecuteError::Error(
            "must match one of the match arms!".to_string(),
        ))
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

    fn evaluate_binary_op(
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
                    return Err("Division by zero".to_string())?;
                }
                Value::Int(a / b)
            }
            (Value::Int(a), ResolvedBinaryOperatorKind::Modulo, Value::Int(b)) => {
                Value::Int(Self::modulo(*a, *b))
            }

            // Float operations
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
                    return Err("Division by zero".to_string())?;
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
                Value::String(a.to_string() + &b)
            }

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
                return Err(
                    format!("Invalid binary operation {op:?} {left_val:?} {right_val:?}").into(),
                )
            }
        };

        Ok(result)
    }

    fn evaluate_unary_op(
        op: &ResolvedUnaryOperatorKind,
        val: Value,
    ) -> Result<Value, ExecuteError> {
        match (op, val) {
            (ResolvedUnaryOperatorKind::Negate, Value::Int(n)) => Ok(Value::Int(-n)),
            (ResolvedUnaryOperatorKind::Negate, Value::Float(n)) => Ok(Value::Float(-n)),
            (ResolvedUnaryOperatorKind::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            _ => Err("Invalid unary operation".to_string())?,
        }
    }

    fn evaluate_postfix_op(
        op: &ResolvedPostfixOperatorKind,
        val: Value,
    ) -> Result<Value, ExecuteError> {
        match op {
            ResolvedPostfixOperatorKind::Unwrap => Self::evaluate_unwrap_op(val),
        }
    }

    #[inline]
    fn evaluate_unwrap_op(val: Value) -> Result<Value, ExecuteError> {
        match val {
            Value::Option(ref unwrapped_boxed_opt) => match unwrapped_boxed_opt {
                Some(value) => Ok(value.borrow().clone()),
                None => Ok(val),
            },
            _ => Err(ExecuteError::CanNotUnwrap),
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

    /// Get the `ValueRef` "location" instead of a Value
    fn evaluate_expression_mut_location_start(
        &mut self,
        expr: &ResolvedExpression,
    ) -> Result<ValueRef, ExecuteError> {
        // TODO: We need support for more locations
        match expr {
            ResolvedExpression::VariableAccess(var_ref) => {
                Ok(self.current_block_scopes.lookup_mut_variable(var_ref)?)
            }
            ResolvedExpression::FieldAccess(base_expression, _type_ref, resolved_access) => {
                //info!(?base_expression, "base expression for field access");
                let start = self.evaluate_expression_mut_location_start(base_expression)?;
                self.get_location(start, resolved_access)
            }
            ResolvedExpression::ArrayAccess(base_expression, _array_item_ref, resolved_access) => {
                //info!(?base_expression, "base expression for field access");
                let start = self.evaluate_expression_mut_location_start(base_expression)?;
                self.get_location(start, resolved_access)
            }

            _ => Err(ExecuteError::NotMutLocationFound),
        }
    }

    fn evaluate_location(
        &mut self,
        expr: &ResolvedExpression,
        lookups: &[ResolvedAccess],
    ) -> Result<ValueRef, ExecuteError> {
        let start = self.evaluate_expression_mut_location_start(expr)?;
        self.get_location(start, lookups)
    }

    #[inline]
    fn evaluate_lookups(
        &mut self,
        expr: &ResolvedExpression,
        lookups: &[ResolvedAccess],
    ) -> Result<Value, ExecuteError> {
        let value = self.evaluate_expression(expr)?;
        self.get_value_from_lookups(value, lookups)
    }

    fn get_next_location(
        &mut self,
        start: &ValueRef,
        lookup: &ResolvedAccess,
    ) -> Result<ValueRef, ExecuteError> {
        let next_val_ref = match lookup {
            ResolvedAccess::FieldIndex(_resolved_node, i) => {
                let field_index = *i;

                let borrowed = start.borrow();
                let next_val = match &*borrowed {
                    Value::Struct(_struct_type, fields) => {
                        fields.get(field_index).ok_or_else(|| {
                            ExecuteError::TypeError("Field index out of range".to_string())
                        })?
                    }
                    _ => {
                        return Err(ExecuteError::TypeError(
                            "field assignment: Expected struct".to_string(),
                        ));
                    }
                };

                next_val.clone()
            }

            ResolvedAccess::ArrayIndex(index_expr) => {
                let index_value = self.evaluate_expression(index_expr)?;
                let index_int = index_value.expect_int()? as usize;
                let borrowed = start.borrow();
                let next_val = match &*borrowed {
                    Value::Array(_array_type, fields) => {
                        fields.get(index_int).ok_or_else(|| {
                            ExecuteError::TypeError("Field index out of range".to_string())
                        })?
                    }
                    _ => {
                        return Err(ExecuteError::TypeError("Expected array".to_string()));
                    }
                };

                next_val.clone()
            }

            ResolvedAccess::MapIndex(index_expr) => {
                let index_value = self.evaluate_expression(index_expr)?;

                let borrowed = start.borrow();
                let next_val = match &*borrowed {
                    Value::Map(_map_type, seq_map) => {
                        seq_map.get(&index_value).ok_or_else(|| {
                            ExecuteError::TypeError("key value not found in map".to_string())
                        })?
                    }
                    _ => {
                        return Err(ExecuteError::TypeError("Expected array".to_string()));
                    }
                };

                next_val.clone()
            }
        };

        Ok(next_val_ref)
    }

    fn get_location(
        &mut self,
        start: ValueRef,
        lookups: &[ResolvedAccess],
    ) -> Result<ValueRef, ExecuteError> {
        let mut next_ref = start;
        for lookup in lookups {
            next_ref = self.get_next_location(&next_ref, lookup)?;
        }
        Ok(next_ref)
    }

    fn get_next_value_from_lookup(
        &mut self,
        start: Value,
        lookup: &ResolvedAccess,
    ) -> Result<Value, ExecuteError> {
        let next_value: Value = match lookup {
            ResolvedAccess::FieldIndex(_resolved_node, i) => {
                let field_index = *i;

                match start {
                    Value::Struct(_struct_type, fields) => fields
                        .get(field_index)
                        .ok_or_else(|| {
                            ExecuteError::TypeError("Field index out of range".to_string())
                        })?
                        .borrow()
                        .clone(),
                    _ => {
                        return Err(ExecuteError::TypeError(
                            "field assignment: Expected struct".to_string(),
                        ));
                    }
                }
            }

            ResolvedAccess::ArrayIndex(index_expr) => {
                let index_value = self.evaluate_expression(index_expr)?;
                let index_int = index_value.expect_int()? as usize;

                match start {
                    Value::Array(_array_type, fields) => fields
                        .get(index_int)
                        .ok_or_else(|| {
                            ExecuteError::TypeError("Field index out of range".to_string())
                        })?
                        .borrow()
                        .clone(),
                    _ => {
                        return Err(ExecuteError::TypeError("Expected array".to_string()));
                    }
                }
            }

            ResolvedAccess::MapIndex(index_expr) => {
                let index_value = self.evaluate_expression(index_expr)?;

                match start {
                    Value::Map(_map_type, seq_map) => seq_map
                        .get(&index_value)
                        .ok_or_else(|| {
                            ExecuteError::TypeError("key value not found in map".to_string())
                        })?
                        .borrow()
                        .clone(),
                    _ => {
                        return Err(ExecuteError::TypeError("Expected array".to_string()));
                    }
                }
            }
        };

        Ok(next_value)
    }

    fn get_value_from_lookups(
        &mut self,
        start: Value,
        lookups: &[ResolvedAccess],
    ) -> Result<Value, ExecuteError> {
        let mut value = start;
        for lookup in lookups {
            value = self.get_next_value_from_lookup(value, lookup)?;
        }
        Ok(value)
    }

    fn location_assignment(
        &mut self,
        start_expression: &ResolvedExpression,
        lookups: &[ResolvedAccess],
        source_expression: &ResolvedExpression,
    ) -> Result<Value, ExecuteError> {
        let source = self.evaluate_expression(source_expression)?;
        let value_ref = self.evaluate_location(start_expression, lookups)?;

        *value_ref.borrow_mut() = source.clone();

        Ok(source)
    }

    fn location_assignment_compound(
        &mut self,
        start_expression: &ResolvedExpression,
        lookups: &[ResolvedAccess],
        operator: &ResolvedCompoundOperatorKind,
        source_expression: &ResolvedExpression,
    ) -> Result<Value, ExecuteError> {
        let source = self.evaluate_expression(source_expression)?;
        let value_ref = self.evaluate_location(start_expression, lookups)?;

        Self::apply_compound_operator(&mut value_ref.borrow_mut(), operator, &source)?;
        let temp = value_ref.borrow();
        Ok(temp.clone())
    }

    #[inline(always)]
    fn apply_compound_operator(
        target: &mut Value,
        operator: &ResolvedCompoundOperatorKind,
        source: &Value,
    ) -> Result<(), ExecuteError> {
        match operator {
            ResolvedCompoundOperatorKind::Mul => {
                *target = Self::evaluate_binary_op(
                    target.clone(),
                    &ResolvedBinaryOperatorKind::Multiply,
                    source.clone(),
                )?;
            }
            ResolvedCompoundOperatorKind::Div => {
                *target = Self::evaluate_binary_op(
                    target.clone(),
                    &ResolvedBinaryOperatorKind::Divide,
                    source.clone(),
                )?;
            }
            ResolvedCompoundOperatorKind::Add => {
                *target = Self::evaluate_binary_op(
                    target.clone(),
                    &ResolvedBinaryOperatorKind::Add,
                    source.clone(),
                )?;
            }
            ResolvedCompoundOperatorKind::Sub => {
                *target = Self::evaluate_binary_op(
                    target.clone(),
                    &ResolvedBinaryOperatorKind::Subtract,
                    source.clone(),
                )?;
            }
            ResolvedCompoundOperatorKind::Modulo => {
                *target = Self::evaluate_binary_op(
                    target.clone(),
                    &ResolvedBinaryOperatorKind::Modulo,
                    source.clone(),
                )?;
            }
        }
        Ok(())
    }

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
            Err(ExecuteError::NotAnArray)
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
            Err(ExecuteError::ExpectedInt)
        }
    }

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
            Err(ExecuteError::ExpectedString)
        }
    }
}
