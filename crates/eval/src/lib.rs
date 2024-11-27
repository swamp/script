/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::value::{SwampExport, Value};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use swamp_script_semantic::module::Module;
pub use swamp_script_semantic::ns::{ResolvedEnumVariantContainerType, ResolvedModuleNamespace};
use swamp_script_semantic::{
    BinaryOperator, LocalTypeIdentifier, ModulePath, Parameter, ResolvedEnumLiteralData,
    ResolvedExpression, ResolvedInternalFunctionCall, ResolvedInternalFunctionDefinitionRef,
    ResolvedLiteral, ResolvedMatch, ResolvedMatchArm, ResolvedParameter, ResolvedPattern,
    ResolvedProgram, ResolvedStatement, ResolvedStringPart, ResolvedType, ResolvedVariable,
    UnaryOperator, Variable,
};
use tracing::{debug, error, trace};
use value::format_value;

mod def;
mod func;
pub mod value;

pub struct SwampFunction {
    pub name: &'static str,
    pub handler: Box<dyn Fn(&[Value]) -> Result<Value, String> + Send + Sync>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExecuteError {
    Error(String),
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

#[derive(Debug, Default)]
enum ScopeType {
    #[default]
    Function, // Never looks up
    Block, // Can look up to parent (for loops, while loops, etc)
}

#[derive(Default, Debug)]
struct Scope {
    variables: HashMap<String, Value>,
    scope_type: ScopeType,
}

pub struct Interpreter {
    scope_stack: Vec<Scope>,
    output: Rc<RefCell<Vec<String>>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let output = Rc::new(RefCell::new(Vec::new()));
        let mut interpreter = Self {
            scope_stack: vec![Scope::default()],
            output: output.clone(),
        };

        interpreter.register_builtins();

        interpreter
    }

    pub fn register_rust_type<T: SwampExport + 'static>(&mut self) {
        let def = T::generate_swamp_definition();
        self.load_external_definitions(&def)
            .expect("Failed to load generated type definition");
    }

    fn load_external_definitions(&mut self, definitions: &str) -> Result<(), String> {
        let _ast = self // TODO: FIX ME
            .parser
            .parse_script(definitions)
            .map_err(|e| format!("Failed to parse external definitions: {}", e))?;

        Ok(())
    }

    fn push_scope(&mut self, scope_type: ScopeType) {
        trace!("push scope: {:?}", scope_type);
        self.scope_stack.push(Scope {
            variables: HashMap::new(),
            scope_type,
        });
    }

    fn pop_scope(&mut self) {
        trace!("pop scope");
        if self.scope_stack.len() == 1 {
            error!("you popped too far");
            panic!("you popped too far");
        }
        self.scope_stack.pop();
    }

    fn lookup_in_scope(&self, name: &str) -> Option<&Value> {
        let current_scope = self.scope_stack.last().unwrap();
        match current_scope.scope_type {
            ScopeType::Function => {
                trace!("lookup_in_scope -> Function for {name:?}");
                // Only look in current scope
                current_scope.variables.get(name)
            }
            ScopeType::Block => {
                trace!("lookup_in_scope -> Block for {name:?}");
                // Look through scopes until we hit a Function scope
                for scope in self.scope_stack.iter().rev() {
                    trace!("...checking in scope {scope:?}");
                    if let Some(value) = scope.variables.get(name) {
                        return Some(value);
                    }
                    if matches!(scope.scope_type, ScopeType::Function) {
                        break;
                    }
                }
                None
            }
        }
    }

    fn set_existing_var_or_create_new_one(&mut self, name: String, value: Value, is_mutable: bool) {
        // First check if we need to look in outer scopes
        let scope_index = if matches!(
            self.scope_stack.last().unwrap().scope_type,
            ScopeType::Block
        ) {
            // Find the scope that contains the variable
            let mut found_index = None;
            for (i, scope) in self.scope_stack.iter().rev().enumerate() {
                if scope.variables.contains_key(&name) {
                    found_index = Some(self.scope_stack.len() - 1 - i);
                    break;
                }
                if matches!(scope.scope_type, ScopeType::Function) {
                    break;
                }
            }
            found_index
        } else {
            // In function scope, only look at current scope
            if self
                .scope_stack
                .last()
                .unwrap()
                .variables
                .contains_key(&name)
            {
                Some(self.scope_stack.len() - 1)
            } else {
                None
            }
        };

        // Now modify the found scope or create new variable
        if let Some(index) = scope_index {
            let scope = &mut self.scope_stack[index];
            let existing = scope.variables.get_mut(&name).unwrap();
            match existing {
                Value::Reference(r) => {
                    *r.borrow_mut() = match value {
                        Value::Reference(new_r) => new_r.borrow().clone(),
                        _ => value,
                    };
                }
                _ => *existing = value,
            }
        } else {
            // Create new variable in current scope
            let bound_value = if is_mutable && !matches!(value, Value::Reference(_)) {
                Value::Reference(Rc::new(RefCell::new(value)))
            } else {
                value
            };
            self.scope_stack
                .last_mut()
                .unwrap()
                .variables
                .insert(name, bound_value);
        }
    }
    fn bind_parameters(
        &mut self,
        params: &[ResolvedParameter],
        args: Vec<Value>,
    ) -> Result<(), String> {
        for (param, arg) in params.iter().zip(args) {
            let value = if param.is_mutable {
                match arg {
                    Value::Reference(r) => {
                        // For mutable parameters, use the SAME reference
                        Value::Reference(r)
                    }
                    v => Value::Reference(Rc::new(RefCell::new(v))),
                }
            } else {
                match arg {
                    Value::Reference(r) => r.borrow().clone(),
                    v => v,
                }
            };

            self.scope_stack
                .last_mut()
                .unwrap()
                .variables
                .insert(param.variable.name().to_string(), value);
        }

        Ok(())
    }
    fn register_builtins(&mut self) {
        let output = self.output.clone();
        let print_fn = FunctionRef::External(
            LocalTypeIdentifier::new("print"),
            (
                vec![ResolvedParameter {
                    variable: ResolvedVariable::new("value", false),
                    param_type: ResolvedType::Any,
                    is_mutable: false,
                }],
                ResolvedType::Unit,
            ),
            Rc::new(Box::new(move |args: &[Value]| {
                if let Some(value) = args.first() {
                    let display_value = value.to_string();
                    output.borrow_mut().push(display_value.clone());
                    println!("{}", display_value);
                    Ok(Value::Unit)
                } else {
                    Err("print requires at least one argument".to_string())?
                }
            })),
        );

        // Add directly to the current module's namespace values
        self.current_module
            .borrow_mut()
            .namespace
            .values
            .insert("print".to_string(), Value::InternalFunction(print_fn));
    }

    pub fn register_external_function(
        &mut self,
        name: &QualifiedTypeIdentifier,
        params: Vec<ResolvedParameter>,
        return_type: ResolvedType,
        handler: Box<dyn Fn(&[Value]) -> Result<Value, ExecuteError> + Send + Sync>,
    ) -> Result<(), String> {
        let func_ref = FunctionRef::External(
            LocalTypeIdentifier::new(&*name.0),
            (params.clone(), return_type.clone()),
            Rc::new(handler),
        );

        // Add to current module's namespace
        self.current_module.borrow_mut().namespace.add_function(
            name.to_string(),
            (params, return_type),
            func_ref,
        )
    }

    // Signals can not travel out of a function call, so only return Value
    fn evaluate_internal_function_call(
        &mut self,
        call: &ResolvedInternalFunctionCall,
    ) -> Result<Value, ExecuteError> {
        let func_val = self.evaluate_expression(&call.function_expression)?;

        let func_ref = match func_val {
            Value::InternalFunction(f) => f,
            _ => return Err("Expected a function".to_string())?,
        };

        // Check mutability requirements
        for (param, arg) in call.arguments.iter().zip(args) {
            if param.is_mutable() {
                match arg {
                    ResolvedExpression::MutRef(_) => (), // This is fine
                    ResolvedExpression::VariableAccess(var) => {
                        return Err(format!("Parameter '{}' is marked as mut but variable '{}' is passed without mut keyword",
                                           param.variable.name(), var.name()))?;
                    }
                    _ => {
                        return Err(format!(
                            "Parameter '{}' is marked as mut but called without mut keyword",
                            param.variable.name()
                        ))?
                    }
                }
            }
        }

        let evaluated_args = self.evaluate_args(args)?;
        debug!("call {:?}", func_ref);
        func_ref.execute(self, evaluated_args)
    }

    fn definitions(&mut self, definitions: &Vec<Definition>) -> Result<(), String> {
        let mut namespace = ResolvedModuleNamespace::new();

        debug!("starting creating definitions");

        for def in definitions {
            DefinitionRunner::execute_definition(&mut namespace, def)?;
        }

        let new_module = Module::new(
            None,
            LocalTypeIdentifier::new(&*"main".to_string()),
            namespace,
        );

        let main_module = Rc::new(RefCell::new(new_module));

        self.current_module = main_module.clone();

        debug!("definitions done");

        Ok(())
    }

    pub fn eval_program(
        &mut self,
        program: ResolvedProgram,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let module = program
            .modules
            .get(&ModulePath(vec![]))
            .expect("main module should exist");

        self.definitions(&module.definitions)?;

        self.register_builtins();

        Ok(self.interpret(&module.statements)?)
    }

    fn interpret(
        &mut self,
        statements: &Vec<ResolvedStatement>,
    ) -> Result<ValueWithSignal, ExecuteError> {
        self.execute_statements(statements)
    }

    fn tabs(&self) -> String {
        "..".repeat(self.scope_stack.len() - 1)
    }

    #[inline]
    fn execute_statements(
        &mut self,
        statements: &Vec<ResolvedStatement>,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let mut value = Value::Unit;

        for statement in statements {
            trace!("{} exec {statement:?}", self.tabs());

            // First handle signal aware statements
            match statement {
                ResolvedStatement::Continue => return Ok(ValueWithSignal::Continue),
                ResolvedStatement::Break => return Ok(ValueWithSignal::Break),
                ResolvedStatement::Return(expr) => {
                    return Ok(ValueWithSignal::Return(self.evaluate_expression(expr)?));
                }

                ResolvedStatement::WhileLoop(condition, body) => {
                    while self.evaluate_expression(condition)?.as_bool()? {
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
                    let cond_value = self.evaluate_expression(condition)?;
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
                    let iterator = self.evaluate_expression(iterator_expr)?;
                    match iterator {
                        Value::ExclusiveRange(start, end) => {
                            // Push a new scope for the loop
                            self.push_scope(ScopeType::Block);

                            for i in *start..*end {
                                match var_pattern {
                                    ResolvedPattern::VariableAssignment(ident) => {
                                        // Use scope_stack instead of variables
                                        self.scope_stack
                                            .last_mut()
                                            .unwrap()
                                            .variables
                                            .insert(ident.name().to_string(), Value::Int(i));
                                    }
                                    _ => return Err("Expected identifier in for loop".to_string())?,
                                }

                                match self.execute_statements(body)? {
                                    ValueWithSignal::Value(_v) => {} // ignore normal values
                                    ValueWithSignal::Return(v) => {
                                        return Ok(ValueWithSignal::Return(v))
                                    } //  Value::Void?
                                    ValueWithSignal::Break => {
                                        break;
                                    }
                                    ValueWithSignal::Continue => {}
                                }
                            }

                            // Pop the loop scope
                            self.pop_scope();
                        }

                        Value::Array(_item_swamp_type, elements) => {
                            // Push a new scope for the loop
                            self.push_scope(ScopeType::Block);

                            for element in elements {
                                match var_pattern {
                                    ResolvedPattern::VariableAssignment(ident) => {
                                        self.scope_stack
                                            .last_mut()
                                            .unwrap()
                                            .variables
                                            .insert(ident.name().to_string(), element);
                                    }
                                    _ => return Err("Expected identifier in for loop".to_string())?,
                                }

                                self.execute_statements(body)?;
                            }
                        }
                        Value::Int(_) => todo!(),
                        Value::Float(_) => todo!(),
                        Value::String(_) => todo!(),
                        Value::Bool(_) => todo!(),
                        Value::Tuple(_, _) => todo!(),
                        Value::Struct(_, _) => todo!(),
                        Value::Unit => todo!(),
                        Value::EnumVariant(_, _) => todo!(),
                        Value::Reference(_) => todo!(),
                        Value::InternalFunction(_) => todo!(),
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
                    self.set_existing_var_or_create_new_one(
                        var.name().to_string(),
                        value,
                        var.is_mutable,
                    );
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
            }
        }

        Ok(ValueWithSignal::Value(value))
    }

    fn evaluate_args(&mut self, args: &[ResolvedExpression]) -> Result<Vec<Value>, ExecuteError> {
        let mut evaluated = Vec::with_capacity(args.len());

        for arg in args {
            match arg {
                Expression::MutRef(var_ref) => {
                    // When we get a mut ref, return the reference directly
                    match self
                        .lookup_in_scope(&var_ref.0.name())
                        .ok_or_else(|| format!("Variable '{}' not found", var_ref.0.name()))?
                    {
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

    fn evaluate_expression(&mut self, expr: &ResolvedExpression) -> Result<Value, ExecuteError> {
        match expr {
            // Constructing
            ResolvedExpression::Literal(lit) => Ok(match lit {
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
                                Value::Tuple(tuple_type.clone(), eval_expressions)
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

                        ResolvedEnumVariantContainerType::Nothing => Value::Unit,
                    };

                    Value::EnumVariant(enum_variant_type.clone(), Box::new(variant_container_value))
                }

                ResolvedLiteral::TupleLiteral(tuple_type, resolved_expressions) => {
                    let values = self.evaluate_expressions(resolved_expressions)?;
                    Value::Tuple(tuple_type, values)
                }

                ResolvedLiteral::UnitLiteral(_) => Value::Unit,
            }),

            ResolvedExpression::Array(elements) => {
                let mut values = Vec::new();
                for element in elements {
                    values.push(self.evaluate_expression(element)?);
                }
                let array_type = if values.is_empty() {
                    ResolvedType::Any // empty array has all array types
                } else {
                    values[0].swamp_type_id()
                };

                Ok(Value::Array(array_type, values))
            }

            ResolvedExpression::StructInstantiation(struct_instantiation) => {
                // Evaluate all field expressions and validate types
                let mut field_values = Vec::new();
                for field_expr in struct_instantiation.expressions_in_order {
                    let value = self.evaluate_expression(&field_expr)?;
                    field_values.push(value);
                }

                Ok(Value::Struct(
                    struct_instantiation.struct_type_ref.clone(),
                    field_values,
                ))
            }

            ResolvedExpression::ExclusiveRange(start, end) => {
                let start_val = self.evaluate_expression(start)?;
                let end_val = self.evaluate_expression(end)?;
                match (start_val, end_val) {
                    (Value::Int(s), Value::Int(e)) => {
                        Ok(Value::ExclusiveRange(Box::new(s), Box::new(e)))
                    }
                    _ => Err("Range bounds must be integers".to_string())?,
                }
            }

            // ==================== ASSIGNMENT ====================
            ResolvedExpression::VariableAssignment(resolved_var_assingment) => {
                let new_value = self.evaluate_expression(value_expr)?;

                if let ResolvedExpression::VariableAccess(name) = &**target {
                    match self.scope_stack.last().unwrap().variables.get(name.name()) {
                        Some(Value::Reference(r)) => {
                            *r.borrow_mut() = new_value.clone();
                            Ok(new_value)
                        }
                        Some(_) => Err(format!(
                            "Cannot assign to immutable variable: {}",
                            name.name()
                        ))?,
                        None => Err(format!("Variable not found: {}", name.name()))?,
                    }
                } else {
                    Err("Invalid assignment target".to_string())?
                }
            }

            ResolvedExpression::ArrayAssignment(array, index, value) => {
                let array_val = self.evaluate_expression(array)?;
                let index_val = self.evaluate_expression(index)?;
                let new_val = self.evaluate_expression(value)?;

                match (array_val, index_val) {
                    (Value::Reference(r), Value::Int(i)) => {
                        if let Value::Array(_type_id, ref mut elements) = &mut *r.borrow_mut() {
                            if i < 0 || i >= elements.len() as i32 {
                                return Err(format!("Array index out of bounds: {}", i))?;
                            }
                            elements[i as usize] = new_val.clone();
                            Ok(Value::Unit)
                        } else {
                            Err("Cannot index into non-array reference".to_string())?
                        }
                    }
                    (arr, idx) => Err(format!(
                        "Invalid array assignment: cannot index {:?} with {:?}",
                        arr, idx
                    ))?,
                }
            }

            ResolvedExpression::StructFieldAssignment(resolved_struct_field_ref, source_expression) => {
                let target = self.evaluate_expression(&resolved_struct_field_ref.target_expression)?;
                let value = self.evaluate_expression(source_expression)?;

                match target {
                    Value::Reference(r) => {
                        let mut borrowed = r.borrow_mut();
                        // We know it must be a struct because references can only point to structs
                        match &mut *borrowed {
                            Value::Struct(struct_type, fields) => {

                                if let Some(field) = fields.get_mut(resolved_struct_field_ref.index) {
                                    *field = value.clone();
                                    Ok(value)
                                } else {
                                    Err(format!(
                                        "Field '{}' not found in struct '{:?}'",
                                        resolved_struct_field_ref.index, struct_type
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
                        resolved_struct_field_ref.index
                    ))?,
                }
            }

            // ------------- LOOKUP ---------------------
            ResolvedExpression::VariableAccess(var) => {
                // First check local scope
                if let Some(value) = self.lookup_in_scope(&var.name) {
                    trace!("found lookup: {var:?} {value:?}");

                    return Ok(value.clone());
                }

                // Then check module namespace
                /* TODO:
                let module = self.current_module.borrow();
                if let Some(value) = module.namespace.get_value(&var.name) {
                    return Ok(value.clone());
                }

                 */

                Err(format!("Variable '{}' not found in module", var.name,))?
            }

            ResolvedExpression::ArrayAccess(array_item_ref) => {
                let array_val = self.evaluate_expression(&array_item_ref.array_expression)?;
                let index_val = self.evaluate_expression(&array_item_ref.int_expression)?;

                match (array_val, index_val) {
                    (Value::Array(_type_id, elements), Value::Int(i)) => {
                        if i < 0 || i >= elements.len() as i32 {
                            return Err(format!("Array index out of bounds: {}", i))?;
                        }
                        Ok(elements[i as usize].clone())
                    }
                    (Value::Reference(r), Value::Int(i)) => {
                        // Handle array access through a reference
                        if let Value::Array(_type_id, elements) = &*r.borrow() {
                            if i < 0 || i >= elements.len() as i32 {
                                return Err(format!("Array index out of bounds: {}", i))?;
                            }
                            Ok(elements[i as usize].clone())
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
                    Value::Struct(struct_type, fields) => fields
                        .get(struct_field_access.index)
                        .cloned()
                        .ok_or_else(|| {
                            format!(
                                "Field '{}' not found in struct '{:?}'",
                                struct_field_access.index, struct_type
                            )
                            .into()
                        }),
                    Value::Reference(r) => {
                        // If it's a reference, dereference and try field access
                        let value = r.borrow();
                        match &*value {
                            Value::Struct(struct_type, fields) => fields
                                .get(struct_field_access.index)
                                .cloned()
                                .ok_or_else(|| {
                                    format!(
                                        "Field '{}' not found in struct '{:?}'",
                                        struct_field_access.index, struct_type
                                    )
                                    .into()
                                }),
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
                let value = self
                    .lookup_in_scope(&var_ref.name())
                    .ok_or_else(|| format!("Variable '{}' not found", var_ref.0.name()))?;

                match value {
                    Value::Reference(r) => Ok(Value::Reference(r.clone())),
                    _ => Err("Can only take mutable reference of mutable variable".to_string())?,
                }
            }

            // Operators
            ResolvedExpression::BinaryOp(binary_operator) => {
                let left_val = self.evaluate_expression(&binary_operator.left)?;
                let right_val = self.evaluate_expression(&binary_operator.right)?;
                self.evaluate_binary_op(left_val, &binary_operator.ast_operator_type, right_val)
            }

            ResolvedExpression::UnaryOp(op, expr) => {
                let val = self.evaluate_expression(expr)?;
                self.evaluate_unary_op(op, val)
            }

            // Calling
            ResolvedExpression::FunctionInternalCall(resolved_internal_call) => {
                // Directly use evaluate_function_call instead of the current implementation
                self.evaluate_internal_function_call(resolved_internal_call)
            }

            ResolvedExpression::MemberCall(resolved_member_call) => {
                let member_value =
                    self.evaluate_expression(&resolved_member_call.resolved_expression)?;

                trace!("{} > member call {:?}", self.tabs(), member_value);
                /*
                                // Look up the method
                                let method = {
                                    let module = self.current_module.borrow();
                                    let impl_methods = module
                                        .namespace
                                        .get_impl(&type_id)
                                        .ok_or_else(|| format!("No impl found for type {:?}", type_id))?;

                                    impl_methods
                                        .members
                                        .get(&method_name.0)
                                        .ok_or_else(|| {
                                            format!(
                                                "Method '{}' not found for type {:?}",
                                                method_name.0, type_id
                                            )
                                        })?
                                        .clone()
                                };
                */
                self.push_scope(ScopeType::Function); // TODO: maybe have automatic pop on Drop

                let mut member_call_arguments = Vec::new();
                member_call_arguments.push(member_value);
                for arg in &resolved_member_call.arguments {
                    member_call_arguments.push(self.evaluate_expression(arg)?);
                }

                let self_param = ResolvedParameter {
                    name: "".to_string(),
                    resolved_type: ResolvedType::Struct(
                        resolved_member_call.struct_type_ref.clone(),
                    ),
                    is_mutable: resolved_member_call.self_param.is_mutable,
                    ast_parameter: Parameter {
                        variable: Variable {
                            name: "".to_string(),
                            is_mutable: false,
                        },
                        param_type: Type::Int,
                        is_mutable: false,
                    },
                };

                let mut all_params = vec![self_param];
                all_params.extend(resolved_member_call.arguments.iter().cloned());

                self.bind_parameters(&all_params, member_call_arguments)?;

                let result = self.execute_statements(
                    &resolved_member_call
                        .impl_member
                        .body
                        .function
                        .statements,
                )?;

                self.pop_scope();

                match result {
                    ValueWithSignal::Value(v) => Ok(v),
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
                self.push_scope(ScopeType::Block);
                let result = self.execute_statements(statements)?;
                self.pop_scope();
                match result {
                    ValueWithSignal::Value(v) => Ok(v),
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

                Ok(Value::String(result))
            }

            // Comparing
            ResolvedExpression::IfElse(condition, then_expr, else_expr) => {
                let cond_value = self.evaluate_expression(&condition)?;
                match cond_value {
                    Value::Bool(true) => self.evaluate_expression(then_expr),
                    Value::Bool(false) => self.evaluate_expression(else_expr),
                    _ => Err("If condition must evaluate to a boolean".to_string())?,
                }
            }

            ResolvedExpression::Match(resolved_match) => self.eval_match(resolved_match),

            _ => Err(format!("Unsupported expression: {:?}", expr))?,
        }
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
                    self.push_scope(ScopeType::Block);
                    self.set_existing_var_or_create_new_one(
                        var.name().to_string(),
                        cond_value.clone(),
                        false,
                    );
                    let result = self.evaluate_expression(&arm.expression);
                    self.pop_scope();
                    return result;
                }

                ResolvedPattern::Tuple(resolved_tuple_type_ref) => {
                    if let Value::Tuple(_tuple_type_ref, values) = &actual_value {
                        if resolved_tuple_type_ref.0.len() == values.len() {
                            self.push_scope(ScopeType::Block);
                            for (field, value) in
                                resolved_tuple_type_ref.0.iter().zip(values.iter())
                            {
                                self.set_existing_var_or_create_new_one(
                                    field.clone(),
                                    value.clone(),
                                    false,
                                );
                            }
                            let result = self.evaluate_expression(&arm.expression);
                            self.pop_scope();
                            return result;
                        }
                    }
                }

                ResolvedPattern::Struct(fields) => {
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

                ResolvedPattern::EnumTuple(resolved_enum_type) => {
                    if let Value::EnumVariant(enum_variant_type_ref, container) = &actual_value {
                        if enum_variant_type_ref.name() == ast_variant_name {
                            if let Value::Tuple(_, values) = &**container {
                                if fields.len() == values.len() {
                                    self.push_scope(ScopeType::Block);
                                    for (field, value) in enum_variant_type_ref.data.iter().zip(values.iter()) {
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
                        }
                    }
                }

                ResolvedPattern::EnumStruct(enum_struct_ref) => {
                    if let Value::EnumVariant(enum_variant_type_ref, container) = &actual_value {
                        if enum_variant_type_ref.name() == ast_variant_name {
                            if let Value::Struct(_, values) = &**container {
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
                    }
                }

                ResolvedPattern::EnumSimple(local_identifier) => match &actual_value {
                    Value::EnumVariant(ref enum_variant_ref, _container) => {
                        if enum_variant_ref.name() == local_identifier {
                            return self.evaluate_expression(&arm.expression);
                        }
                    }

                    _ => return Err(format!("was not a simple enum variant {cond_value:?}"))?,
                },

                ResolvedPattern::Wildcard => {}
            }
        }

        Ok(cond_value)
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
                if b.abs() <= 0.001 {
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

            _ => return Err(format!("Invalid binary operation {op:?} ").into()),
        };

        Ok(result)
    }

    fn evaluate_unary_op(&self, op: &UnaryOperator, val: Value) -> Result<Value, ExecuteError> {
        match (op, val) {
            (UnaryOperator::Negate, Value::Int(n)) => Ok(Value::Int(-n)),
            (UnaryOperator::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            _ => Err(format!("Invalid unary operation"))?,
        }
    }

    // Hardcoded print function for now
    pub fn print(&mut self, val: Value) {
        self.output.borrow_mut().push(val.to_string());
    }

    pub fn get_output(&self) -> Vec<String> {
        self.output.borrow().clone()
    }
}
