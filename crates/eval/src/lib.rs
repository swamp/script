/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::def::DefinitionRunner;
use crate::module::Module;
use crate::ns::{EnumVariantContainerType, ModuleNamespace, SwampTypeId};
use crate::value::{FunctionRef, SwampExport, Value};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
use swamp_script_ast::{
    BinaryOperator, Definition, EnumLiteralData, Expression, Literal, LocalTypeIdentifier,
    MatchArm, Parameter, Pattern, Program, ScopedIdentifier, Statement, StringPart, Type,
    UnaryOperator, Variable,
};
use swamp_script_parser::AstParser;
use tracing::{debug, error, trace};
use value::format_value;

mod def;
pub mod module;
pub mod name;
pub mod ns;
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
    parser: AstParser,
    modules: HashMap<String, Rc<RefCell<Module>>>,
    current_module: Rc<RefCell<Module>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let output = Rc::new(RefCell::new(Vec::new()));
        let main_module = Rc::new(RefCell::new(Module::new(
            None,
            LocalTypeIdentifier::new(&*"main".to_string()),
            ModuleNamespace::default(),
        )));
        let mut interpreter = Self {
            scope_stack: vec![Scope::default()],
            output: output.clone(),
            modules: HashMap::new(),
            parser: AstParser::new(),
            current_module: main_module.clone(),
        };
        interpreter.modules.insert("main".to_string(), main_module);

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

    // Helper method to check type compatibility
    #[allow(unused)]
    fn type_matches(&self, actual: &SwampTypeId, expected: &Type) -> bool {
        match (actual, expected) {
            (SwampTypeId::Int, Type::Int) => true,
            (SwampTypeId::Float, Type::Float) => true,
            (SwampTypeId::String, Type::String) => true,
            (SwampTypeId::Bool, Type::Bool) => true,
            (SwampTypeId::Struct(struct_type), Type::Struct(ast_struct_name)) => {
                let mentioned_struct_type = {
                    let module = self.current_module.borrow();
                    module
                        .namespace
                        .get_struct(ast_struct_name)
                        .as_deref()
                        .cloned() // Clone to own the value and extend its lifetime
                        .expect("should find struct") // TODO: Error handling
                };

                *struct_type == mentioned_struct_type
            }
            (SwampTypeId::Array(t1), Type::Array(t2)) => self.type_matches(t1, t2),
            // TODO: Add more type matching rules
            _ => false,
        }
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
    fn bind_parameters(&mut self, params: &[Parameter], args: Vec<Value>) -> Result<(), String> {
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
                vec![Parameter {
                    variable: Variable::new("value", false),
                    param_type: Type::Any,
                    is_mutable: false,
                }],
                Type::Unit,
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
            .insert("print".to_string(), Value::Function(print_fn));
    }

    pub fn register_external_function(
        &mut self,
        name: &ScopedIdentifier,
        params: Vec<Parameter>,
        return_type: Type,
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
    fn evaluate_function_call(
        &mut self,
        func: &Expression,
        args: &[Expression],
    ) -> Result<Value, ExecuteError> {
        let func_val = self.evaluate_expression(func)?;

        let func_ref = match func_val {
            Value::Function(f) => f,
            _ => return Err("Expected a function".to_string())?,
        };

        // Check mutability requirements
        for (param, arg) in func_ref.parameters().iter().zip(args) {
            if param.is_mutable {
                match arg {
                    Expression::MutRef(_) => (), // This is fine
                    Expression::VariableAccess(var) => {
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
        let mut namespace = ModuleNamespace::new();

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

    pub fn eval_program(&mut self, program: Program) -> Result<ValueWithSignal, ExecuteError> {
        self.definitions(program.definitions())?;
        self.register_builtins();

        Ok(self.interpret(program.statements())?)
    }

    fn interpret(&mut self, statements: &Vec<Statement>) -> Result<ValueWithSignal, ExecuteError> {
        self.execute_statements(statements)
    }

    fn tabs(&self) -> String {
        "..".repeat(self.scope_stack.len() - 1)
    }

    #[inline]
    fn execute_statements(
        &mut self,
        statements: &Vec<Statement>,
    ) -> Result<ValueWithSignal, ExecuteError> {
        let mut value = Value::Unit;

        for statement in statements {
            trace!("{} exec {statement:?}", self.tabs());

            // First handle signal aware statements
            match statement {
                Statement::Continue => return Ok(ValueWithSignal::Continue),
                Statement::Break => return Ok(ValueWithSignal::Break),
                Statement::Return(expr) => {
                    return Ok(ValueWithSignal::Return(self.evaluate_expression(expr)?));
                }

                Statement::WhileLoop(condition, body) => {
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

                Statement::If(condition, consequences, optional_alternative) => {
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

                Statement::ForLoop(var_pattern, iterator_expr, body) => {
                    let iterator = self.evaluate_expression(iterator_expr)?;
                    match iterator {
                        Value::ExclusiveRange(start, end) => {
                            // Push a new scope for the loop
                            self.push_scope(ScopeType::Block);

                            for i in *start..*end {
                                match var_pattern {
                                    Pattern::Variable(ident) => {
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
                                    Pattern::Variable(ident) => {
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
                        Value::Function(_) => todo!(),
                    }

                    continue;
                }
                Statement::Block(body) => {
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
                Statement::Let(Pattern::Variable(var), expr) => {
                    let value = self.evaluate_expression(expr)?;
                    self.set_existing_var_or_create_new_one(
                        var.name().to_string(),
                        value,
                        var.is_mutable,
                    );
                    Value::Unit
                }

                Statement::Expression(expr) => {
                    let result = self.evaluate_expression(expr); // since it is statement_expression, the value is intentionally discarded
                    if result.is_err() {
                        return Err(result.unwrap_err());
                    }
                    result?
                }

                // destructuring
                Statement::Let(Pattern::Tuple(_), _) => Value::Unit,
                Statement::Let(Pattern::Struct(_), _) => Value::Unit,
                Statement::Let(Pattern::Literal(_), _) => Value::Unit,
                Statement::Let(Pattern::EnumTuple(_, _), _) => Value::Unit,
                Statement::Let(Pattern::EnumStruct(_, _), _) => Value::Unit,

                // ignore the let
                Statement::Let(Pattern::Wildcard, _) => Value::Unit,
                Statement::Let(Pattern::EnumSimple(_), _) => Value::Unit,

                // Ignore signal aware statements, they have been handled earlier
                Statement::Return(_) => panic!("return should have been handled earlier"),
                Statement::ForLoop(_, _, _) => panic!("for_loop should have been handled earlier"),
                Statement::WhileLoop(_, _) => panic!("while_loop should have been handled earlier"),
                Statement::Break => panic!("break should have been handled earlier"),
                Statement::Continue => panic!("continue should have been handled earlier"),
                Statement::Block(_) => panic!("block should have been handled earlier"),
                Statement::If(_, _, _) => panic!("if should have been handled earlier"),
            }
        }

        Ok(ValueWithSignal::Value(value))
    }

    fn evaluate_args(&mut self, args: &[Expression]) -> Result<Vec<Value>, ExecuteError> {
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

    fn evaluate_expressions(&mut self, exprs: &[Expression]) -> Result<Vec<Value>, ExecuteError> {
        let mut values = vec![];
        for expr in exprs {
            let value = self.evaluate_expression(expr)?;
            values.push(value);
        }

        Ok(values)
    }

    fn evaluate_expression(&mut self, expr: &Expression) -> Result<Value, ExecuteError> {
        match expr {
            // Constructing
            Expression::Literal(lit) => Ok(match lit {
                Literal::Int(n) => Value::Int(*n),
                Literal::Float(f) => Value::Float(*f),
                Literal::String(s) => Value::String(s.0.clone()),
                Literal::Bool(b) => Value::Bool(*b),
                Literal::EnumVariant(enum_type_name, variant_name, data) => {
                    let enum_variant_type = {
                        let module = self.current_module.borrow();
                        module
                            .namespace
                            .get_enum_variant_type(enum_type_name, variant_name.clone())
                            .expect("should find enum variant") // TODO: error handling
                            .clone() // Extend the lifetime by cloning
                    };
                    let variant_container_value: Value = match &enum_variant_type.data {
                        EnumVariantContainerType::Tuple(_) => match data {
                            EnumLiteralData::Tuple(tuple_expressions) => {
                                let eval_expressions =
                                    self.evaluate_expressions(tuple_expressions)?;
                                let types = eval_expressions
                                    .iter()
                                    .map(|v| v.swamp_type_id())
                                    .collect::<Vec<_>>();
                                let tuple_type = self
                                    .current_module
                                    .borrow_mut()
                                    .namespace
                                    .get_or_create_tuple(types);
                                Value::Tuple(tuple_type, eval_expressions)
                            }
                            _ => return Err("wrong container type".to_string())?,
                        },

                        EnumVariantContainerType::Struct(struct_type_ref) => match data {
                            EnumLiteralData::Struct(ast_struct_fields) => {
                                let mut values = Vec::with_capacity(ast_struct_fields.len());
                                for ast_expression in ast_struct_fields.values() {
                                    let value = self.evaluate_expression(ast_expression)?;
                                    values.push(value);
                                }
                                Value::Struct(struct_type_ref.clone(), values)
                            }
                            _ => return Err("wrong container type".to_string())?,
                        },

                        EnumVariantContainerType::Nothing => Value::Unit,
                    };

                    Value::EnumVariant(enum_variant_type.clone(), Box::new(variant_container_value))
                }

                Literal::Tuple(expressions) => {
                    let expressions = self.evaluate_expressions(expressions)?;
                    let types = expressions
                        .iter()
                        .map(|v| v.swamp_type_id())
                        .collect::<Vec<_>>();
                    let tuple_type = self
                        .current_module
                        .borrow_mut()
                        .namespace
                        .get_or_create_tuple(types);
                    Value::Tuple(tuple_type, expressions)
                }

                Literal::Unit => Value::Unit,
            }),

            Expression::Array(elements) => {
                let mut values = Vec::new();
                for element in elements {
                    values.push(self.evaluate_expression(element)?);
                }
                let array_type = if values.is_empty() {
                    SwampTypeId::Any // empty array has all array types
                } else {
                    values[0].swamp_type_id()
                };

                Ok(Value::Array(array_type, values))
            }

            Expression::StructInstantiation(struct_name, fields) => {
                // First, validate the struct and collect type information
                let struct_def = {
                    let module = self.current_module.borrow();
                    module
                        .namespace
                        .get_struct(&struct_name)
                        .ok_or_else(|| format!("Struct '{}' not found", struct_name.0))?
                        .clone() // Clone the struct definition so we can drop the borrow
                };

                // Validate field presence
                for (field_name, _field_type) in &struct_def.fields {
                    if !fields.contains_key(field_name) {
                        return Err(format!(
                            "Missing field '{}' in struct '{}'",
                            field_name.0, struct_name.0
                        ))?;
                    }
                }

                // Validate no extra fields
                for field_name in fields.keys() {
                    if !struct_def.fields.contains_key(field_name) {
                        return Err(format!(
                            "Unknown field '{}' in struct '{}'",
                            field_name.0, struct_name.0
                        ))?;
                    }
                }

                // Evaluate all field expressions and validate types
                let mut field_values = Vec::new();
                for (_field_name, field_expr) in fields {
                    let value = self.evaluate_expression(field_expr)?;

                    /*
                                let expected_type = struct_def
                        .fields
                        .get(field_name)
                        .expect("Field existence already validated");


                    if !self.type_matches(&value.swamp_type_id(), expected_type) {
                        return Err(format!(
                            "Type mismatch for field '{}': expected {:?}, got {:?}",
                            field_name.0,
                            expected_type,
                            value.swamp_type_id()
                        ));
                    }

                     */

                    field_values.push(value);
                }

                Ok(Value::Struct(struct_def, field_values))
            }

            Expression::ExclusiveRange(start, end) => {
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
            Expression::VariableAssignment(target, value_expr) => {
                let new_value = self.evaluate_expression(value_expr)?;

                if let Expression::VariableAccess(name) = &**target {
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

            Expression::ArrayAssignment(array, index, value) => {
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

            Expression::FieldAssignment(target_expr, field_name, value_expr) => {
                let target = self.evaluate_expression(target_expr)?;
                let value = self.evaluate_expression(value_expr)?;

                match target {
                    Value::Reference(r) => {
                        let mut borrowed = r.borrow_mut();
                        // We know it must be a struct because references can only point to structs
                        match &mut *borrowed {
                            Value::Struct(struct_type, fields) => {
                                let field_index = struct_type
                                    .field_index(&field_name)
                                    .expect("always find field");

                                if let Some(field) = fields.get_mut(field_index) {
                                    *field = value.clone();
                                    Ok(value)
                                } else {
                                    Err(format!(
                                        "Field '{}' not found in struct '{:?}'",
                                        field_name.0, struct_type
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
                        field_name.0
                    ))?,
                }
            }

            // ------------- LOOKUP ---------------------
            Expression::VariableAccess(var) => {
                // First check local scope
                if let Some(value) = self.lookup_in_scope(&var.name) {
                    trace!("found lookup: {var:?} {value:?}");

                    return Ok(value.clone());
                }

                // Then check module namespace
                let module = self.current_module.borrow();
                if let Some(value) = module.namespace.get_value(&var.name) {
                    return Ok(value.clone());
                }

                Err(format!(
                    "Variable '{}' not found in module '{}'",
                    var.name,
                    module.name()
                ))?
            }

            Expression::ArrayAccess(array, index) => {
                let array_val = self.evaluate_expression(array)?;
                let index_val = self.evaluate_expression(index)?;

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

            Expression::FieldAccess(obj_expr, field_name) => {
                let obj = self.evaluate_expression(obj_expr)?;

                match obj {
                    Value::Struct(struct_type, fields) => {
                        let field_index = struct_type
                            .field_index(&field_name)
                            .expect("always find field"); // TODO: Error handling
                        fields.get(field_index).cloned().ok_or_else(|| {
                            format!(
                                "Field '{}' not found in struct '{:?}'",
                                field_name.0, struct_type
                            )
                            .into()
                        })
                    }
                    Value::Reference(r) => {
                        // If it's a reference, dereference and try field access
                        let value = r.borrow();
                        match &*value {
                            Value::Struct(struct_type, fields) => {
                                let field_index = struct_type
                                    .field_index(&field_name)
                                    .expect("always find field"); // TODO: Error handling

                                fields.get(field_index).cloned().ok_or_else(|| {
                                    format!(
                                        "Field '{}' not found in struct '{:?}'",
                                        field_name.0, struct_type
                                    )
                                    .into()
                                })
                            }
                            _ => Err(format!(
                                "Cannot access field '{}' on non-struct value",
                                field_name.0
                            ))?,
                        }
                    }
                    _ => Err(format!(
                        "Cannot access field '{}' on non-struct value",
                        field_name.0
                    ))?,
                }
            }

            Expression::MutRef(var_ref) => {
                let value = self
                    .lookup_in_scope(&var_ref.0.name())
                    .ok_or_else(|| format!("Variable '{}' not found", var_ref.0.name()))?;

                match value {
                    Value::Reference(r) => Ok(Value::Reference(r.clone())),
                    _ => Err("Can only take mutable reference of mutable variable".to_string())?,
                }
            }

            // Operators
            Expression::BinaryOp(left, op, right) => {
                let left_val = self.evaluate_expression(left)?;
                let right_val = self.evaluate_expression(right)?;
                self.evaluate_binary_op(left_val, op, right_val)
            }

            Expression::UnaryOp(op, expr) => {
                let val = self.evaluate_expression(expr)?;
                self.evaluate_unary_op(op, val)
            }

            // Calling
            Expression::FunctionCall(func_expr, arg_exprs) => {
                // Directly use evaluate_function_call instead of the current implementation
                self.evaluate_function_call(func_expr, arg_exprs)
            }

            Expression::MemberCall(member_expression, method_name, args) => {
                let member_value = self.evaluate_expression(member_expression)?;
                let type_id = member_value.swamp_type_id();

                trace!(
                    "{} > member call {:?} {:?}",
                    self.tabs(),
                    type_id,
                    member_value
                );
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

                self.push_scope(ScopeType::Function); // TODO: maybe have automatic pop on Drop

                let mut member_call_arguments = Vec::new();
                member_call_arguments.push(member_value);
                for arg in args {
                    member_call_arguments.push(self.evaluate_expression(arg)?);
                }

                let mut all_params = vec![Parameter {
                    variable: Variable::new("self", method.self_param.is_mutable),
                    param_type: Type::Any, // The actual type doesn't matter here
                    is_mutable: method.self_param.is_mutable,
                }];
                all_params.extend(method.params.iter().cloned());

                self.bind_parameters(&all_params, member_call_arguments)?;

                let result = self.execute_statements(&method.body)?;

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

            Expression::Block(statements) => {
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

            Expression::InterpolatedString(parts) => {
                let mut result = String::new();

                for part in parts {
                    match part {
                        StringPart::Literal(text) => {
                            result.push_str(text);
                        }
                        StringPart::Interpolation(expr, format_spec) => {
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
            Expression::IfElse(condition, then_expr, else_expr) => {
                let cond_value = self.evaluate_expression(condition)?;
                match cond_value {
                    Value::Bool(true) => self.evaluate_expression(then_expr),
                    Value::Bool(false) => self.evaluate_expression(else_expr),
                    _ => Err("If condition must evaluate to a boolean".to_string())?,
                }
            }

            Expression::Match(condition, match_arms) => self.eval_match(condition, match_arms),

            _ => Err(format!("Unsupported expression: {:?}", expr))?,
        }
    }

    #[inline(always)]
    fn eval_match(
        &mut self,
        condition: &Box<Expression>,
        match_arms: &Vec<MatchArm>,
    ) -> Result<Value, ExecuteError> {
        let cond_value = self.evaluate_expression(condition)?;
        // Dereference if we got a reference
        let actual_value = match &cond_value {
            Value::Reference(r) => r.borrow().clone(),
            _ => cond_value.clone(),
        };

        for arm in match_arms {
            match &arm.pattern {
                Pattern::Variable(var) => {
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

                Pattern::Tuple(fields) => {
                    if let Value::Tuple(_tuple_type_ref, values) = &actual_value {
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

                Pattern::Struct(fields) => {
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

                Pattern::Literal(lit) => match (lit, &actual_value) {
                    (Literal::Int(a), Value::Int(b)) if a == b => {
                        return self.evaluate_expression(&arm.expression);
                    }
                    (Literal::Float(a), Value::Float(b)) if a == b => {
                        return self.evaluate_expression(&arm.expression);
                    }
                    (Literal::String(a), Value::String(b)) if a.0 == *b => {
                        return self.evaluate_expression(&arm.expression);
                    }
                    (Literal::Bool(a), Value::Bool(b)) if a == b => {
                        return self.evaluate_expression(&arm.expression);
                    }
                    _ => continue,
                },

                Pattern::EnumTuple(ast_variant_name, fields) => {
                    if let Value::EnumVariant(enum_variant_type_ref, container) = &actual_value {
                        if enum_variant_type_ref.name() == ast_variant_name {
                            if let Value::Tuple(_, values) = &**container {
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
                    }
                }

                Pattern::EnumStruct(ast_variant_name, fields) => {
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

                Pattern::EnumSimple(local_identifier) => match &actual_value {
                    Value::EnumVariant(ref enum_variant_ref, _container) => {
                        if enum_variant_ref.name() == local_identifier {
                            return self.evaluate_expression(&arm.expression);
                        }
                    }

                    _ => return Err(format!("was not a simple enum variant {cond_value:?}"))?,
                },

                Pattern::Wildcard => {}
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

        let result = match (left_val, op, right_val) {
            // Integer operations
            (Value::Int(a), BinaryOperator::Add, Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::Int(a), BinaryOperator::Subtract, Value::Int(b)) => Ok(Value::Int(a - b)),
            (Value::Int(a), BinaryOperator::Multiply, Value::Int(b)) => Ok(Value::Int(a * b)),
            (Value::Int(a), BinaryOperator::Divide, Value::Int(b)) => {
                if b == 0 {
                    return Err("Division by zero".to_string())?;
                }
                Ok(Value::Int(a / b))
            }
            (Value::Int(a), BinaryOperator::Modulo, Value::Int(b)) => Ok(Value::Int(a % b)),

            // Boolean operations
            (Value::Bool(a), BinaryOperator::LogicalAnd, Value::Bool(b)) => Ok(Value::Bool(a && b)),
            (Value::Bool(a), BinaryOperator::LogicalOr, Value::Bool(b)) => Ok(Value::Bool(a || b)),

            // Comparison operations
            (Value::Int(a), BinaryOperator::Equal, Value::Int(b)) => Ok(Value::Bool(a == b)),
            (Value::Int(a), BinaryOperator::NotEqual, Value::Int(b)) => Ok(Value::Bool(a != b)),
            (Value::Int(a), BinaryOperator::LessThan, Value::Int(b)) => Ok(Value::Bool(a < b)),
            (Value::Int(a), BinaryOperator::GreaterThan, Value::Int(b)) => Ok(Value::Bool(a > b)),
            (Value::Int(a), BinaryOperator::LessThanOrEqual, Value::Int(b)) => {
                Ok(Value::Bool(a <= b))
            }
            (Value::Int(a), BinaryOperator::GreaterThanOrEqual, Value::Int(b)) => {
                Ok(Value::Bool(a >= b))
            }

            // String operations
            (Value::String(a), BinaryOperator::Add, Value::String(b)) => Ok(Value::String(a + &b)),
            (Value::String(a), BinaryOperator::Add, Value::Int(b)) => {
                Ok(Value::String(a + &b.to_string()))
            }
            (Value::Int(a), BinaryOperator::Add, Value::String(b)) => {
                Ok(Value::String(a.to_string() + &b))
            }

            _ => Err(format!("Invalid binary operation")),
        }?;

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
