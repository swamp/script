/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::ns::{EnumVariantTypeRef, StructTypeRef, SwampTypeId, TupleTypeRef};
use crate::{ExecuteError, Interpreter, ScopeType, ValueWithSignal};
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use swamp_script_ast::{
    FormatSpecifier, LocalTypeIdentifier, Parameter, PrecisionType, Statement, Type,
};

pub trait SwampExport {
    fn generate_swamp_definition() -> String;
    fn to_swamp_value(&self) -> Value;
    fn from_swamp_value(value: &Value) -> Result<Self, String>
    where
        Self: Sized;
}

impl SwampExport for f32 {
    // Todo: change to fixed point 32 bit
    fn to_swamp_value(&self) -> Value {
        Value::Float(*self)
    }

    fn from_swamp_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Float(f) => Ok(*f),
            _ => Err("Expected Float value".to_string()),
        }
    }

    fn generate_swamp_definition() -> String {
        // Primitives don't need struct definitions
        String::new()
    }
}

impl SwampExport for i32 {
    fn to_swamp_value(&self) -> Value {
        Value::Int(*self)
    }

    fn from_swamp_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Int(i) => Ok(*i),
            _ => Err("Expected Int value".to_string()),
        }
    }

    fn generate_swamp_definition() -> String {
        String::new()
    }
}

type FunctionDef = (Vec<Parameter>, Type);
type FunctionFn = Box<dyn Fn(&[Value]) -> Result<Value, ExecuteError>>;

pub enum FunctionRef {
    External(LocalTypeIdentifier, FunctionDef, Rc<FunctionFn>),
    Internal(LocalTypeIdentifier, FunctionDef, Vec<Statement>),
}

impl Debug for FunctionRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "FunctionRef {}({:?})",
            self.identifier(),
            self.parameters(),
        )
    }
}

impl Clone for FunctionRef {
    fn clone(&self) -> Self {
        match self {
            FunctionRef::External(debug_name, def, f) => {
                FunctionRef::External(debug_name.clone(), def.clone(), f.clone())
            }
            FunctionRef::Internal(debug_name, def, body) => {
                FunctionRef::Internal(debug_name.clone(), def.clone(), body.clone())
            }
        }
    }
}

impl FunctionRef {
    pub fn parameters(&self) -> &Vec<Parameter> {
        match self {
            FunctionRef::External(_, (params, _), _) => params,
            FunctionRef::Internal(_, (params, _), _) => params,
        }
    }

    pub fn identifier(&self) -> &LocalTypeIdentifier {
        match self {
            FunctionRef::External(debug_name, _, _) => debug_name,
            FunctionRef::Internal(debug_name, _, _) => debug_name,
        }
    }

    pub fn execute(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Value>,
    ) -> Result<Value, ExecuteError> {
        match self {
            FunctionRef::External(_, (_, _), f) => {
                let v = f(&args)?;
                Ok(v)
            }
            FunctionRef::Internal(_, (params, _), body) => {
                interpreter.push_scope(ScopeType::Function);

                // Bind parameters before executing body
                interpreter.bind_parameters(params, args)?;
                let result = interpreter.execute_statements(body)?;

                interpreter.pop_scope();

                // Since signals can not propagate from the function call, we just return a normal Value
                let v = match result {
                    ValueWithSignal::Value(v) => v,
                    ValueWithSignal::Return(v) => v,
                    ValueWithSignal::Break => Value::Unit,
                    ValueWithSignal::Continue => Value::Unit,
                };

                Ok(v)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Float(f32), // TODO: Replace with fixed
    String(String),
    Bool(bool),
    Unit, // Means 'no value' ()
    Reference(Rc<RefCell<Value>>),

    // Containers
    Array(SwampTypeId, Vec<Value>),
    Tuple(TupleTypeRef, Vec<Value>),
    Struct(StructTypeRef, Vec<Value>), // type of the struct, and the fields themselves in strict order
    EnumVariant(EnumVariantTypeRef, Box<Value>), // (enum_type_name, variant_name, value). value is either tuple or struct

    // Number generators
    ExclusiveRange(Box<i32>, Box<i32>),

    // Higher order
    Function(FunctionRef),
}

impl Value {
    pub fn as_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err("Expected boolean value".to_string()),
        }
    }

    pub fn swamp_type_id(&self) -> SwampTypeId {
        match self {
            Value::Int(_) => SwampTypeId::Int,
            Value::Float(_) => SwampTypeId::Float,
            Value::String(_) => SwampTypeId::String,
            Value::Bool(_) => SwampTypeId::Bool,
            Value::Array(item_type, _) => SwampTypeId::Array(Box::new(item_type.clone())),
            Value::Function(_) => SwampTypeId::Function,
            Value::Unit => SwampTypeId::Void,
            Value::ExclusiveRange(_, _) => SwampTypeId::Range,
            Value::Reference(r) => r.borrow().swamp_type_id(),
            Value::Struct(struct_type, _) => SwampTypeId::Struct(struct_type.clone()),
            Value::Tuple(tuple_type, _values) => SwampTypeId::Tuple(tuple_type.clone()),
            Value::EnumVariant(enum_variant_type_ref, _data) => {
                SwampTypeId::EnumVariant(enum_variant_type_ref.clone())
            }
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Array(_item_type, arr) => {
                write!(f, "[")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, "]")
            }
            Value::Tuple(_tuple_type, arr) => {
                write!(f, "(")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, ")")
            }
            Value::Struct(struct_type_ref, fields_in_strict_order) => {
                let prefix = if struct_type_ref.name().0.is_empty() {
                    "".to_string()
                } else {
                    struct_type_ref.name().0.to_string() + &*" ".to_string()
                };
                write!(f, "{}{{ ", prefix)?;
                for (i, val) in fields_in_strict_order.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    let field_name = struct_type_ref.fields.keys().nth(i).unwrap();
                    write!(f, "{}: {}", field_name, val)?;
                }
                write!(f, " }}")
            }
            Value::Function(_reference) => write!(f, "<function>"), // TODO:
            Value::Unit => write!(f, "()"),
            Value::ExclusiveRange(start, end) => write!(f, "{}..{}", start, end),
            Value::EnumVariant(enum_name, data) => {
                let mut s = enum_name.to_string();
                match data.as_ref() {
                    Value::Unit => {} // skip the unit output
                    _ => s += &data.to_string(),
                }
                write!(f, "{}", s)
            }
            Value::Reference(reference) => write!(f, "{}", reference.borrow()),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Reference(r1), Value::Reference(r2)) => {
                // Compare the actual values inside the references
                r1.borrow().eq(&*r2.borrow())
            }
            (Value::Reference(r1), other) => {
                // Compare reference value with direct value
                r1.borrow().eq(other)
            }
            (other, Value::Reference(r2)) => {
                // Compare direct value with reference value
                other.eq(&*r2.borrow())
            }
            // Regular value comparisons
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Unit, Value::Unit) => true,
            _ => false,
        }
    }
}

pub fn format_value(value: &Value, spec: &FormatSpecifier) -> Result<String, String> {
    match (value, spec) {
        (Value::Int(n), FormatSpecifier::Debug) => Ok(format!("{:?}", n)),
        (Value::Int(n), FormatSpecifier::LowerHex) => Ok(format!("{:x}", n)),
        (Value::Int(n), FormatSpecifier::UpperHex) => Ok(format!("{:X}", n)),
        (Value::Int(n), FormatSpecifier::Binary) => Ok(format!("{:b}", n)),

        (Value::Float(f), FormatSpecifier::Float) => Ok(format!("{}", f)),
        (Value::Float(f), FormatSpecifier::Precision(prec, PrecisionType::Float)) => {
            Ok(format!("{:.*}", *prec as usize, f))
        }

        (Value::String(s), FormatSpecifier::Precision(prec, PrecisionType::String)) => {
            Ok(format!("{:.*}", *prec as usize, s))
        }

        // Debug format for complex types
        (Value::Struct(_type_id, fields), FormatSpecifier::Debug) => Ok(format!("{:?}", fields)),
        (Value::Array(_type_id, elements), FormatSpecifier::Debug) => Ok(format!("{:?}", elements)),
        (Value::EnumVariant(_type_id, variant), FormatSpecifier::Debug) => {
            Ok(format!("{:?}", variant))
        }

        // Default string conversion for other cases
        (value, FormatSpecifier::Debug) => Ok(format!("{:?}", value)),

        _ => Err(format!(
            "Unsupported format specifier {:?} for value type {:?}",
            spec,
            value.swamp_type_id()
        )),
    }
}
