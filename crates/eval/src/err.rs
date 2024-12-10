use swamp_script_core::value::ValueError;

#[derive(Debug, PartialEq, Eq)]
pub enum ConversionError {
    TypeError(String),
    ValueError(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExecuteError {
    Error(String),
    TypeError(String),
    ConversionError(ConversionError),
    ValueError(ValueError),
    ArgumentIsNotMutable(String),
    CanNotUnwrap,
    IllegalIterator,
    ExpectedOptional,
    NonUniqueKeysInMapLiteralDetected,
    NotAnArray,
    ValueIsNotMutable,
    NotSparseValue,
    CoerceOptionToBoolFailed,
    VariableWasNotMutable,
}
