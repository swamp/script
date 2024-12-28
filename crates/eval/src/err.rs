use swamp_script_core::value::ValueError;
use swamp_script_semantic::ResolvedNode;

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
    ArgumentIsNotMutable(ResolvedNode),
    CanNotUnwrap,
    IllegalIterator,
    ExpectedOptional,
    NonUniqueKeysInMapLiteralDetected,
    NotAnArray,
    ValueIsNotMutable,
    NotSparseValue,
    CoerceOptionToBoolFailed,
    VariableWasNotMutable,
    ContinueNotAllowedHere,
    BreakNotAllowedHere,
}
