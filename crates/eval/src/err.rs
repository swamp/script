#[derive(Debug, PartialEq, Eq)]
pub enum ConversionError {
    TypeError(String),
    ValueError(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExecuteError {
    Error(String),
    ConversionError(ConversionError),
    ArgumentIsNotMutable,
    CanNotUnwrap,
    IllegalIterator,
    ExpectedOptional,
    NonUniqueKeysInMapLiteralDetected,
    NotAnArray,
    ValueIsNotMutable,
    NotSparseValue,
}
