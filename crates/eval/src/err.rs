/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use swamp_script_core::value::ValueError;
use swamp_script_semantic::{ExternalFunctionId, ResolvedNode};

#[derive(Debug, PartialEq, Eq)]
pub enum ConversionError {
    TypeError(String),
    ValueError(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExecuteError {
    pub node: ResolvedNode,
    pub kind: ExecuteErrorKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExecuteErrorKind {
    MustHaveGuardArmThatMatches,
    ConversionError(ConversionError),
    ValueError(ValueError),
    ArgumentIsNotMutable,
    WrongNumberOfArguments(usize, usize),
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
    NotMutLocationFound,
    IndexWasNotInteger,
    NotAMap,
    MissingExternalFunction(ExternalFunctionId),
    ExpectedInt,
    ExpectedString,
    IncompatibleTypes,
    RangeItemMustBeInt,
    OperationRequiresArray,
    ExpectedFloat,
    ExpectedTwoFloatTuple,
    ExpectedFunction,
    NotSparseId,
    ReturnNotAllowedHere,
    ExpectedStruct,
    ExpectedArray,
    ExpectedMap,
    PostfixChainError,
    IndexOutOfBounds,
    DivideByZero,
    MapKeyAlreadyExists,
    CouldNotConvertFromSignal,
}
