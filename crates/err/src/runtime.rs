use crate::{Builder, Report, build_and_print};
use eira::Kind;
use std::path::Path;
use swamp_script_eval::err::RuntimeErrorKind;
use swamp_script_eval::prelude::RuntimeError;
use swamp_script_source_map::SourceMap;

#[must_use]
pub fn build_runtime_error(err: &RuntimeError) -> Builder<usize> {
    let span = &err.node.span;
    let mut b = match &err.kind {
        RuntimeErrorKind::StackCouldNotBePopped => {
            Report::build(Kind::Error, 104, "stack could not pop", span)
        }
        RuntimeErrorKind::VecSubscriptNonExisting => {
            Report::build(Kind::Error, 104, "vector subscript failed", span)
        }
        RuntimeErrorKind::MapKeyNonExisting => {
            Report::build(Kind::Error, 104, "map key does not exist", span)
        }
        RuntimeErrorKind::ExpectedInt => Report::build(Kind::Error, 104, "expected int", span),
        RuntimeErrorKind::ExpectedString => {
            Report::build(Kind::Error, 104, "expected string", span)
        }
        RuntimeErrorKind::ValueError(_) => todo!(),
        RuntimeErrorKind::ArgumentIsNotMutable => {
            Report::build(Kind::Error, 104, "argument is not mutable", span)
        }
        RuntimeErrorKind::ExpectedOptional => {
            Report::build(Kind::Error, 104, "expected optional", span)
        }
        RuntimeErrorKind::NonUniqueKeysInMapLiteralDetected => todo!(),
        RuntimeErrorKind::NotAnArray => todo!(),
        RuntimeErrorKind::NotSparseValue => todo!(),
        RuntimeErrorKind::CoerceOptionToBoolFailed => todo!(),
        RuntimeErrorKind::VariableWasNotMutable => todo!(),
        RuntimeErrorKind::ContinueNotAllowedHere => todo!(),
        RuntimeErrorKind::BreakNotAllowedHere => todo!(),
        RuntimeErrorKind::NotAMap => todo!(),
        RuntimeErrorKind::NotAMap2 => todo!(),
        RuntimeErrorKind::MissingExternalFunction(_) => todo!(),
        RuntimeErrorKind::WrongNumberOfArguments(_, _) => todo!(),
        RuntimeErrorKind::RangeItemMustBeInt => todo!(),
        RuntimeErrorKind::OperationRequiresArray => todo!(),
        RuntimeErrorKind::ExpectedFloat => todo!(),
        RuntimeErrorKind::ExpectedTwoFloatTuple => todo!(),
        RuntimeErrorKind::ExpectedFunction => todo!(),
        RuntimeErrorKind::NotSparseId => todo!(),
        RuntimeErrorKind::ReturnNotAllowedHere => todo!(),
        RuntimeErrorKind::ExpectedStruct => {
            Report::build(Kind::Error, 154, "expected struct", span)
        }
        RuntimeErrorKind::ExpectedArray => todo!(),
        RuntimeErrorKind::ExpectedMap => todo!(),
        RuntimeErrorKind::PostfixChainError => todo!(),
        RuntimeErrorKind::IndexOutOfBounds => todo!(),
        &RuntimeErrorKind::DivideByZero | &RuntimeErrorKind::MapKeyAlreadyExists => todo!(),
        RuntimeErrorKind::MustHaveGuardArmThatMatches => todo!(),
        RuntimeErrorKind::CouldNotConvertFromSignal => todo!(),
        &RuntimeErrorKind::UnknownMutIntrinsic | &RuntimeErrorKind::UnknownGenericIntrinsic => {
            todo!()
        }
    };

    b.error_module = "R".to_string();
    b
}

///
pub fn show_runtime_error(err: &RuntimeError, source_map: &SourceMap, current_path: &Path) {
    let builder = build_runtime_error(err);
    build_and_print(builder, source_map, current_path)
}
