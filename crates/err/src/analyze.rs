use crate::semantic::build_semantic_error;
use crate::{Builder, Report, build_and_print};
use eira::Kind;
use std::path::Path;
use swamp_script_analyzer::err::{Error, ErrorKind};
use swamp_script_source_map::SourceMap;

#[must_use]
#[allow(clippy::too_many_lines)]
pub fn build_analyzer_error(err: &Error) -> Builder<usize> {
    let span = &err.node.span;
    let mut b = match &err.kind {
        ErrorKind::UnknownEnumType => Report::build(Kind::Error, 1, "unknown enum type", span),
        ErrorKind::TooManyDestructureVariables => {
            Report::build(Kind::Error, 4203, "TooManyDestructureVariables", span)
        }
        ErrorKind::CanNotDestructure => {
            Report::build(Kind::Error, 4203, "Can Not Destructure", span)
        }
        ErrorKind::UnknownConstant => Report::build(Kind::Error, 903, "Unknown constant", span),
        ErrorKind::UnknownStructTypeReference => {
            Report::build(Kind::Error, 105, "Unknown Struct Type Reference", span)
        }
        ErrorKind::DuplicateFieldName => todo!(),
        ErrorKind::MissingFieldInStructInstantiation(fields, _struct_type) => {
            Report::build(Kind::Error, 903, "missing fields in instantiation", span)
                .with_note(&format!("fields: {fields:?}"))
        }
        ErrorKind::UnknownVariable => Report::build(Kind::Error, 105, "Unknown variable", span),
        ErrorKind::OverwriteVariableWithAnotherType => Report::build(
            Kind::Error,
            14505,
            "overwrite variable with another type",
            span,
        ),
        ErrorKind::WrongNumberOfArguments(_expected, _encountered) => {
            Report::build(Kind::Error, 105, "wrong number of arguments", span)
        }
        //    .with_label("first_type", a.to_string())
        //.with_label("second_type", b.to_string())
        ErrorKind::CanOnlyOverwriteVariableWithMut => Report::build(
            Kind::Error,
            90423,
            "Variable needs to be mut to overwrite",
            span,
        ),
        ErrorKind::UnknownEnumVariantType => {
            Report::build(Kind::Error, 903, "Unknown enum variant type", span)
        }
        ErrorKind::UnknownStructField => {
            Report::build(Kind::Error, 106, "Unknown Struct Field Reference", span)
        }
        ErrorKind::UnknownEnumVariantTypeInPattern => Report::build(
            Kind::Error,
            106,
            "Unknown enum variant type in pattern",
            span,
        ),
        ErrorKind::ExpectedEnumInPattern => todo!(),
        ErrorKind::WrongEnumVariantContainer(_) => todo!(),
        ErrorKind::VariableIsNotMutable => {
            Report::build(Kind::Error, 75301, "Variable is not mutable", span)
        }
        ErrorKind::ArgumentIsNotMutable => {
            Report::build(Kind::Error, 1401, "Argument is not mutable", span)
        }
        ErrorKind::UnknownTypeReference => {
            Report::build(Kind::Error, 101, "Unknown type reference", span)
        }
        ErrorKind::SemanticError(semantic_error) => build_semantic_error(semantic_error, span),
        ErrorKind::ExpectedOptional => {
            Report::build(Kind::Error, 7801, "expected optional type", span)
        }
        ErrorKind::MapKeyTypeMismatch { .. } => todo!(),
        ErrorKind::MapValueTypeMismatch { .. } => todo!(),
        ErrorKind::IncompatibleTypes(_a, _b) => {
            Report::build(Kind::Error, 102, "Incompatible types", span)
            //.with_label("first_type", a.clone())
            //.with_note(&format!("second_type {b:?}"))
        }
        ErrorKind::UnknownMemberFunction => {
            Report::build(Kind::Error, 101, "Unknown member function", span)
        }
        ErrorKind::ExpressionsNotAllowedInLetPattern => todo!(),
        ErrorKind::UnknownField => todo!(),
        ErrorKind::EnumVariantHasNoFields => {
            Report::build(Kind::Error, 102, "enum variant has no fields", span)
        }
        ErrorKind::TooManyTupleFields { .. } => todo!(),
        ErrorKind::ExpectedBooleanExpression => {
            Report::build(Kind::Error, 102, "Expected a boolean expression", span)
        }
        ErrorKind::NotAnIterator => Report::build(Kind::Error, 101, "Not an iterator", span),
        ErrorKind::IntConversionError(_) => todo!(),
        ErrorKind::FloatConversionError(_) => todo!(),
        ErrorKind::BoolConversionError => todo!(),
        ErrorKind::DuplicateFieldInStructInstantiation(_) => todo!(),
        ErrorKind::UnknownFunction => Report::build(Kind::Error, 1026, "Unknown function", span),
        ErrorKind::NoDefaultImplemented(_resolved_type) => {
            Report::build(Kind::Error, 104, "No default() function", span)
        }
        ErrorKind::GuardHasNoType => Report::build(Kind::Error, 105, "guard has no type", span),
        ErrorKind::NoneNeedsExpectedTypeHint => {
            Report::build(Kind::Error, 107, "None needs expected type hint", span)
        }
        ErrorKind::NotValidLocationStartingPoint => {
            Report::build(Kind::Error, 108, "not valid location starting point", span)
        }
        ErrorKind::CallsCanNotBePartOfChain
        | ErrorKind::UnwrapCanNotBePartOfChain
        | ErrorKind::NoneCoalesceCanNotBePartOfChain => todo!(),
        ErrorKind::SelfNotCorrectType => {
            Report::build(Kind::Error, 108, "SelfNotCorrectType", span)
        }
        ErrorKind::CanNotNoneCoalesce => todo!(),
        &ErrorKind::GuardCanNotHaveMultipleWildcards
        | &ErrorKind::WildcardMustBeLastInGuard
        | &ErrorKind::GuardMustHaveWildcard => todo!(),
        ErrorKind::UnknownModule => Report::build(Kind::Error, 140, "Unknown module", span),
        ErrorKind::UnknownSymbol => Report::build(Kind::Error, 140, "some error", span),
        _ => todo!(),
    };
    b.error_module = "A".to_string();
    b
}

/// # Panics
///
pub fn show_analyzer_error(err: &Error, source_map: &SourceMap, current_dir: &Path) {
    let builder = build_analyzer_error(err);
    build_and_print(builder, source_map, current_dir);
}
