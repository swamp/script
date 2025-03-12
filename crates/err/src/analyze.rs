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
            Report::build(Kind::Error, 2, "too many destructure variables", span)
        }
        ErrorKind::CanNotDestructure => Report::build(Kind::Error, 3, "can not destructure", span),
        ErrorKind::UnknownConstant => Report::build(Kind::Error, 4, "unknown constant", span),
        ErrorKind::UnknownStructTypeReference => {
            Report::build(Kind::Error, 5, "unknown struct type reference", span)
        }
        ErrorKind::DuplicateFieldName => {
            Report::build(Kind::Error, 6, "duplicate field name", span)
        }
        ErrorKind::MissingFieldInStructInstantiation(fields, _struct_type) => {
            Report::build(Kind::Error, 7, "missing fields in instantiation", span)
                .with_note(&format!("fields: {fields:?}"))
        }
        ErrorKind::UnknownVariable => Report::build(Kind::Error, 8, "unknown variable", span),
        ErrorKind::OverwriteVariableWithAnotherType => {
            Report::build(Kind::Error, 9, "overwrite variable with another type", span)
        }
        ErrorKind::WrongNumberOfArguments(_expected, _encountered) => {
            Report::build(Kind::Error, 10, "wrong number of arguments", span)
        }
        //    .with_label("first_type", a.to_string())
        //.with_label("second_type", b.to_string())
        ErrorKind::CanOnlyOverwriteVariableWithMut => Report::build(
            Kind::Error,
            11,
            "overwrite requires variable to be mut",
            span,
        ),
        ErrorKind::UnknownEnumVariantType => {
            Report::build(Kind::Error, 12, "unknown enum variant type", span)
        }
        ErrorKind::UnknownStructField => {
            Report::build(Kind::Error, 13, "unknown struct field reference", span)
        }
        ErrorKind::UnknownEnumVariantTypeInPattern => Report::build(
            Kind::Error,
            14,
            "unknown enum variant type in pattern",
            span,
        ),
        ErrorKind::ExpectedEnumInPattern => {
            Report::build(Kind::Error, 15, "expected enum in pattern", span)
        }
        ErrorKind::WrongEnumVariantContainer(_) => {
            Report::build(Kind::Error, 16, "wrong enum variant", span)
        }
        ErrorKind::VariableIsNotMutable => {
            Report::build(Kind::Error, 17, "variable is not mutable", span)
        }
        ErrorKind::ArgumentIsNotMutable => {
            Report::build(Kind::Error, 18, "argument is not mutable", span)
        }
        ErrorKind::UnknownTypeReference => {
            Report::build(Kind::Error, 19, "unknown type reference", span)
        }
        ErrorKind::SemanticError(semantic_error) => build_semantic_error(semantic_error, span),
        ErrorKind::ExpectedOptional => {
            Report::build(Kind::Error, 20, "expected optional type", span)
        }
        ErrorKind::MapKeyTypeMismatch { .. } => {
            Report::build(Kind::Error, 21, "map key type mismatch", span)
        }
        ErrorKind::MapValueTypeMismatch { .. } => {
            Report::build(Kind::Error, 22, "map value type mismatch", span)
        }
        ErrorKind::IncompatibleTypes(_a, _b) => {
            Report::build(Kind::Error, 23, "incompatible types", span)
            //.with_label("first_type", a.clone())
            //.with_note(&format!("second_type {b:?}"))
        }
        ErrorKind::UnknownMemberFunction => {
            Report::build(Kind::Error, 24, "unknown member function", span)
        }
        ErrorKind::ExpressionsNotAllowedInLetPattern => Report::build(
            Kind::Error,
            25,
            "expressions not allowed in let pattern",
            span,
        ),
        ErrorKind::UnknownField => Report::build(Kind::Error, 26, "unknown field", span),
        ErrorKind::EnumVariantHasNoFields => {
            Report::build(Kind::Error, 27, "enum variant has no fields", span)
        }
        ErrorKind::TooManyTupleFields { .. } => {
            Report::build(Kind::Error, 28, "too many tuple fields", span)
        }
        ErrorKind::ExpectedBooleanExpression => {
            Report::build(Kind::Error, 29, "expected boolean expression", span)
        }
        ErrorKind::NotAnIterator => Report::build(Kind::Error, 30, "not an iterator", span),
        ErrorKind::IntConversionError(_) => {
            Report::build(Kind::Error, 31, "int conversion error", span)
        }
        ErrorKind::FloatConversionError(_) => {
            Report::build(Kind::Error, 32, "float conversion error", span)
        }
        ErrorKind::BoolConversionError => {
            Report::build(Kind::Error, 33, "bool conversion error", span)
        }
        ErrorKind::DuplicateFieldInStructInstantiation(_) => {
            Report::build(Kind::Error, 34, "duplicate field in struct literal", span)
        }
        ErrorKind::UnknownFunction => Report::build(Kind::Error, 34, "unknown function", span),
        ErrorKind::NoDefaultImplemented(_resolved_type) => {
            Report::build(Kind::Error, 35, "no default() function", span)
        }
        ErrorKind::GuardHasNoType => Report::build(Kind::Error, 36, "guard has no type", span),
        ErrorKind::NoneNeedsExpectedTypeHint => {
            Report::build(Kind::Error, 37, "none needs expected type hint", span)
        }
        ErrorKind::NotValidLocationStartingPoint => {
            Report::build(Kind::Error, 38, "not valid location starting point", span)
        }
        ErrorKind::CallsCanNotBePartOfChain
        | ErrorKind::UnwrapCanNotBePartOfChain
        | ErrorKind::NoneCoalesceCanNotBePartOfChain => todo!(),
        ErrorKind::SelfNotCorrectType => Report::build(Kind::Error, 9901, "chain error", span),
        ErrorKind::CanNotNoneCoalesce => {
            Report::build(Kind::Error, 39, "can not coalesce to none", span)
        }
        &ErrorKind::GuardCanNotHaveMultipleWildcards
        | &ErrorKind::WildcardMustBeLastInGuard
        | &ErrorKind::GuardMustHaveWildcard => {
            Report::build(Kind::Error, 9902, "guard error", span)
        }
        ErrorKind::UnknownModule => Report::build(Kind::Error, 40, "unknown module", span),
        ErrorKind::UnknownSymbol => Report::build(Kind::Error, 41, "unknown symbol", span),

        ErrorKind::NoAssociatedFunction(_, _) => {
            Report::build(Kind::Error, 42, "no associated function", span)
        }
        ErrorKind::MissingSubscriptMember => {
            Report::build(Kind::Error, 43, "missing subscript member", span)
        }
        ErrorKind::UnusedVariablesCanNotBeMut => {
            Report::build(Kind::Error, 44, "unused variables must not be mut", span)
        }
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
