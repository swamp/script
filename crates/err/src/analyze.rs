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
    match &err.kind {
        ErrorKind::UnknownEnumType => todo!(),
        ErrorKind::TypeDoNotSupportRangeAccess => {
            Report::build(Kind::Error, 4253, "type do not support range access", span)
        }
        ErrorKind::NoneCoalesceNeedsOptionalType => Report::build(
            Kind::Error,
            4243,
            "none coalesce `??` needs an optional type on the left hand side",
            span,
        ),
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
        ErrorKind::NotAnArray => Report::build(Kind::Error, 5405, "was not an array", span),
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
        ErrorKind::WrongNumberOfTupleDeconstructVariables => todo!(),
        ErrorKind::UnknownTypeReference => {
            Report::build(Kind::Error, 101, "Unknown type reference", span)
        }
        ErrorKind::SemanticError(semantic_error) => build_semantic_error(semantic_error, span),
        ErrorKind::ExpectedMemberCall => todo!(),
        ErrorKind::CouldNotFindStaticMember(x, _y) => {
            Report::build(Kind::Error, 9101, "Could not find static member", &x.span)
        }
        ErrorKind::TypeAliasNotAStruct => todo!(),
        ErrorKind::ModuleNotUnique => todo!(),
        ErrorKind::ExpressionIsOfWrongFieldType(span, expected_type, encountered_type) => {
            Report::build(
                Kind::Error,
                23401,
                &format!(
                    "Field initialization expression is of wrong type. expected {expected_type}, encountered: {encountered_type}"
                ),
                span,
            )
        }
        ErrorKind::ExpectedOptional => {
            Report::build(Kind::Error, 7801, "expected optional type", span)
        }
        ErrorKind::ExpectedVariable => Report::build(Kind::Error, 26201, "Expected variable", span),
        ErrorKind::EmptyMapLiteral => todo!(),
        ErrorKind::MapKeyTypeMismatch { .. } => todo!(),
        ErrorKind::MapValueTypeMismatch { .. } => todo!(),
        ErrorKind::TypeIsNotAnIndexCollection(_) => todo!(),
        ErrorKind::NotSameKeyTypeForMapIndex(_, _) => todo!(),
        ErrorKind::UnknownIndexAwareCollection => todo!(),
        ErrorKind::InvalidOperatorForArray => todo!(),
        ErrorKind::IncompatibleTypes(_a, _b) => {
            Report::build(Kind::Error, 102, "Incompatible types", span)
            //.with_label("first_type", a.clone())
            //.with_note(&format!("second_type {b:?}"))
        }
        ErrorKind::ExpectedArray(_) => Report::build(Kind::Error, 102, "ExpectedArray", span),
        ErrorKind::UnknownMemberFunction => {
            Report::build(Kind::Error, 101, "Unknown member function", span)
        }
        ErrorKind::WrongNumberOfTypeArguments(_, _) => {
            Report::build(Kind::Error, 101, "WrongNumberOfTypeArguments", span)
        }
        ErrorKind::OnlyVariablesAllowedInEnumPattern => todo!(),
        ErrorKind::ExpressionsNotAllowedInLetPattern => todo!(),
        ErrorKind::UnknownField => todo!(),
        ErrorKind::EnumVariantHasNoFields => todo!(),
        ErrorKind::TooManyTupleFields { .. } => todo!(),
        ErrorKind::NotInFunction => todo!(),
        ErrorKind::ExpectedBooleanExpression => {
            Report::build(Kind::Error, 102, "Expected a boolean expression", span)
        }
        ErrorKind::NotAnIterator => Report::build(Kind::Error, 101, "Not an iterator", span),
        ErrorKind::UnsupportedIteratorPairs => todo!(),
        ErrorKind::NeedStructForFieldLookup => {
            Report::build(Kind::Error, 12301, "need struct for field lookup", span)
        }
        ErrorKind::IntConversionError(_) => todo!(),
        ErrorKind::FloatConversionError(_) => todo!(),
        ErrorKind::BoolConversionError => todo!(),
        ErrorKind::DuplicateFieldInStructInstantiation(_) => todo!(),
        ErrorKind::WasNotFieldMutRef => todo!(),
        ErrorKind::UnknownFunction => Report::build(Kind::Error, 1026, "Unknown function", span),
        ErrorKind::NoDefaultImplemented(_resolved_type) => {
            Report::build(Kind::Error, 104, "No default() function", span)
        }
        ErrorKind::NoDefaultImplementedForType(_) => todo!(),
        ErrorKind::TypeDoNotSupportIndexAccess => todo!(),
        ErrorKind::GuardHasNoType => Report::build(Kind::Error, 105, "guard has no type", span),
        ErrorKind::EmptyBlockWrongType => {
            Report::build(Kind::Error, 106, "empty block wrong type", span)
        }
        ErrorKind::NoneNeedsExpectedTypeHint => {
            Report::build(Kind::Error, 107, "None needs expected type hint", span)
        }
        ErrorKind::NotValidLocationStartingPoint => {
            Report::build(Kind::Error, 108, "not valid location starting point", span)
        }
        ErrorKind::ArgumentMustBeImmutable => {
            Report::build(Kind::Error, 109, "argument must be immutable", span)
        }
        ErrorKind::NotValidLocationItem => {
            Report::build(Kind::Error, 110, "not valid location item", span)
        }
        ErrorKind::ExpectedImmutableExpression => {
            Report::build(Kind::Error, 112, "expected immutable expression", span)
        }
        ErrorKind::CallsCanNotBePartOfChain
        | ErrorKind::UnwrapCanNotBePartOfChain
        | ErrorKind::NoneCoalesceCanNotBePartOfChain => todo!(),
        ErrorKind::SelfNotCorrectType => todo!(),
        ErrorKind::IllegalIndexInChain => {
            Report::build(Kind::Error, 140, "illegal index in chain", span)
        }
        ErrorKind::CanNotNoneCoalesce => todo!(),
        &ErrorKind::GuardCanNotHaveMultipleWildcards
        | &ErrorKind::WildcardMustBeLastInGuard
        | &ErrorKind::GuardMustHaveWildcard => todo!(),
        ErrorKind::UnknownModule => Report::build(Kind::Error, 140, "Unknown module", span),
        ErrorKind::UnknownSymbol => Report::build(Kind::Error, 140, "some error", span),
        _ => todo!(),
    }
}

/// # Panics
///
pub fn show_analyzer_error(err: &Error, source_map: &SourceMap, current_dir: &Path) {
    let builder = build_analyzer_error(err);
    build_and_print(builder, "A", source_map, current_dir);
}
