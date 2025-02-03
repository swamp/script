/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use eira::Kind::Error;
use eira::{Color, Kind, Pos, PosSpan, SourceLines};
use std::fmt::Display;
use std::io;
use std::io::{stderr, Write};
use swamp_script_analyzer::err::ResolveErrorKind;
use swamp_script_analyzer::prelude::ResolveError;
use swamp_script_eval::err::ExecuteErrorKind;
use swamp_script_eval::prelude::ExecuteError;
use swamp_script_parser::SpecificError;
use swamp_script_semantic::Span;
use swamp_script_source_map::{FileId, SourceMap};

pub struct SourceLinesWrap<'a> {
    pub file_id: FileId,
    pub source_map: &'a SourceMap,
}

impl<'a> SourceLines for SourceLinesWrap<'a> {
    fn get_line(&self, line_number: usize) -> Option<&str> {
        self.source_map.get_source_line(self.file_id, line_number)
    }
}

pub struct Report<C> {
    config: Builder<C>,
}

impl<C: Display + Clone> Report<C> {
    pub fn build(kind: Kind, code: C, error_name: &str, primary_span: &Span) -> Builder<C> {
        Builder {
            primary_span: primary_span.clone(),
            kind,
            error_code: code,
            error_name: error_name.to_string(),
            labels: vec![],
            note: None,
        }
    }

    pub const fn new(config: Builder<C>) -> Self {
        Self { config }
    }

    /// # Errors
    ///
    pub fn print(&self, source_map: &SourceMap, mut writer: impl Write) -> io::Result<()> {
        let header = eira::Header {
            header_kind: self.config.kind,
            code: self.config.error_code.clone(),
            message: self.config.error_name.clone(),
        };
        header.write(&mut writer)?;
        let primary_span = &self.config.primary_span;
        let (row, col) =
            source_map.get_span_location_utf8(primary_span.file_id, primary_span.offset as usize);
        let filename = source_map.fetch_relative_filename(primary_span.file_id);

        eira::FileSpanMessage::write(
            filename,
            &PosSpan {
                pos: Pos { x: col, y: row },
                length: primary_span.length as usize,
            },
            &mut writer,
        )?;

        let mut source_file_section = eira::SourceFileSection::new();
        for label in &self.config.labels {
            let (row, col) =
                source_map.get_span_location_utf8(label.span.file_id, label.span.offset as usize);

            source_file_section.labels.push(eira::Label {
                start: Pos { x: col, y: row },
                character_count: label.span.length as usize,
                text: label.description.clone(),
                color: Color::default(),
            });
        }

        if self.config.labels.is_empty() {
            source_file_section.labels.push(eira::Label {
                start: Pos { x: col, y: row },
                character_count: primary_span.length as usize,
                text: self.config.error_name.clone(),
                color: Color::default(),
            });
        }

        source_file_section.layout();

        let source_line_wrap = SourceLinesWrap {
            file_id: primary_span.file_id,
            source_map,
        };
        source_file_section.draw(&source_line_wrap, &mut writer)?;

        if let Some(found_note) = &self.config.note {
            let header = eira::Header {
                header_kind: Kind::Note,
                code: 100,
                message: found_note.to_string(),
            };
            header.write(&mut writer)?;
        }

        Ok(())
    }
}

pub struct Label {
    pub span: Span,
    pub description: String,
}

pub struct Builder<C> {
    pub primary_span: Span,
    pub kind: Kind,
    pub error_code: C,
    pub error_name: String,
    pub labels: Vec<Label>,
    pub note: Option<String>,
}

impl<C: Display + Clone> Builder<C> {
    #[must_use]
    pub fn with_label(mut self, label: &str, span: Span) -> Self {
        let l = Label {
            span,
            description: label.to_string(),
        };

        self.labels.push(l);
        self
    }

    #[must_use]
    pub fn with_note(mut self, note: &str) -> Self {
        self.note = Some(note.to_string());
        self
    }

    pub const fn build(self) -> Report<C> {
        Report::new(self)
    }
}

/// # Panics
///
pub fn show_execute_error(err: &ExecuteError, source_map: &SourceMap) {
    let builder = build_execute_error(err);
    let report = builder.build();
    report.print(source_map, stderr()).unwrap();
}

/// # Panics
///
pub fn show_parse_error(err: &SpecificError, span: &Span, source_map: &SourceMap) {
    let builder = build_parse_error(err, span);
    let report = builder.build();
    report.print(source_map, stderr()).unwrap();
}

#[must_use]
pub fn build_parse_error(err: &SpecificError, span: &Span) -> Builder<usize> {
    match err {
        SpecificError::General(general) => {
            Report::build(Error, 1, &format!("General Parse Error: {general}"), span)
        }

        SpecificError::MissingTypeName => Report::build(Error, 1, "missing type name", span),
        SpecificError::UnknownEscapeCharacter(char) => Report::build(
            Error,
            1,
            &format!("Unknown escape character '{char}'"),
            span,
        ),
        SpecificError::UnfinishedEscapeSequence => {
            Report::build(Error, 1, "Unfinished escape sequence", span)
        }
        SpecificError::InvalidUnicodeEscape => {
            Report::build(Error, 1, "invalid unicode escape sequence", span)
        }
        SpecificError::InvalidHexEscape => {
            Report::build(Error, 1, "invalid hex escape sequence", span)
        }
        SpecificError::InvalidUtf8Sequence => {
            Report::build(Error, 1, "invalid utf-8 escape sequence", span)
        }
        SpecificError::ExpectingTypeIdentifier => todo!(),
        SpecificError::ExpectingInnerPair => todo!(),
        SpecificError::UnexpectedTypeRule => Report::build(Error, 1, "unexpected type rule", span),
        SpecificError::ExpectedTypeIdentifier(_) => todo!(),
        SpecificError::ExpectedLocalTypeIdentifier(_) => todo!(),
        SpecificError::UnexpectedRuleInParseScript(rule) => {
            Report::build(Error, 1, &format!("unexpected type rule {rule}"), span)
        }
        SpecificError::ExpectedControlStatement(_) => todo!(),
        SpecificError::ExpectedStatement(_) => todo!(),
        SpecificError::ExpectedIfOrElse(_) => todo!(),
        SpecificError::MissingFunctionSignature => todo!(),
        SpecificError::MissingFunctionBody => todo!(),
        SpecificError::ExpectedStatementBlock => todo!(),
        SpecificError::ExpectedFunctionDefinition => todo!(),
        SpecificError::ExpectedParameter => todo!(),
        SpecificError::ExpectedImplItem => todo!(),
        SpecificError::ExpectedMemberSignature => todo!(),
        SpecificError::ExpectedBlockInWhileLoop => todo!(),
        SpecificError::UnexpectedExpressionType(_) => todo!(),
        SpecificError::UnexpectedAccessType(_) => todo!(),
        SpecificError::UnknownAssignmentOperator(_) => todo!(),
        SpecificError::CompoundOperatorCanNotContainMut => todo!(),
        SpecificError::InvalidAssignmentTarget => {
            Report::build(Error, 1, "invalid assignment target", span)
        }
        SpecificError::CompoundOperatorCanNotHaveMultipleVariables => todo!(),
        SpecificError::ExpectedExpressionAfterPrefixOperator => todo!(),
        SpecificError::UnknownOperator(_) => Report::build(Error, 1, "unknown operator", span),
        SpecificError::UnexpectedPostfixOperator => todo!(),
        SpecificError::UnexpectedUnaryOperator(_) => todo!(),
        SpecificError::InvalidMemberCall => todo!(),
        SpecificError::UnknownMatchType => todo!(),
        SpecificError::UnexpectedElementInPatternList => todo!(),
        SpecificError::InvalidPrecisionValue => todo!(),
        SpecificError::InvalidPrecisionType => todo!(),
        SpecificError::ExpectedTypeIdentifierAfterPath => todo!(),
        SpecificError::UnexpectedPatternListElement(_) => todo!(),
        SpecificError::MustHaveAtLeastOneArm => todo!(),
        SpecificError::UnexpectedMatchArmRule(_) => todo!(),
        SpecificError::UnknownEnumVariant(_) => todo!(),
        SpecificError::UnknownLiteral => todo!(),
        SpecificError::UnknownPrimary(_) => todo!(),
        SpecificError::InvalidFormatSpecifier => todo!(),
        SpecificError::UnexpectedVariantField => todo!(),
        SpecificError::MutOnlyForVariables => todo!(),
        SpecificError::UnexpectedTokenInFunctionCall => todo!(),
        SpecificError::ExpectedExpressionInInterpolation => todo!(),
        SpecificError::UnexpectedRuleInInterpolation => todo!(),
        SpecificError::ExpectedForPattern => todo!(),
        SpecificError::ExpectedBlock => Report::build(Error, 321, "ExpectedBlock", span),
        SpecificError::InvalidForPattern => todo!(),
        SpecificError::UnexpectedRuleInElse(_) => todo!(),
        SpecificError::ExpectedLocationExpression => todo!(),
        SpecificError::ExpectedImportPath => todo!(),
        SpecificError::ExpectedIdentifier => todo!(),
        SpecificError::ExpectedIdentifierAfterPath => todo!(),
        SpecificError::ExpectedFieldOrRest => todo!(),
        SpecificError::UnknownTerm(_) => Report::build(Error, 32241, "UnknownTerm", span),
        SpecificError::UnknownExpr(_) => todo!(),
        &swamp_script_parser::SpecificError::CouldNotMoveDown
        | &swamp_script_parser::SpecificError::CouldNotMoveRight
        | &swamp_script_parser::SpecificError::UnexpectedTokenInMutableExpression => todo!(),
    }
}

/// # Panics
///
pub fn show_error(err: &ResolveError, source_map: &SourceMap) {
    let builder = build_resolve_error(err);
    let report = builder.build();
    report.print(source_map, stderr()).unwrap();
}

#[must_use]
#[allow(clippy::too_many_lines)]
pub fn build_resolve_error(err: &ResolveError) -> Builder<usize> {
    let span = &err.node.span;
    match &err.kind {
        ResolveErrorKind::TypeDoNotSupportRangeAccess => Report::build(
            Error,
            4253,
            "type do not support range access",
            span,
        ),
        ResolveErrorKind::NoneCoalesceNeedsOptionalType => Report::build(
            Error,
            4243,
            "none coalesce `??` needs an optional type on the left hand side",
            span,
        ),
        ResolveErrorKind::TooManyDestructureVariables => Report::build(
            Error,
            4203,
            "TooManyDestructureVariables",
            span,
        ),
        ResolveErrorKind::CanNotDestructure => Report::build(
            Error,
            4203,
            "Can Not Destructure",
            span,
        ),
        ResolveErrorKind::EmptyArrayCanOnlyBeMapOrArray => Report::build(
            Error,
            903,
            "EmptyArrayCanOnlyBeMapOrArray",
            &Span::default(),
        ),
        ResolveErrorKind::UnknownConstant => Report::build(
            Error,
            903,
            "Unknown constant",
            span,
        ),
        ResolveErrorKind::CanNotFindModule(x) => Report::build(
            Error,
            902,
            &format!("Can not find module {x:?}"),
            &Span::default(),
        ),
        ResolveErrorKind::UnknownStructTypeReference => Report::build(
            Error,
            105,
            "Unknown Struct Type Reference",
            span,
        ),
        ResolveErrorKind::UnknownLocalStructTypeReference(_) => todo!(),
        ResolveErrorKind::DuplicateFieldName => todo!(),
        ResolveErrorKind::Unknown(_) => todo!(),
        ResolveErrorKind::UnknownImplTargetTypeReference(_) => todo!(),
        ResolveErrorKind::WrongFieldCountInStructInstantiation(_, _) => todo!(),
        ResolveErrorKind::MissingFieldInStructInstantiation(fields, _struct_type) => Report::build(
            Error,
            903,
            "missing fields in instantiation",
            span,
        ).with_note(&format!("fields: {fields:?}")
        ),
        ResolveErrorKind::ExpectedFunctionExpression => todo!(),
        ResolveErrorKind::CouldNotFindMember(_, _) => todo!(),
        ResolveErrorKind::UnknownVariable => {
            Report::build(Error, 105, "Unknown variable", span)
        }
        ResolveErrorKind::NotAnArray => Report::build(Error, 5405, "was not an array", span),
        ResolveErrorKind::ArrayIndexMustBeInt(_) => todo!(),
        ResolveErrorKind::OverwriteVariableWithAnotherType => Report::build(Error, 14505, "overwrite variable with another type", span),
        ResolveErrorKind::WrongNumberOfArguments(_expected, _encountered) => {
            Report::build(Error, 105, "wrong number of arguments", span)
        }
        ResolveErrorKind::IncompatibleArguments(_a, _b) => Report::build(
            Error,
            904,
            "Incompatible arguments",
            span,
        )
        //    .with_label("first_type", a.to_string())
        //.with_label("second_type", b.to_string())
        ,
        ResolveErrorKind::CanOnlyOverwriteVariableWithMut => Report::build(
            Error,
            90423,
            "Variable needs to be mut to overwrite",
            span,
        ),
        ResolveErrorKind::OverwriteVariableNotAllowedHere => todo!(),
        ResolveErrorKind::NotNamedStruct(_) => todo!(),
        ResolveErrorKind::UnknownEnumVariantType => Report::build(
            Error,
            903,
            "Unknown enum variant type", span,
        ),
        ResolveErrorKind::WasNotStructType => Report::build(
            Error,
            903,
            "Not a struct type",
            span,
        ),
        ResolveErrorKind::UnknownStructField => Report::build(
            Error,
            106,
            "Unknown Struct Field Reference",
            span,
        ),
        ResolveErrorKind::MustBeEnumType(_) => todo!(),
        ResolveErrorKind::UnknownEnumVariantTypeInPattern => Report::build(
            Error,
            106,
            "Unknown enum variant type in pattern",
            span,
        ),
        ResolveErrorKind::ExpectedEnumInPattern => todo!(),
        ResolveErrorKind::WrongEnumVariantContainer(_) => todo!(),
        ResolveErrorKind::VariableIsNotMutable =>  Report::build(Error, 75301, "Variable is not mutable", span),
        ResolveErrorKind::ArgumentIsNotMutable => Report::build(Error, 1401, "Argument is not mutable", span),
        ResolveErrorKind::WrongNumberOfTupleDeconstructVariables => todo!(),
        ResolveErrorKind::UnknownTypeReference => {
            Report::build(Error, 101, "Unknown type reference", span)
        }
        ResolveErrorKind::SemanticError(a) => Report::build(Error, 141, &format!("semantic error {a:?}"), &Span::dummy()),
        ResolveErrorKind::SeqMapError(_) => todo!(),
        ResolveErrorKind::ExpectedMemberCall => todo!(),
        ResolveErrorKind::CouldNotFindStaticMember(x, _y) => {
            Report::build(Error, 9101, "Could not find static member", &x.span)
        }
        ResolveErrorKind::TypeAliasNotAStruct => todo!(),
        ResolveErrorKind::ModuleNotUnique => todo!(),
        ResolveErrorKind::ExpressionIsOfWrongFieldType(span, expected_type, encountered_type) => {
            Report::build(Error, 23401, &format!("Field initialization expression is of wrong type. expected {expected_type}, encountered: {encountered_type}"), span)
        }
        ResolveErrorKind::ExpectedOptional => Report::build(Error, 7801, "expected optional type", span),
        ResolveErrorKind::ExpectedVariable => {
            Report::build(Error, 26201, "Expected variable", span)
        }
        ResolveErrorKind::EmptyMapLiteral => todo!(),
        ResolveErrorKind::MapKeyTypeMismatch { .. } => todo!(),
        ResolveErrorKind::MapValueTypeMismatch { .. } => todo!(),
        ResolveErrorKind::TypeIsNotAnIndexCollection(_) => todo!(),
        ResolveErrorKind::NotSameKeyTypeForMapIndex(_, _) => todo!(),
        ResolveErrorKind::UnknownIndexAwareCollection => todo!(),
        ResolveErrorKind::InvalidOperatorForArray => todo!(),
        ResolveErrorKind::IncompatibleTypes(_a, _b) => {
            Report::build(Error, 102, "Incompatible types", span)
            //.with_label("first_type", a.clone())
            //.with_note(&format!("second_type {b:?}"))
        }
        ResolveErrorKind::ExpectedArray(_) => Report::build(Error, 102, "ExpectedArray", span),
        ResolveErrorKind::UnknownMemberFunction => {
            Report::build(Error, 101, "Unknown member function", span)
        }
        ResolveErrorKind::WrongNumberOfTypeArguments(_, _) => todo!(),
        ResolveErrorKind::OnlyVariablesAllowedInEnumPattern => todo!(),
        ResolveErrorKind::ExpressionsNotAllowedInLetPattern => todo!(),
        ResolveErrorKind::UnknownField => todo!(),
        ResolveErrorKind::EnumVariantHasNoFields => todo!(),
        ResolveErrorKind::TooManyTupleFields { .. } => todo!(),
        ResolveErrorKind::NotInFunction => todo!(),
        ResolveErrorKind::ExpectedBooleanExpression => {
            Report::build(Error, 102, "Expected a boolean expression", span)
        }
        ResolveErrorKind::NotAnIterator => Report::build(Error, 101, "Not an iterator", span),
        ResolveErrorKind::UnsupportedIteratorPairs => todo!(),
        ResolveErrorKind::NeedStructForFieldLookup => Report::build(Error, 12301, "need struct for field lookup", span),
        ResolveErrorKind::IntConversionError(_) => todo!(),
        ResolveErrorKind::FloatConversionError(_) => todo!(),
        ResolveErrorKind::BoolConversionError => todo!(),
        ResolveErrorKind::DuplicateFieldInStructInstantiation(_) => todo!(),
        ResolveErrorKind::InternalError(_) => todo!(),
        ResolveErrorKind::WasNotFieldMutRef => todo!(),
        ResolveErrorKind::UnknownFunction => {
            Report::build(Error, 1026, "Unknown function", span)
        }
        ResolveErrorKind::NoDefaultImplemented(_resolved_type) => {
            Report::build(Error, 104, "No default() function", span)
        }
        ResolveErrorKind::NoDefaultImplementedForStruct(_) => todo!(),
        ResolveErrorKind::ExpectedFunctionTypeForFunctionCall => Report::build(Error, 4404, "expected function type for function call", span),
        &ResolveErrorKind::TypeDoNotSupportIndexAccess => todo!(),
        ResolveErrorKind::ExpectedMutableLocation => Report::build(Error, 104, "expected mutable location", span),
        ResolveErrorKind::GuardHasNoType =>Report::build(Error, 105, "guard has no type", span),
        ResolveErrorKind::EmptyBlockWrongType =>Report::build(Error, 106, "empty block wrong type", span),
        ResolveErrorKind::NoneNeedsExpectedTypeHint =>Report::build(Error, 107, "None needs expected type hint", span),
        ResolveErrorKind::NotValidLocationStartingPoint =>Report::build(Error, 108, "not valid location starting point", span),
        ResolveErrorKind::ArgumentMustBeImmutable =>Report::build(Error, 109, "argument must be immutable", span),
        ResolveErrorKind::NotValidLocationItem =>Report::build(Error, 110, "not valid location item", span),
        ResolveErrorKind::ExpectedImmutableExpression =>Report::build(Error, 112, "expected immutable expression", span),
        &swamp_script_analyzer::err::ResolveErrorKind::CallsCanNotBePartOfChain | &swamp_script_analyzer::err::ResolveErrorKind::UnwrapCanNotBePartOfChain | &swamp_script_analyzer::err::ResolveErrorKind::NoneCoalesceCanNotBePartOfChain => todo!(),
        &swamp_script_analyzer::err::ResolveErrorKind::SelfNotCorrectType => todo!(),
        &swamp_script_analyzer::err::ResolveErrorKind::IllegalIndexInChain => Report::build(Error, 140, "illegal index in chain", span),
        &swamp_script_analyzer::err::ResolveErrorKind::CanNotNoneCoalesce => todo!(),
        &ResolveErrorKind::GuardCanNotHaveMultipleWildcards | &ResolveErrorKind::WildcardMustBeLastInGuard | &ResolveErrorKind::GuardMustHaveWildcard => todo!(),
    }
}

#[must_use]
pub fn build_execute_error(err: &ExecuteError) -> Builder<usize> {
    let span = &err.node.span;
    match &err.kind {
        ExecuteErrorKind::ExpectedInt => Report::build(Error, 104, "expected int", span),
        ExecuteErrorKind::ExpectedString => Report::build(Error, 104, "expected string", span),
        ExecuteErrorKind::ConversionError(_) => todo!(),
        ExecuteErrorKind::ValueError(_) => todo!(),
        ExecuteErrorKind::ArgumentIsNotMutable => {
            Report::build(Error, 104, "argument is not mutable", span)
        }
        ExecuteErrorKind::CanNotUnwrap => todo!(),
        ExecuteErrorKind::IllegalIterator => todo!(),
        ExecuteErrorKind::ExpectedOptional => todo!(),
        ExecuteErrorKind::NonUniqueKeysInMapLiteralDetected => todo!(),
        ExecuteErrorKind::NotAnArray => todo!(),
        ExecuteErrorKind::ValueIsNotMutable => todo!(),
        ExecuteErrorKind::NotSparseValue => todo!(),
        ExecuteErrorKind::CoerceOptionToBoolFailed => todo!(),
        ExecuteErrorKind::VariableWasNotMutable => todo!(),
        ExecuteErrorKind::ContinueNotAllowedHere => todo!(),
        ExecuteErrorKind::BreakNotAllowedHere => todo!(),
        ExecuteErrorKind::NotMutLocationFound => todo!(),
        ExecuteErrorKind::IndexWasNotInteger => todo!(),
        ExecuteErrorKind::NotAMap => todo!(),
        ExecuteErrorKind::MissingExternalFunction(_) => todo!(),
        ExecuteErrorKind::WrongNumberOfArguments(_, _) => todo!(),
        ExecuteErrorKind::IncompatibleTypes => todo!(),
        ExecuteErrorKind::RangeItemMustBeInt => todo!(),
        ExecuteErrorKind::OperationRequiresArray => todo!(),
        ExecuteErrorKind::ExpectedFloat => todo!(),
        ExecuteErrorKind::ExpectedTwoFloatTuple => todo!(),
        ExecuteErrorKind::ExpectedFunction => todo!(),
        ExecuteErrorKind::NotSparseId => todo!(),
        ExecuteErrorKind::ReturnNotAllowedHere => todo!(),
        ExecuteErrorKind::ExpectedStruct => Report::build(Error, 154, "expected struct", span),
        ExecuteErrorKind::ExpectedArray => todo!(),
        ExecuteErrorKind::ExpectedMap => todo!(),
        &swamp_script_eval::err::ExecuteErrorKind::PostfixChainError => todo!(),
        &swamp_script_eval::err::ExecuteErrorKind::IndexOutOfBounds => todo!(),
        &ExecuteErrorKind::DivideByZero | &ExecuteErrorKind::MapKeyAlreadyExists => todo!(),
        &swamp_script_eval::err::ExecuteErrorKind::MustHaveGuardArmThatMatches => todo!(),
        ExecuteErrorKind::CouldNotConvertFromSignal => todo!(),
    }
}
