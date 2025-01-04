/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use eira::Kind::Error;
use eira::{Kind, Pos, PosSpan, SourceLines};
use std::fmt::Display;
use std::io;
use std::io::{stderr, Write};
use swamp_script_analyzer::ResolveError;
use swamp_script_parser::SpecificError;
use swamp_script_semantic::{Span, Spanned};
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

    pub fn new(config: Builder<C>) -> Self {
        Self { config }
    }

    pub fn print(&self, source_map: &SourceMap, mut writer: impl Write) -> io::Result<()> {
        let header = eira::Header {
            header_kind: self.config.kind.clone(),
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
                color: Default::default(),
            });
        }

        if self.config.labels.is_empty() {
            source_file_section.labels.push(eira::Label {
                start: Pos { x: col, y: row },
                character_count: primary_span.length as usize,
                text: self.config.error_name.clone(),
                color: Default::default(),
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
    pub labels: Vec<crate::Label>,
    pub note: Option<String>,
}

impl<C: Display + std::clone::Clone> Builder<C> {
    pub fn with_label(mut self, label: &str, span: Span) -> Self {
        let l = crate::Label {
            span,
            description: label.to_string(),
        };

        self.labels.push(l);
        self
    }

    pub fn with_note(mut self, note: &str) -> Self {
        self.note = Some(note.to_string());
        self
    }

    pub fn build(self) -> Report<C> {
        Report::new(self)
    }
}

pub fn show_parse_error(err: &SpecificError, span: &Span, source_map: &SourceMap) {
    let builder = build_parse_error(&err, span);
    let report = builder.build();
    report.print(source_map, stderr()).unwrap();
}

pub fn build_parse_error(err: &SpecificError, span: &Span) -> Builder<usize> {
    match err {
        SpecificError::General(general) => Report::build(
            Error,
            1,
            &format!("General Parse Error: {}", general),
            &span,
        ),
        SpecificError::ExpectingTypeIdentifier => todo!(),
        SpecificError::ExpectingInnerPair => todo!(),
        SpecificError::UnexpectedTypeRule => todo!(),
        SpecificError::ExpectedTypeIdentifier(_) => todo!(),
        SpecificError::ExpectedLocalTypeIdentifier(_) => todo!(),
        SpecificError::UnexpectedRuleInParseScript(_) => todo!(),
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
        SpecificError::InvalidAssignmentTarget => todo!(),
        SpecificError::CompoundOperatorCanNotHaveMultipleVariables => todo!(),
        SpecificError::ExpectedExpressionAfterPrefixOperator => todo!(),
        SpecificError::UnknownOperator(_) => todo!(),
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
        SpecificError::ExpectedBlock => todo!(),
        SpecificError::InvalidForPattern => todo!(),
        SpecificError::UnexpectedRuleInElse(_) => todo!(),
        SpecificError::ExpectedLocationExpression => todo!(),
        SpecificError::ExpectedImportPath => todo!(),
        SpecificError::ExpectedIdentifier => todo!(),
        SpecificError::ExpectedIdentifierAfterPath => todo!(),
        SpecificError::ExpectedFieldOrRest => todo!(),
    }
}

pub fn show_error(err: &ResolveError, source_map: &SourceMap) {
    let builder = build_resolve_error(err);
    let report = builder.build();
    report.print(&source_map, stderr()).unwrap();
}

#[must_use]
pub fn build_resolve_error(err: &ResolveError) -> Builder<usize> {
    match err {
        ResolveError::UnknownConstant(_) => todo!(),
        ResolveError::NamespaceError(_) => todo!(),
        ResolveError::CanNotFindModule(x) => Report::build(
            Error,
            902,
            &format!("Can not find module {x:?}"),
            &Span::default(),
        ),
        ResolveError::UnknownStructTypeReference(qualified_type_identifier) => Report::build(
            Error,
            105,
            "Unknown Struct Type Reference",
            &qualified_type_identifier.span,
        ),
        ResolveError::UnknownLocalStructTypeReference(_) => todo!(),
        ResolveError::DuplicateFieldName(_) => todo!(),
        ResolveError::Unknown(_) => todo!(),
        ResolveError::UnknownImplTargetTypeReference(_) => todo!(),
        ResolveError::WrongFieldCountInStructInstantiation(_, _) => todo!(),
        ResolveError::MissingFieldInStructInstantiation(_, _) => todo!(),
        ResolveError::ExpectedFunctionExpression => todo!(),
        ResolveError::CouldNotFindMember(_, _) => todo!(),
        ResolveError::UnknownVariable(node) => {
            Report::build(Error, 105, "Unknown variable", &node.span)
        }
        ResolveError::NotAnArray(_) => todo!(),
        ResolveError::ArrayIndexMustBeInt(_) => todo!(),
        ResolveError::OverwriteVariableWithAnotherType(_) => todo!(),
        ResolveError::WrongNumberOfArguments(_, _) => {
            todo!()
        }
        ResolveError::IncompatibleArguments(_, _) => todo!(),
        ResolveError::CanOnlyOverwriteVariableWithMut(_) => todo!(),
        ResolveError::OverwriteVariableNotAllowedHere(_) => todo!(),
        ResolveError::NotNamedStruct(_) => todo!(),
        ResolveError::UnknownEnumVariantType(_) => todo!(),
        ResolveError::WasNotStructType(_) => todo!(),
        ResolveError::UnknownStructField(field_node) => Report::build(
            Error,
            106,
            "Unknown Struct Field Reference",
            &field_node.span,
        ),
        ResolveError::MustBeEnumType(_) => todo!(),
        ResolveError::UnknownEnumVariantTypeInPattern(node) => Report::build(
            Error,
            106,
            "Unknown enum variant type in pattern",
            &node.span,
        ),
        ResolveError::ExpectedEnumInPattern(_) => todo!(),
        ResolveError::WrongEnumVariantContainer(_) => todo!(),
        ResolveError::VariableIsNotMutable(_) => todo!(),
        ResolveError::ArgumentIsNotMutable => todo!(),
        ResolveError::WrongNumberOfTupleDeconstructVariables => todo!(),
        ResolveError::UnknownTypeReference(x) => {
            Report::build(Error, 101, "Unknown type reference", &x.span)
        }
        ResolveError::SemanticError(_) => todo!(),
        ResolveError::SeqMapError(_) => todo!(),
        ResolveError::ExpectedMemberCall(_) => todo!(),
        ResolveError::CouldNotFindStaticMember(x, _y) => {
            Report::build(Error, 234101, "Could not find static member", &x.span)
        }
        ResolveError::TypeAliasNotAStruct(_) => todo!(),
        ResolveError::ModuleNotUnique => todo!(),
        ResolveError::ExpressionIsOfWrongFieldType(span, expected_type, encountered_type) => {
            Report::build(Error, 234101, &format!("Field initialization expression is of wrong type. expected {expected_type}, encountered: {encountered_type}"), &span)
        },
        ResolveError::ExpectedOptional => todo!(),
        ResolveError::ExpectedVariable => todo!(),
        ResolveError::EmptyMapLiteral => todo!(),
        ResolveError::MapKeyTypeMismatch { .. } => todo!(),
        ResolveError::MapValueTypeMismatch { .. } => todo!(),
        ResolveError::TypeIsNotAnIndexCollection(_) => todo!(),
        ResolveError::NotSameKeyTypeForMapIndex(_, _) => todo!(),
        ResolveError::NonUniqueKeyValueInMap(_) => todo!(),
        ResolveError::UnknownIndexAwareCollection => todo!(),
        ResolveError::InvalidOperatorForArray(_) => todo!(),
        ResolveError::IncompatibleTypes(a, b) => {
            Report::build(Error, 102, "Incompatible types", &a)
                .with_label("first_type", a.clone())
                .with_note(&format!("second_type {:?}", b))
        }
        ResolveError::ExpectedArray(_) => todo!(),
        ResolveError::UnknownMemberFunction(node) => {
            Report::build(Error, 101, "Unknown member function", &node.span)
        }
        ResolveError::WrongNumberOfTypeArguments(_, _) => todo!(),
        ResolveError::OnlyVariablesAllowedInEnumPattern => todo!(),
        ResolveError::ExpressionsNotAllowedInLetPattern => todo!(),
        ResolveError::UnknownField(_) => todo!(),
        ResolveError::EnumVariantHasNoFields(_) => todo!(),
        ResolveError::TooManyTupleFields { .. } => todo!(),
        ResolveError::NotInFunction => todo!(),
        ResolveError::ExpectedBooleanExpression(span) => {
            Report::build(Error, 102, "Expected a boolean expression", &span)
        }
        ResolveError::NotAnIterator(span) => Report::build(Error, 101, "Not an iterator", &span),
        ResolveError::UnsupportedIteratorPairs => todo!(),
        ResolveError::NeedStructForFieldLookup => todo!(),
        ResolveError::IntConversionError(_) => todo!(),
        ResolveError::FloatConversionError(_) => todo!(),
        ResolveError::BoolConversionError => todo!(),
        ResolveError::DuplicateFieldInStructInstantiation(_) => todo!(),
        ResolveError::InternalError(_) => todo!(),
        ResolveError::WasNotFieldMutRef => todo!(),
        ResolveError::UnknownFunction(node) => {
            Report::build(Error, 1026, "Unknown function", &node.span)
        }
        ResolveError::NoDefaultImplemented(resolved_type) => {
            Report::build(Error, 104, "No default() function", &resolved_type.span())
        },
        ResolveError::NoDefaultImplementedForStruct(_) => todo!(),
    }
}
