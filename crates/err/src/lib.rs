pub mod layout;

use std::fmt::Display;
use swamp_script_analyzer::ResolveError;
use swamp_script_semantic::Span;
use swamp_script_source_map::SourceMap;
use yansi::Color;

pub struct Characters {
    pub up_arrow: char,
    pub right_arrow: char,

    pub underline_and_vertical: char,
    pub underline: char,

    pub horizontal: char,
    pub vertical: char,

    pub vertical_gap: char,
    pub vertical_break: char,

    pub box_upper_left: char,
    pub box_upper_right: char,
    pub box_lower_left: char,
    pub box_lower_right: char,

    pub left_bracket: char,
    pub right_bracket: char,

    pub crossing: char,
    pub left_crossing: char,
    pub right_crossing: char,
    pub upper_crossing: char,
    pub lower_crossing: char,
}

impl Default for Characters {
    fn default() -> Self {
        Self::new()
    }
}

impl Characters {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            underline_and_vertical: '┬',
            underline: '─',
            horizontal: '─',
            vertical: '│',
            left_bracket: '[',
            right_bracket: ']',
            up_arrow: '▲',
            right_arrow: '▶',
            box_upper_right: '╮',
            box_lower_left: '╰',
            box_upper_left: '╭',
            upper_crossing: '┬',
            lower_crossing: '┴',
            box_lower_right: '╯',
            crossing: '┼',
            left_crossing: '├',
            right_crossing: '┤',
            vertical_break: '┆',
            vertical_gap: '┆',
        }
    }
}


pub struct Report<C> {
    config: Builder<C>,
}

impl<C: Display> Report<C> {
    pub fn build(kind: Kind, code: C, error_name: &str, primary_span: Span) -> Builder<C> {
        Builder {
            primary_span,
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

    pub fn print(&self, source_map: &SourceMap) {
        show_error(
            &self.config.primary_span,
            &self.config.kind,
            &self.config.error_code,
            &self.config.error_name,
            source_map,
        );
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

impl<C: Display> Builder<C> {
    pub fn with_label(mut self, label: &str, span: Span, color: Color) -> Self {
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

pub fn show_error<C: Display>(
    span: &Span,
    kind: &Kind,
    code: &C,
    error_text: &str,
    source_map: &SourceMap,
) {
    let source =
        source_map.get_span_source(span.file_id, span.offset as usize, span.length as usize);
    let (row, col) = source_map.get_span_location_utf8(span.file_id, span.offset as usize);
    let file_info = source_map.fetch_relative_filename(span.file_id);

    eprintln!("{kind:?}[{code}]:  {error_text}:\n  --> {file_info}:{row}:{col}:\n{source}");
}

pub fn show_resolve_error(err: ResolveError, source_map: &SourceMap) {
    match err {
        ResolveError::NamespaceError(_) => todo!(),
        ResolveError::CanNotFindModule(_) => todo!(),
        ResolveError::UnknownStructTypeReference(_) => todo!(),
        ResolveError::UnknownLocalStructTypeReference(_) => todo!(),
        ResolveError::DuplicateFieldName(_) => todo!(),
        ResolveError::Unknown(_) => todo!(),
        ResolveError::UnknownImplTargetTypeReference(_) => todo!(),
        ResolveError::WrongFieldCountInStructInstantiation(_, _) => todo!(),
        ResolveError::MissingFieldInStructInstantiation(_, _) => todo!(),
        ResolveError::ExpectedFunctionExpression => todo!(),
        ResolveError::CouldNotFindMember(_, _) => todo!(),
        ResolveError::UnknownVariable(_) => todo!(),
        ResolveError::NotAnArray(_) => todo!(),
        ResolveError::ArrayIndexMustBeInt(_) => todo!(),
        ResolveError::OverwriteVariableWithAnotherType(_) => todo!(),
        ResolveError::WrongNumberOfArguments(_, _) => todo!(),
        ResolveError::IncompatibleArguments(_, _) => todo!(),
        ResolveError::CanOnlyOverwriteVariableWithMut(_) => todo!(),
        ResolveError::OverwriteVariableNotAllowedHere(_) => todo!(),
        ResolveError::NotNamedStruct(_) => todo!(),
        ResolveError::UnknownEnumVariantType(_) => todo!(),
        ResolveError::WasNotStructType(_) => todo!(),
        ResolveError::UnknownStructField(_) => todo!(),
        ResolveError::MustBeEnumType(_) => todo!(),
        ResolveError::UnknownEnumVariantTypeInPattern(_) => todo!(),
        ResolveError::ExpectedEnumInPattern(_) => todo!(),
        ResolveError::WrongEnumVariantContainer(_) => todo!(),
        ResolveError::VariableIsNotMutable(_) => todo!(),
        ResolveError::ArgumentIsNotMutable => todo!(),
        ResolveError::WrongNumberOfTupleDeconstructVariables => todo!(),
        ResolveError::UnknownTypeReference(x) => {
            //show_error(&x.span, "Unknown type reference", &source_map)
            todo!()
        }
        ResolveError::SemanticError(_) => todo!(),
        ResolveError::SeqMapError(_) => todo!(),
        ResolveError::ExpectedMemberCall(_) => todo!(),
        ResolveError::CouldNotFindStaticMember(_, _) => todo!(),
        ResolveError::TypeAliasNotAStruct(_) => todo!(),
        ResolveError::ModuleNotUnique => todo!(),
        ResolveError::ExpressionIsOfWrongFieldType => todo!(),
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
        ResolveError::IncompatibleTypes(_, _) => todo!(),
        ResolveError::ExpectedArray(_) => todo!(),
        ResolveError::UnknownMemberFunction(_) => todo!(),
        ResolveError::WrongNumberOfTypeArguments(_, _) => todo!(),
        ResolveError::OnlyVariablesAllowedInEnumPattern => todo!(),
        ResolveError::ExpressionsNotAllowedInLetPattern => todo!(),
        ResolveError::UnknownField(_) => todo!(),
        ResolveError::EnumVariantHasNoFields(_) => todo!(),
        ResolveError::TooManyTupleFields { .. } => todo!(),
        ResolveError::NotInFunction => todo!(),
        ResolveError::ExpectedBooleanExpression => todo!(),
        ResolveError::NotAnIterator(resolved_type) => {
            //
            // show_error(&resolved_type.span(), "Not an iterator", &source_map)
            todo!()
        }
        ResolveError::UnsupportedIteratorPairs => todo!(),
        ResolveError::NeedStructForFieldLookup => todo!(),
        ResolveError::IntConversionError(_) => todo!(),
        ResolveError::FloatConversionError(_) => todo!(),
        ResolveError::BoolConversionError => todo!(),
        ResolveError::DuplicateFieldInStructInstantiation(_, _) => todo!(),
        ResolveError::InternalError(_) => todo!(),
    }
}
