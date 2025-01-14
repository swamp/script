/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use seq_map::SeqMapError;
use std::num::{ParseFloatError, ParseIntError};
use swamp_script_ast::{LocalTypeIdentifier, Pattern};
use swamp_script_semantic::{
    ResolvedAnonymousStructType, ResolvedEnumVariantTypeRef, ResolvedNode, ResolvedStructTypeRef,
    ResolvedType, SemanticError, Span,
};

#[derive(Debug)]
pub enum ResolveError {
    TooManyDestructureVariables,
    CanNotDestructure(Span),
    EmptyArrayCanOnlyBeMapOrArray,
    NamespaceError(NamespaceError),
    CanNotFindModule(Vec<String>),
    UnknownStructTypeReference(ResolvedNode),
    UnknownLocalStructTypeReference(LocalTypeIdentifier),
    DuplicateFieldName(ResolvedNode),
    Unknown(String),
    UnknownImplTargetTypeReference(LocalTypeIdentifier),
    WrongFieldCountInStructInstantiation(ResolvedStructTypeRef, usize),
    MissingFieldInStructInstantiation(Span, Vec<String>, ResolvedAnonymousStructType),
    ExpectedFunctionExpression,
    CouldNotFindMember(ResolvedNode, ResolvedNode),
    UnknownVariable(ResolvedNode),
    NotAnArray(Span),
    ArrayIndexMustBeInt(ResolvedType),
    OverwriteVariableWithAnotherType(ResolvedNode),
    ExpectedMutableLocation(Span),
    WrongNumberOfArguments(Span, usize, usize),
    IncompatibleArguments(Span, ResolvedType, ResolvedType),
    CanOnlyOverwriteVariableWithMut(ResolvedNode),
    OverwriteVariableNotAllowedHere(ResolvedNode),
    NotNamedStruct(ResolvedType),
    UnknownEnumVariantType(ResolvedNode),
    WasNotStructType(ResolvedNode),
    UnknownStructField(ResolvedNode),
    MustBeEnumType(Pattern),
    UnknownEnumVariantTypeInPattern(ResolvedNode),
    ExpectedEnumInPattern(ResolvedNode),
    WrongEnumVariantContainer(ResolvedEnumVariantTypeRef),
    VariableIsNotMutable(ResolvedNode),
    ArgumentIsNotMutable(Span),
    WrongNumberOfTupleDeconstructVariables,
    UnknownTypeReference(ResolvedNode),
    SemanticError(SemanticError),
    SeqMapError(SeqMapError),
    ExpectedMemberCall(ResolvedNode),
    CouldNotFindStaticMember(ResolvedNode, ResolvedNode),
    TypeAliasNotAStruct(ResolvedNode),
    ModuleNotUnique,
    ExpressionIsOfWrongFieldType(Span, ResolvedType, ResolvedType),
    ExpectedOptional(Span),
    ExpectedVariable(ResolvedNode),
    EmptyMapLiteral,
    MapKeyTypeMismatch {
        expected: ResolvedType,
        found: ResolvedType,
    },
    MapValueTypeMismatch {
        expected: ResolvedType,
        found: ResolvedType,
    },
    TypeIsNotAnIndexCollection(ResolvedType),
    NotSameKeyTypeForMapIndex(ResolvedType, ResolvedType),
    NonUniqueKeyValueInMap(SeqMapError),
    UnknownIndexAwareCollection,
    InvalidOperatorForArray(ResolvedNode),
    IncompatibleTypes(Span, ResolvedType),
    ExpectedArray(ResolvedType),
    UnknownMemberFunction(ResolvedNode),
    WrongNumberOfTypeArguments(usize, i32),
    OnlyVariablesAllowedInEnumPattern,
    ExpressionsNotAllowedInLetPattern,
    UnknownField(ResolvedNode),
    EnumVariantHasNoFields(ResolvedNode),
    TooManyTupleFields {
        max: usize,
        got: usize,
    },
    NotInFunction,
    ExpectedBooleanExpression(Span),
    NotAnIterator(Span),
    UnsupportedIteratorPairs,
    NeedStructForFieldLookup(Span),
    IntConversionError(ParseIntError),
    FloatConversionError(ParseFloatError),
    BoolConversionError,
    DuplicateFieldInStructInstantiation(String),
    InternalError(&'static str),
    WasNotFieldMutRef,
    UnknownFunction(ResolvedNode),
    NoDefaultImplemented(ResolvedType),
    NoDefaultImplementedForStruct(ResolvedStructTypeRef),
    UnknownConstant(ResolvedNode),
    ExpectedFunctionTypeForFunctionCall(Span),
    TypeDoNotSupportIndexAccess(Span),
    NoneCoalesceNeedsOptionalType(Span),
    TypeDoNotSupportRangeAccess(Span),
}

impl From<SemanticError> for ResolveError {
    fn from(value: SemanticError) -> Self {
        Self::SemanticError(value)
    }
}

impl From<SeqMapError> for ResolveError {
    fn from(value: SeqMapError) -> Self {
        Self::SeqMapError(value)
    }
}

#[derive(Debug)]
pub enum NamespaceError {}

impl From<NamespaceError> for ResolveError {
    fn from(error: NamespaceError) -> Self {
        Self::NamespaceError(error)
    }
}
