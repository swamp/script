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
pub struct ResolveError {
    pub node: ResolvedNode,
    pub kind: ResolveErrorKind,
}
#[derive(Debug)]
pub enum ResolveErrorKind {
    GuardCanNotHaveMultipleWildcards,
    WildcardMustBeLastInGuard,
    GuardMustHaveWildcard,
    GuardHasNoType,
    TooManyDestructureVariables,
    EmptyBlockWrongType,
    CanNotDestructure,
    EmptyArrayCanOnlyBeMapOrArray,
    //NamespaceError(NamespaceError),
    CanNotFindModule(Vec<String>),
    UnknownStructTypeReference,
    UnknownLocalStructTypeReference(LocalTypeIdentifier),
    DuplicateFieldName,
    Unknown(String),
    UnknownImplTargetTypeReference(LocalTypeIdentifier),
    WrongFieldCountInStructInstantiation(ResolvedStructTypeRef, usize),
    MissingFieldInStructInstantiation(Vec<String>, ResolvedAnonymousStructType),
    ExpectedFunctionExpression,
    CouldNotFindMember(ResolvedNode, ResolvedNode),
    UnknownVariable,
    NotAnArray,
    ArrayIndexMustBeInt(ResolvedType),
    OverwriteVariableWithAnotherType,
    NoneNeedsExpectedTypeHint,
    ExpectedMutableLocation,
    WrongNumberOfArguments(usize, usize),
    IncompatibleArguments(ResolvedType, ResolvedType),
    CanOnlyOverwriteVariableWithMut,
    OverwriteVariableNotAllowedHere,
    NotNamedStruct(ResolvedType),
    UnknownEnumVariantType,
    WasNotStructType,
    UnknownStructField,
    MustBeEnumType(Pattern),
    UnknownEnumVariantTypeInPattern,
    ExpectedEnumInPattern,
    WrongEnumVariantContainer(ResolvedEnumVariantTypeRef),
    VariableIsNotMutable,
    ArgumentIsNotMutable,
    WrongNumberOfTupleDeconstructVariables,
    UnknownTypeReference,
    SemanticError(SemanticError),
    SeqMapError(SeqMapError),
    ExpectedMemberCall,
    CouldNotFindStaticMember(ResolvedNode, ResolvedNode),
    TypeAliasNotAStruct,
    ModuleNotUnique,
    ExpressionIsOfWrongFieldType(Span, ResolvedType, ResolvedType),
    ExpectedOptional,
    ExpectedVariable,
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
    UnknownIndexAwareCollection,
    InvalidOperatorForArray,
    IncompatibleTypes(ResolvedType, ResolvedType),
    ExpectedArray(ResolvedType),
    UnknownMemberFunction,
    WrongNumberOfTypeArguments(usize, i32),
    OnlyVariablesAllowedInEnumPattern,
    ExpressionsNotAllowedInLetPattern,
    UnknownField,
    EnumVariantHasNoFields,
    TooManyTupleFields {
        max: usize,
        got: usize,
    },
    NotInFunction,
    ExpectedBooleanExpression,
    NotAnIterator,
    UnsupportedIteratorPairs,
    NeedStructForFieldLookup,
    IntConversionError(ParseIntError),
    FloatConversionError(ParseFloatError),
    BoolConversionError,
    DuplicateFieldInStructInstantiation(String),
    InternalError(&'static str),
    WasNotFieldMutRef,
    UnknownFunction,
    NoDefaultImplemented(ResolvedType),
    NoDefaultImplementedForStruct(ResolvedStructTypeRef),
    UnknownConstant,
    ExpectedFunctionTypeForFunctionCall,
    TypeDoNotSupportIndexAccess,
    NoneCoalesceNeedsOptionalType,
    TypeDoNotSupportRangeAccess,
    ArgumentMustBeImmutable,
    NotValidLocationStartingPoint,
    NotValidLocationItem,
    ExpectedImmutableExpression,
    CallsCanNotBePartOfChain,
    UnwrapCanNotBePartOfChain,
    NoneCoalesceCanNotBePartOfChain,
    SelfNotCorrectType,
    IllegalIndexInChain,
    CanNotNoneCoalesce,
}

impl From<SemanticError> for ResolveError {
    fn from(value: SemanticError) -> Self {
        Self {
            node: Default::default(),
            kind: ResolveErrorKind::SemanticError(value),
        }
    }
}
