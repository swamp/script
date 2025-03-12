use crate::{Builder, Report, build_and_print};
use eira::Kind;
use std::path::Path;
use swamp_script_node::Span;
use swamp_script_semantic::SemanticError;
use swamp_script_source_map::SourceMap;

#[must_use]
pub fn build_semantic_error(err: &SemanticError, span: &Span) -> Builder<usize> {
    let mut b = match err {
        SemanticError::CouldNotInsertStruct => {
            Report::build(Kind::Error, 140, "CouldNotInsertStruct", span)
        }
        SemanticError::DuplicateTypeAlias(_) => {
            Report::build(Kind::Error, 140, "DuplicateTypeAlias", span)
        }
        SemanticError::CanOnlyUseStructForMemberFunctions => {
            Report::build(Kind::Error, 140, "CanOnlyUseStructForMemberFunctions", span)
        }
        SemanticError::ResolveNotStruct => {
            Report::build(Kind::Error, 140, "ResolveNotStruct", span)
        }
        SemanticError::DuplicateStructName(_) => {
            Report::build(Kind::Error, 140, "DuplicateStructName", span)
        }
        SemanticError::DuplicateEnumType(_) => {
            Report::build(Kind::Error, 140, "DuplicateEnumType", span)
        }
        SemanticError::DuplicateEnumVariantType(_, _) => {
            Report::build(Kind::Error, 140, "DuplicateEnumVariantType", span)
        }
        SemanticError::DuplicateFieldName(_) => {
            Report::build(Kind::Error, 140, "DuplicateFieldName", span)
        }
        SemanticError::DuplicateExternalFunction(_) => {
            Report::build(Kind::Error, 140, "DuplicateExternalFunction", span)
        }
        SemanticError::DuplicateRustType(_) => {
            Report::build(Kind::Error, 140, "DuplicateRustType", span)
        }
        SemanticError::DuplicateConstName(_) => {
            Report::build(Kind::Error, 140, "DuplicateConstName", span)
        }
        SemanticError::CircularConstantDependency(_) => {
            Report::build(Kind::Error, 140, "CircularConstantDependency", span)
        }
        SemanticError::DuplicateConstantId(_) => {
            Report::build(Kind::Error, 140, "DuplicateConstantId", span)
        }
        SemanticError::IncompatibleTypes => {
            Report::build(Kind::Error, 140, "IncompatibleTypes", span)
        }
        SemanticError::WasNotImmutable => Report::build(Kind::Error, 140, "WasNotImmutable", span),
        SemanticError::WasNotMutable => Report::build(Kind::Error, 140, "WasNotMutable", span),
        SemanticError::UnknownImplOnType => {
            Report::build(Kind::Error, 140, "UnknownImplOnType", span)
        }
        SemanticError::DuplicateNamespaceLink(_) => {
            Report::build(Kind::Error, 140, "DuplicateNamespaceLink", span)
        }
        &swamp_script_semantic::SemanticError::DuplicateSymbolName(_)
        | &swamp_script_semantic::SemanticError::MismatchedTypes { .. } => todo!(),
        &swamp_script_semantic::SemanticError::UnknownTypeVariable => todo!(),
    };

    b.error_module = "S".to_string();
    b
}

pub fn show_semantic_error(err: &SemanticError, source_map: &SourceMap, current_dir: &Path) {
    let builder = build_semantic_error(err, &Span::default());
    build_and_print(builder, source_map, current_dir);
}
