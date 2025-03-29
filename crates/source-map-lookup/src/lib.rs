/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::fmt::Debug;
use std::path::PathBuf;
use swamp_node::{Node, Span};
use swamp_source_map::SourceMap;

pub struct FileLineInfo {
    pub row: usize,
    pub col: usize,
    pub line: String,
    pub relative_file_name: String,
}

pub trait SourceMapLookup: Debug {
    fn get_text(&self, resolved_node: &Node) -> &str;
    fn get_text_span(&self, span: &Span) -> &str;

    fn get_line(&self, span: &Span) -> FileLineInfo;
}

#[derive(Debug)]
pub struct SourceMapWrapper<'a> {
    pub source_map: &'a SourceMap,
    pub current_dir: PathBuf,
}

impl SourceMapLookup for SourceMapWrapper<'_> {
    fn get_text(&self, resolved_node: &Node) -> &str {
        self.source_map.get_span_source(
            resolved_node.span.file_id,
            resolved_node.span.offset as usize,
            resolved_node.span.length as usize,
        )
    }

    fn get_text_span(&self, span: &Span) -> &str {
        self.source_map
            .get_span_source(span.file_id, span.offset as usize, span.length as usize)
    }

    fn get_line(&self, span: &Span) -> FileLineInfo {
        let relative_file_name = self
            .source_map
            .get_relative_path_to(span.file_id, &self.current_dir)
            .unwrap();
        let (row, col) = self
            .source_map
            .get_span_location_utf8(span.file_id, span.offset as usize);
        let line = self.source_map.get_source_line(span.file_id, row).unwrap();

        FileLineInfo {
            row,
            col,
            line: line.to_string(),
            relative_file_name: relative_file_name.to_str().unwrap().to_string(),
        }
    }
}
