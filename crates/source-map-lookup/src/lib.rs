/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use swamp_script_core_extra::prelude::SourceMapLookup;
use swamp_script_node::{Node, Span};
use swamp_script_source_map::SourceMap;

#[derive(Debug)]
pub struct SourceMapWrapper<'a> {
    pub source_map: &'a SourceMap,
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
}
