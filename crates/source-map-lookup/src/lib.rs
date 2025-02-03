use swamp_script_core::prelude::SourceMapLookup;
use swamp_script_semantic::{ResolvedNode, Span};
use swamp_script_source_map::SourceMap;

#[derive(Debug)]
pub struct SourceMapWrapper {
    pub source_map: SourceMap,
}

impl SourceMapLookup for SourceMapWrapper {
    fn get_text(&self, resolved_node: &ResolvedNode) -> &str {
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
