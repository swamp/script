use std::fmt::{Debug, Formatter};

type FileId = u16;

#[derive(PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub file_id: FileId,
    pub offset: u32,
    pub length: u16,
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}:{} ({})>", self.offset, self.length, self.file_id)
    }
}

#[must_use]
pub const fn convert_span(without: &SpanWithoutFileId, file_id: FileId) -> Span {
    Span {
        file_id,
        offset: without.offset,
        length: without.length,
    }
}

#[derive(PartialEq, Eq, Hash, Default)]
pub struct SpanWithoutFileId {
    pub offset: u32,
    pub length: u16,
}

impl Debug for SpanWithoutFileId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}:{}>", self.offset, self.length)
    }
}
