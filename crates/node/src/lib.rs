/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::fmt;
use std::fmt::{Debug, Formatter};

#[derive(Clone, Eq, PartialEq, Default, Hash)]
pub struct Node {
    pub span: Span,
}

impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.span.file_id == 0xffff {
            write!(f, "<{}:{}>", self.span.offset, self.span.length)
        } else {
            write!(
                f,
                "<{}:{} ({})>",
                self.span.offset, self.span.length, self.span.file_id
            )
        }
    }
}

pub type FileId = u16;

#[derive(PartialEq, Eq, Hash, Default, Clone)]
pub struct Span {
    pub file_id: FileId,
    pub offset: u32,
    pub length: u16,
}

impl Span {
    pub fn dummy() -> Self {
        Span {
            offset: 0,
            length: 0,
            file_id: 0xffff,
        }
    }

    // Helper method to get the end position
    pub fn end(&self) -> u32 {
        self.offset + self.length as u32
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}:{} ({})>", self.offset, self.length, self.file_id)
    }
}

impl Node {
    pub fn new_unknown() -> Self {
        Self {
            span: Span {
                file_id: 0xffff,
                offset: 0,
                length: 0,
            },
        }
    }
}
