use crate::aligner::align;
use std::fmt::{Alignment, Display, Formatter};

pub mod aligner;
pub mod opcode;

#[repr(C, packed)]
pub struct BinaryInstruction {
    pub opcode: u8,
    pub opcode_count: u8, // It is mainly for alignment, but use it for opcode_count or maybe checksum or extra flags?
    pub operands: [u16; 4],
}

#[derive(Copy, Clone, Debug)]
pub struct MemoryAddress(pub u16);

#[derive(Copy, Clone)]
pub struct StackMemoryAddress(pub u16);

impl StackMemoryAddress {
    #[must_use]
    pub const fn add(&self, memory_size: MemorySize) -> Self {
        Self(self.0 + memory_size.0)
    }
}
// relative to the stack pointer

#[derive(Debug, Copy, Clone)]
pub struct FrameMemoryAddress(pub u16);

impl FrameMemoryAddress {
    #[must_use]
    pub fn advance(&self, memory_offset: MemoryOffset) -> FrameMemoryAddress {
        FrameMemoryAddress(self.0 + memory_offset.0)
    }
}
// relative to the frame pointer

impl FrameMemoryAddress {
    #[must_use]
    pub const fn add(&self, memory_size: MemorySize) -> Self {
        Self(self.0 + memory_size.0)
    }
    #[must_use]
    pub const fn as_size(&self) -> FrameMemorySize {
        FrameMemorySize(self.0)
    }
}

impl MemoryAddress {
    #[must_use]
    pub const fn space(&self, memory_size: MemorySize, alignment: Alignment) -> Self {
        Self(self.0 + memory_size.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct MemoryOffset(pub u16);

impl MemoryOffset {
    pub fn space(&mut self, memory_size: MemorySize, alignment: MemoryAlignment) -> Self {
        let start = align(self.0 as usize, alignment.into()) as u16;
        self.0 = start + memory_size.0;
        MemoryOffset(start)
    }
}

impl MemoryOffset {
    pub fn as_size(&self) -> MemorySize {
        MemorySize(self.0)
    }
}

impl MemoryOffset {
    pub fn add(&self, size: MemorySize, alignment: MemoryAlignment) -> MemoryOffset {
        let new_start = align(self.0 as usize, alignment.into());
        MemoryOffset(new_start as u16 + size.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct MemorySize(pub u16);

#[derive(Debug, Copy, Clone)]
pub enum MemoryAlignment {
    U8,
    U16,
    U32,
    U64,
}

impl MemoryAlignment {
    #[must_use]
    const fn rank(&self) -> usize {
        match self {
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 3,
            Self::U64 => 4,
        }
    }
    #[must_use]
    pub const fn greater_than(&self, other: MemoryAlignment) -> bool {
        self.rank() > other.rank()
    }
}

impl Into<usize> for MemoryAlignment {
    fn into(self) -> usize {
        match self {
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 4,
            Self::U64 => 8,
        }
    }
}

#[must_use]
pub fn align_frame_addr(
    memory_address: FrameMemoryAddress,
    alignment: MemoryAlignment,
) -> FrameMemoryAddress {
    let raw_addr = align(memory_address.0 as usize, alignment.into());

    FrameMemoryAddress(raw_addr as u16)
}

#[must_use]
pub fn align_offset(memory_address: MemoryOffset, alignment: MemoryAlignment) -> MemoryOffset {
    let raw_addr = align(memory_address.0 as usize, alignment.into());

    MemoryOffset(raw_addr as u16)
}

#[derive(Copy, Clone)]
pub struct FrameMemorySize(pub u16);

impl Display for FrameMemorySize {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "frame size: {:04X}", self.0)
    }
}

impl FrameMemorySize {
    #[must_use]
    pub const fn add(&self, inc: MemorySize) -> Self {
        Self(self.0 + inc.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InstructionPosition(pub u16);
