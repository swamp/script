pub mod opcode;

#[repr(C, packed)]
pub struct BinaryInstruction {
    pub opcode: u8,
    pub opcode_count: u8, // It is mainly for alignment, but use it for opcode_count or maybe checksum or extra flags?
    pub operands: [u16; 4],
}

#[derive(Copy, Clone)]
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
pub struct FrameMemoryAddress(pub u16); // relative to the frame pointer

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
    pub const fn add(&self, memory_size: MemorySize) -> Self {
        Self(self.0 + memory_size.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct MemoryOffset(pub u16);

#[derive(Debug, Copy, Clone)]
pub struct MemorySize(pub u16);

#[derive(Copy, Clone)]
pub struct FrameMemorySize(pub u16);

#[derive(Debug, Clone)]
pub struct InstructionPosition(pub u16);
