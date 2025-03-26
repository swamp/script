use swamp_vm_types::aligner::align;
use swamp_vm_types::{ConstantMemoryAddress, MemoryAlignment, MemorySize};
use tracing::info;

const ALIGNMENT_MASK: usize = 0x7;

pub struct ConstantsAllocator {
    current_addr: u32,
}

impl Default for ConstantsAllocator {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstantsAllocator {
    #[must_use]
    pub const fn new() -> Self {
        Self { current_addr: 0 }
    }

    pub fn allocate(
        &mut self,
        size: MemorySize,
        alignment_enum: MemoryAlignment,
    ) -> ConstantMemoryAddress {
        let alignment: usize = alignment_enum.into();
        let start_addr = align(self.current_addr as usize, alignment) as u32;
        info!(?start_addr, "getting constant memory");

        self.current_addr = start_addr + size.0 as u32;

        ConstantMemoryAddress(start_addr)
    }

    pub fn reset(&mut self) {
        self.current_addr = 0;
    }
}

pub struct ConstantsManager {
    allocator: ConstantsAllocator,
    data: Vec<u8>,
}

impl Default for ConstantsManager {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstantsManager {
    #[must_use]
    pub fn new() -> Self {
        Self {
            allocator: ConstantsAllocator::new(),
            data: vec![0u8; 1024 * 1024],
        }
    }

    pub fn reserve(
        &mut self,
        memory_size: MemorySize,
        memory_alignment: MemoryAlignment,
    ) -> ConstantMemoryAddress {
        self.allocator.allocate(memory_size, memory_alignment)
    }

    pub fn allocate(
        &mut self,
        data: &[u8],
        alignment_enum: MemoryAlignment,
    ) -> ConstantMemoryAddress {
        let addr = self
            .allocator
            .allocate(MemorySize(data.len() as u16), alignment_enum);

        let start_idx = addr.0 as usize;
        self.data[start_idx..start_idx + data.len()].copy_from_slice(data);
        info!(?start_idx, "copying into data");

        ConstantMemoryAddress(addr.0)
    }

    pub fn take_data(self) -> Vec<u8> {
        self.data
    }
}
