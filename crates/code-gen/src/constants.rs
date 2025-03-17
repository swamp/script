use swamp_vm_types::{MemoryAddress, MemoryAlignment, MemorySize};

const CONSTANTS_START: u16 = 0xFFFF;
const ALIGNMENT_MASK: usize = 0x7;

pub struct ConstantsAllocator {
    current_addr: MemoryAddress,
}

impl Default for ConstantsAllocator {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstantsAllocator {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            current_addr: MemoryAddress(CONSTANTS_START),
        }
    }

    pub fn allocate(&mut self, size: MemorySize, alignment_enum: MemoryAlignment) -> MemoryAddress {
        let alignment: usize = alignment_enum.into();

        let aligned_size = (size.0 as usize + alignment - 1) & !(alignment - 1);

        self.current_addr =
            MemoryAddress((self.current_addr.0 - aligned_size as u16) & !(alignment - 1) as u16);

        self.current_addr
    }

    pub fn reset(&mut self) {
        self.current_addr = MemoryAddress(CONSTANTS_START);
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
    pub const fn new() -> Self {
        Self {
            allocator: ConstantsAllocator::new(),
            data: Vec::new(),
        }
    }

    pub fn allocate(&mut self, data: &[u8], alignment_enum: MemoryAlignment) -> MemoryAddress {
        let addr = self
            .allocator
            .allocate(MemorySize(data.len() as u16), alignment_enum);

        let required_len = (CONSTANTS_START - addr.0 + 1) as usize;
        if required_len > self.data.len() {
            self.data.resize(required_len, 0);
        }

        let start_idx = (CONSTANTS_START - addr.0 - data.len() as u16) as usize;
        self.data[start_idx..start_idx + data.len()].copy_from_slice(data);

        addr
    }

    pub fn take_data(self) -> Vec<u8> {
        self.data
    }
}
