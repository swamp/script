use swamp_vm_types::{FrameMemoryAddress, MemorySize};

const ALIGNMENT: u16 = 8;
const ALIGNMENT_REST: u16 = ALIGNMENT - 1;
const ALIGNMENT_MASK: u16 = !ALIGNMENT_REST;

#[derive(Debug, Copy, Clone)]
pub struct TargetInfo {
    pub addr: FrameMemoryAddress,
    pub size: MemorySize,
}

impl TargetInfo {
    #[must_use]
    pub fn addr(&self) -> FrameMemoryAddress {
        self.addr
    }
}

#[derive(Copy, Clone)]
pub struct ScopeAllocator {
    initial_addr: FrameMemoryAddress,
    addr: FrameMemoryAddress,
}

impl ScopeAllocator {
    #[must_use]
    pub const fn new(start_addr: FrameMemoryAddress) -> Self {
        Self {
            initial_addr: start_addr,
            addr: start_addr,
        }
    }

    pub fn allocate(&mut self, size: MemorySize) -> FrameMemoryAddress {
        self.addr.0 = (self.addr.0 + ALIGNMENT_REST) & ALIGNMENT_MASK;
        let addr = self.addr;
        self.addr.0 += size.0;

        addr
    }

    pub fn reserve(&mut self, size: MemorySize) -> TargetInfo {
        TargetInfo {
            addr: self.allocate(size),
            size,
        }
    }

    #[must_use]
    pub const fn create_scope(&self) -> Self {
        Self {
            addr: self.addr,
            initial_addr: self.addr,
        }
    }

    #[must_use]
    pub const fn addr(&self) -> FrameMemoryAddress {
        self.addr
    }

    pub fn reset(&mut self) {
        self.addr = self.initial_addr;
    }
}
