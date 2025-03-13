use swamp_script_vm::instr_bldr::{FrameMemoryAddress, MemoryAddress, MemorySize};

#[derive(Copy, Clone)]
pub struct TargetInfo {
    pub addr: FrameMemoryAddress,
    pub size: MemorySize,
}

#[derive(Copy, Clone)]
pub struct ScopeAllocator {
    addr: FrameMemoryAddress,
}

impl ScopeAllocator {
    #[must_use]
    pub const fn new(start_addr: FrameMemoryAddress) -> Self {
        Self { addr: start_addr }
    }

    pub fn allocate(&mut self, size: MemorySize) -> FrameMemoryAddress {
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
        Self { addr: self.addr }
    }

    #[must_use]
    pub const fn addr(&self) -> FrameMemoryAddress {
        self.addr
    }
}
