use swamp_script_vm::instr_bldr::MemoryAddress;

#[derive(Copy, Clone)]
pub struct TargetInfo {
    pub addr: MemoryAddress,
    pub size: usize,
}

#[derive(Copy, Clone)]
pub struct ScopeAllocator {
    addr: MemoryAddress,
}

impl ScopeAllocator {
    #[must_use]
    pub const fn new(start_addr: MemoryAddress) -> Self {
        Self { addr: start_addr }
    }

    pub fn allocate(&mut self, size: u16) -> MemoryAddress {
        let addr = self.addr;
        self.addr.0 += size;
        addr
    }

    pub fn reserve(&mut self, size: u16) -> TargetInfo {
        TargetInfo {
            addr: self.allocate(size),
            size: size as usize,
        }
    }

    #[must_use]
    pub const fn create_scope(&self) -> Self {
        Self { addr: self.addr }
    }

    #[must_use]
    pub const fn addr(&self) -> MemoryAddress {
        self.addr
    }
}
