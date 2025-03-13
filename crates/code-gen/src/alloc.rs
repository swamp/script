#[derive(Copy, Clone)]
pub struct TargetInfo {
    pub addr: u16,
    pub size: usize,
}

#[derive(Copy, Clone)]
pub struct ScopeAllocator {
    addr: u16,
}

impl ScopeAllocator {
    #[must_use]
    pub const fn new(start_addr: u16) -> Self {
        Self { addr: start_addr }
    }

    pub fn allocate(&mut self, size: u16) -> u16 {
        let addr = self.addr;
        self.addr += size;
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
    pub const fn addr(&self) -> u16 {
        self.addr
    }
}
