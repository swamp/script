use crate::alloc::{ScopeAllocator, TargetInfo};
use crate::alloc_util::reserve_space_for_type;
use swamp_script_types::Type;
use swamp_script_vm::instr_bldr::MemoryAddress;

pub struct Context {
    target_info: TargetInfo,
    allocator: ScopeAllocator,
}

impl Context {
    #[must_use]
    pub const fn new(addr: MemoryAddress, size: usize) -> Self {
        Self {
            target_info: TargetInfo { addr, size },
            allocator: ScopeAllocator::new(MemoryAddress(addr.0 + size as u16)),
        }
    }

    pub(crate) fn context_for_type(&mut self, ty: &Type) -> Self {
        let new_target_info = reserve_space_for_type(ty, &mut self.allocator);
        Self {
            target_info: new_target_info,
            allocator: self.allocator.create_scope(),
        }
    }
    pub(crate) const fn addr(&self) -> MemoryAddress {
        self.target_info.addr
    }
    pub(crate) const fn target_size(&self) -> usize {
        self.target_info.size
    }

    #[must_use]
    pub const fn with_target(&self, addr: MemoryAddress, size: usize) -> Self {
        Self {
            target_info: TargetInfo { addr, size },
            allocator: self.allocator,
        }
    }

    #[must_use]
    pub const fn create_scope(&self) -> Self {
        Self {
            target_info: self.target_info,
            allocator: self.allocator.create_scope(),
        }
    }
}
