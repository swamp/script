use crate::alloc::{ScopeAllocator, TargetInfo};
use crate::alloc_util::reserve_space_for_type;
use swamp_script_types::Type;
use swamp_script_vm::instr_bldr::{FrameMemoryAddress, MemorySize};

pub struct Context {
    target_info: TargetInfo,
    allocator: ScopeAllocator,
}

impl Context {
    pub(crate) fn with_offset(&self, offset: MemorySize) -> Self {
        Self {
            target_info: TargetInfo {
                addr: self.addr().add(offset),
                size: MemorySize(self.target_info.size.0 - 1),
            },
            allocator: self.allocator,
        }
    }
}

impl Context {
    #[must_use]
    pub const fn new(addr: FrameMemoryAddress, size: MemorySize) -> Self {
        Self {
            target_info: TargetInfo { addr, size },
            allocator: ScopeAllocator::new(FrameMemoryAddress(addr.0 + size.0)),
        }
    }

    pub(crate) fn context_for_type(&mut self, ty: &Type) -> Self {
        let new_target_info = reserve_space_for_type(ty, &mut self.allocator);
        Self {
            target_info: new_target_info,
            allocator: self.allocator.create_scope(),
        }
    }
    pub(crate) const fn addr(&self) -> FrameMemoryAddress {
        self.target_info.addr
    }
    pub(crate) const fn target_size(&self) -> MemorySize {
        self.target_info.size
    }

    #[must_use]
    pub const fn with_target(&self, addr: FrameMemoryAddress, size: MemorySize) -> Self {
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

    #[must_use]
    pub const fn create_function_scope(&self, return_target: TargetInfo) -> Self {
        Self {
            target_info: self.target_info,
            allocator: self.allocator.create_scope(),
        }
    }
}
