use crate::alloc::{ScopeAllocator, TargetInfo};
use crate::alloc_util::reserve_space_for_type;
use swamp_script_types::Type;
use swamp_script_vm::instr_bldr::{FrameMemoryAddress, MemorySize};

pub struct Context {
    target_info: TargetInfo,
    temp_allocator: ScopeAllocator,
}

impl Context {
    pub(crate) fn allocate_temp(&mut self, memory_size: MemorySize) -> FrameMemoryAddress {
        self.temp_allocator.allocate(memory_size)
    }
}

impl Context {
    pub fn reset_temp(&mut self) {
        self.temp_allocator.reset()
    }
}

impl Context {
    pub(crate) fn with_offset(&self, offset: MemorySize) -> Self {
        Self {
            target_info: TargetInfo {
                addr: self.addr().add(offset),
                size: MemorySize(self.target_info.size.0 - 1),
            },
            temp_allocator: self.temp_allocator,
        }
    }
}

impl Context {
    #[must_use]
    pub const fn new(addr: FrameMemoryAddress, size: MemorySize) -> Self {
        Self {
            target_info: TargetInfo { addr, size },
            temp_allocator: ScopeAllocator::new(FrameMemoryAddress(addr.0 + size.0)),
        }
    }

    pub(crate) fn temp_space_for_type(&mut self, ty: &Type) -> Self {
        let new_target_info = reserve_space_for_type(ty, &mut self.temp_allocator);
        Self {
            target_info: new_target_info,
            temp_allocator: self.temp_allocator.create_scope(),
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
            temp_allocator: self.temp_allocator,
        }
    }

    #[must_use]
    pub const fn create_scope(&self) -> Self {
        Self {
            target_info: self.target_info,
            temp_allocator: self.temp_allocator.create_scope(),
        }
    }

    #[must_use]
    pub const fn create_function_scope(&self, return_target: TargetInfo) -> Self {
        Self {
            target_info: self.target_info,
            temp_allocator: self.temp_allocator.create_scope(),
        }
    }
}
