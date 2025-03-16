use crate::alloc::{ScopeAllocator, TargetInfo};
use crate::alloc_util::reserve_space_for_type;
use swamp_script_types::Type;
use swamp_vm_types::{FrameMemoryAddress, MemorySize};
use tracing::info;

pub struct Context {
    target_info: TargetInfo,
    temp_allocator: ScopeAllocator,
    comment: String,
}

impl Context {
    pub(crate) fn comment(&self) -> &str {
        &self.comment
    }
}

impl Context {}

impl Context {}

impl Context {
    pub(crate) const fn target(&self) -> TargetInfo {
        self.target_info
    }
}

impl Context {
    pub(crate) fn allocate_temp(&mut self, memory_size: MemorySize) -> FrameMemoryAddress {
        self.temp_allocator.allocate(memory_size)
    }
}

impl Context {
    pub fn reset_temp(&mut self) {
        self.temp_allocator.reset();
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
            comment: "".to_string(),
        }
    }
}

impl Context {
    #[must_use]
    pub const fn new(addr: FrameMemoryAddress, size: MemorySize) -> Self {
        Self {
            target_info: TargetInfo { addr, size },
            temp_allocator: ScopeAllocator::new(FrameMemoryAddress(addr.0 + size.0)),
            comment: String::new(),
        }
    }

    pub fn temp_space_for_type(&mut self, ty: &Type, comment: &str) -> Self {
        let new_target_info = reserve_space_for_type(ty, &mut self.temp_allocator);
        info!(?new_target_info, "creating temporary space");
        Self {
            target_info: new_target_info,
            temp_allocator: self.temp_allocator.create_scope(),
            comment: comment.to_string(),
        }
    }

    pub const fn addr(&self) -> FrameMemoryAddress {
        self.target_info.addr
    }
    pub const fn target_size(&self) -> MemorySize {
        self.target_info.size
    }

    #[must_use]
    pub fn with_target(&self, addr: FrameMemoryAddress, size: MemorySize, comment: &str) -> Self {
        Self {
            target_info: TargetInfo { addr, size },
            temp_allocator: self.temp_allocator,
            comment: comment.to_string(),
        }
    }

    #[must_use]
    pub fn create_scope(&self) -> Self {
        Self {
            target_info: self.target_info,
            temp_allocator: self.temp_allocator.create_scope(),
            comment: self.comment.clone(),
        }
    }

    #[must_use]
    pub fn create_function_scope(&self, return_target: TargetInfo, comment: &str) -> Self {
        Self {
            target_info: self.target_info,
            temp_allocator: self.temp_allocator.create_scope(),
            comment: comment.to_string(),
        }
    }
}
