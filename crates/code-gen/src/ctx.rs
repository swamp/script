use crate::alloc::{FrameMemoryRegion, ScopeAllocator};
use crate::alloc_util::reserve_space_for_type;
use swamp_script_types::Type;
use swamp_vm_types::{FrameMemoryAddress, MemoryAlignment, MemorySize};
use tracing::info;

pub struct Context {
    target_info: FrameMemoryRegion,
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
    pub(crate) const fn target(&self) -> FrameMemoryRegion {
        self.target_info
    }
}

impl Context {
    pub(crate) fn allocate_temp(
        &mut self,
        memory_size: MemorySize,
        alignment: MemoryAlignment,
    ) -> FrameMemoryAddress {
        self.temp_allocator.allocate(memory_size, alignment)
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
            target_info: FrameMemoryRegion {
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
    pub fn new(target_info: FrameMemoryRegion) -> Self {
        Self {
            target_info,
            temp_allocator: ScopeAllocator::new(FrameMemoryRegion::new(
                FrameMemoryAddress(target_info.addr.0 + target_info.size.0),
                MemorySize(1024),
            )),
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
            target_info: FrameMemoryRegion { addr, size },
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
    pub fn create_function_scope(&self, return_target: FrameMemoryRegion, comment: &str) -> Self {
        Self {
            target_info: self.target_info,
            temp_allocator: self.temp_allocator.create_scope(),
            comment: comment.to_string(),
        }
    }
}
