use crate::alloc::{FrameMemoryRegion, ScopeAllocator};
use crate::alloc_util::reserve_space_for_type;
use swamp_script_types::Type;
use swamp_vm_types::{FrameMemoryAddress, MemoryAlignment, MemorySize};
use tracing::info;

pub struct Context {
    target_info: FrameMemoryRegion,
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
    pub(crate) fn with_offset(&self, offset: MemorySize) -> Self {
        Self {
            target_info: FrameMemoryRegion {
                addr: self.addr().add(offset),
                size: MemorySize(self.target_info.size.0 - 1),
            },
            comment: "".to_string(),
        }
    }
}

impl Context {
    #[must_use]
    pub fn new(target_info: FrameMemoryRegion) -> Self {
        Self {
            target_info,
            comment: String::new(),
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
            comment: comment.to_string(),
        }
    }

    #[must_use]
    pub fn create_scope(&self) -> Self {
        Self {
            target_info: self.target_info,
            comment: self.comment.clone(),
        }
    }

    #[must_use]
    pub fn create_function_scope(&self, return_target: FrameMemoryRegion, comment: &str) -> Self {
        Self {
            target_info: self.target_info,
            comment: comment.to_string(),
        }
    }
}
