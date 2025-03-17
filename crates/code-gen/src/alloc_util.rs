use crate::alloc::{FrameMemoryRegion, ScopeAllocator};
use swamp_script_types::Type;
use swamp_vm_instr_build::{BOOL_SIZE, INT_SIZE, PTR_SIZE};
use swamp_vm_types::{MemoryAlignment, MemorySize};

pub fn type_size(ty: &Type) -> MemorySize {
    let size = match ty {
        Type::Int => INT_SIZE,
        Type::Float => INT_SIZE,
        Type::String => PTR_SIZE,
        Type::Bool => BOOL_SIZE,
        Type::Unit => 0,
        Type::Never => 0,
        Type::Tuple(types) => todo!(),
        Type::NamedStruct(_) => todo!(),
        Type::AnonymousStruct(_) => todo!(),
        Type::Enum(_) => todo!(),
        Type::Function(_) => 2,
        Type::Iterable(_) => todo!(),
        Type::Optional(_) => todo!(),
        Type::Generic(_, _) => todo!(),
        Type::Blueprint(_) => todo!(),
        Type::Variable(_) => todo!(),
        Type::External(_) => todo!(),
        Type::MutableReference(_referenced_type) => PTR_SIZE,
    };

    MemorySize(size)
}

pub fn type_alignment(ty: &Type) -> MemoryAlignment {
    let alignment = match ty {
        Type::Int => MemoryAlignment::U32,
        Type::Float => MemoryAlignment::U32,
        Type::String => MemoryAlignment::U16,
        Type::Bool => MemoryAlignment::U8,
        Type::Unit => MemoryAlignment::U8,
        Type::Never => MemoryAlignment::U8,
        Type::Tuple(types) => todo!(),
        Type::NamedStruct(_) => todo!(),
        Type::AnonymousStruct(_) => todo!(),
        Type::Enum(_) => todo!(),
        Type::Function(_) => MemoryAlignment::U16,
        Type::Iterable(_) => todo!(),
        Type::Optional(_) => todo!(),
        Type::Generic(_, _) => todo!(),
        Type::Blueprint(_) => todo!(),
        Type::Variable(_) => todo!(),
        Type::External(_) => todo!(),
        Type::MutableReference(_referenced_type) => MemoryAlignment::U16,
    };

    alignment
}

pub fn type_size_and_alignment(ty: &Type) -> (MemorySize, MemoryAlignment) {
    let size = type_size(ty);
    let alignment = type_alignment(ty);

    (size, alignment)
}

pub fn reserve_space_for_type(ty: &Type, allocator: &mut ScopeAllocator) -> FrameMemoryRegion {
    let (size, alignment) = type_size_and_alignment(ty);

    allocator.reserve(size, alignment)
}
