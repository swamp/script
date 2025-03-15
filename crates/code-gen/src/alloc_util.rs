use crate::alloc::{ScopeAllocator, TargetInfo};
use swamp_script_types::Type;
use swamp_vm::{BOOL_SIZE, INT_SIZE, PTR_SIZE};
use swamp_vm_types::MemorySize;

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
        Type::Function(_) => todo!(),
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

pub fn reserve_space_for_type(ty: &Type, allocator: &mut ScopeAllocator) -> TargetInfo {
    let size = type_size(ty);

    allocator.reserve(size)
}
