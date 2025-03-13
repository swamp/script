use crate::alloc::{ScopeAllocator, TargetInfo};
use swamp_script_types::Type;
use swamp_script_vm::{BOOL_SIZE, INT_SIZE, PTR_SIZE};

pub fn reserve_space_for_type(ty: &Type, allocator: &mut ScopeAllocator) -> TargetInfo {
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
    };

    allocator.reserve(size)
}
