use crate::alloc::{FrameMemoryRegion, ScopeAllocator};
use swamp_script_types::{AnonymousStructType, Type};
use swamp_vm_instr_build::{FLOAT_SIZE, INT_SIZE, STR_SIZE};
use swamp_vm_types::{MemoryAlignment, MemoryOffset, MemorySize};
use tracing::error;

pub fn layout_struct(
    anon_struct: &AnonymousStructType,
    memory_offset: MemoryOffset,
) -> (MemorySize, MemoryAlignment) {
    let mut calculated_offset = memory_offset;
    let mut largest_alignment = MemoryAlignment::U8;
    for (_name, field) in &anon_struct.field_name_sorted_fields {
        let (field_size, field_alignment) = type_size_and_alignment(&field.field_type);
        if field_alignment.greater_than(largest_alignment) {
            largest_alignment = field_alignment;
        }
        calculated_offset.space(field_size, field_alignment);
    }

    let total_offset = calculated_offset.space(MemorySize(0), largest_alignment);

    (total_offset.as_size(), largest_alignment)
}

pub fn type_size_and_alignment(ty: &Type) -> (MemorySize, MemoryAlignment) {
    match ty {
        Type::Int => (MemorySize(INT_SIZE), MemoryAlignment::U32),
        Type::Float => (MemorySize(FLOAT_SIZE), MemoryAlignment::U32),
        Type::String => (MemorySize(STR_SIZE), MemoryAlignment::U16),
        Type::Bool => (MemorySize(1), MemoryAlignment::U8),
        Type::Unit => (MemorySize(0), MemoryAlignment::U8),
        Type::Never => (MemorySize(0), MemoryAlignment::U8),
        Type::Tuple(types) => todo!(),
        Type::NamedStruct(named_struct) => type_size_and_alignment(&Type::AnonymousStruct(
            named_struct.anon_struct_type.clone(),
        )),
        Type::AnonymousStruct(anon_struct) => layout_struct(anon_struct, MemoryOffset(0)),
        Type::Enum(_) => todo!(),
        Type::Function(_) => (MemorySize(2), MemoryAlignment::U16),
        Type::Iterable(_) => todo!(),
        Type::Optional(_) => todo!(),
        Type::Generic(a, b) => {
            error!(?a, ?b, "generic can not be generated");
            panic!("generic is not supported")
        }
        Type::Blueprint(_) => todo!(),
        Type::Variable(_) => todo!(),
        Type::External(_) => todo!(),
        Type::MutableReference(referenced_type) => type_size_and_alignment(referenced_type),
    }
}

pub fn reserve_space_for_type(ty: &Type, allocator: &mut ScopeAllocator) -> FrameMemoryRegion {
    let (size, alignment) = type_size_and_alignment(ty);

    allocator.reserve(size, alignment)
}
