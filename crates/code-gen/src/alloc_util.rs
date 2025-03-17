use crate::alloc::{FrameMemoryRegion, ScopeAllocator};
use seq_map::SeqMap;
use swamp_script_types::{AnonymousStructType, EnumVariantType, Type};
use swamp_vm_instr_build::{FLOAT_SIZE, INT_SIZE, STR_SIZE};
use swamp_vm_types::{MemoryAlignment, MemoryOffset, MemorySize};
use tracing::error;

pub fn layout_struct(anon_struct: &AnonymousStructType) -> (MemorySize, MemoryAlignment) {
    let mut calculated_offset = MemoryOffset(0);
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

pub fn layout_tuple(types: &Vec<Type>) -> (MemorySize, MemoryAlignment) {
    let mut calculated_offset = MemoryOffset(0);
    let mut largest_alignment = MemoryAlignment::U8;
    for ty in types {
        let (field_size, field_alignment) = type_size_and_alignment(&ty);
        if field_alignment.greater_than(largest_alignment) {
            largest_alignment = field_alignment;
        }
        calculated_offset.space(field_size, field_alignment);
    }
    let total_offset = calculated_offset.space(MemorySize(0), largest_alignment);
    (total_offset.as_size(), largest_alignment)
}

pub fn layout_union(variants: &SeqMap<String, EnumVariantType>) -> (MemorySize, MemoryAlignment) {
    let mut max_variant_alignment = MemoryAlignment::U8;
    let mut max_variant_size = MemorySize(0);
    let mut calculated_offset = MemoryOffset(0);
    for (_name, variant) in variants {
        let (variant_size, variant_alignment) = match variant {
            EnumVariantType::Struct(anon_struct) => layout_struct(&anon_struct.anon_struct),
            EnumVariantType::Tuple(types) => layout_tuple(&types.fields_in_order),
            EnumVariantType::Nothing(_) => (MemorySize(0), MemoryAlignment::U8),
        };

        if variant_alignment.greater_than(max_variant_alignment) {
            max_variant_alignment = variant_alignment;
        }

        if variant_size.0 > max_variant_size.0 {
            max_variant_size = variant_size;
        }
    }

    (max_variant_size, max_variant_alignment)
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
        Type::AnonymousStruct(anon_struct) => layout_struct(anon_struct),
        Type::Enum(enum_type) => {
            let (offset, alignment) = layout_union(&enum_type.variants);

            let alignment_octets: usize = alignment.into();

            (MemorySize(offset.0 + alignment_octets as u16), alignment)
        }
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
