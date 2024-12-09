use crate::idx_gen::IndexAllocator;
use crate::value::{to_rust_value, Value};
use sparse_slot::{Id, SparseSlot};
use std::fmt::{Display, Formatter};
use swamp_script_semantic::{
    create_rust_type, create_rust_type_generic, ResolvedRustTypeRef, ResolvedType,
};

#[derive(Debug)]
pub struct SparseValueId(pub Id);

impl Display for SparseValueId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "id:{}:{}", self.0.index, self.0.generation)
    }
}

#[derive(Debug)]
pub struct SparseValueMap {
    pub sparse_slot: SparseSlot<Value>,
    pub id_generator: IndexAllocator,
    pub type_parameter: ResolvedType,
    pub rust_type_ref: ResolvedRustTypeRef,
    pub rust_type_ref_for_id: ResolvedRustTypeRef,
}

impl Display for SparseValueMap {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Sparse<{}> len:{}",
            self.type_parameter,
            self.sparse_slot.len()
        )
    }
}

impl SparseValueMap {
    pub fn new(type_parameter: ResolvedType) -> Self {
        let rust_type_ref = create_rust_type_generic("Sparse", &type_parameter, 0);
        let rust_type_ref_for_id = create_rust_type("SparseId", 1);

        Self {
            sparse_slot: SparseSlot::<Value>::new(1024),
            id_generator: IndexAllocator::new(),
            type_parameter,
            rust_type_ref,
            rust_type_ref_for_id,
        }
    }

    pub fn add(&mut self, v: Value) -> Value {
        // TODO: Check that value is of correct type parameter
        let (index, generation) = self.id_generator.create();

        let id = Id { index, generation };

        self.sparse_slot.try_set(id, v).expect("sparse should work");

        let script_id = SparseValueId(id);

        to_rust_value(self.rust_type_ref.clone(), script_id)
    }

    pub fn remove(&mut self, id: &SparseValueId) {
        self.id_generator.remove((id.0.index, id.0.generation));
        self.sparse_slot.remove(id.0);
    }

    #[allow(unused)]
    pub fn iter(&self) -> sparse_slot::Iter<'_, Value> {
        self.sparse_slot.iter()
    }

    pub fn values(&self) -> Vec<Value> {
        self.sparse_slot.iter().map(|(_id, v)| v.clone()).collect()
    }
}
