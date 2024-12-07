use crate::idx_gen::IndexAllocator;
use crate::value::{to_rust_value, Value};
use sparse_slot::{Id, SparseSlot};
use std::fmt::{Display, Formatter};
use swamp_script_semantic::ResolvedType;

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
        Self {
            sparse_slot: SparseSlot::<Value>::new(1024),
            id_generator: IndexAllocator::new(),
            type_parameter,
        }
    }

    pub fn add(&mut self, v: Value) -> Value {
        // TODO: Check that value is of correct type parameter
        let (index, generation) = self.id_generator.create();

        let id = Id { index, generation };

        self.sparse_slot.try_set(id, v).expect("sparse should work");

        let script_id = SparseValueId(id);

        to_rust_value(script_id)
    }

    pub fn remove(&mut self, id: &SparseValueId) {
        self.id_generator.remove((id.0.index, id.0.generation));
        self.sparse_slot.remove(id.0);
    }
}
