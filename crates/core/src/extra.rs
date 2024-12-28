use crate::idx_gen::IndexAllocator;
use crate::value::{to_rust_value, Value};
use sparse_slot::{Id, SparseSlot};
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use swamp_script_semantic::{create_rust_type, ResolvedRustTypeRef, ResolvedType};

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
    //pub rust_type_ref: ResolvedRustTypeRef,
    pub resolved_type: ResolvedType,
    pub rust_type_ref_for_id: ResolvedRustTypeRef,
}

impl Display for SparseValueMap {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Sparse<{:?}> len:{}",
            self.type_parameter,
            self.sparse_slot.len()
        )
    }
}

impl SparseValueMap {
    #[must_use]
    pub fn new(resolved_type: ResolvedType) -> Self {
        let type_parameter = match &resolved_type {
            ResolvedType::Generic(_, parameters) => parameters[0].clone(),
            _ => panic!("illegal sparse type. not generic"),
        };
        let rust_type_ref_for_id = create_rust_type("SparseId", 998);
        Self {
            sparse_slot: SparseSlot::<Value>::new(1024),
            id_generator: IndexAllocator::new(),
            type_parameter,
            resolved_type,
            rust_type_ref_for_id,
        }
    }

    pub fn add(&mut self, v: Value) -> Value {
        // TODO: Check that value is of correct type parameter
        let (index, generation) = self.id_generator.create();

        let id = Id { index, generation };

        // Always store mutable references
        let mutable_reference = Value::Reference(Rc::new(RefCell::new(v)));

        self.sparse_slot
            .try_set(id, mutable_reference)
            .expect("sparse should work");

        let script_id = SparseValueId(id);

        to_rust_value(self.rust_type_ref_for_id.clone(), script_id)
    }

    pub fn remove(&mut self, id: &SparseValueId) {
        self.id_generator.remove((id.0.index, id.0.generation));
        self.sparse_slot.remove(id.0);
    }

    #[allow(unused)]
    #[must_use]
    pub fn iter(&self) -> sparse_slot::Iter<'_, Value> {
        self.sparse_slot.iter()
    }
    pub fn iter_mut(&mut self) -> sparse_slot::IterMut<'_, Value> {
        self.sparse_slot.iter_mut()
    }

    #[must_use]
    pub fn values(&self) -> Vec<Value> {
        self.sparse_slot.iter().map(|(_id, v)| v.clone()).collect()
    }
}
