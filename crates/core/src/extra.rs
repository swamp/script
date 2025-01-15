/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::idx_gen::IndexAllocator;
use crate::value::{to_rust_value, Value};
use crate::value::{QuickSerialize, ValueRef};
use sparse_slot::{Id, SparseSlot};
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use swamp_script_semantic::{ResolvedRustTypeRef, ResolvedType};

#[derive(Debug)]
pub struct SparseValueId(pub Id);

impl Display for SparseValueId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "id:{}:{}", self.0.index, self.0.generation)
    }
}

impl QuickSerialize for SparseValueId {
    fn quick_serialize(&self, octets: &mut [u8]) -> usize {
        let mut offset = 0;

        // Serialize the index
        let index_octets = self.0.index.to_ne_bytes();
        octets[offset..offset + index_octets.len()].copy_from_slice(&index_octets);
        offset += index_octets.len();

        // Serialize the generation
        let generation_octets = self.0.generation.to_ne_bytes();
        octets[offset..offset + generation_octets.len()].copy_from_slice(&generation_octets);
        offset += generation_octets.len();

        offset
    }
}

#[derive(Debug)]
pub struct SparseValueMap {
    pub sparse_slot: SparseSlot<Rc<RefCell<Value>>>,
    pub id_generator: IndexAllocator,
    pub type_parameter: ResolvedType,
    pub resolved_type: ResolvedType,
    pub rust_type_ref_for_id: ResolvedRustTypeRef,
}

impl QuickSerialize for SparseValueMap {
    fn quick_serialize(&self, octets: &mut [u8]) -> usize {
        let mut offset = 0;
        for (id, value) in self.sparse_slot.iter() {
            let value_octets = id.index.to_ne_bytes();
            octets[offset..offset + value_octets.len()].copy_from_slice(&value_octets);
            offset += value_octets.len();

            let value_size = value.borrow().quick_serialize(&mut octets[offset..]);
            offset += value_size;
        }

        offset
    }
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
    pub fn new(rust_type_ref_for_id: ResolvedRustTypeRef, resolved_type: ResolvedType) -> Self {
        let type_parameter = match &resolved_type {
            ResolvedType::Generic(_, parameters) => parameters[0].clone(),
            _ => panic!("illegal sparse type. not generic"),
        };

        Self {
            sparse_slot: SparseSlot::<Rc<RefCell<Value>>>::new(2048),
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
        let mutable_reference = Rc::new(RefCell::new(v));

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

    #[must_use]
    pub fn get(&self, id: &SparseValueId) -> Option<&ValueRef> {
        self.sparse_slot.get(id.0)
    }

    #[allow(unused)]
    #[must_use]
    pub fn iter(&self) -> sparse_slot::Iter<'_, Rc<RefCell<Value>>> {
        self.sparse_slot.iter()
    }
    pub fn iter_mut(&mut self) -> sparse_slot::IterMut<'_, Rc<RefCell<Value>>> {
        self.sparse_slot.iter_mut()
    }

    #[must_use]
    pub fn values(&self) -> Vec<Rc<RefCell<Value>>> {
        self.sparse_slot.iter().map(|(_id, v)| v.clone()).collect()
    }
}
