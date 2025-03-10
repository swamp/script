/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use std::cell::{Ref, RefCell};
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;
use swamp_script_core_extra::extra::{SparseValueId, SparseValueMap};
use swamp_script_core_extra::prelude::{Value, ValueError};
use swamp_script_core_extra::value::RustType;
use swamp_script_core_extra::value::ValueRef;
use tracing::info;

#[derive(Debug, Clone)]
pub struct ValueReference(pub ValueRef);

impl ValueReference {}

impl ValueReference {
    #[inline]
    pub fn set(&self, v: Value) {
        *self.0.borrow_mut() = v;
    }

    pub(crate) fn into_iter_mut(self) -> Result<Box<dyn Iterator<Item = ValueRef>>, ValueError> {
        let mut inner = self.0.borrow_mut();
        let result = match &mut *inner {
            Value::Sparse(_rust_type_ref, sparse_map) => {
                let pairs: Vec<_> = sparse_map.iter_mut().map(|(_k, v)| v.clone()).collect();

                Box::new(pairs.into_iter()) as Box<dyn Iterator<Item = ValueRef>>
            }
            Value::Map(_type_ref, seq_map) => {
                // Clone each Rc<RefCell<Value>> and collect into a Vec
                let cloned_rc: Vec<ValueRef> = seq_map.values().cloned().collect();

                // Box the iterator from the Vec
                Box::new(cloned_rc.into_iter()) as Box<dyn Iterator<Item = ValueRef> + 'static>
            }

            /*
                        Self::Array(_, values) => Ok(Box::new(values.into_iter())),
            Self::Map(_, seq_map) => Ok(Box::new(seq_map.into_values())),
             */
            _ => {
                info!(?inner, "not sure what this is:");
                todo!()
            }
        };
        Ok(result)
    }

    /// # Errors
    ///
    /// # Panics
    ///
    pub fn into_iter_mut_pairs(
        self,
    ) -> Result<Box<dyn Iterator<Item = (Value, Self)>>, ValueError> {
        let mut inner = self.0.borrow_mut();
        let result = match &mut *inner {
            Value::Sparse(_rust_type_ref, sparse_map) => {
                let id_type_ref = sparse_map.rust_type_ref_for_id.clone();

                let pairs: Vec<_> = sparse_map
                    .iter_mut()
                    .map(|(k, v)| {
                        (
                            Value::RustValue(
                                id_type_ref.clone(),
                                Rc::new(RefCell::new(Box::new(SparseValueId(k)))),
                            ),
                            Self(v.clone()),
                        )
                    })
                    .collect();

                Box::new(pairs.into_iter()) as Box<dyn Iterator<Item = (Value, Self)>>
            }
            _ => return Err(ValueError::CanNotCoerceToIterator),
        };
        Ok(result)
    }
    #[must_use]
    pub fn downcast_rust_mut<T: RustType + 'static>(&self) -> Option<Rc<RefCell<Box<T>>>> {
        match &*self.0.borrow() {
            Value::RustValue(_rust_type_ref, rc) => {
                let type_matches = {
                    let guard = rc.borrow();
                    (**guard).as_any().is::<T>()
                };

                if type_matches {
                    Some(unsafe { std::mem::transmute(rc.clone()) })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    #[must_use]
    pub fn unref(&self) -> Ref<Value> {
        self.0.borrow()
    }

    #[must_use]
    pub fn convert_to_string_if_needed(&self) -> String {
        self.0.borrow().convert_to_string_if_needed()
    }
}

impl PartialEq<Self> for ValueReference {
    fn eq(&self, other: &Self) -> bool {
        self.0.borrow().eq(&*other.0.borrow())
    }
}

impl Eq for ValueReference {}

impl Hash for ValueReference {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.borrow().hash(state);
    }
}

impl Display for ValueReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.borrow())
    }
}
