/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
// TODO: Move to sparse-slot crate

#[derive(Debug, PartialEq, Eq)]
pub struct IndexAllocator {
    // Stores the generation count for each index
    generations: Vec<u8>,
    // Freelist of available indices
    freelist: Vec<usize>,
}

impl IndexAllocator {
    // Create a new allocator
    pub(crate) fn new() -> Self {
        Self {
            generations: Vec::new(),
            freelist: Vec::new(),
        }
    }

    // Create a new index or reuse one from the freelist, incrementing its generation
    pub fn create(&mut self) -> (usize, u8) {
        if let Some(index) = self.freelist.pop() {
            // Reuse index from freelist, increment generation
            //self.generations[index] = self.generations[index].wrapping_add(1);
            (index, self.generations[index])
        } else {
            // No free index, create new one
            let index = self.generations.len();
            self.generations.push(0); // Start with generation 0
            (index, 0)
        }
    }

    pub fn reserve(&mut self, index: usize, generation: u8) {
        if index >= self.generations.len() {
            let count = index - self.generations.len() + 1;
            for _i in 0..count {
                self.generations.push(0);
            }
        }
        self.generations[index] = generation;
    }

    /// # Panics
    ///
    // Mark an index as removed, allowing it to be reused later with an incremented generation
    #[allow(unused)]
    pub fn remove(&mut self, value: (usize, u8)) {
        assert!(value.0 < self.generations.len());
        // Push the index to freelist for reuse
        self.generations[value.0] = self.generations[value.0].wrapping_add(1);
        self.freelist.push(value.0);
    }
}
