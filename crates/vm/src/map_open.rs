/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::Vm;

impl Vm {
    /*
    const MAX_PROBES: usize = 8;
    const EMPTY: u8 = 0;
    const DELETED: u8 = 1;
    const OCCUPIED: u8 = 2;

    pub(crate) fn execute_map_open_addressing_from_slice(
        &mut self,
        dst_offset: u16,
        slice_addr: u16,
        key_size: u16,
        value_size: u16,
        element_count: u16,
    ) {
        let min_capacity = ((element_count as u32 * 4 + 2) / 3).max(8) as u16;
        let capacity = min_capacity.next_power_of_two();

        let dst_ptr = self.ptr_at_u16(self.frame_offset + dst_offset as usize);

        let buckets_ptr_addr = self.heap_allocate((capacity * (1 + key_size + value_size)) as usize);

        unsafe {
            *dst_ptr = 0;
            *dst_ptr.add(1) = capacity;
            *dst_ptr.add(2) = buckets_ptr_addr;
            *dst_ptr.add(3) = key_size;
            *dst_ptr.add(4) = value_size;
        }

        let buckets_ptr = self.ptr_at_u8(buckets_ptr_addr as usize);
        unsafe {
            std::ptr::write_bytes(
                buckets_ptr,
                0,
                (capacity * (1 + key_size + value_size)) as usize,
            );
        }

        let slice_ptr = self.ptr_at_u8(self.frame_offset + slice_addr as usize);

        let pair_size = key_size + value_size;
        for i in 0..element_count {
            eprintln!("inserting {i}");
            let pair_offset = i * pair_size;
            let key_ptr = unsafe { slice_ptr.add(pair_offset as usize) };
            let value_ptr = unsafe { key_ptr.add(key_size as usize) };

            assert!(self.insert_open_addressing(dst_offset, key_ptr, value_ptr));
        }
    }

    fn insert_open_addressing(
        &mut self,
        dst_offset: u16,
        key_ptr: *const u8,
        value_ptr: *const u8,
    ) -> bool {
        let dst_ptr = self.ptr_at_u16(self.frame_offset + dst_offset as usize);

        let capacity = unsafe { *dst_ptr.add(1) };
        let buckets_ptr_addr = unsafe { *dst_ptr.add(2) };
        let key_size = unsafe { *dst_ptr.add(3) };
        let value_size = unsafe { *dst_ptr.add(4) };

        let buckets_ptr = self.ptr_at_u8(buckets_ptr_addr as usize);

        let hash = Self::hash_bytes(key_ptr, key_size as usize);
        let mut bucket_idx = hash % capacity;

        for i in 0..Self::MAX_PROBES {
            let entry_ptr = unsafe {
                buckets_ptr.add(bucket_idx as usize * (1 + key_size as usize + value_size as usize))
            };

            let state = unsafe { *entry_ptr };

            if state != Self::OCCUPIED {
                unsafe {
                    std::ptr::copy_nonoverlapping(key_ptr, entry_ptr.add(1), key_size as usize);
                    std::ptr::copy_nonoverlapping(
                        value_ptr,
                        entry_ptr.add(1 + key_size as usize),
                        value_size as usize,
                    );
                }

                unsafe {
                    *entry_ptr = Self::OCCUPIED;
                }

                unsafe {
                    *dst_ptr += 1;
                }

                return true;
            }

            let entry_key_ptr = unsafe { entry_ptr.add(1) };
            if Self::keys_equal(key_ptr, entry_key_ptr, key_size as usize) {
                unsafe {
                    std::ptr::copy_nonoverlapping(
                        value_ptr,
                        entry_ptr.add(1 + key_size as usize),
                        value_size as usize,
                    );
                }
                return true;
            }

            bucket_idx = (hash + i as u16 + 1) % capacity;
        }

        panic!("could not insert, max_probes");
    }

    /// # Panics
    ///
    pub fn map_insert_open_addressing(
        &mut self,
        dst_offset: u16,
        key_offset: u16,
        value_offset: u16,
    ) {
        let dst_ptr = self.ptr_at_u16(self.frame_offset + dst_offset as usize);

        let capacity = unsafe { *dst_ptr.add(1) };
        let buckets_ptr_addr = unsafe { *dst_ptr.add(2) };
        let key_size = unsafe { *dst_ptr.add(3) };
        let value_size = unsafe { *dst_ptr.add(4) };

        let buckets_ptr = self.ptr_at_u8(buckets_ptr_addr as usize);
        let key_ptr = self.ptr_at_u8(self.frame_offset + key_offset as usize);
        let value_ptr = self.ptr_at_u8(self.frame_offset + value_offset as usize);

        let hash = Self::hash_bytes(key_ptr, key_size as usize);
        let mut bucket_idx = hash % capacity;

        for i in 0..Self::MAX_PROBES {
            let entry_ptr = unsafe {
                buckets_ptr.add(bucket_idx as usize * (1 + key_size as usize + value_size as usize))
            };

            let state = unsafe { *entry_ptr };

            if state != Self::OCCUPIED {
                unsafe {
                    std::ptr::copy_nonoverlapping(key_ptr, entry_ptr.add(1), key_size as usize);
                    std::ptr::copy_nonoverlapping(
                        value_ptr,
                        entry_ptr.add(1 + key_size as usize),
                        value_size as usize,
                    );
                }

                unsafe {
                    *entry_ptr = Self::OCCUPIED;
                }
                unsafe {
                    *dst_ptr += 1;
                }

                return;
            }

            let entry_key_ptr = unsafe { entry_ptr.add(1) };
            if Self::keys_equal(key_ptr, entry_key_ptr, key_size as usize) {
                unsafe {
                    std::ptr::copy_nonoverlapping(
                        value_ptr,
                        entry_ptr.add(1 + key_size as usize),
                        value_size as usize,
                    );
                }
                return;
            }

            // Probe to the next slot
            bucket_idx = (hash + i as u16 + 1) % capacity;
        }

        panic!("MAX_PROBES");
    }

    fn map_remove_open_addressing(&mut self, dst_offset: u16, key_offset: u16) {
        let dst_ptr = self.ptr_at_u16(self.frame_offset + dst_offset as usize);

        let capacity = unsafe { *dst_ptr.add(1) };
        let buckets_ptr_addr = unsafe { *dst_ptr.add(2) };
        let key_size = unsafe { *dst_ptr.add(3) };
        let value_size = unsafe { *dst_ptr.add(4) };

        let buckets_ptr = self.ptr_at_u8(buckets_ptr_addr as usize);

        let key_ptr = self.ptr_at_u8(self.frame_offset + key_offset as usize);

        let hash = Self::hash_bytes(key_ptr, key_size as usize);
        let mut bucket_idx = hash % capacity;

        for i in 0..Self::MAX_PROBES {
            let entry_ptr = unsafe {
                buckets_ptr.add(bucket_idx as usize * (1 + key_size as usize + value_size as usize))
            };

            let state = unsafe { *entry_ptr };

            if state == Self::EMPTY {
                return;
            }

            let entry_key_ptr = unsafe { entry_ptr.add(1) };
            if Self::keys_equal(key_ptr, entry_key_ptr, key_size as usize) {
                unsafe {
                    *entry_ptr = Self::DELETED;
                }

                unsafe {
                    *dst_ptr -= 1;
                }

                return;
            }

            // Probe to the next slot
            bucket_idx = (hash + i as u16 + 1) % capacity;
        }

        panic!("could not find key to delete");
    }

     */
}
