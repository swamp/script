use crate::Vm;
use swamp_vm_types::PTR_SIZE;

/// # Map Header Structure
///
/// The map header is a 10-bytes structure with this layout:
///
/// | Offset (bytes) | Field        | Type | Size (bytes) | Description                      |
/// |----------------|--------------|------|--------------|----------------------------------|
/// | 0              | length       | u16  | 2            | Number of entries in the map     |
/// | 2              | capacity     | u16  | 2            | Total bucket count (power of 2)  |
/// | 4              | buckets_ptr  | u16  | 2            | Pointer to bucket array          |
/// | 6              | key_size     | u16  | 2            | Key size in bytes                |
/// | 8              | value_size   | u16  | 2            | Value size in bytes              |
///
/// # Bucket Structure
/// Each bucket contains a linked list of entries. Entries are stored as:
///
/// ```text
/// [next_ptr: u16][key_bytes][value_bytes]
/// ```
///
/// | Offset (octets) | Field                 | Type    | Size (octets) | Description                          |
/// |-----------------|-----------------------|---------|---------------|--------------------------------------|
/// | 0               | pointer to next entry | u16     | 2             | Linked list for collision resolution |
/// | 2               | key octets            | u8[]    | key_size      | Key data in raw octets               |
/// | 2 + key_size    | value octets          | u8[]    | value_size    | Value data in raw octets             |
///
/// Each bucket starts with a null pointer (0x0000). New entries are prepended to
/// the chain for cache locality. Existing keys overwrite their values.
impl Vm {
    #[inline]
    pub fn execute_map_new_from_pairs(
        &mut self,
        dst_offset: u16,
        pairs_offset: u16,
        key_size: u16,
        value_size: u16,
        pair_count: u16,
    ) {
        const BUCKET_COUNT: u16 = 8;
        let min_capacity = ((pair_count as f32 / 0.75) as u16).max(BUCKET_COUNT);
        let capacity = min_capacity.next_power_of_two();

        // Map header structure (10 bytes)
        let dst_ptr = self.ptr_at_u16(self.frame_offset + dst_offset as usize);
        let buckets_ptr_addr = self.allocate((capacity * 2) as usize); // Allocate buckets
        unsafe {
            *dst_ptr = 0; // Initialize length of map to 0
            *dst_ptr.add(1) = capacity;
            *dst_ptr.add(2) = buckets_ptr_addr;
            *dst_ptr.add(3) = key_size;
            *dst_ptr.add(4) = value_size;
        }

        // Initialize buckets to "null" pointers
        let buckets_ptr = self.ptr_at_u16(buckets_ptr_addr as usize);
        unsafe {
            std::ptr::write_bytes(buckets_ptr, 0, (capacity * PTR_SIZE) as usize);
        }

        let pair_size = key_size + value_size;
        for i in 0..pair_count {
            let pair_offset = pairs_offset + i * pair_size;
            let key_ptr = self.ptr_at_u8(self.frame_offset + pair_offset as usize);
            let hash = Self::hash_bytes(key_ptr, key_size as usize);
            let bucket_idx = hash & (capacity - 1);

            let bucket_head_ptr = unsafe { buckets_ptr.add(bucket_idx as usize) };
            let mut current_entry_ptr = unsafe { *bucket_head_ptr };

            // Search through linked list
            let mut found = false;
            while current_entry_ptr != 0 {
                let entry_ptr = self.ptr_at_u8(current_entry_ptr as usize);

                let entry_key_ptr = unsafe { entry_ptr.add(2) }; // Skip next pointer
                if Self::keys_equal(key_ptr, entry_key_ptr, key_size as usize) {
                    // Overwrite value - maybe issue warning in the future?
                    let value_ptr = unsafe { entry_key_ptr.add(key_size as usize) };
                    unsafe {
                        std::ptr::copy_nonoverlapping(
                            key_ptr.add(key_size as usize),
                            value_ptr,
                            value_size as usize,
                        );
                    }
                    found = true;
                    break;
                }
                current_entry_ptr = unsafe { *(entry_ptr as *const u16) };
            }

            if !found {
                // Create new entry: [next_ptr: u16][key][value]
                let new_entry_size = PTR_SIZE + key_size + value_size;
                let new_entry_ptr = self.allocate(new_entry_size as usize);
                let new_entry_u8 = self.ptr_at_u8(new_entry_ptr as usize);

                // Link to existing entries - unsafe write
                unsafe {
                    *(new_entry_u8 as *mut u16) = *bucket_head_ptr;
                }

                unsafe {
                    std::ptr::copy_nonoverlapping(
                        key_ptr,
                        new_entry_u8.add(PTR_SIZE as usize),
                        pair_size as usize,
                    );
                }

                unsafe {
                    *bucket_head_ptr = new_entry_ptr; // now the bucket start with the newly added entry
                    *dst_ptr += 1; // Increment map length
                }
            }
        }
    }

    // Helper function to compare keys
    // Must exist a faster way? memcmp?
    #[inline]
    fn keys_equal(a: *const u8, b: *const u8, len: usize) -> bool {
        for i in 0..len {
            unsafe {
                if *a.add(i) != *b.add(i) {
                    return false;
                }
            }
        }
        true
    }

    #[inline]
    // TODO: Replace this simple FNV-1a-like
    fn hash_bytes(ptr: *const u8, len: usize) -> u16 {
        let mut hash: u32 = 2166136261;
        for i in 0..len {
            unsafe {
                hash ^= *ptr.add(i) as u32;
                hash = hash.wrapping_mul(16777619);
            }
        }
        (hash & 0xFFFF) as u16
    }
}
