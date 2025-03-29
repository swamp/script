/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use swamp_vm_types::StringHeader;

pub struct HostArgs {
    current_index: usize, // Current argument being processed
    // references into the Vm
    frame_memory: *const u8,
    frame_memory_len: usize,
    heap_memory: *const u8,
    heap_memory_len: usize,
}

impl HostArgs {
    #[must_use]
    pub const fn new(
        frame_memory: *const u8,
        frame_memory_len: usize,
        heap_memory: *const u8,
        heap_memory_len: usize,
    ) -> Self {
        Self {
            frame_memory,
            frame_memory_len,
            heap_memory,
            heap_memory_len,
            current_index: 0,
        }
    }

    pub fn get_i32(&mut self) -> i32 {
        let size = size_of::<i32>();
        debug_assert!(
            self.current_index + size <= self.frame_memory_len,
            "Attempted to read beyond the end of frame memory"
        );

        unsafe {
            let int_ptr = self.frame_memory.add(self.current_index) as *const i32;
            self.current_index += size;
            *int_ptr
        }
    }

    pub fn get_str(&mut self) -> &str {
        unsafe {
            let offset_ptr = self.frame_memory.add(self.current_index) as *const u32;
            self.current_index += size_of::<u32>();
            let header_offset = *offset_ptr as usize;

            let string_header = self.heap_memory.add(header_offset) as *const StringHeader;

            let rune_offset = (*string_header).heap_offset as usize;
            let string_byte_length = (*string_header).byte_count as usize;

            debug_assert!(
                rune_offset + string_byte_length <= self.heap_memory_len,
                "String read out-of-bounds in heap memory"
            );

            let ptr_to_utf8 = self.heap_memory.add(rune_offset);

            let bytes = std::slice::from_raw_parts(ptr_to_utf8, string_byte_length);

            std::str::from_utf8_unchecked(bytes)
        }
    }
}

pub type HostFunctionCallback = Box<dyn FnMut(HostArgs)>;
