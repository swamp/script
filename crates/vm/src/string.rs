use crate::Vm;
use std::ptr;
use swamp_vm_types::{STRING_HEADER_SIZE, StringHeader};

impl Vm {
    #[inline]
    pub fn execute_string_from_constant_slice(
        &mut self,
        target_string_addr: u16,
        constant_lower: u16,
        constant_upper: u16,
        byte_count: u16,
    ) {
        let constant_offset = ((constant_upper as u32) << 16) | (constant_lower as u32);

        let heap_offset = self.heap_allocate(byte_count as usize);

        // Copy the constant slice to heap
        unsafe {
            let source_constant = self.const_ptr_immute_at(constant_offset);
            let dest_ptr = self.heap_ptr_at(heap_offset as usize);

            ptr::copy_nonoverlapping(source_constant, dest_ptr, byte_count as usize);
        }

        let string_header = StringHeader {
            byte_count,
            capacity: byte_count,
            heap_offset,
        };

        let header_offset = self.heap_allocate(STRING_HEADER_SIZE as usize);

        // Copy String header to heap
        unsafe {
            let header_ptr = self.heap_ptr_at(header_offset as usize) as *mut StringHeader;
            ptr::write(header_ptr, string_header);
        }

        // Copy the heap offset of the string header to the frame.
        unsafe {
            let target_ptr = self.frame_ptr_at(target_string_addr) as *mut u32;
            ptr::write(target_ptr, header_offset);
        }
    }

    #[inline]
    fn get_string(&self, frame_offset: u16) -> (*const StringHeader, &str) {
        let header = self.frame_ptr_indirect_heap_immut_at(frame_offset) as *const StringHeader;
        unsafe {
            let runes = self.heap_ptr_immut_at((*header).heap_offset as usize);

            let bytes = std::slice::from_raw_parts(runes, (*header).byte_count as usize);

            (header, std::str::from_utf8_unchecked(bytes))
        }
    }

    #[inline]
    pub fn execute_string_append(&mut self, target_string_addr: u16, string_a: u16, string_b: u16) {
        let (_header, str_a) = self.get_string(string_a);
        let (_header, str_b) = self.get_string(string_b);

        let result = str_a.to_string() + str_b;

        let byte_count = result.len();

        let heap_runes_offset = self.heap_allocate(byte_count);

        // Copy new runes to heap
        unsafe {
            ptr::copy_nonoverlapping(
                result.as_bytes().as_ptr(),
                self.heap_ptr_at(heap_runes_offset as usize),
                byte_count,
            );
        }

        let string_header = StringHeader {
            byte_count: result.len() as u16,
            capacity: result.len() as u16,
            heap_offset: heap_runes_offset,
        };

        let header_offset = self.heap_allocate(STRING_HEADER_SIZE as usize);

        // Copy String header to heap
        unsafe {
            let header_ptr = self.heap_ptr_at(header_offset as usize) as *mut StringHeader;
            ptr::write(header_ptr, string_header);
        }

        // Copy the heap offset of the string header to the frame.
        unsafe {
            let target_ptr = self.frame_ptr_at(target_string_addr) as *mut u32;
            ptr::write(target_ptr, header_offset);
        }
    }
}
