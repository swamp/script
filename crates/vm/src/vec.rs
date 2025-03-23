use crate::Vm;
use std::ptr;
use swamp_vm_types::{VEC_HEADER_SIZE, VecHeader};

impl Vm {
    #[inline]
    pub fn execute_vec_from_slice(
        &mut self,
        target_vec_pointer: u16,
        source_slice_addr: u16,
        element_size: u16,
        element_count: u16,
    ) {
        let slice_size = element_size as usize * element_count as usize;

        // Allocate space on the heap for the slice data.
        let heap_offset: u32 = self.heap_allocate(slice_size);

        // Copy the slice from Frame to Heap.
        unsafe {
            let source_ptr = self.frame_ptr_bool_at(source_slice_addr);
            let dest_ptr = self.heap_ptr_at(heap_offset as usize);

            ptr::copy_nonoverlapping(source_ptr, dest_ptr, slice_size);
        }

        let vec_header = VecHeader {
            count: element_count,
            capacity: element_count,
            size: element_size,
            heap_offset,
        };

        // Allocated heap memory for Vec header.
        let header_offset: u32 = self.heap_allocate(VEC_HEADER_SIZE as usize);

        // Copy the Vec header to Heap.
        unsafe {
            let header_ptr = self.heap_ptr_at(header_offset as usize) as *mut VecHeader;
            ptr::write(header_ptr, vec_header);
        }

        // Copy the heap offset of the vec header to the frame.
        unsafe {
            let target_ptr = self.frame_ptr_at(target_vec_pointer) as *mut u32;
            ptr::write(target_ptr, header_offset);
        }
    }
    #[inline]
    pub fn execute_vec_iter_init(
        &mut self,
        target_iterator_addr: u16,
        lower_bits: u16,
        upper_bits: u16,
    ) {
        let heap_offset = ((upper_bits as u32) << 16) | (lower_bits as u32);
    }

    #[inline]
    pub fn execute_vec_iter_next(
        &mut self,
        target_iterator_addr: u16,
        target_variable: u16,
        jump: u16,
    ) {
        let has_more_in_iterator = false;

        if has_more_in_iterator {
        } else {
            self.ip = jump as usize;
        }
    }
}
