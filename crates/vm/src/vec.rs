use crate::Vm;
use std::ptr;
use swamp_vm_types::{VEC_HEADER_SIZE, VEC_ITERATOR_SIZE, VecHeader, VecIterator};

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
    pub fn execute_vec_iter_init(&mut self, target_iterator_addr: u16, vec_indirect: u16) {
        let vec_header = self.frame_ptr_indirect_heap_immut_at(vec_indirect) as *const VecHeader;
        unsafe {
            let vec_iterator = VecIterator {
                data_heap_offset: (*vec_header).heap_offset,
                count: (*vec_header).count as u16,
                element_size: (*vec_header).size,
                index: 0,
            };

            ptr::copy_nonoverlapping(
                &vec_iterator,
                self.frame_ptr_at(target_iterator_addr) as *mut VecIterator,
                VEC_ITERATOR_SIZE as usize,
            );
        }
    }

    #[inline]
    pub fn execute_vec_iter_next(
        &mut self,
        target_iterator_addr: u16,
        target_variable: u16,
        jump: u16,
    ) {
        // Get a pointer to the VecIterator in frame memory.
        let vec_iterator = self.frame_ptr_at(target_iterator_addr) as *mut VecIterator;

        // Read fields from the iterator in one small unsafe block.
        let (count, index, element_size, data_heap_offset) = unsafe {
            (
                (*vec_iterator).count,
                (*vec_iterator).index,
                (*vec_iterator).element_size,
                (*vec_iterator).data_heap_offset,
            )
        };

        if index == count {
            self.ip = jump as usize;
        } else {
            let new_index = index + 1;
            let heap_data_offset = data_heap_offset + element_size as u32 * index as u32;
            let source = self.heap_ptr_immut_at(heap_data_offset as usize);
            let target_ptr = self.frame_ptr_at(target_variable);

            // Copy the data in a minimal unsafe block.
            unsafe {
                ptr::copy_nonoverlapping(source, target_ptr, element_size as usize);
            }

            // Update the iterator's index in a separate, small unsafe block.
            unsafe {
                (*vec_iterator).index = new_index;
            }
        }
    }
}
