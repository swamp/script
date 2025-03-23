use crate::Vm;

impl Vm {
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
