use crate::Vm;

pub struct HostArgs<'a> {
    vm: &'a Vm,
    args_offset: usize,   // Offset to arguments related to frame offset
    args_count: u8,       // Number of arguments
    current_index: usize, // Current argument being processed
}

impl<'a> HostArgs<'a> {
    pub const fn new(vm: &'a Vm, args_offset: usize, args_count: u8) -> Self {
        Self {
            vm,
            args_offset,
            args_count,
            current_index: 0,
        }
    }

    pub fn get_i32(&mut self) -> Result<i32, &'static str> {
        if self.current_index >= self.args_count as usize {
            return Err("Argument index out of bounds");
        }

        let offset = self.args_offset + (self.current_index * 4);
        let ptr = self.vm.ptr_at_i32(self.vm.frame_offset + offset);
        let value = unsafe { *(ptr as *const i32) };

        self.current_index += 1;
        Ok(value)
    }

    pub fn get_f32(&mut self) -> Result<f32, &'static str> {
        todo!()
    }

    pub fn get_bool(&mut self) -> Result<bool, &'static str> {
        todo!()
    }

    pub fn get_ptr(&mut self) -> Result<*const u8, &'static str> {
        todo!()
    }
}

pub type HostFunctionCallback = Box<dyn FnMut(HostArgs)>;
