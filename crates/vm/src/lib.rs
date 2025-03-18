use crate::host::HostFunction;
use swamp_vm_types::BinaryInstruction;
use swamp_vm_types::opcode::OpCode;

pub mod host;

type Handler0 = fn(&mut Vm);
type Handler1 = fn(&mut Vm, u16);
type Handler2 = fn(&mut Vm, u16, u16);
type Handler3 = fn(&mut Vm, u16, u16, u16);
type Handler4 = fn(&mut Vm, u16, u16, u16, u16);

#[derive(Copy, Clone)]
enum HandlerType {
    Args0(Handler0),
    Args1(Handler1),
    Args2(Handler2),
    Args3(Handler3),
    Args4(Handler4),
}

pub struct Vm {
    // Memory
    memory: *mut u8,
    memory_size: usize,

    // Memory regions (offsets)
    alloc_offset: usize,      // Current allocation point
    stack_base_offset: usize, // Base of stack
    stack_offset: usize,      // Current stack position
    constants_offset: usize,  // Start of constants region
    frame_offset: usize,      // Current frame position

    // Execution state
    ip: usize,                            // Instruction pointer
    instructions: Vec<BinaryInstruction>, // Bytecode
    execution_complete: bool,             // Flag for completion

    // Function call management
    call_stack: Vec<CallFrame>, // Track function calls

    // Host function integration
    host_functions: Vec<HostFunction>, // Registered host functions

    handlers: [HandlerType; 256],

    // TODO: Error state
    debug_call_depth: usize,
}

impl Vm {
    #[must_use]
    pub fn instructions(&self) -> &[BinaryInstruction] {
        &self.instructions
    }
}

impl Vm {
    pub fn reset(&mut self) {
        self.stack_offset = self.stack_base_offset;
        self.ip = 0;
        self.frame_offset = self.stack_offset;
        self.execution_complete = false;
        self.call_stack.clear();
    }
}

impl Drop for Vm {
    fn drop(&mut self) {
        unsafe {
            // Free the memory that was allocated in new()
            let layout = std::alloc::Layout::from_size_align(self.memory_size, ALIGNMENT).unwrap();
            std::alloc::dealloc(self.memory, layout);
        }
    }
}

impl Vm {
    pub fn memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.memory, self.memory_size) }
    }

    pub fn stack_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.stack_ptr(), self.memory_size) }
    }

    pub fn stack_base_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.stack_base_ptr(), self.memory_size) }
    }
    pub fn frame_memory(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.frame_ptr(), self.memory_size) }
    }
}

const ALIGNMENT: usize = 8;
const ALIGNMENT_REST: usize = ALIGNMENT - 1;
const ALIGNMENT_MASK: usize = !ALIGNMENT_REST;

impl Vm {
    pub fn new(instructions: Vec<BinaryInstruction>, constants: &[u8], memory_size: usize) -> Self {
        let memory = unsafe {
            std::alloc::alloc(std::alloc::Layout::from_size_align(memory_size, ALIGNMENT).unwrap())
        };

        // Reserve 20% for constants at the end
        let constants_size = (memory_size / 5) & ALIGNMENT_MASK;
        let constants_offset = memory_size - constants_size;

        // Reserve 30% for stack in the middle
        let stack_size = (memory_size * 3 / 10) & ALIGNMENT_MASK;
        let stack_base_offset = (constants_offset - stack_size) & ALIGNMENT_MASK;

        let mut vm = Self {
            memory,          // Raw memory pointer
            memory_size,     // Total memory size
            alloc_offset: 0, // Heap starts at beginning
            stack_base_offset,
            stack_offset: stack_base_offset,
            constants_offset,                // Constants at the end
            frame_offset: stack_base_offset, // Frame starts at stack base
            ip: 0,
            instructions,
            execution_complete: false,
            call_stack: vec![],
            host_functions: vec![],
            handlers: [const { HandlerType::Args0(Self::execute_unimplemented) }; 256],
            debug_call_depth: 0,
        };

        // Load immediate
        vm.handlers[OpCode::Ld8 as usize] = HandlerType::Args2(Self::execute_ld8);
        vm.handlers[OpCode::Ld16 as usize] = HandlerType::Args2(Self::execute_ld16);
        vm.handlers[OpCode::Ld32 as usize] = HandlerType::Args3(Self::execute_ld32);

        // Load indirect
        vm.handlers[OpCode::Ldx as usize] = HandlerType::Args4(Self::execute_ldx);
        vm.handlers[OpCode::Stx as usize] = HandlerType::Args4(Self::execute_stx);
        vm.handlers[OpCode::St32x as usize] = HandlerType::Args4(Self::execute_st32x);

        // Alloc
        vm.handlers[OpCode::Alloc as usize] = HandlerType::Args2(Self::execute_alloc);

        // Copy data in frame memory
        vm.handlers[OpCode::Mov as usize] = HandlerType::Args3(Self::execute_mov);

        // Comparisons
        vm.handlers[OpCode::LtI32 as usize] = HandlerType::Args3(Self::execute_lt_i32);

        // Conditional jumps
        vm.handlers[OpCode::Bnz as usize] = HandlerType::Args2(Self::execute_bnz);
        vm.handlers[OpCode::Bz as usize] = HandlerType::Args2(Self::execute_bz);

        // Unconditional jump
        vm.handlers[OpCode::Jmp as usize] = HandlerType::Args1(Self::execute_jmp);

        // Operators
        vm.handlers[OpCode::AddI32 as usize] = HandlerType::Args3(Self::execute_add_i32);

        // Call, enter, ret
        vm.handlers[OpCode::Call as usize] = HandlerType::Args1(Self::execute_call);
        vm.handlers[OpCode::Enter as usize] = HandlerType::Args1(Self::execute_enter);
        vm.handlers[OpCode::Ret as usize] = HandlerType::Args0(Self::execute_ret);

        // Halt - return to host
        vm.handlers[OpCode::Hlt as usize] = HandlerType::Args0(Self::execute_hlt);

        // Optional: Zero out the memory for safety?
        unsafe {
            std::ptr::write_bytes(memory, 0, memory_size);
        }

        let start_addr = memory_size - constants.len();
        unsafe {
            std::ptr::copy_nonoverlapping(
                constants.as_ptr(),
                memory.add(start_addr),
                constants.len(),
            );
        }

        vm
    }

    #[must_use]
    pub fn frame_offset(&self) -> usize {
        self.frame_offset
    }

    // Read a value at a specific offset from memory
    #[must_use]
    pub fn get_i32(&self, offset: usize) -> i32 {
        unsafe { *(self.ptr_at_i32(offset) as *const i32) }
    }

    pub fn load_bytecode(&mut self, instructions: Vec<BinaryInstruction>) {
        self.instructions = instructions;
        self.ip = 0;
        self.execution_complete = false;
    }

    #[inline]
    fn execute_ld32(&mut self, dst_offset: u16, lower_bits: u16, upper_bits: u16) {
        let value = ((upper_bits as u32) << 16) | (lower_bits as u32);

        let dst_ptr = self.ptr_at_u32(self.frame_offset + dst_offset as usize) as *mut u32;
        unsafe {
            *dst_ptr = value;
        }
    }

    #[inline]
    fn execute_ld32ptr(&mut self, base_offset: u16, offset: u16, lower_bits: u16, upper_bits: u16) {
        let value = ((upper_bits as u32) << 16) | (lower_bits as u32);

        let ptr_ptr = self.ptr_at_u16(self.frame_offset + base_offset as usize) as *const u16;
        let ptr = unsafe { *ptr_ptr };

        let addr = ptr + offset;

        let dst_ptr = self.ptr_at_u32(addr as usize) as *mut u32;
        unsafe {
            *dst_ptr = value;
        }
    }
    fn execute_ldx(&mut self, dst_offset: u16, base_offset: u16, offset: u16, size: u16) {
        let ptr_ptr = self.ptr_at_u16(self.frame_offset + base_offset as usize) as *const u16;
        let ptr = unsafe { *ptr_ptr };

        let src_addr = ptr + offset;

        let src_ptr = self.ptr_at_u8(src_addr as usize);
        let dst_ptr = self.ptr_at_u8(self.frame_offset + dst_offset as usize);

        unsafe {
            std::ptr::copy_nonoverlapping(src_ptr, dst_ptr, size as usize);
        }
    }

    fn execute_stx(&mut self, base_offset: u16, offset: u16, src_offset: u16, size: u16) {
        let ptr_ptr = self.ptr_at_u16(self.frame_offset + base_offset as usize) as *const u16;
        let ptr = unsafe { *ptr_ptr };

        let dst_addr = ptr + offset;

        let src_ptr = self.ptr_at_u8(self.frame_offset + src_offset as usize);
        let dst_ptr = self.ptr_at_u8(dst_addr as usize);

        unsafe {
            std::ptr::copy_nonoverlapping(src_ptr, dst_ptr, size as usize);
        }
    }

    #[inline]
    fn execute_st32x(&mut self, base_offset: u16, offset: u16, lower_bits: u16, upper_bits: u16) {
        let value = ((upper_bits as u32) << 16) | (lower_bits as u32);

        let ptr_ptr = self.ptr_at_u16(self.frame_offset + base_offset as usize) as *const u16;
        let ptr = unsafe { *ptr_ptr };

        let dst_addr = ptr + offset;

        let dst_ptr = self.ptr_at_u32(dst_addr as usize) as *mut u32;
        unsafe {
            *dst_ptr = value;
        }
    }

    #[inline]
    fn execute_ld16(&mut self, dst_offset: u16, data: u16) {
        let dst_ptr = self.ptr_at_u16(self.frame_offset + dst_offset as usize) as *mut u16;
        unsafe {
            *dst_ptr = data;
        }
    }

    #[inline]
    fn execute_alloc(&mut self, dst_offset: u16, memory_size: u16) {
        let data_ptr = self.allocate(memory_size as usize);
        let dst_ptr = self.ptr_at_u16(self.frame_offset + dst_offset as usize) as *mut u16;
        unsafe {
            *dst_ptr = data_ptr;
        }
    }

    #[inline]
    fn execute_ld8(&mut self, dst_offset: u16, octet: u16) {
        let dst_ptr = self.frame_ptr_bool_at(dst_offset);
        unsafe {
            *dst_ptr = octet as u8;
        }
    }

    #[inline]
    fn execute_add_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.ptr_at_i32(self.frame_offset + lhs_offset as usize) as *const i32;
        let rhs_ptr = self.ptr_at_i32(self.frame_offset + rhs_offset as usize) as *const i32;
        let dst_ptr = self.ptr_at_i32(self.frame_offset + dst_offset as usize) as *mut i32;

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            *dst_ptr = lhs + rhs;
        }
    }

    #[inline]
    fn execute_lt_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.frame_ptr_i32_const_at(lhs_offset);
        let rhs_ptr = self.frame_ptr_i32_const_at(rhs_offset);
        let dst_ptr = self.frame_ptr_bool_at(dst_offset);

        unsafe {
            let lhs = *lhs_ptr;
            let rhs = *rhs_ptr;
            *dst_ptr = (lhs < rhs) as u8;
        }
    }

    #[inline]
    fn execute_bnz(&mut self, condition_offset: u16, absolute_ip: u16) {
        let is_true = self.frame_ptr_bool_const_at(condition_offset);
        if is_true {
            self.ip = absolute_ip as usize;
        }
    }

    #[inline]
    fn execute_bz(&mut self, condition_offset: u16, absolute_ip: u16) {
        let is_true = self.frame_ptr_bool_const_at(condition_offset);
        if !is_true {
            self.ip = absolute_ip as usize;
        }
    }

    #[inline]
    fn execute_jmp(&mut self, absolute_ip: u16) {
        self.ip = absolute_ip as usize;
    }

    #[inline]
    fn execute_hlt(&mut self) {
        self.execution_complete = true;
    }

    fn execute_unimplemented(&mut self) {
        panic!("unknown OPCODE HALT!");
    }

    #[inline]
    fn execute_mov(&mut self, dst_offset: u16, src_offset: u16, size: u16) {
        let src_ptr = self.ptr_at_u16(self.frame_offset + src_offset as usize);
        let dst_ptr = self.ptr_at_u16(self.frame_offset + dst_offset as usize);

        unsafe {
            std::ptr::copy_nonoverlapping(src_ptr, dst_ptr, size as usize);
        }
    }

    // Helper to convert offset to pointer
    #[inline(always)]
    fn ptr_at_i32(&self, offset: usize) -> *mut i32 {
        // Ensure alignment
        debug_assert_eq!(offset % 4, 0, "Unaligned i32 access at offset {}", offset);
        // Inline ptr_at functionality
        unsafe { self.memory.add(offset) as *mut i32 }
    }

    #[inline(always)]
    fn ptr_at_u32(&self, offset: usize) -> *mut u32 {
        // Ensure alignment
        debug_assert_eq!(offset % 4, 0, "Unaligned i32 access at offset {}", offset);
        // Inline ptr_at functionality
        unsafe { self.memory.add(offset) as *mut u32 }
    }

    #[inline(always)]
    fn ptr_at_u16(&self, offset: usize) -> *mut u16 {
        // Ensure alignment
        debug_assert_eq!(offset % 2, 0, "Unaligned u16 access at offset {}", offset);
        // Inline ptr_at functionality
        unsafe { self.memory.add(offset) as *mut u16 }
    }

    #[inline(always)]
    fn ptr_at_u8(&self, offset: usize) -> *mut u8 {
        // Inline ptr_at functionality
        unsafe { self.memory.add(offset) }
    }

    // Helper to get current frame pointer
    fn frame_ptr(&self) -> *mut u8 {
        self.ptr_at_u8(self.frame_offset)
    }

    fn stack_ptr(&self) -> *mut u8 {
        self.ptr_at_u8(self.stack_offset)
    }

    fn stack_base_ptr(&self) -> *mut u8 {
        self.ptr_at_u8(self.stack_base_offset)
    }

    #[inline(always)]
    fn frame_ptr_i32_at(&self, offset: u16) -> *mut i32 {
        self.ptr_at_i32(self.frame_offset + offset as usize)
    }

    #[inline(always)]
    fn frame_ptr_i32_const_at(&self, offset: u16) -> *const i32 {
        self.ptr_at_i32(self.frame_offset + offset as usize)
            .cast_const()
    }

    #[inline(always)]
    fn frame_ptr_bool_at(&self, offset: u16) -> *mut u8 {
        self.ptr_at_u8(self.frame_offset + offset as usize)
    }

    #[inline(always)]
    fn frame_ptr_bool_const_at(&self, offset: u16) -> bool {
        unsafe { *self.ptr_at_u8(self.frame_offset + offset as usize) != 0 }
    }

    fn allocate(&mut self, size: usize) -> u16 {
        let aligned_size = (size + ALIGNMENT_REST) & ALIGNMENT_MASK;
        let result_offset = self.alloc_offset;

        assert!(
            result_offset + aligned_size <= self.stack_base_offset,
            "Out of memory"
        );

        self.alloc_offset += aligned_size;
        result_offset as u16
    }

    pub fn debug_opcode(&self, opcode: u8, operands: &[u16; 4]) {
        eprintln!(
            "{:8} [{}]",
            OpCode::from(opcode),
            match self.handlers[opcode as usize] {
                HandlerType::Args0(_) => String::new(),
                HandlerType::Args1(_) => format!("{:04x}", operands[0]),
                HandlerType::Args2(_) => format!("{:04x}, {:04x}", operands[0], operands[1]),
                HandlerType::Args3(_) => format!(
                    "{:04x}, {:04x}, {:04x}",
                    operands[0], operands[1], operands[2]
                ),
                HandlerType::Args4(_) => format!(
                    "{:04x}, {:04x}, {:04x}, {:04x}",
                    operands[0], operands[1], operands[2], operands[3]
                ),
            }
        );
    }

    fn debug_instructions(&self) {
        for (ip, instruction) in self.instructions.iter().enumerate() {
            eprint!("|> {ip:04x}: ");
            let operands = instruction.operands;
            self.debug_opcode(instruction.opcode, &operands);
        }
    }

    pub fn execute(&mut self) {
        self.ip = 0;
        self.execution_complete = false;
        #[cfg(feature = "debug_vm")]
        {
            eprintln!("program:");
            self.debug_instructions();
            eprintln!("start executing");
        }

        self.call_stack.push(CallFrame {
            return_address: 1,
            previous_frame_offset: 0,
            frame_size: 0,
        });

        while !self.execution_complete {
            let instruction = &self.instructions[self.ip];
            let opcode = instruction.opcode;

            #[cfg(feature = "debug_vm")]
            {
                let operands = instruction.operands;
                eprint!("> {:04x}: ", self.ip);
                self.debug_opcode(opcode, &operands);

                //    let s = hexify::format_hex(&self.frame_memory()[..16]);
                //  eprintln!("mem: {s}");
            }

            match self.handlers[opcode as usize] {
                HandlerType::Args0(handler) => handler(self),
                HandlerType::Args1(handler) => handler(self, instruction.operands[0]),
                HandlerType::Args2(handler) => {
                    handler(self, instruction.operands[0], instruction.operands[1]);
                }
                HandlerType::Args3(handler) => handler(
                    self,
                    instruction.operands[0],
                    instruction.operands[1],
                    instruction.operands[2],
                ),
                HandlerType::Args4(handler) => handler(
                    self,
                    instruction.operands[0],
                    instruction.operands[1],
                    instruction.operands[2],
                    instruction.operands[3],
                ),
            };

            self.ip += 1;
        }
    }

    fn execute_call(&mut self, target: u16) {
        let return_info = CallFrame {
            return_address: self.ip + 1,              // Instruction to return to
            previous_frame_offset: self.frame_offset, // Previous frame position
            frame_size: 0,                            // Will be filled by ENTER
        };

        self.call_stack.push(return_info);

        self.ip = target as usize;
    }

    #[inline]
    fn execute_enter(&mut self, aligned_size: u16) {
        //let aligned_size = (frame_size as usize + ALIGNMENT_REST) & ALIGNMENT_MASK; // 8-byte alignment

        let frame = self.call_stack.last_mut().unwrap();
        frame.frame_size = aligned_size as usize;

        // the functions frame of reference should be the stack offset
        self.frame_offset = self.stack_offset;

        // and we push the stack with the space of the local variables
        self.stack_offset += aligned_size as usize;
    }

    #[inline]
    fn execute_ret(&mut self) {
        let frame = self.call_stack.pop().unwrap();

        // Bring back the frame to the old frame
        self.frame_offset = frame.previous_frame_offset;

        // "pop" the space for the local variables of the stack
        self.stack_offset -= frame.frame_size;

        // going back to the old instruction
        self.ip = frame.return_address;
        self.ip -= 1; // Adjust for automatic increment

        // NOTE: Any return value is always at frame_offset + 0
    }
}

pub struct CallFrame {
    return_address: usize,        // Instruction to return to
    previous_frame_offset: usize, // Previous frame position
    frame_size: usize,            // Size of this frame
}
