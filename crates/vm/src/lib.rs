use crate::host::HostFunction;
use crate::opcode::OpCode;

pub const INT_SIZE: u16 = 4;
pub const BOOL_SIZE: u16 = 1;
pub const PTR_SIZE: u16 = 2;

#[repr(C, packed)]
pub struct BinaryInstruction {
    opcode: u8,
    opcode_count: u8, // It is mainly for alignment, but use it for opcode_couont or maybe checksum or extra flags?
    operands: [u16; 4],
}

pub mod host;
pub mod instr_bldr;
pub mod opcode;

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

const ALIGNMENT: usize = 8;
const ALIGNMENT_REST: usize = ALIGNMENT - 1;
const ALIGNMENT_MASK: usize = !ALIGNMENT_REST;

impl Vm {
    pub fn new(instructions: Vec<BinaryInstruction>, memory_size: usize) -> Self {
        let memory = unsafe {
            std::alloc::alloc(std::alloc::Layout::from_size_align(memory_size, ALIGNMENT).unwrap())
        };

        // Reserve 20% for constants at the end
        let constants_size = (memory_size / 5) & ALIGNMENT_MASK;
        let constants_offset = memory_size - constants_size;

        // Reserve 30% for stack in the middle
        let stack_size = (memory_size * 3 / 10) & ALIGNMENT_MASK;
        let stack_base_offset = (constants_offset - stack_size) & ALIGNMENT_MASK;

        let mut vm = Vm {
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

        vm.handlers[OpCode::LdLocal as usize] = HandlerType::Args2(Self::execute_ld_local);
        vm.handlers[OpCode::LdImmI32 as usize] = HandlerType::Args3(Self::execute_ld_imm_i32);
        vm.handlers[OpCode::AddI32 as usize] = HandlerType::Args3(Self::execute_add_i32);
        vm.handlers[OpCode::LtI32 as usize] = HandlerType::Args3(Self::execute_lt_i32);
        vm.handlers[OpCode::JmpIf as usize] = HandlerType::Args2(Self::execute_jmp_if);
        vm.handlers[OpCode::JmpIfNot as usize] = HandlerType::Args2(Self::execute_jmp_if_not);
        vm.handlers[OpCode::Call as usize] = HandlerType::Args1(Self::execute_call);

        vm.handlers[OpCode::Enter as usize] = HandlerType::Args1(Self::execute_enter);
        vm.handlers[OpCode::Ret as usize] = HandlerType::Args0(Self::execute_ret);

        vm.handlers[OpCode::End as usize] = HandlerType::Args0(Self::execute_end);

        // Optional: Zero out the memory for safety?
        unsafe {
            std::ptr::write_bytes(memory, 0, memory_size);
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
        unsafe { *(self.ptr_at(offset) as *const i32) }
    }

    pub fn load_bytecode(&mut self, instructions: Vec<BinaryInstruction>) {
        self.instructions = instructions;
        self.ip = 0;
        self.execution_complete = false;
    }

    fn execute_ld_imm_i32(&mut self, dst_offset: u16, lower_bits: u16, upper_bits: u16) {
        let value = ((upper_bits as i32) << 16) | (lower_bits as i32);

        let dst_ptr = self.ptr_at(self.frame_offset + dst_offset as usize) as *mut i32;
        unsafe {
            *dst_ptr = value;
        }
    }

    #[inline]
    fn execute_add_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        let lhs_ptr = self.ptr_at(self.frame_offset + lhs_offset as usize) as *const i32;
        let rhs_ptr = self.ptr_at(self.frame_offset + rhs_offset as usize) as *const i32;
        let dst_ptr = self.ptr_at(self.frame_offset + dst_offset as usize) as *mut i32;

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
    fn execute_jmp_if(&mut self, condition_offset: u16, jump_offset: u16) {
        let is_true = self.frame_ptr_bool_const_at(condition_offset);
        if is_true {
            self.ip = (self.ip as isize + jump_offset as isize) as usize;
        }
    }

    #[inline]
    fn execute_jmp_if_not(&mut self, condition_offset: u16, jump_offset: u16) {
        let is_true = self.frame_ptr_bool_const_at(condition_offset);
        if !is_true {
            self.ip = (self.ip as isize + jump_offset as isize) as usize;
        }
    }

    #[inline]
    fn execute_end(&mut self) {
        self.execution_complete = true;
    }

    fn execute_unimplemented(&mut self) {}

    // Type-specific handlers
    fn execute_ld_local(&mut self, dst_offset: u16, src_offset: u16) {
        let src_ptr = self.ptr_at(self.frame_offset + src_offset as usize);
        let dst_ptr = self.ptr_at(self.frame_offset + dst_offset as usize);

        unsafe {
            std::ptr::copy_nonoverlapping(src_ptr, dst_ptr, 4);
        }
    }

    // Helper to convert offset to pointer
    #[must_use]
    pub const fn ptr_at(&self, offset: usize) -> *mut u8 {
        unsafe { self.memory.add(offset) }
    }

    // Helper to get current frame pointer
    const fn frame_ptr(&self) -> *mut u8 {
        self.ptr_at(self.frame_offset)
    }

    #[inline]
    fn frame_ptr_i32_at(&self, offset: u16) -> *mut i32 {
        self.ptr_at(self.frame_offset + offset as usize) as *mut i32
    }

    #[inline]
    fn frame_ptr_i32_const_at(&self, offset: u16) -> *const i32 {
        self.ptr_at(self.frame_offset + offset as usize) as *const i32
    }

    #[inline]
    fn frame_ptr_bool_at(&self, offset: u16) -> *mut u8 {
        self.ptr_at(self.frame_offset + offset as usize) as *mut u8
    }

    #[inline]
    fn frame_ptr_bool_const_at(&self, offset: u16) -> bool {
        unsafe { *self.ptr_at(self.frame_offset + offset as usize) != 0 }
    }

    const fn frame_ptr_at(&self, offset: u16) -> *mut u8 {
        self.ptr_at(self.frame_offset + offset as usize)
    }

    fn allocate(&mut self, size: usize) -> usize {
        let aligned_size = (size + ALIGNMENT_REST) & ALIGNMENT_MASK;
        let result_offset = self.alloc_offset;

        assert!(
            result_offset + aligned_size <= self.stack_base_offset,
            "Out of memory"
        );

        self.alloc_offset += aligned_size;
        result_offset
    }

    pub fn execute(&mut self) {
        self.ip = 0;
        self.execution_complete = false;

        while !self.execution_complete {
            let instruction = &self.instructions[self.ip];
            let opcode = instruction.opcode;

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
        // Save return information
        let return_info = CallFrame {
            return_address: self.ip + 1,              // Instruction to return to
            previous_frame_offset: self.frame_offset, // Previous frame position
            frame_size: 0,                            // Will be filled by ENTER
        };

        // Push to call stack
        self.call_stack.push(return_info);

        // Jump to function
        self.ip = target as usize;
        self.ip -= 1; // Adjust for automatic increment
    }

    fn execute_enter(&mut self, frame_size: u16) {
        let aligned_size = (frame_size as usize + ALIGNMENT_REST) & ALIGNMENT_MASK; // 8-byte alignment

        let frame = self.call_stack.last_mut().unwrap();
        frame.frame_size = aligned_size;

        // the functions frame of reference should be the stack offset
        self.frame_offset = self.stack_offset;

        // and we push the stack with the space of the local variables
        self.stack_offset += aligned_size;
    }

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
