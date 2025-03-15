use crate::BinaryInstruction;
use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::{FrameMemoryAddress, InstructionPosition, MemoryAddress, MemorySize};

#[derive(Debug)]
pub struct PatchPosition(pub InstructionPosition);

pub struct InstructionBuilder {
    pub instructions: Vec<BinaryInstruction>,
}

impl InstructionBuilder {}

impl InstructionBuilder {}

impl InstructionBuilder {}

impl Default for InstructionBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl InstructionBuilder {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    #[must_use]
    pub fn position(&self) -> InstructionPosition {
        InstructionPosition(self.instructions.len() as u16)
    }

    pub fn add_conditional_jump_placeholder(
        &mut self,
        condition_addr: FrameMemoryAddress,
    ) -> PatchPosition {
        let position = self.position();

        self.add_instruction(OpCode::Bz, &[condition_addr.0, 0]);

        PatchPosition(position)
    }

    pub fn add_jump_placeholder(&mut self) -> PatchPosition {
        let position = self.position();

        self.add_instruction(OpCode::Jmp, &[0]);

        PatchPosition(position)
    }

    // Mov is more of a copy. Keeping the name Mov because it is old school and idiomatic.
    pub fn add_mov(
        &mut self,
        target: FrameMemoryAddress,
        source: FrameMemoryAddress,
        size: MemorySize,
    ) {
        self.add_instruction(OpCode::Mov, &[target.0, source.0]);
    }

    pub fn add_ret(&mut self) {
        self.add_instruction(OpCode::Ret, &[0]);
    }

    pub fn add_call(&mut self, function_ip: &InstructionPosition) {
        self.add_instruction(OpCode::Call, &[function_ip.0]);
    }

    /// # Panics
    ///
    pub fn patch_jump(
        &mut self,
        patch_position: PatchPosition,
        target_position: &InstructionPosition,
    ) {
        const JMP_IF_NOT: u8 = OpCode::Bz as u8;
        const JMP: u8 = OpCode::Jmp as u8;

        let instruction = &mut self.instructions[patch_position.0.0 as usize];

        match instruction.opcode {
            JMP_IF_NOT => {
                // For conditional jump, target ip addr is the second operand
                instruction.operands[1] = target_position.0 as u16 - 1;
            }
            JMP => {
                // For conditional jump, target ip addr is the first operand
                // TODO: maybe have them both at the first operand?
                instruction.operands[0] = target_position.0 as u16 - 1;
            }
            _ => panic!("Attempted to patch a non-jump instruction at position {patch_position:?}"),
        }
    }

    // It takes ownership of the patch position
    pub fn patch_jump_here(&mut self, jump_position: PatchPosition) {
        self.patch_jump(jump_position, &self.position());
    }

    pub fn add_jmp(&mut self, ip: InstructionPosition) {
        self.add_instruction(OpCode::Jmp, &[ip.0 - 1]);
    }

    pub fn add_ld_local(&mut self, dst_offset: FrameMemoryAddress, src_offset: u16) {
        self.add_instruction(OpCode::Ld, &[dst_offset.0, src_offset]);
    }
    pub fn add_st_local(&mut self, dst_offset: FrameMemoryAddress, src_offset: u16) {
        self.add_instruction(OpCode::St, &[dst_offset.0, src_offset]);
    }

    pub fn add_ld_imm_i32(&mut self, dst_offset: FrameMemoryAddress, value: i32) {
        let value_u32 = value as u32;

        let lower_bits = (value_u32 & 0xFFFF) as u16;
        let upper_bits = (value_u32 >> 16) as u16;

        self.add_instruction(OpCode::Ld32, &[dst_offset.0, lower_bits, upper_bits]);
    }

    pub fn add_ld_imm_u8(&mut self, dst_offset: FrameMemoryAddress, value: u8) {
        self.add_instruction(OpCode::Ld8, &[dst_offset.0, value as u16]);
    }

    pub fn add_load_frame_address(&mut self, dest: FrameMemoryAddress, addr: FrameMemoryAddress) {
        self.add_ld_imm_u16(dest, addr.0);
    }

    pub fn add_ld_imm_u16(&mut self, dest: FrameMemoryAddress, data: u16) {
        self.add_instruction(OpCode::Ld16, &[dest.0, data]);
    }

    pub fn add_add_i32(
        &mut self,
        dst_offset: FrameMemoryAddress,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
    ) {
        self.add_instruction(OpCode::AddI32, &[dst_offset.0, lhs_offset.0, rhs_offset.0]);
    }

    pub fn add_jmp_if(
        &mut self,
        condition_offset: FrameMemoryAddress,
        jmp_target: &InstructionPosition,
    ) {
        self.add_instruction(OpCode::Bnz, &[condition_offset.0, jmp_target.0]);
    }

    pub fn add_jmp_if_not(
        &mut self,
        condition_offset: MemoryAddress,
        jmp_target: InstructionPosition,
    ) {
        self.add_instruction(OpCode::Bz, &[condition_offset.0, jmp_target.0]);
    }

    pub fn add_lt_i32(
        &mut self,
        dst_offset: FrameMemoryAddress,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
    ) {
        self.add_instruction(OpCode::LtI32, &[dst_offset.0, lhs_offset.0, rhs_offset.0]);
    }

    pub fn add_lt_u16(
        &mut self,
        dest: FrameMemoryAddress,
        a: FrameMemoryAddress,
        b: FrameMemoryAddress,
    ) {
        self.add_instruction(OpCode::LtU16, &[dest.0, a.0, b.0]);
    }

    pub fn add_ld_indirect(
        &mut self,
        dest: FrameMemoryAddress,
        base_ptr: FrameMemoryAddress,
        offset: FrameMemoryAddress,
        size: u16,
    ) {
        self.add_instruction(OpCode::LdIndirect, &[dest.0, base_ptr.0, offset.0, size]);
    }

    pub fn add_end(&mut self) {
        self.add_instruction(OpCode::Hlt, &[]);
    }

    fn add_instruction(&mut self, op_code: OpCode, operands: &[u16]) {
        let mut array: [u16; 4] = [0; 4];
        let len = operands.len().min(4);
        array[..len].copy_from_slice(&operands[..len]);
        self.instructions.push(BinaryInstruction {
            opcode: op_code as u8,
            opcode_count: operands.len() as u8,
            operands: array,
        });
    }
}
