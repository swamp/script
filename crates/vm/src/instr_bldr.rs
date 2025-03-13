use crate::BinaryInstruction;
use crate::opcode::OpCode;

pub struct InstructionBuilder {
    pub instructions: Vec<BinaryInstruction>,
}
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
    pub fn position(&self) -> usize {
        self.instructions.len()
    }

    pub fn add_conditional_jump_placeholder(&mut self, condition_addr: u16) -> usize {
        let position = self.instructions.len();

        self.add_instruction(OpCode::JmpIfNot, &[condition_addr, 0]);

        position
    }

    pub fn add_jump_placeholder(&mut self) -> usize {
        let position = self.instructions.len();

        self.add_instruction(OpCode::Jmp, &[0]);

        position
    }

    /// # Panics
    ///
    pub fn patch_jump(&mut self, jump_position: usize, target_position: u16) {
        const JMP_IF_NOT: u8 = OpCode::JmpIfNot as u8;
        const JMP: u8 = OpCode::Jmp as u8;

        let instruction = &mut self.instructions[jump_position];

        match instruction.opcode {
            JMP_IF_NOT => {
                // For conditional jump, target ip addr is the second operand
                instruction.operands[1] = target_position;
            }
            JMP => {
                // For conditional jump, target ip addr is the first operand
                // TODO: maybe have them both at the first operand?
                instruction.operands[0] = target_position;
            }
            _ => panic!("Attempted to patch a non-jump instruction at position {jump_position}"),
        }
    }

    pub fn patch_jump_here(&mut self, jump_position: usize) {
        self.patch_jump(jump_position, self.position() as u16);
    }

    pub fn add_ld_local(&mut self, dst_offset: u16, src_offset: u16) {
        self.add_instruction(OpCode::LdLocal, &[dst_offset, src_offset]);
    }
    pub fn add_st_local(&mut self, dst_offset: u16, src_offset: u16) {
        self.add_instruction(OpCode::StoreLocal, &[dst_offset, src_offset]);
    }

    pub fn add_ld_imm_i32(&mut self, dst_offset: u16, value: i32) {
        let lower_bits = (value & 0xFFFF) as u16;
        let upper_bits = ((value >> 16) & 0xFFFF) as u16;

        self.add_instruction(OpCode::LdImmI32, &[dst_offset, lower_bits, upper_bits]);
    }

    pub fn add_add_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        self.add_instruction(OpCode::AddI32, &[dst_offset, lhs_offset, rhs_offset]);
    }

    pub fn add_jmp_if(&mut self, condition_offset: u16, jmp_offset: u16) {
        self.add_instruction(OpCode::JmpIf, &[condition_offset, jmp_offset]);
    }

    pub fn add_jmp_if_not(&mut self, condition_offset: u16, jmp_offset: u16) {
        self.add_instruction(OpCode::JmpIfNot, &[condition_offset, jmp_offset]);
    }

    pub fn add_lt_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) {
        self.add_instruction(OpCode::LtI32, &[dst_offset, lhs_offset, rhs_offset]);
    }

    pub fn add_end(&mut self) {
        self.add_instruction(OpCode::End, &[]);
    }

    fn add_instruction(&mut self, op_code: OpCode, operands: &[u16]) {
        let mut array: [u16; 4] = [0; 4];
        array.clone_from_slice(operands);
        self.instructions.push(BinaryInstruction {
            opcode: op_code as u8,
            opcode_count: operands.len() as u8,
            operands: array,
        });
    }
}
