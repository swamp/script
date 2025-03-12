use crate::BinaryInstruction;
use crate::opcode::OpCode;

pub struct InstructionBuilder {
    pub instructions: Vec<BinaryInstruction>,
}

impl InstructionBuilder {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    pub fn add_ld_local(&mut self, dst_offset: u16, src_offset: u16) -> usize {
        let position = self.instructions.len();
        self.instructions.push(BinaryInstruction {
            opcode: OpCode::LdLocal as u8,
            opcode_count: 2,
            operands: [dst_offset, src_offset, 0, 0],
        });
        position
    }
    pub fn add_st_local(&mut self, dst_offset: u16, src_offset: u16) -> usize {
        let position = self.instructions.len();
        self.instructions.push(BinaryInstruction {
            opcode: OpCode::StoreLocal as u8,
            opcode_count: 2,
            operands: [dst_offset, src_offset, 0, 0],
        });
        position
    }

    pub fn add_ld_imm_i32(&mut self, dst_offset: u16, value: i32) -> usize {
        let position = self.instructions.len();

        let lower_bits = (value & 0xFFFF) as u16;
        let upper_bits = ((value >> 16) & 0xFFFF) as u16;

        self.instructions.push(BinaryInstruction {
            opcode: OpCode::LdImmI32 as u8,
            opcode_count: 3,
            operands: [dst_offset, lower_bits, upper_bits, 0],
        });
        position
    }

    pub fn add_add_i32(&mut self, dst_offset: u16, lhs_offset: u16, rhs_offset: u16) -> usize {
        let position = self.instructions.len();
        self.instructions.push(BinaryInstruction {
            opcode: OpCode::AddI32 as u8,
            opcode_count: 3,
            operands: [dst_offset, lhs_offset, rhs_offset, 0],
        });
        position
    }

    pub fn add_end(&mut self) -> usize {
        let position = self.instructions.len();
        self.instructions.push(BinaryInstruction {
            opcode: OpCode::End as u8,
            opcode_count: 0,
            operands: [0, 0, 0, 0],
        });
        position
    }
}
