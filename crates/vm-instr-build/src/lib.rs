use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::{
    BinaryInstruction, CountU16, FrameMemoryAddress, FrameMemorySize, InstructionPosition,
    MemoryAddress, MemorySize,
};

#[derive(Debug)]
pub struct PatchPosition(pub InstructionPosition);

pub struct InstructionBuilder {
    pub instructions: Vec<BinaryInstruction>,
    pub comments: Vec<String>,
}

impl InstructionBuilder {}

impl InstructionBuilder {}

impl InstructionBuilder {}

impl InstructionBuilder {}

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
            comments: Vec::new(),
        }
    }

    #[must_use]
    pub fn position(&self) -> InstructionPosition {
        InstructionPosition(self.instructions.len() as u16)
    }

    pub fn add_jmp_if_equal_placeholder(&mut self, comment: &str) -> PatchPosition {
        let position = self.position();

        self.add_instruction(OpCode::Bz, &[0], comment);

        PatchPosition(position)
    }

    pub fn add_jmp_if_not_equal_placeholder(&mut self, comment: &str) -> PatchPosition {
        let position = self.position();

        self.add_instruction(OpCode::Bnz, &[0], comment);

        PatchPosition(position)
    }

    pub fn add_eq_u8_immediate(
        &mut self,
        source_addr: FrameMemoryAddress,
        immediate: u8,
        comment: &str,
    ) {
        self.add_instruction(OpCode::Eq8Imm, &[source_addr.0, immediate as u16], comment);
    }

    pub fn add_call_placeholder(&mut self, comment: &str) -> PatchPosition {
        let position = self.position();
        self.add_instruction(OpCode::Call, &[0], comment);
        PatchPosition(position)
    }

    pub fn add_jump_placeholder(&mut self, comment: &str) -> PatchPosition {
        let position = self.position();

        self.add_instruction(OpCode::Jmp, &[0], comment);

        PatchPosition(position)
    }

    pub fn add_enter(&mut self, size: FrameMemorySize, comment: &str) {
        self.add_instruction(OpCode::Enter, &[size.0], comment);
    }

    // Mov is more of a copy. Keeping the name Mov because it is old school and idiomatic.
    pub fn add_mov(
        &mut self,
        target: FrameMemoryAddress,
        source: FrameMemoryAddress,
        size: MemorySize,
        comment: &str,
    ) {
        self.add_instruction(OpCode::Mov, &[target.0, source.0, size.0], comment);
    }

    pub fn add_ret(&mut self, comment: &str) {
        self.add_instruction(OpCode::Ret, &[], comment);
    }

    pub fn add_hlt(&mut self, comment: &str) {
        self.add_instruction(OpCode::Hlt, &[], comment);
    }

    pub fn add_call(&mut self, function_ip: &InstructionPosition, comment: &str) {
        self.add_instruction(OpCode::Call, &[function_ip.0], comment);
    }

    /// # Panics
    ///
    pub fn patch_jump(
        &mut self,
        patch_position: PatchPosition,
        target_position: &InstructionPosition,
    ) {
        const JMP_IF_NOT: u8 = OpCode::Bz as u8;
        const JMP_IF: u8 = OpCode::Bnz as u8;
        const JMP: u8 = OpCode::Jmp as u8;

        let instruction = &mut self.instructions[patch_position.0.0 as usize];

        match instruction.opcode {
            JMP_IF_NOT => {
                instruction.operands[0] = target_position.0 as u16 - 1;
            }
            JMP_IF => {
                instruction.operands[0] = target_position.0 as u16 - 1;
            }
            JMP => {
                instruction.operands[0] = target_position.0 as u16 - 1;
            }
            _ => panic!("Attempted to patch a non-jump instruction at position {patch_position:?}"),
        }
    }

    // It takes ownership of the patch position
    pub fn patch_jump_here(&mut self, jump_position: PatchPosition) {
        self.patch_jump(jump_position, &self.position());
    }

    /// # Panics
    ///
    pub fn patch_call(&mut self, patch_position: PatchPosition, ip: &InstructionPosition) {
        const CALL: u8 = OpCode::Call as u8;

        let instruction = &mut self.instructions[patch_position.0.0 as usize];

        match instruction.opcode {
            CALL => {
                instruction.operands[0] = ip.0 as u16 - 1;
            }
            _ => panic!("Attempted to patch a non-call instruction at position {patch_position:?}"),
        }
    }

    pub fn add_jmp(&mut self, ip: InstructionPosition, comment: &str) {
        self.add_instruction(OpCode::Jmp, &[ip.0 - 1], comment);
    }

    pub fn add_ld32(&mut self, dst_offset: FrameMemoryAddress, value: i32, comment: &str) {
        let value_u32 = value as u32;

        let lower_bits = (value_u32 & 0xFFFF) as u16;
        let upper_bits = (value_u32 >> 16) as u16;

        self.add_instruction(
            OpCode::Ld32,
            &[dst_offset.0, lower_bits, upper_bits],
            comment,
        );
    }

    pub fn add_ld8(&mut self, dst_offset: FrameMemoryAddress, value: u8, comment: &str) {
        self.add_instruction(OpCode::Ld8, &[dst_offset.0, value as u16], comment);
    }

    pub fn add_load_frame_address(
        &mut self,
        dest: FrameMemoryAddress,
        addr: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_ld_u16(dest, addr.0, comment);
    }

    pub fn add_ld_u16(&mut self, dest: FrameMemoryAddress, data: u16, comment: &str) {
        self.add_instruction(OpCode::Ld16, &[dest.0, data], comment);
    }

    pub fn add_add_i32(
        &mut self,
        dst_offset: FrameMemoryAddress,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::AddI32,
            &[dst_offset.0, lhs_offset.0, rhs_offset.0],
            comment,
        );
    }

    pub fn add_jmp_if(
        &mut self,
        condition_offset: FrameMemoryAddress,
        jmp_target: &InstructionPosition,
        comment: &str,
    ) {
        self.add_instruction(OpCode::Bnz, &[condition_offset.0, jmp_target.0], comment);
    }

    pub fn add_jmp_if_not(
        &mut self,
        condition_offset: MemoryAddress,
        jmp_target: InstructionPosition,
        comment: &str,
    ) {
        self.add_instruction(OpCode::Bz, &[condition_offset.0, jmp_target.0], comment);
    }

    pub fn add_lt_i32(
        &mut self,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::LtI32, &[lhs_offset.0, rhs_offset.0], comment);
    }

    pub fn add_gt_i32(
        &mut self,
        lhs_offset: FrameMemoryAddress,
        rhs_offset: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::GtI32, &[lhs_offset.0, rhs_offset.0], comment);
    }

    pub fn add_lt_u16(
        &mut self,
        dest: FrameMemoryAddress,
        a: FrameMemoryAddress,
        b: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::LtU16, &[dest.0, a.0, b.0], comment);
    }

    pub fn add_tst8(&mut self, addr: FrameMemoryAddress, comment: &str) {
        self.add_instruction(OpCode::Tst8, &[addr.0], comment);
    }

    // Collection specific
    pub fn add_map_new_from_slice(
        &mut self,
        map_target_addr: FrameMemoryAddress,
        slice_source_addr: FrameMemoryAddress,
        key_size: MemorySize,
        value_size: MemorySize,
        count: CountU16,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::MapNewFromPairs,
            &[
                map_target_addr.0,
                slice_source_addr.0,
                key_size.0,
                value_size.0,
                count.0,
            ],
            comment,
        );
    }

    pub fn add_map_remove(
        &mut self,
        map_target_addr: FrameMemoryAddress,
        key_addr: FrameMemoryAddress,
        comment: &str,
    ) {
        self.add_instruction(OpCode::MapRemove, &[map_target_addr.0, key_addr.0], comment);
    }

    fn add_instruction(&mut self, op_code: OpCode, operands: &[u16], comment: &str) {
        let mut array: [u16; 5] = [0; 5];
        assert!(operands.len() <= 5);
        let len = operands.len();
        array[..len].copy_from_slice(&operands[..len]);
        self.instructions.push(BinaryInstruction {
            opcode: op_code as u8,
            operands: array,
        });
        self.comments.push(comment.to_string());
    }

    /*

    pub fn add_stx(
        &mut self,
        indirect_target: FrameMemoryAddress,
        offset: MemoryOffset,
        source_address: FrameMemoryAddress,
        size: MemorySize,
        comment: &str,
    ) {
        self.add_instruction(
            OpCode::Stx,
            &[indirect_target.0, offset.0, source_address.0, size.0],
            comment,
        );
    }
    pub fn add_alloc(&mut self, target: FrameMemoryAddress, size: MemorySize, comment: &str) {
        self.add_instruction(OpCode::Alloc, &[target.0, size.0], comment);
    }

     */
}
