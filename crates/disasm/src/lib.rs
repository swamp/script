use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::{BinaryInstruction, FrameMemoryAddress, InstructionPosition};
use yansi::Paint;

#[derive(Debug, Clone)]
pub enum DecoratedOperandKind {
    ReadFrameAddress(FrameMemoryAddress, DecoratedMemoryKind),
    WriteFrameAddress(FrameMemoryAddress, DecoratedMemoryKind),
    Ip(InstructionPosition),
    ImmediateU32(u32),
}

#[derive(Clone, Debug)]
pub enum DecoratedMemoryKind {
    U8,
    U16,
    U32,
    S32,
    Fp32,
    B8,
}
pub struct DecoratedOperand {
    pub kind: DecoratedOperandKind,
}

pub struct DecoratedOpcode {
    pub name: String,
    pub operands: Vec<DecoratedOperand>,
}

fn memory_kind_color(kind: &DecoratedMemoryKind) -> String {
    let short_string = match kind {
        DecoratedMemoryKind::B8 => "b8",
        DecoratedMemoryKind::U8 => "u8",
        DecoratedMemoryKind::U16 => "u16",
        DecoratedMemoryKind::U32 => "u32",
        DecoratedMemoryKind::S32 => "i32",
        DecoratedMemoryKind::Fp32 => "fp32",
    };

    format!("{}", short_string.yellow())
}

#[must_use]
pub fn disasm_instructions_color(binary_instruction: &[BinaryInstruction]) -> String {
    let mut string = String::new();

    for (ip_index, instruction) in binary_instruction.iter().enumerate() {
        string += &format!("> {:04X}: {}\n", ip_index, disasm_color(instruction));
    }

    string
}

#[must_use]
pub fn disasm_color(binary_instruction: &BinaryInstruction) -> String {
    let decorated = disasm(binary_instruction);

    let name = format!("{}", decorated.name.blue());

    let mut converted_operands = Vec::new();
    let mut converted_comments = Vec::new();

    for operand in decorated.operands {
        let (new_str, comment_str) = match operand.kind {
            DecoratedOperandKind::ReadFrameAddress(a, b) => {
                (format!("${}", a.0.green()), memory_kind_color(&b))
            }
            DecoratedOperandKind::WriteFrameAddress(a, b) => {
                (format!("${}", a.0.red()), memory_kind_color(&b))
            }
            DecoratedOperandKind::Ip(ip) => {
                (format!("@{}", format!("{:X}", ip.0).cyan()), String::new())
            }
            DecoratedOperandKind::ImmediateU32(data) => (
                format!("{}", format!("{data:08X}",).bright_magenta()),
                format!("int:{}", data as i32),
            ),
        };
        converted_operands.push(new_str);
        converted_comments.push(comment_str);
    }

    let comment_suffix = if converted_comments.is_empty() {
        String::new()
    } else {
        format!(" # {}", converted_comments.join(", "))
    };

    format!(
        "{} {}{}",
        name,
        converted_operands.join(" "),
        comment_suffix
    )
}

pub fn disasm(binary_instruction: &BinaryInstruction) -> DecoratedOpcode {
    let opcode: OpCode = binary_instruction.opcode.into();
    let operands = binary_instruction.operands;

    let operands_slice: &[DecoratedOperandKind] = match opcode {
        OpCode::Hlt => &[],
        OpCode::Ld => todo!(),
        OpCode::St => todo!(),
        OpCode::Ld32 => {
            let data = ((operands[2] as u32) << 16) | operands[1] as u32;
            //let lower_bits = (value_u32 & 0xFFFF) as u16;
            //let upper_bits = (value_u32 >> 16) as u16;

            &[
                to_write_frame(operands[0], DecoratedMemoryKind::S32),
                DecoratedOperandKind::ImmediateU32(data),
            ]
        }
        OpCode::AddI32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32),
            to_read_frame(operands[1], DecoratedMemoryKind::S32),
            to_read_frame(operands[2], DecoratedMemoryKind::S32),
        ],
        OpCode::LtI32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::B8),
            to_read_frame(operands[1], DecoratedMemoryKind::S32),
            to_read_frame(operands[2], DecoratedMemoryKind::S32),
        ],
        OpCode::Bnz => &[
            to_read_frame(operands[0], DecoratedMemoryKind::B8),
            to_jmp_ip(operands[1]),
        ],
        OpCode::Bz => &[
            to_read_frame(operands[0], DecoratedMemoryKind::B8),
            to_jmp_ip(operands[1]),
        ],
        OpCode::Call => todo!(),
        OpCode::Enter => todo!(),
        OpCode::Ret => todo!(),
        OpCode::Jmp => &[to_jmp_ip(operands[0])],
        OpCode::Mov => todo!(),
        OpCode::Ld16 => todo!(),
        OpCode::Ld8 => todo!(),
        OpCode::LtU16 => todo!(),
        OpCode::LdIndirect => todo!(),
    };

    let converted_operands = operands_slice
        .iter()
        .map(|kind| DecoratedOperand { kind: kind.clone() })
        .collect();

    DecoratedOpcode {
        name: opcode.to_string(),
        operands: converted_operands,
    }
}

fn to_write_frame(addr: u16, mem: DecoratedMemoryKind) -> DecoratedOperandKind {
    DecoratedOperandKind::WriteFrameAddress(to_frame(addr), mem)
}

fn to_ip(ip: u16) -> DecoratedOperandKind {
    DecoratedOperandKind::Ip(InstructionPosition(ip))
}

fn to_jmp_ip(ip: u16) -> DecoratedOperandKind {
    DecoratedOperandKind::Ip(InstructionPosition(ip + 1))
}

fn to_read_frame(addr: u16, mem: DecoratedMemoryKind) -> DecoratedOperandKind {
    DecoratedOperandKind::ReadFrameAddress(to_frame(addr), mem)
}

fn to_frame(val: u16) -> FrameMemoryAddress {
    FrameMemoryAddress(val)
}
