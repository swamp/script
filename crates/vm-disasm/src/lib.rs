use seq_map::SeqMap;
use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::{BinaryInstruction, FrameMemoryAddress, InstructionPosition};
use yansi::Paint;

#[derive(Debug, Clone)]
pub enum DecoratedOperandKind {
    ReadFrameAddress(FrameMemoryAddress, DecoratedMemoryKind),
    WriteFrameAddress(FrameMemoryAddress, DecoratedMemoryKind),
    Ip(InstructionPosition),
    ImmediateU32(u32),
    ImmediateU16(u16),
    MemorySize(u16),
}

#[derive(Clone, Debug)]
pub enum DecoratedMemoryKind {
    U8,
    U16,
    U32,
    S32,
    Fp32,
    B8,
    Octets,
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
        DecoratedMemoryKind::Octets => "*b8",
    };

    format!("{}", short_string.white())
}

#[must_use]
pub fn disasm_instructions_color(
    binary_instruction: &[BinaryInstruction],
    descriptions: &[String],
    ip_infos: &SeqMap<InstructionPosition, String>,
) -> String {
    let mut string = String::new();

    for (ip_index, (instruction, comment)) in
        binary_instruction.iter().zip(descriptions).enumerate()
    {
        if let Some(found) = ip_infos.get(&InstructionPosition(ip_index as u16)) {
            string += &format!("------- {} --------\n", found.bright_blue());
        }

        string += &format!(
            "> {:04X}: {}\n",
            ip_index,
            disasm_color(instruction, comment)
        );
    }

    string
}

#[must_use]
pub fn disasm_instructions_no_color(
    binary_instruction: &[BinaryInstruction],
    descriptions: &[String],
    ip_infos: &SeqMap<InstructionPosition, String>,
) -> String {
    let mut string = String::new();

    for (ip_index, (instruction, comment)) in
        binary_instruction.iter().zip(descriptions).enumerate()
    {
        if let Some(found) = ip_infos.get(&InstructionPosition(ip_index as u16)) {
            string += &format!("- {} -\n", found);
        }

        string += &format!(
            "> {:04X}: {}\n",
            ip_index,
            disasm_no_color(instruction, comment)
        );
    }

    string
}

#[must_use]
pub fn disasm_color(binary_instruction: &BinaryInstruction, comment: &str) -> String {
    let decorated = disasm(binary_instruction);

    let name = format!("{}", decorated.name.blue());

    let mut converted_operands = Vec::new();
    let mut converted_comments = Vec::new();

    for operand in decorated.operands {
        let (new_str, comment_str) = match operand.kind {
            DecoratedOperandKind::ReadFrameAddress(addr, memory_kind) => (
                format!("{}{}", "$".green(), format!("{:04X}", addr.0).green()),
                memory_kind_color(&memory_kind),
            ),
            DecoratedOperandKind::WriteFrameAddress(addr, memory_kind) => (
                format!("{}{}", "$".red(), format!("{:04X}", addr.0).red()),
                memory_kind_color(&memory_kind),
            ),
            DecoratedOperandKind::Ip(ip) => (
                format!("{}{}", "@".cyan(), format!("{:X}", ip.0).bright_cyan()),
                String::new(),
            ),
            DecoratedOperandKind::MemorySize(data) => (
                format!("{}", format!("{data:X}",).bright_yellow()),
                format!("{}{}", "int:".white(), data.white()),
            ),
            DecoratedOperandKind::ImmediateU32(data) => (
                format!("{}", format!("{data:08X}",).bright_magenta()),
                format!("{}{}", "int:".white(), (data as i32).white()),
            ),
            DecoratedOperandKind::ImmediateU16(data) => (
                format!("{}", format!("{data:04X}",).bright_magenta()),
                format!("{}{}", "int:".white(), (data as i32).white()),
            ),
        };
        converted_operands.push(new_str);
        if !comment_str.is_empty() {
            converted_comments.push(comment_str);
        }
    }

    let comment_suffix = if converted_comments.is_empty() {
        String::new()
    } else {
        format!(" ({})", converted_comments.join(", "))
    };

    let total_comment = format!("{}{}", comment, comment_suffix);
    let print_comment = if total_comment.is_empty() {
        String::new()
    } else {
        format!(" {} {}", "#".white(), total_comment.white())
    };

    format!("{} {}{}", name, converted_operands.join(" "), print_comment)
}

#[must_use]
pub fn disasm_no_color(binary_instruction: &BinaryInstruction, comment: &str) -> String {
    let decorated = disasm(binary_instruction);

    let name = format!("{}", decorated.name);

    let mut converted_operands = Vec::new();

    for operand in decorated.operands {
        let new_str = match operand.kind {
            DecoratedOperandKind::ReadFrameAddress(addr, memory_kind) => {
                format!("{}{}", "$", format!("{:04X}", addr.0))
            }
            DecoratedOperandKind::WriteFrameAddress(addr, memory_kind) => {
                format!("{}{}", "$", format!("{:04X}", addr.0))
            }

            DecoratedOperandKind::MemorySize(data) => format!("{}", format!("{data:X}",)),

            DecoratedOperandKind::Ip(ip) => {
                format!("{}{}", "@", format!("{:X}", ip.0))
            }
            DecoratedOperandKind::ImmediateU32(data) => format!("{}", format!("{data:08X}",)),
            DecoratedOperandKind::ImmediateU16(data) => format!("{}", format!("{data:04X}",)),
        };
        converted_operands.push(new_str);
    }

    format!("{} {} # {}", name, converted_operands.join(" "), comment)
}

pub fn disasm(binary_instruction: &BinaryInstruction) -> DecoratedOpcode {
    let opcode: OpCode = binary_instruction.opcode.into();
    let operands = binary_instruction.operands;

    let operands_slice: &[DecoratedOperandKind] = match opcode {
        OpCode::Hlt => &[],
        OpCode::Ret => &[],
        OpCode::Ld => todo!(),
        OpCode::St => todo!(),
        OpCode::Ld32 => {
            let data = ((operands[2] as u32) << 16) | operands[1] as u32;

            &[
                to_write_frame(operands[0], DecoratedMemoryKind::S32),
                DecoratedOperandKind::ImmediateU32(data),
            ]
        }
        OpCode::Ld16 => {
            let data = operands[1] as u16;

            &[
                to_write_frame(operands[0], DecoratedMemoryKind::U16),
                DecoratedOperandKind::ImmediateU16(data),
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
        OpCode::Call => &[to_jmp_ip(operands[0])],
        OpCode::Enter => &[DecoratedOperandKind::MemorySize(operands[0])],
        OpCode::Jmp => &[to_jmp_ip(operands[0])],
        OpCode::Mov => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets),
            DecoratedOperandKind::MemorySize(operands[2]),
        ],
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
