use seq_map::SeqMap;
use swamp_vm_types::opcode::OpCode;
use swamp_vm_types::{
    BinaryInstruction, FrameMemoryAddress, FrameMemorySize, InstructionPosition, MemoryAddress,
    MemoryOffset, MemorySize,
};
use yansi::{Color, Paint};

#[derive(Debug, Clone)]
pub struct FrameMemoryAttribute {
    pub is_temporary: bool,
}

#[derive(Debug, Clone)]
pub enum DecoratedOperandKind {
    ReadFrameAddress(
        FrameMemoryAddress,
        DecoratedMemoryKind,
        FrameMemoryAttribute,
    ),
    WriteFrameAddress(
        FrameMemoryAddress,
        DecoratedMemoryKind,
        FrameMemoryAttribute,
    ),
    Ip(InstructionPosition),
    ImmediateU32(u32),
    ImmediateU16(u16),
    MemorySize(MemorySize),
    ImmediateU8(u16),
    WriteIndirectMemory(MemoryAddress, MemoryOffset, DecoratedMemoryKind),
    ReadIndirectMemory(MemoryAddress, MemoryOffset, DecoratedMemoryKind),
    CountU16(u16),
    //PointerOffset(MemoryOffset),
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

    format!("{}", short_string)
}

#[must_use]
pub fn disasm_instructions_color(
    binary_instructions: &[BinaryInstruction],
    descriptions: &[String],
    ip_infos: &SeqMap<InstructionPosition, String>,
) -> String {
    let mut string = String::new();
    let mut last_frame_size: u16 = 0;

    for (ip_index, (instruction, comment)) in
        binary_instructions.iter().zip(descriptions).enumerate()
    {
        if OpCode::Enter as u8 == instruction.opcode {
            last_frame_size = instruction.operands[0];
        }
        if let Some(found) = ip_infos.get(&InstructionPosition(ip_index as u16)) {
            string += &format!("------- {} --------\n", found.bright_blue());
        }

        string += &format!(
            "> {:04X}: {}\n",
            ip_index,
            disasm_color(instruction, FrameMemorySize(last_frame_size), comment)
        );
    }

    string
}

#[must_use]
pub fn disasm_instructions_no_color(
    binary_instruction: &[BinaryInstruction],
    descriptions: &[String],
    ip_infos: &SeqMap<InstructionPosition, String>,
    include_comments: bool,
) -> String {
    let mut string = String::new();
    let mut last_frame_size: u16 = 0;

    for (ip_index, (instruction, comment)) in
        binary_instruction.iter().zip(descriptions).enumerate()
    {
        if OpCode::Enter as u8 == instruction.opcode {
            last_frame_size = instruction.operands[0];
        }
        if let Some(found) = ip_infos.get(&InstructionPosition(ip_index as u16)) {
            string += &format!("- {found} -\n");
        }

        let comment_to_use = if include_comments {
            comment.clone()
        } else {
            String::new()
        };

        string += &format!(
            "> {:04X}: {}\n",
            ip_index,
            disasm_no_color(
                instruction,
                FrameMemorySize(last_frame_size),
                &comment_to_use
            )
        );
    }

    string
}

#[must_use]
pub fn disasm_color(
    binary_instruction: &BinaryInstruction,
    frame_size: FrameMemorySize,
    comment: &str,
) -> String {
    let decorated = disasm(binary_instruction, frame_size);

    let name = format!("{:5}", decorated.name.blue());

    let mut converted_operands = Vec::new();
    let mut converted_comments = Vec::new();

    for operand in decorated.operands {
        let (new_str, comment_str) = match operand.kind {
            DecoratedOperandKind::ReadFrameAddress(addr, memory_kind, attr) => {
                let color = if attr.is_temporary {
                    Color::BrightGreen
                } else {
                    Color::Green
                };

                (
                    format!("{}{}", "$".fg(color), format!("{:04X}", addr.0).fg(color)),
                    memory_kind_color(&memory_kind),
                )
            }
            DecoratedOperandKind::WriteFrameAddress(addr, memory_kind, attr) => {
                let color = if attr.is_temporary {
                    Color::BrightMagenta
                } else {
                    Color::Red
                };

                (
                    format!("{}{}", "$".fg(color), format!("{:04X}", addr.0).fg(color)),
                    memory_kind_color(&memory_kind),
                )
            }
            DecoratedOperandKind::WriteIndirectMemory(addr, memory_offset, memory_kind) => (
                format!(
                    "({}{})",
                    "$".red(),
                    format!("{:04X}+{}", addr.0, memory_offset.0).red(),
                ),
                memory_kind_color(&memory_kind),
            ),
            DecoratedOperandKind::ReadIndirectMemory(addr, memory_offset, memory_kind) => (
                format!(
                    "({}{})",
                    "$".green(),
                    format!("{:04X}+{}", addr.0, memory_offset.0).green()
                ),
                memory_kind_color(&memory_kind),
            ),

            DecoratedOperandKind::Ip(ip) => (
                format!("{}{}", "@".cyan(), format!("{:X}", ip.0).bright_cyan()),
                String::new(),
            ),
            DecoratedOperandKind::MemorySize(data) => (
                format!("{}", format!("{:X}", data.0).yellow()),
                format!("{}{}", "int:", data.0),
            ),

            DecoratedOperandKind::ImmediateU32(data) => (
                format!("{}", format!("{data:08X}",).magenta()),
                format!("{}{}", "int:", data as i32),
            ),
            DecoratedOperandKind::ImmediateU16(data) => (
                format!("{}", format!("{data:04X}",).magenta()),
                format!("{}{}", "int:", data as i32),
            ),
            DecoratedOperandKind::ImmediateU8(data) => (
                format!("{}", format!("{data:02X}",).magenta()),
                format!("{}{}", "int:", data as i8),
            ),
            DecoratedOperandKind::CountU16(data) => (
                format!("{}", format!("{data:04X}",).yellow()),
                format!("{}", "count"),
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
        format!(" {} {}", ";".bright_black(), total_comment.bright_black())
    };

    format!("{} {}{}", name, converted_operands.join(" "), print_comment)
}

#[must_use]
pub fn disasm_no_color(
    binary_instruction: &BinaryInstruction,
    frame_memory_size: FrameMemorySize,
    comment: &str,
) -> String {
    let decorated = disasm(binary_instruction, frame_memory_size);

    let name = decorated.name.to_string();

    let mut converted_operands = Vec::new();

    for operand in decorated.operands {
        let new_str = match operand.kind {
            DecoratedOperandKind::ReadFrameAddress(addr, memory_kind, attr) => {
                format!("{}{}", "$", format!("{:04X}", addr.0))
            }
            DecoratedOperandKind::WriteFrameAddress(addr, memory_kind, attr) => {
                format!("{}{}", "$", format!("{:04X}", addr.0))
            }

            DecoratedOperandKind::ReadIndirectMemory(addr, memory_offset, memory_kind) => {
                format!("({}{})", "$", format!("{:04X}+{}", addr.0, memory_offset.0))
            }
            DecoratedOperandKind::WriteIndirectMemory(addr, memory_offset, memory_kind) => {
                format!("({}{})", "$", format!("{:04X}+{}", addr.0, memory_offset.0))
            }
            DecoratedOperandKind::MemorySize(data) => format!("{}", format!("{:X}", data.0)),

            DecoratedOperandKind::Ip(ip) => {
                format!("{}{}", "@", format!("{:X}", ip.0))
            }
            DecoratedOperandKind::ImmediateU32(data) => format!("{}", format!("{data:08X}",)),
            DecoratedOperandKind::ImmediateU16(data) => format!("{}", format!("{data:04X}",)),
            DecoratedOperandKind::ImmediateU8(data) => format!("{}", format!("{data:02X}",)),
            DecoratedOperandKind::CountU16(data) => format!("{}", format!("{data:04X}",)),
        };
        converted_operands.push(new_str);
    }

    let comment_suffix = if comment.is_empty() {
        String::new()
    } else {
        format!(" ; {comment}")
    };

    format!(
        "{} {}{}",
        name,
        converted_operands.join(" "),
        comment_suffix
    )
}

#[allow(clippy::too_many_lines)]
#[must_use]
pub fn disasm(
    binary_instruction: &BinaryInstruction,
    frame_memory_size: FrameMemorySize,
) -> DecoratedOpcode {
    let opcode: OpCode = binary_instruction.opcode.into();
    let operands = binary_instruction.operands;

    let operands_slice: &[DecoratedOperandKind] = match opcode {
        OpCode::Hlt => &[],
        OpCode::Ret => &[],
        OpCode::Ld32 => {
            let data = ((operands[2] as u32) << 16) | operands[1] as u32;

            &[
                to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
                DecoratedOperandKind::ImmediateU32(data),
            ]
        }
        OpCode::Ld16 => {
            let data = operands[1] as u16;

            &[
                to_write_frame(operands[0], DecoratedMemoryKind::U16, frame_memory_size),
                DecoratedOperandKind::ImmediateU16(data),
            ]
        }

        OpCode::Ld8 => {
            let data = operands[1];

            &[
                to_write_frame(operands[0], DecoratedMemoryKind::U8, frame_memory_size),
                DecoratedOperandKind::ImmediateU8(data),
            ]
        }

        OpCode::AddI32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::MulI32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[2], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::NegI32 => &[
            to_write_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
        ],

        OpCode::LtI32 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::GtI32 => &[
            to_read_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::S32, frame_memory_size),
        ],
        OpCode::Eq8Imm => {
            let data = operands[1];

            &[
                to_read_frame(operands[0], DecoratedMemoryKind::S32, frame_memory_size),
                DecoratedOperandKind::ImmediateU8(data),
            ]
        }
        OpCode::Tst8 => &[to_read_frame(
            operands[0],
            DecoratedMemoryKind::S32,
            frame_memory_size,
        )],

        OpCode::Bnz => &[to_jmp_ip(operands[0])],
        OpCode::Bz => &[to_jmp_ip(operands[0])],
        OpCode::Call => &[to_jmp_ip(operands[0])],
        OpCode::HostCall => &[
            DecoratedOperandKind::ImmediateU16(operands[0]),
            DecoratedOperandKind::MemorySize(MemorySize(operands[1])),
        ],
        OpCode::Enter => &[DecoratedOperandKind::MemorySize(MemorySize(operands[0]))],
        OpCode::Jmp => &[to_jmp_ip(operands[0])],
        OpCode::Mov => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandKind::MemorySize(MemorySize(operands[2])),
        ],
        OpCode::MovLp => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandKind::MemorySize(MemorySize(operands[2])),
        ],
        OpCode::Nop => &[],

        OpCode::VecPush => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
        ],
        OpCode::MapNewFromPairs => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandKind::MemorySize(MemorySize(operands[2])),
            DecoratedOperandKind::MemorySize(MemorySize(operands[3])),
            DecoratedOperandKind::CountU16(operands[4]),
        ],

        OpCode::MapRemove => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            to_read_frame(operands[1], DecoratedMemoryKind::Octets, frame_memory_size),
        ],
        /*
                OpCode::Alloc => &[
            to_write_frame(operands[0], DecoratedMemoryKind::Octets, frame_memory_size),
            DecoratedOperandKind::MemorySize(MemorySize(operands[1])),
        ],
        OpCode::LtU16 => todo!(),
        OpCode::St32x => {
            let data = ((operands[3] as u32) << 16) | operands[2] as u32;
            &[
                DecoratedOperandKind::WriteIndirectMemory(
                    MemoryAddress(operands[0]),
                    MemoryOffset(operands[1]),
                    DecoratedMemoryKind::U32,
                ),
                DecoratedOperandKind::ImmediateU32(data),
            ]
        }
        OpCode::Stx => &[
            DecoratedOperandKind::WriteIndirectMemory(
                MemoryAddress(operands[0]),
                MemoryOffset(operands[1]),
                DecoratedMemoryKind::Octets,
            ),
            DecoratedOperandKind::ReadFrameAddress(
                FrameMemoryAddress(operands[2]),
                DecoratedMemoryKind::Octets,
                FrameMemoryAttribute {
                    is_temporary: operands[2] >= frame_memory_size.0,
                },
            ),
            DecoratedOperandKind::MemorySize(MemorySize(operands[3])),
        ],
        OpCode::Ldx => &[
            DecoratedOperandKind::ReadIndirectMemory(
                MemoryAddress(operands[0]),
                MemoryOffset(operands[1]),
                DecoratedMemoryKind::Octets,
            ),
            DecoratedOperandKind::ReadFrameAddress(
                FrameMemoryAddress(operands[2]),
                DecoratedMemoryKind::Octets,
                FrameMemoryAttribute {
                    is_temporary: operands[2] >= frame_memory_size.0,
                },
            ),
            DecoratedOperandKind::MemorySize(MemorySize(operands[3])),
        ],

         */
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

fn to_write_frame(
    addr: u16,
    mem: DecoratedMemoryKind,
    frame_memory_size: FrameMemorySize,
) -> DecoratedOperandKind {
    let is_temporary = addr >= frame_memory_size.0;
    DecoratedOperandKind::WriteFrameAddress(
        to_frame(addr),
        mem,
        FrameMemoryAttribute { is_temporary },
    )
}

fn to_read_frame(
    addr: u16,
    mem: DecoratedMemoryKind,
    frame_memory_size: FrameMemorySize,
) -> DecoratedOperandKind {
    let is_temporary = addr >= frame_memory_size.0;
    DecoratedOperandKind::ReadFrameAddress(
        to_frame(addr),
        mem,
        FrameMemoryAttribute { is_temporary },
    )
}

fn to_ip(ip: u16) -> DecoratedOperandKind {
    DecoratedOperandKind::Ip(InstructionPosition(ip))
}

fn to_jmp_ip(ip: u16) -> DecoratedOperandKind {
    DecoratedOperandKind::Ip(InstructionPosition(ip + 1))
}

fn to_frame(val: u16) -> FrameMemoryAddress {
    FrameMemoryAddress(val)
}
