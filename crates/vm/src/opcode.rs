use std::fmt::{Display, Formatter};

#[repr(u8)]
pub enum OpCode {
    Hlt = 0,
    Ld = 1,
    St = 2,
    Ld32 = 3,
    AddI32 = 4,
    LtI32 = 5,
    JmpIf = 6,
    JmpIfNot = 7,
    Call = 8, // Introduce CallLong if needed
    Enter = 9,
    Ret = 10,
    Jmp = 11,
    Mov = 12,
    Ld16 = 13,
    Ld8 = 14,
    LtU16,
    LdIndirect,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Hlt => write!(f, "hlt"), // Halt execution
            Self::Ld => write!(f, "ld"),   // Load
            Self::St => write!(f, "st"),   // Store

            Self::Ld8 => write!(f, "ld8"),
            Self::Ld16 => write!(f, "ld16"),
            Self::Ld32 => write!(f, "ld32"),

            Self::AddI32 => write!(f, "sadd32"), // Signed Add
            Self::JmpIfNot => write!(f, "bz"),   // Branch if Zero (False)
            Self::JmpIf => write!(f, "bnz"),     // Branch if Not Zero (True)
            Self::Call => write!(f, "call"),     // Call function
            Self::Enter => write!(f, "enter"),   // Function prologue
            Self::Ret => write!(f, "ret"),       // Return from function
            Self::Jmp => write!(f, "jmp"),       // Unconditional jump
            Self::Mov => write!(f, "mov"),       // Move data

            Self::LtU16 => write!(f, "lt16"), // Set Less Than Unsigned
            Self::LtI32 => write!(f, "slt32"), // Set Less Than

            Self::LdIndirect => write!(f, "ldx"), // Load Indexed/Indirect
        }
    }
}

// Add this to your OpCode implementation
impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        // Safety: This assumes the u8 value corresponds to a valid OpCode variant
        // For production code, consider using TryFrom instead to validate the value
        unsafe { std::mem::transmute(value) }
    }
}
