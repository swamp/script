use std::fmt::{Display, Formatter};

#[repr(u8)]
pub enum OpCode {
    End = 0,
    LdLocal = 1,
    StoreLocal = 2,
    LdImmU32 = 3,
    AddI32 = 4,
    LtI32 = 5,
    JmpIf = 6,
    JmpIfNot = 7,
    Call = 8, // Introduce CallLong if needed
    Enter = 9,
    Ret = 10,
    Jmp = 11,
    Mov = 12,
    LdImmU16 = 13,
    LdImmU8 = 14,
    LtU16,
    LdIndirect,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::End => write!(f, "End"),
            Self::LdLocal => write!(f, "LdLocal"),
            Self::StoreLocal => write!(f, "StoreLocal"),
            Self::LdImmU32 => write!(f, "LdImmU32"),
            Self::AddI32 => write!(f, "AddI32"),
            Self::LtI32 => write!(f, "LtI32"),
            Self::JmpIf => write!(f, "JmpIf"),
            Self::JmpIfNot => write!(f, "JmpIfNot"),
            Self::Call => write!(f, "Call"),
            Self::Enter => write!(f, "Enter"),
            Self::Ret => write!(f, "Ret"),
            Self::Jmp => write!(f, "Jmp"),
            Self::Mov => write!(f, "Mov"),
            Self::LdImmU16 => write!(f, "LdImmU16"),
            Self::LdImmU8 => write!(f, "LdImmU8"),
            Self::LtU16 => write!(f, "LtU16"),
            Self::LdIndirect => write!(f, "LdIndirect"),
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
