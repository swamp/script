use std::fmt::{Display, Formatter};

#[repr(u8)]
pub enum OpCode {
    Hlt, // Return to the host

    // Operators
    AddI32,

    // Comparisons
    LtU16,
    LtI32,

    // Conditional branching
    Bnz,
    Bz,
    // Unconditional branching
    Jmp,

    // Call, frame and return
    Call, // Introduce CallLong if needed
    Enter,
    Ret,

    // Frame copy
    Mov,

    // Load immediate into frame
    Ld8,
    Ld16,
    Ld32,

    // Indirect operations (using pointer)
    St32x,
    Stx,
    Ldx,

    // Allocate heap
    Alloc,
    VecPush,
    Nop,

    // Intrinsic more advanced opcodes

    // Collection intrinsics
    MapNewFromPairs,
    MapRemove,
    Eq8Imm,
    Tst8,
    GtI32,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Hlt => write!(f, "hlt"), // Halt execution

            Self::Ld8 => write!(f, "ld8"),
            Self::Ld16 => write!(f, "ld16"),
            Self::Ld32 => write!(f, "ld32"),

            Self::AddI32 => write!(f, "sadd32"), // Signed Add
            Self::Bz => write!(f, "bz"),         // Branch if Zero (False)
            Self::Bnz => write!(f, "bnz"),       // Branch if Not Zero (True)
            Self::Call => write!(f, "call"),     // Call function
            Self::Enter => write!(f, "enter"),   // Function prologue
            Self::Ret => write!(f, "ret"),       // Return from function
            Self::Jmp => write!(f, "jmp"),       // Unconditional jump
            Self::Mov => write!(f, "mov"),       // Move data

            Self::LtU16 => write!(f, "lt16"), // Set Less Than Unsigned
            Self::LtI32 => write!(f, "slt32"), // signed Less Than
            Self::GtI32 => write!(f, "sgt32"), // Set Less Than
            Self::Eq8Imm => write!(f, "eq8"),
            Self::Tst8 => write!(f, "tst8"),

            Self::Ldx => write!(f, "ldx"),     // Load Indexed/Indirect
            Self::Stx => write!(f, "stx"),     // Store Indexed/Indirect
            Self::St32x => write!(f, "st32x"), // Store Indexed/Indirect
            Self::Alloc => write!(f, "alloc"),
            Self::VecPush => write!(f, "vec_push"),
            Self::Nop => write!(f, "nop"),

            Self::MapNewFromPairs => write!(f, "map_new_from_pairs"),
            Self::MapRemove => write!(f, "map_remove"),
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
