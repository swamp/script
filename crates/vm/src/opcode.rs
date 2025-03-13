#[repr(u8)]
pub enum OpCode {
    End = 0,
    LdLocal = 1,
    StoreLocal = 2,
    LdImmI32 = 3,
    AddI32 = 4,
    LtI32 = 5,
    JmpIf = 6,
    JmpIfNot = 7,
    Call = 8, // Introduce CallLong if needed
    Enter = 9,
    Ret = 10,
    Jmp = 11,
    Mov = 12,
    LdImm = 13,
}
