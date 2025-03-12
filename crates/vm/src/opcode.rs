#[repr(u8)]
pub enum OpCode {
    LdLocal = 0,
    StoreLocal = 1,
    LdImmI32 = 2,
    AddI32 = 3,
    End = 4,
}
