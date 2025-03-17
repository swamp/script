pub fn align(addr: usize, alignment: usize) -> usize {
    assert!(
        alignment.is_power_of_two(),
        "alignment must be a power of two"
    );
    (addr + alignment - 1) & !(alignment - 1)
}
