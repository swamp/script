pub struct HostArgs {
    current_index: usize, // Current argument being processed
    args: Vec<u8>,
}

impl HostArgs {
    pub fn new(args: &[u8]) -> Self {
        Self {
            args: args.to_vec(),
            current_index: 0,
        }
    }

    /// Reads the next 4 bytes from `args`, interpreting them as a little-endian i32.
    /// Advances the current index by 4.
    ///
    /// # Panics
    ///
    /// Panics if there are not enough bytes remaining.
    pub fn get_i32(&mut self) -> i32 {
        // Ensure there are enough bytes remaining
        assert!(
            self.current_index + 4 <= self.args.len(),
            "Not enough bytes to read an i32"
        );

        // Get the next four bytes
        let bytes = &self.args[self.current_index..self.current_index + 4];

        // Advance the current index
        self.current_index += 4;

        // Convert the bytes to an i32 (assuming little-endian encoding)
        i32::from_le_bytes(bytes.try_into().expect("Slice with incorrect length"))
    }
}

pub type HostFunctionCallback = Box<dyn FnMut(HostArgs)>;
