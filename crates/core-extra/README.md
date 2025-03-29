# Swamp Core Extra

Provides the Rust runtime implementations for core collection types and operations defined in the core of the [Swamp programming language](https://github.com/swamp/swamp).

## Overview

This crate contains the underlying Rust functions that power Swamp's built-in data structures like `Vec`, `Map`, `Map2`, `Grid`, etc. When you use functions like `Vec<T>`,in Swamp code, the actual logic executed at runtime often comes from this crate.

It acts as a bridge between the Swamp language definition (types defined in `swamp-types`, semantic representation in `swamp-semantic`) and the host Rust environment where the Swamp code is executed.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Copyright

Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
