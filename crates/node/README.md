# Swamp Node

A small Rust crate providing `Span` and `Node` structs for tracking source code locations, for use in compilers, linters, or language servers.

It uses compact representations:
*   `FileId`: `u16`
*   `offset`: `u32`
*   `length`: `u16`

## Overview

*   **`Span`**: Represents a contiguous region within a specific source file. It includes the file identifier (`file_id`), the starting byte offset (`offset`), and the length in bytes (`length`).
*   **`Node`**: A simple wrapper around a `Span`, often used as a base for Abstract Syntax Tree (AST) nodes or other elements that need associated source location information.

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
swamp-node = "0.1.10"
```

# License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

# Copyright

Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
