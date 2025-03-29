# Swamp Source Map Lookup

Provides a convenient trait and implementation for querying source text and detailed location information (file, line, column) from a `swamp_source_map::SourceMap`.

## Overview

This crate simplifies the process of retrieving user-friendly source information when you have a `swamp_node::Node` or `swamp_node::Span` (typically obtained from parsing or semantic analysis phases in the Swamp compiler) and a populated `swamp_source_map::SourceMap`.

It's particularly useful for building tools like linters, language servers, or error reporting systems that need to display source snippets or precise locations to the user.

## Key Components

*   **`SourceMapLookup` Trait:** Defines the interface for querying source information.
    *   `get_text(&self, resolved_node: &Node) -> &str`: Gets the source text corresponding to a `Node`.
    *   `get_text_span(&self, span: &Span) -> &str`: Gets the source text corresponding to a `Span`.
    *   `get_line(&self, span: &Span) -> FileLineInfo`: Gets detailed location information for a `Span`.
*   **`SourceMapWrapper<'a>` Struct:** A concrete implementation of `SourceMapLookup` that wraps a reference to a `swamp_source_map::SourceMap` and requires the current working directory to calculate relative paths.
*   **`FileLineInfo` Struct:** Holds the results from `get_line`, including:
    *   `row`: 1-based line number.
    *   `col`: 1-based column number (UTF-8 character offset).
    *   `line`: The full text content of the line.
    *   `relative_file_name`: The file path relative to the provided current directory.

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
swamp-source-map-lookup = "0.1.10"
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Copyright

Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
