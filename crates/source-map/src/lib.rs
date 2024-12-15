use seq_map::SeqMap;
use std::path::{Path, PathBuf};
use std::{fs, io};
use swamp_script_node::Span;
use tracing::info;

type FileId = u16;

pub struct FileInfo {
    pub path: PathBuf,
    pub contents: String,
    pub line_offsets: Box<[u16]>,
}

pub struct SourceMap {
    pub cache: SeqMap<FileId, FileInfo>,
    pub id: FileId,
}

impl SourceMap {
    pub fn add(&mut self, path: &Path) -> io::Result<FileId> {
        let contents = fs::read_to_string(path)?;

        let line_offsets = Self::compute_line_offsets(&contents);
        let id = self.id;
        self.id += 1;

        self.cache
            .insert(
                id,
                FileInfo {
                    path: path.to_path_buf(),
                    contents,
                    line_offsets,
                },
            )
            .expect("could not add file info");

        /*
          let path_buf = self.to_file_system_path(to_relative_path(module_path));
        let contents = fs::read_to_string(path_buf)?;

         */

        Ok(id)
    }

    fn to_relative_path(path: &ModulePath) -> RelativePath {
        RelativePath(
            path.0
                .iter()
                .map(|local_type_identifier| local_type_identifier.as_str())
                .collect::<Vec<_>>()
                .join("/"),
        )
    }

    fn to_file_system_path(&self, path: RelativePath) -> PathBuf {
        info!("converting from {path:?}");
        let mut path_buf = self.base_path.to_path_buf();

        path_buf.push(path.0);
        path_buf.set_extension("swamp");

        info!("converted to {path_buf:?}");
        path_buf
    }

    fn compute_line_offsets(contents: &str) -> Box<[u16]> {
        let mut offsets = Vec::new();
        offsets.push(0);
        for (i, &byte) in contents.as_bytes().iter().enumerate() {
            if byte == b'\n' {
                // Safety: new line is always encoded as single octet
                let next_line_start = u16::try_from(i + 1).expect("too big file");
                offsets.push(next_line_start);
            }
        }
        offsets.into_boxed_slice()
    }

    pub fn get_span_source(&self, span: &Span) -> &str {
        let file_info = self
            .cache
            .get(&span.file_id)
            .expect("Invalid file_id in span");

        let start = span.offset as usize;
        let end = start + span.length as usize;
        &file_info.contents[start..end]
    }

    pub fn get_span_location_utf8(&self, span: &Span) -> (usize, usize) {
        let file_info = self
            .cache
            .get(&span.file_id)
            .expect("Invalid file_id in span");

        let offset = span.offset as u16;

        // Find the line containing 'offset' via binary search.
        let line_idx = match file_info.line_offsets.binary_search(&offset) {
            Ok(line_start_idx) => line_start_idx,
            Err(insert_point) => insert_point.saturating_sub(1),
        };

        // Convert zero-based line index to one-based for user reporting
        let line_number = line_idx + 1;

        // Determine the start of the line in bytes
        let line_start = file_info.line_offsets[line_idx] as usize;
        let byte_offset = offset as usize;

        // Extract the line slice from line_start to offset
        // This substring contains all the characters from the start of the line up to the span offset.
        let line_text = &file_info.contents[line_start..byte_offset];

        // Count UTF-8 characters in that range
        let column_number = line_text.chars().count() + 1;

        (line_number, column_number)
    }
}
