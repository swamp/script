use pathdiff::diff_paths;
use seq_map::SeqMap;
use std::path::{Path, PathBuf};
use std::{fs, io};
use tracing::info;

pub type FileId = u16;

pub struct FileInfo {
    pub relative_path: PathBuf,
    pub contents: String,
    pub line_offsets: Box<[u16]>,
}

pub struct SourceMap {
    pub base_path: PathBuf,
    pub cache: SeqMap<FileId, FileInfo>,
    pub next_file_id: FileId,
}

#[derive(Debug)]
pub struct RelativePath(pub String);

impl SourceMap {
    pub fn new(base_path: &Path) -> Self {
        let canon_path = base_path.canonicalize().expect("can not canonicalize");
        Self {
            base_path: canon_path,
            cache: SeqMap::new(),
            next_file_id: 1,
        }
    }

    pub fn add(&mut self, path: &Path) -> io::Result<FileId> {
        let relative_path =
            diff_paths(path, &self.base_path).expect("could not find relative path");

        info!(?relative_path, "add relative");
        let contents = fs::read_to_string(path)?;

        let line_offsets = Self::compute_line_offsets(&contents);
        info!(?line_offsets, "scanned");
        let id = self.next_file_id;
        self.next_file_id += 1;

        self.cache
            .insert(
                id,
                FileInfo {
                    relative_path,
                    contents,
                    line_offsets,
                },
            )
            .expect("could not add file info");

        Ok(id)
    }

    pub fn add_relative(&mut self, relative_path: &str) -> io::Result<FileId> {
        let buf = self.to_file_system_path(relative_path);
        info!(complete_path=?buf, "complete path");
        self.add(&buf)
    }

    /*

    fn to_relative_path(path: &ModulePath) -> RelativePath {
        RelativePath(
            path.0
                .iter()
                .map(|local_type_identifier| local_type_identifier.as_str())
                .collect::<Vec<_>>()
                .join("/"),
        )
    }

     */

    fn to_file_system_path(&self, path: &str) -> PathBuf {
        info!("converting from {path:?}");
        let mut path_buf = self.base_path.clone();

        path_buf.push(path);
        path_buf.set_extension("swamp");

        let canon_path = path_buf.canonicalize().expect("can not canonicalize");

        info!("converted to {canon_path:?}");
        canon_path
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

    pub fn get_span_source(&self, file_id: FileId, offset: usize, length: usize) -> &str {
        let file_info = self.cache.get(&file_id).expect("Invalid file_id in span");

        let start = offset as usize;
        let end = start + length as usize;
        &file_info.contents[start..end]
    }

    pub fn get_span_location_utf8(&self, file_id: FileId, offset: usize) -> (usize, usize) {
        let file_info = self.cache.get(&file_id).expect("Invalid file_id in span");

        let offset = offset as u16;

        // Find the line containing 'offset' via binary search.
        let line_idx = file_info
            .line_offsets
            .binary_search(&offset)
            .unwrap_or_else(|insert_point| insert_point.saturating_sub(1));

        // Determine the start of the line in bytes
        let line_start = file_info.line_offsets[line_idx] as usize;
        let octet_offset = offset as usize;

        // Extract the line slice from line_start to offset
        let line_text = &file_info.contents[line_start..octet_offset];

        // Count UTF-8 characters in that range, because that is what the end user sees in their editor.
        let column_character_offset = line_text.chars().count();

        // Add one so it makes more sense to the end user
        (line_idx + 1, column_character_offset + 1)
    }
}
