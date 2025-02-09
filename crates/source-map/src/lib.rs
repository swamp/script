/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use pathdiff::diff_paths;
use seq_map::SeqMap;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::{fs, io};

pub mod prelude;

pub type FileId = u16;

#[derive(Debug)]
pub struct FileInfo {
    pub mount_name: String,
    pub relative_path: PathBuf,
    pub contents: String,
    pub line_offsets: Box<[u16]>,
}

#[derive(Debug)]
pub struct SourceMap {
    pub mounts: SeqMap<String, PathBuf>,
    pub cache: SeqMap<FileId, FileInfo>,
    pub next_file_id: FileId,
}

#[derive(Debug)]
pub struct RelativePath(pub String);

impl SourceMap {
    pub fn new(mounts: &SeqMap<String, PathBuf>) -> Self {
        let mut canonical_mounts = SeqMap::new();
        for (mount_name, base_path) in mounts {
            let canon_path = base_path
                .canonicalize()
                .unwrap_or_else(|_| panic!("can not canonicalize {base_path:?}"));
            canonical_mounts
                .insert(mount_name.clone(), canon_path)
                .unwrap();
        }
        Self {
            mounts: canonical_mounts,
            cache: SeqMap::new(),
            next_file_id: 1,
        }
    }

    pub fn base_path(&self, name: &str) -> &Path {
        &self.mounts.get(&name.to_string()).unwrap()
    }

    pub fn read_file(&mut self, path: &Path, mount_name: &str) -> io::Result<(FileId, String)> {
        let found_base_path = self.base_path(mount_name);
        let relative_path = diff_paths(path, &found_base_path).expect(&format!(
            "could not find relative path {:?} {:?}",
            path, found_base_path
        ));

        let contents = fs::read_to_string(path)?;

        let id = self.next_file_id;
        self.next_file_id += 1;

        self.add_manual(id, mount_name, &relative_path, &contents);

        Ok((id, contents))
    }

    pub fn add_manual(
        &mut self,
        id: FileId,
        mount_name: &str,
        relative_path: &Path,
        contents: &str,
    ) {
        let line_offsets = Self::compute_line_offsets(contents);

        self.cache
            .insert(
                id,
                FileInfo {
                    mount_name: mount_name.to_string(),
                    relative_path: relative_path.to_path_buf(),
                    contents: contents.to_string(),
                    line_offsets,
                },
            )
            .expect("could not add file info");
    }

    pub fn add_manual_no_id(
        &mut self,
        mount_name: &str,
        relative_path: &Path,
        contents: &str,
    ) -> FileId {
        let line_offsets = Self::compute_line_offsets(contents);
        let id = self.next_file_id;
        self.next_file_id += 1;

        self.cache
            .insert(
                id,
                FileInfo {
                    mount_name: mount_name.to_string(),
                    relative_path: relative_path.to_path_buf(),
                    contents: contents.to_string(),
                    line_offsets,
                },
            )
            .expect("could not add file info");
        id
    }

    pub fn read_file_relative(
        &mut self,
        mount_name: &str,
        relative_path: &str,
    ) -> io::Result<(FileId, String)> {
        let buf = self.to_file_system_path(mount_name, relative_path)?;
        self.read_file(&buf, mount_name)
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

    fn to_file_system_path(&self, mount_name: &str, relative_path: &str) -> io::Result<PathBuf> {
        let base_path = self.base_path(mount_name).to_path_buf();
        let mut path_buf = base_path.clone();

        path_buf.push(relative_path);
        //path_buf.set_extension("swamp");

        path_buf.canonicalize().map_err(|_| {
            io::Error::new(
                ErrorKind::Other,
                format!(
                    "path is wrong mount:{} relative:{}",
                    mount_name, relative_path
                ),
            )
        })
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
        self.cache.get(&file_id).map_or_else(
            || {
                panic!("{}", &format!("Invalid file_id {file_id} in span"));
            },
            |file_info| {
                let start = offset;
                let end = start + length;
                &file_info.contents[start..end]
            },
        )
    }

    #[must_use]
    pub fn get_source_line(&self, file_id: FileId, line_number: usize) -> Option<&str> {
        let file_info = self.cache.get(&file_id)?;

        let start_offset = file_info.line_offsets[line_number - 1] as usize;
        let end_offset = file_info.line_offsets[line_number] as usize;
        Some(&file_info.contents[start_offset..end_offset - 1])
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

    pub fn fetch_relative_filename(&self, file_id: FileId) -> &str {
        self.cache
            .get(&file_id)
            .unwrap()
            .relative_path
            .to_str()
            .unwrap()
    }
}
