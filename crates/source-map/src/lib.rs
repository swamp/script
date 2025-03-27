/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use pathdiff::diff_paths;
use seq_map::SeqMap;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::{fs, io};
use tracing::trace;

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
    pub file_cache: SeqMap<(String, String), FileId>,
    pub next_file_id: FileId,
}

#[derive(Debug)]
pub struct RelativePath(pub String);

impl SourceMap {
    /// # Errors
    ///
    pub fn new(mounts: &SeqMap<String, PathBuf>) -> io::Result<Self> {
        let mut canonical_mounts = SeqMap::new();
        for (mount_name, base_path) in mounts {
            let canon_path = base_path.canonicalize().map_err(|_| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("could not canonicalize {base_path:?}"),
                )
            })?;

            if !canon_path.is_dir() {
                return Err(io::Error::new(
                    ErrorKind::NotFound,
                    format!("{canon_path:?} is not a directory"),
                ));
            }
            canonical_mounts
                .insert(mount_name.clone(), canon_path)
                .map_err(|_| {
                    io::Error::new(io::ErrorKind::InvalidData, "could not insert mount")
                })?;
        }
        Ok(Self {
            mounts: canonical_mounts,
            cache: SeqMap::new(),
            file_cache: SeqMap::new(),
            next_file_id: 1,
        })
    }

    /// # Errors
    ///
    pub fn add_mount(&mut self, name: &str, path: &Path) -> io::Result<()> {
        if !path.is_dir() {
            return Err(io::Error::new(
                ErrorKind::NotFound,
                format!("{path:?} is not a directory"),
            ));
        }
        self.mounts
            .insert(name.to_string(), path.to_path_buf())
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "could not insert mount"))
    }

    #[must_use]
    pub fn base_path(&self, name: &str) -> &Path {
        self.mounts.get(&name.to_string()).map_or_else(
            || {
                panic!("could not find path {name}");
            },
            |found| found,
        )
    }

    pub fn read_file(&mut self, path: &Path, mount_name: &str) -> io::Result<(FileId, String)> {
        let found_base_path = self.base_path(mount_name);
        let relative_path = diff_paths(path, found_base_path)
            .unwrap_or_else(|| panic!("could not find relative path {path:?} {found_base_path:?}"));

        let contents = fs::read_to_string(path)?;

        let id = self.next_file_id;
        self.next_file_id += 1;

        self.add_manual(id, mount_name, &relative_path, &contents);

        Ok((id, contents))
    }

    pub fn add_to_cache(
        &mut self,
        mount_name: &str,
        relative_path: &Path,
        contents: &str,
        file_id: FileId,
    ) {
        self.add_manual(file_id, mount_name, relative_path, contents);
        self.file_cache
            .insert(
                (
                    mount_name.to_string(),
                    relative_path.to_str().unwrap().to_string(),
                ),
                file_id,
            )
            .unwrap();
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
        if let Some(found_in_cache) = self
            .file_cache
            .get(&(mount_name.to_string(), relative_path.to_string()))
        {
            let contents = self.cache.get(found_in_cache).unwrap().contents.clone();
            return Ok((found_in_cache.clone(), contents));
        }

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
        let mut path_buf = base_path;

        path_buf.push(relative_path);

        path_buf.canonicalize().map_err(|_| {
            io::Error::new(
                ErrorKind::Other,
                format!("path is wrong mount:{mount_name} relative:{relative_path}",),
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

    #[must_use]
    pub fn get_span_source(&self, file_id: FileId, offset: usize, length: usize) -> &str {
        self.cache.get(&file_id).map_or_else(
            || {
                "ERROR"
                //panic!("{}", &format!("Invalid file_id {file_id} in span"));
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

    #[must_use]
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

    #[must_use]
    pub fn fetch_relative_filename(&self, file_id: FileId) -> &str {
        self.cache
            .get(&file_id)
            .unwrap()
            .relative_path
            .to_str()
            .unwrap()
    }
    pub fn minimal_relative_path(target: &Path, current_dir: &Path) -> io::Result<PathBuf> {
        //let target = target.canonicalize()?;

        let current_dir_components = current_dir.components().collect::<Vec<_>>();
        let target_components = target.components().collect::<Vec<_>>();

        let mut common_prefix_len = 0;
        for i in 0..std::cmp::min(current_dir_components.len(), target_components.len()) {
            if current_dir_components[i] == target_components[i] {
                common_prefix_len += 1;
            } else {
                break;
            }
        }

        let mut relative_path = PathBuf::new();

        for _ in 0..(current_dir_components.len() - common_prefix_len) {
            relative_path.push("..");
        }

        for component in &target_components[common_prefix_len..] {
            relative_path.push(component);
        }
        Ok(relative_path)
    }
    pub fn get_relative_path_to(&self, file_id: FileId, current_dir: &Path) -> io::Result<PathBuf> {
        let file_info = self.cache.get(&file_id).unwrap();
        let mount_path = self.mounts.get(&file_info.mount_name).unwrap();

        let absolute_path = mount_path.join(&file_info.relative_path);

        Self::minimal_relative_path(&absolute_path, current_dir)
    }
}
