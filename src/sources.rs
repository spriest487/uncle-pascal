use std::collections::LinkedList;
use std::env;
use std::path::PathBuf;
use pas_common::span::*;
use crate::{CompileError, Args};

fn find_in_paths(filename: &PathBuf, search_paths: &[PathBuf]) -> Option<PathBuf> {
    for search_path in search_paths.iter() {
        if search_path.exists() && search_path.is_dir() {
            let file_path = search_path.join(filename);

            if file_path.exists() {
                // try to canonicalize the filename (not the rest of the path)
                let file_path_with_canon_name = file_path.canonicalize().ok()
                    .and_then(|canon_path| {
                        let canon_filename = canon_path.file_name()?;
                        Some(file_path.with_file_name(canon_filename))
                    })
                    .unwrap_or(file_path);

                return Some(file_path_with_canon_name);
            }
        }
    }

    None
}

pub struct SourceCollection {
    source_dirs: Vec<PathBuf>,
    source_filenames: LinkedList<PathBuf>,
}

impl SourceCollection {
    pub fn new(args: &Args) -> Result<Self, CompileError> {
        let source_dirs = args.search_dirs.iter()
            .filter(|dir| dir.exists())
            .cloned()
            .chain({
                let cwd = env::current_dir().ok();
                let units_dir = env::var("PASCAL2_UNITS").ok().map(PathBuf::from);

                [cwd, units_dir]
                    .iter()
                    .filter_map(|dir| dir.as_ref())
                    .filter(|dir| dir.exists())
                    .cloned()
            })
            .collect();

        let mut sources = Self {
            source_dirs,
            source_filenames: LinkedList::new(),
        };

        // add system units
        if !args.no_stdlib {
            sources.add(&PathBuf::from("System.pas"), None)?;
        }

        // add extra referenced units
        for unit_arg in &args.units {
            let unit_arg_filename = PathBuf::from(unit_arg.clone());
            sources.add(&unit_arg_filename, None)?;
        }

        // add main source unit
        sources.add(&args.file, None)?;

        Ok(sources)
    }

    pub fn source_dirs(&self) -> &[PathBuf] {
        &self.source_dirs
    }

    pub fn add(&mut self, unit_filename: &PathBuf, span: Option<Span>) -> Result<(), CompileError> {
        match find_in_paths(unit_filename, &self.source_dirs) {
            Some(path) => {
                self.source_filenames.push_back(path);
                Ok(())
            }

            None => Err(CompileError::FileNotFound(unit_filename.clone(), span)),
        }
    }

    pub fn next(&mut self) -> Option<PathBuf> {
        self.source_filenames.pop_front()
    }
}
