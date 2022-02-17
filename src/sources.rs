use std::collections::{LinkedList};
use std::env;
use std::path::{Path, PathBuf};
use pas_common::span::*;
use pas_syn::IdentPath;
use crate::{CompileError, Args};

fn find_in_path(filename: &PathBuf, dir: &Path) -> Option<PathBuf> {
    if !dir.exists() || !dir.is_dir() {
        return None;
    }

    let file_path = dir.join(filename);

    if file_path.exists() {
        // try to canonicalize the filename (not the rest of the path)
        let file_path_with_canon_name = file_path.canonicalize().ok()
            .and_then(|canon_path| {
                let canon_filename = canon_path.file_name()?;
                Some(file_path.with_file_name(canon_filename))
            })
            .unwrap_or(file_path);

        Some(file_path_with_canon_name)
    } else {
        None
    }
}

fn find_in_paths(filename: &PathBuf, search_paths: &[PathBuf]) -> Option<PathBuf> {
    for search_path in search_paths.iter() {
        if search_path.exists() && search_path.is_dir() {
            if let Some(result_path) = find_in_path(filename, search_path) {
                return Some(result_path);
            }
        }
    }

    None
}

pub struct SourceCollection {
    verbose: bool,

    source_dirs: Vec<PathBuf>,
    source_filenames: LinkedList<PathBuf>,
}

impl SourceCollection {
    pub fn new(args: &Args) -> Result<Self, CompileError> {
        let source_dirs = args.search_dirs.iter()
            .filter(|dir| dir.exists())
            .cloned()
            .chain({
                let mut unit_dirs = Vec::with_capacity(4);
                if let Ok(units_var) = env::var("PASCAL2_UNITS") {
                    unit_dirs.extend(units_var.split(";").map(PathBuf::from));
                }

                if let Ok(cwd) = env::current_dir() {
                    unit_dirs.push(cwd);
                }

                unit_dirs.into_iter()
                    .filter(|dir| dir.exists())
            })
            .collect();

        let mut sources = Self {
            verbose: args.verbose,

            source_dirs,
            source_filenames: LinkedList::new(),
        };

        // add system units
        sources.add(&PathBuf::from("System.pas"), None)?;

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

    fn add(&mut self, unit_filename: &PathBuf, span: Option<Span>) -> Result<(), CompileError> {
        match find_in_paths(unit_filename, &self.source_dirs) {
            Some(path) => {
                if self.source_filenames.contains(&path) {
                    return Ok(());
                }

                if self.verbose {
                    println!("added source path {}", path.display());
                }

                self.source_filenames.push_back(path);
                Ok(())
            }

            None => Err(CompileError::FileNotFound(unit_filename.clone(), span)),
        }
    }

    pub fn add_used_unit(&mut self, base_unit_path: &PathBuf, used_unit: &IdentPath) -> Result<(), CompileError> {
        let unit_filename = PathBuf::from(used_unit.to_string() + ".pas");

        if let Some(from_unit_dir) = base_unit_path.parent() {
            if let Some(used_path) = find_in_path(&unit_filename, from_unit_dir) {
                self.source_filenames.push_back(used_path);
                return Ok(());
            }
        }

        self.add(&unit_filename, Some(used_unit.span().clone()))
    }

    pub fn next(&mut self) -> Option<PathBuf> {
        self.source_filenames.pop_front()
    }
}
