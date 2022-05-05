#[cfg(test)]
mod test;

use std::{path::PathBuf, rc::Rc, vec};
use crate::{Location, Span};

pub struct SourceMapBuilder {
    source_map: SourceMap,
}

impl SourceMapBuilder {
    pub fn new(src_filename: Rc<PathBuf>) -> Self {
        Self {
            source_map: SourceMap {
                entries: vec![
                    SourceMapEntry {
                        start: Location::zero(),
                        end: Location::zero(),
                        src: Span {
                            file: src_filename,
                            start: Location::zero(),
                            end: Location::zero(),
                        }
                    }
                ]
            }
        }
    }

    pub fn add(&mut self, start: Location, end: Location, src: Span) {
        self.source_map.entries.push(SourceMapEntry {
            start,
            end,
            src,
        })
    }

    pub fn build(mut self) -> SourceMap {
        self.source_map.entries.sort_by_key(|e| e.start.line);

        self.source_map
    }
}

#[derive(Debug, Clone)]
pub struct SourceMap {
    entries: Vec<SourceMapEntry>,
}

impl SourceMap {
    pub fn lookup(&self, start: Location, end: Location) -> Span {
        let start_line_index = match self.entries.binary_search_by_key(&start.line, |e| e.start.line) {
            Ok(index) => index,
            Err(insert_index) => insert_index,
        };

        let mut closest_entry = self.entries[start_line_index].clone();

        for entry in self.entries.iter().skip(start_line_index) {
            if entry.src.file != closest_entry.src.file {
                continue;
            }

            if entry.start <= start && entry.end >= end {
                closest_entry = entry.clone();
            }
        }

        let mut src = closest_entry.src;
        if start.col > closest_entry.start.col {
            src.start.col += start.col - closest_entry.start.col;
        }
        if end.col < closest_entry.end.col {
            src.end.col -= closest_entry.end.col - end.col;
        }

        if src.file.to_string_lossy().contains("Empty.pas") {
            eprintln!("here: start={start}, end={end}, src={src}");
        }

        src
    }
}

impl IntoIterator for SourceMap {
    type Item = SourceMapEntry;
    type IntoIter = vec::IntoIter<SourceMapEntry>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.into_iter()
    }
}

#[derive(Debug, Clone)]
pub struct SourceMapEntry {
    // range in the processed text
    pub start: Location,
    pub end: Location,

    // span in the original source file
    pub src: Span,
}