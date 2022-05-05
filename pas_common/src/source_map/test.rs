use std::path::PathBuf;
use crate::source_map::*;
use crate::{Location, Span};
use std::rc::Rc;

fn build_with_mappings(
    filename: &'static str,
    entries: impl IntoIterator<Item=((usize, usize), (usize, usize), &'static str, (usize, usize), (usize, usize))>
) -> SourceMap {
    let mut builder = SourceMapBuilder::new(Rc::new(filename.into()));

    for ((start_line, start_col), (end_line, end_col), src_file, (src_start_line, src_start_col), (src_end_line, src_end_col)) in entries {
        builder.add(
            Location { line: start_line, col: start_col },
            Location { line: end_line, col: end_col},
            Span::new(
                src_file,
                Location { line: src_start_line, col: src_start_col },
                Location { line: src_end_line, col: src_end_col },
            )
        )
    }

    builder.build()
}

#[test]
fn finds_single_entry() {
    let map = build_with_mappings("test_src.pas", [
        ((2, 2), (2, 4), "test_src.pas", (0, 3), (1, 8))
    ]);

    let span = map.lookup(
        Location { line: 2, col: 2 },
        Location { line: 2, col: 4 },
    );

    assert_eq!(PathBuf::from("test_src.pas"), *span.file);
    assert_eq!(0, span.start.line);
    assert_eq!(3, span.start.col);
    assert_eq!(1, span.end.line);
    assert_eq!(8, span.end.col);
}

#[test]
fn finds_nearest_entry() {
    let map = build_with_mappings("test_src.pas", [
        ((0, 0), (0, 10), "test_src.pas", (0, 0), (0, 10)),
        ((1, 0), (2, 2), "test_src.pas", (0, 10), (0, 15)),
        ((2, 2), (2, 4), "test_src.pas", (0, 16), (0, 20))
    ]);

    let span = map.lookup(
        Location { line: 2, col: 1 },
        Location { line: 2, col: 3 },
    );

    assert_eq!(PathBuf::from("test_src.pas"), *span.file);
    assert_eq!(0, span.start.line);
    assert_eq!(16, span.start.col);
    assert_eq!(0, span.end.line);
    assert_eq!(20, span.end.col);
}