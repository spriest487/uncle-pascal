use codespan::{ByteIndex, ByteSpan, CodeMap, ColumnIndex, FileMap, LineIndex, RawIndex};
use codespan_reporting::{termcolor, Diagnostic, Label, LabelStyle, Severity};
use pas_common::{span::*, DiagnosticMessage, DiagnosticOutput};
use std::io;

fn find_location_index(file_map: &FileMap, loc: &Location, end: bool) -> io::Result<ByteIndex> {
    let line = LineIndex(loc.line as RawIndex);
    let col = if end {
        // our spans are inclusive, CodeSpan spans are exclusive
        ColumnIndex(loc.col as RawIndex + 1)
    } else {
        ColumnIndex(loc.col as RawIndex)
    };

    file_map.byte_index(line, col).map_err(|location_err| {
        io::Error::new(io::ErrorKind::InvalidData, location_err.to_string())
    })
}

fn output_to_report_diag(
    diag: DiagnosticMessage,
    code_map: &mut CodeMap,
    style: LabelStyle,
    severity: Severity,
) -> io::Result<Diagnostic> {
    let mut report_diag = Diagnostic::new(severity, diag.title);

    if let Some(label) = diag.label {
        let file_map = code_map.add_filemap_from_disk(label.span.file.as_ref())?;

        let err_start = find_location_index(file_map.as_ref(), &label.span.start, false)?;
        let err_end = find_location_index(file_map.as_ref(), &label.span.end, true)?;

        let mut report_label = Label::new(ByteSpan::new(err_start, err_end), style);
        if let Some(text) = label.text {
            report_label = report_label.with_message(text);
        }

        report_diag = report_diag.with_label(report_label);
    }

    Ok(report_diag)
}

pub fn report_err(err: &impl DiagnosticOutput) -> io::Result<()> {
    let out = termcolor::StandardStream::stdout(termcolor::ColorChoice::Auto);

    let mut code_map = CodeMap::new();
    let main_diag = output_to_report_diag(
        err.main(),
        &mut code_map,
        LabelStyle::Primary,
        Severity::Error,
    )?;

    codespan_reporting::emit(out.lock(), &code_map, &main_diag)?;

    let see_also_diags: Vec<_> = err
        .see_also()
        .into_iter()
        .map(|diag| output_to_report_diag(diag, &mut code_map, LabelStyle::Primary, Severity::Note))
        .collect::<Result<_, _>>()?;

    for diag in see_also_diags {
        println!();
        codespan_reporting::emit(out.lock(), &code_map, &diag)?;
    }

    Ok(())
}
