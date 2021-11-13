use codespan_reporting::{
    files::SimpleFiles,
    diagnostic::{Diagnostic, Label, LabelStyle, Severity},
    term::termcolor,
};
use pas_common::{path_relative_to_cwd, DiagnosticMessage, DiagnosticOutput};
use std::{
    fs::File,
    io::{Read as _},
};
use std::collections::HashMap;
use codespan_reporting::files::{Files, Error as FileError};

type CodeMap = SimpleFiles<String, String>;

fn output_to_report_diag(
    diag: DiagnosticMessage,
    code_map: &mut CodeMap,
    style: LabelStyle,
    severity: Severity,
) -> Result<Diagnostic<usize>, FileError> {
    let mut file_ids = HashMap::new();

    let mut labels = Vec::new();

    if let Some(label) = diag.label {
        let nice_filename = path_relative_to_cwd(&label.span.file);
        let file_id = match file_ids.get(&nice_filename) {
            Some(file_id) => *file_id,
            None => {
                let mut src = String::new();

                let mut file = File::open(label.span.file.as_ref())?;
                file.read_to_string(&mut src)?;

                let file_id = code_map.add(nice_filename.display().to_string(), src);
                file_ids.insert(nice_filename, file_id);
                file_id
            }
        };

        let start_loc = &label.span.start;
        let end_loc = &label.span.end;

        let err_start = code_map.line_range(file_id, start_loc.line)?.start + start_loc.col;
        let err_end = code_map.line_range(file_id, end_loc.line)?.start + end_loc.col + 1;

        let report_label = Label::new(style, file_id, err_start..err_end);

        labels.push(match label.text {
            Some(text) => report_label.with_message(text),
            None => report_label,
        });
    }

    Ok(Diagnostic::new(severity)
        .with_labels(labels)
        .with_message(diag.title))
}

pub fn report_err(err: &impl DiagnosticOutput) -> Result<(), FileError> {
    let out = termcolor::StandardStream::stdout(termcolor::ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    let mut code_map = CodeMap::new();
    let main_diag = output_to_report_diag(
        err.main(),
        &mut code_map,
        LabelStyle::Primary,
        Severity::Error,
    )?;

    codespan_reporting::term::emit(&mut out.lock(), &config, &code_map, &main_diag)?;

    let see_also_diags: Vec<_> = err
        .see_also()
        .into_iter()
        .map(|diag| output_to_report_diag(diag, &mut code_map, LabelStyle::Primary, Severity::Note))
        .collect::<Result<_, _>>()?;


    for diag in see_also_diags {
        println!();
        codespan_reporting::term::emit(&mut out.lock(), &config, &code_map, &diag)?;
    }

    Ok(())
}
