use std::{
    path::Path,
    fs,
    process,
    io::{self, Write},
};

use pretty_path;
use CompileError;
use ProgramModule;

mod writer;

const HEADER: &str = include_str!("header.h");

pub fn pas_to_c(module: &ProgramModule,
                out_path: &Path) -> Result<(), CompileError> {
    let c_unit = writer::write_c(&module)?;

    let compile_with_clang = out_path.extension().map(|ext| ext != "c")
        .unwrap_or(true);

    let _out_dir = out_path.parent().ok_or_else(|| {
        let msg = format!("unable to resolve output directory from path `{}`", pretty_path(out_path));
        CompileError::IOError(io::Error::new(io::ErrorKind::NotFound, msg))
    })?;

    if compile_with_clang {
        invoke_clang(&c_unit, &out_path)?;
    } else {
        println!("Writing output to `{}`...", pretty_path(&out_path));

        let mut out_file = fs::File::create(out_path)?;
        out_file.write_all(c_unit.as_bytes())?;

        println!("Done!");
    }
    Ok(())
}

fn invoke_clang<'a>(c_src: &str, out_path: &Path) -> Result<(), CompileError> {
    let mut clang = process::Command::new("clang++");
    clang.arg("-Wno-parentheses-equality");
    clang.arg("-Wno-non-literal-null-conversion");
    clang.arg("-x").arg("c++");
    clang.arg("-std=c++17");

    if cfg!(target="macos") {
        clang.arg("-stdlib=libc++");
    }

    let mut clang_proc = clang
        .arg("-o").arg(out_path)
        .arg("-")
        .stdout(process::Stdio::inherit())
        .stdin(process::Stdio::piped())
        .spawn()?;

    {
        let clang_in = clang_proc.stdin.as_mut().unwrap();
        clang_in.write_all(c_src.to_owned().as_bytes())?;
    }
    clang_proc.wait()?;

    Ok(())
}