use std::{
    path::Path,
    fs,
    process,
    io::{self, Write},
};

use pretty_path;
use CompileError;
use semantic::ProgramModule;
use opts::CompileOptions;

mod writer;

const HEADER: &str = include_str!("header.h");
const RT: &str = include_str!("rt.h");

pub fn pas_to_c(module: &ProgramModule,
                out_path: &Path,
                opts: &CompileOptions) -> Result<(), CompileError> {
    let c_unit = writer::write_c(&module)?;

    let compile_with_clang = out_path.extension().map(|ext| ext != "cpp" && ext != "cxx")
        .unwrap_or(true);

    let _out_dir = out_path.parent().ok_or_else(|| {
        let msg = format!("unable to resolve output directory from path `{}`", pretty_path(out_path));
        CompileError::IOError(io::Error::new(io::ErrorKind::NotFound, msg))
    })?;

    if compile_with_clang {
        invoke_clang(&c_unit, &out_path, opts)?;
    } else {
        println!("Writing output to `{}`...", pretty_path(&out_path));

        let mut out_file = fs::File::create(out_path)?;
        out_file.write_all(c_unit.as_bytes())?;

        println!("Done!");
    }
    Ok(())
}

fn invoke_clang<'a>(c_src: &str,
                    out_path: &Path,
                    opts: &CompileOptions)
                    -> Result<(), CompileError> {
    let mut clang = process::Command::new("clang++");
    clang.arg("-Wno-parentheses-equality");
    clang.arg("-Wno-non-literal-null-conversion");
    clang.arg("-Werror");
    clang.arg("-fno-exceptions");
    clang.arg("-fno-rtti");
    clang.arg("-x").arg("c++");
    clang.arg("-std=c++17");

    for lib_path in opts.lib_paths() {
        clang.arg(&format!("-L{}", lib_path));
    }

    for lib in opts.link_libs() {
        clang.arg(&format!("-l{}", lib));
    }

    if cfg!(target="macos") {
        clang.arg("-stdlib=libc++");
    }

    clang.arg("-o").arg(out_path);
    clang.arg("-");
    clang.stdout(process::Stdio::inherit());
    clang.stdin(process::Stdio::piped());

    println!("invoking clang: {:#?}", clang);

    let mut clang_proc = clang.spawn()?;
    {
        let clang_in = clang_proc.stdin.as_mut().unwrap();
        clang_in.write_all(c_src.to_owned().as_bytes())?;
    }
    clang_proc.wait()?;

    Ok(())
}