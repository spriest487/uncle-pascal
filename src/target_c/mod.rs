mod ast;

use std::{
    fmt::{self, Write as FmtWrite},
};

pub use self::ast::{
    TranslationError,
    TranslationResult,
    TranslationUnit
};

use node::Identifier;
use std::{
    path::Path,
    fs,
    process,
    io::Write,
};

use pretty_path;
use CompileError;
use semantic::{
    ProgramModule,
};
use opts::CompileOptions;

const HEADER: &str = include_str!("header.h");
const RT: &str = include_str!("rt.h");

pub fn identifier_to_c(id: &Identifier) -> String {
    let mut parts = id.namespace.clone();
    parts.push(id.name.clone());

    parts.join("_")
}

pub fn pas_to_c(module: &ProgramModule,
                out_path: &Path,
                opts: &CompileOptions) -> Result<(), CompileError> {
    let translated = ast::TranslationUnit::from_program(module)?;
    let c_unit = write_cpp(&translated)?;

    let compile_with_clang = out_path.extension().map(|ext| ext != "cpp" && ext != "cxx")
        .unwrap_or(true);

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

pub fn write_cpp(unit: &TranslationUnit) -> Result<String, fmt::Error> {
    let mut out = String::new();

    out.write_str(HEADER)?;

    // forward declare System.String
    writeln!(out, "struct System_String;")?;

    // declare initialize string literal global vars
    unit.declare_string_literals(&mut out)?;

    for decl in unit.decls() {
        decl.write_forward(&mut out)?;
    }

    /* write impls */
    for decl_impl in unit.decls().iter() {
        decl_impl.write_impl(&mut out)?;
    }

    /* write main function */
    writeln!(out, "int main(int argc, char* argv[]) {{")?;

    // init the string class and string literals
    writeln!(out, "System_Internal_InitClass(\"System.String\", (System_Internal_Destructor)&System_Disposable_Dispose_System_String);")?;
    unit.init_string_literals(&mut out)?;

    // init other classes
    for class in unit.classes().iter() {
        class.write_init(&mut out)?;
    }

    /* write initialization for each unit's initialization */
    for init_block in unit.initialization().iter() {
        init_block.write(&mut out)?;
    }

    for final_block in unit.finalization().iter() {
        final_block.write(&mut out)?;
    }

    writeln!(out, "return EXIT_SUCCESS;")?;
    writeln!(out, "}}")?;

    /* write native runtime library impls */
    out.write_str(RT)?;
    writeln!(out)?;

    Ok(out)
}
