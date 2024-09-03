use std::env;
use std::ffi::OsStr;
use std::fs::DirEntry;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, ChildStdout, Command, Output, Stdio};
use crate::opts::{ExecutionMethod, Opts};
use crate::test_script::{TestScript, TestScriptStep};

#[derive(Clone)]
pub struct TestCase {
    pub path: PathBuf,
    pub script: TestScript,
}

impl TestCase {
    fn find_from_entry(entry: DirEntry) -> io::Result<Vec<TestCase>> {
        let file_type = entry.file_type()?;

        let paths = if file_type.is_dir() {
            TestCase::find_at_path(&entry.path())
        } else {
            let path = entry.path();

            match path.extension() {
                Some(ext) if ext.eq_ignore_ascii_case("pas") => {
                    let script = TestScript::find_for_path(&path)
                        .unwrap_or_else(|err| {
                            eprintln!("failed to read test script: {err}");
                            TestScript::default()
                        });

                    if script.ignore {
                        Vec::new()
                    } else {
                        vec![TestCase { path, script }] 
                    }
                }

                _ => Vec::new(),
            }
        };

        Ok(paths)
    }

    pub fn find_at_path(root: &Path) -> Vec<TestCase> {
        root.read_dir()
            .ok()
            .map(|read_dir|
                read_dir
                    .filter_map(|read_entry| {
                        read_entry
                            .and_then(TestCase::find_from_entry)
                            .ok()
                    })
                    .flat_map(|paths| paths)
                    .collect()
            )
            .unwrap_or_else(Vec::new)
    }
    
    fn run_interpreted(&self, opts: &Opts) -> io::Result<Child> {
        Command::new(&opts.compiler)
            .arg(&self.path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
    }
    
    fn run_clang(&self, opts: &Opts) -> io::Result<Child> {
        let path_as_rel = match self.path.strip_prefix(&opts.search_path) {
            Ok(path) => path,
            Err(err) => {
                return Err(io::Error::new(io::ErrorKind::Other, err.to_string()));
            }
        };
        
        let mut c_file_path = opts.target_path.join(&path_as_rel);
        c_file_path.set_extension("c");

        let exe_ext = match env::consts::OS {
            "windows" => "exe",
            _ => "",
        };

        let mut exe_path = c_file_path.clone();
        exe_path.set_extension(exe_ext);
        
        if is_target_outdated(&exe_path, &self.path) {
            try_run_command(Command::new(&opts.compiler)
                .arg(&self.path)
                .arg("-o").arg(&c_file_path))?;
            
            let mut clang_args = Vec::new();
            if opts.clang_debug {
                clang_args.push(OsStr::new("-g"));
                clang_args.push(OsStr::new("-O0"));
            }
            if opts.clang_codeview {
                clang_args.push(OsStr::new("-gcodeview"));   
            }

            try_run_command(Command::new("clang")
                .arg(c_file_path)
                .arg("-o").arg(&exe_path)
                .args(clang_args))?;
        }
        
        Command::new(exe_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
    }
    
    fn try_run(&self, opts: &Opts) -> io::Result<bool> {
        println!("RUNNING: {}", self.path.display());

        let mut proc = match opts.execution_method {
            ExecutionMethod::Interpret => self.run_interpreted(opts),
            ExecutionMethod::Clang => self.run_clang(opts),
        }?;

        let mut stdin = proc.stdin.take()
            .expect("stdin was not captured for child process");
        let mut stdout = proc.stdout.take()
            .expect("stdout was not captured for child process");

        let mut line_buf = Vec::new();
        for step in &self.script.steps {
            match run_step(step, &mut stdin, &mut stdout, &mut line_buf) {
                Ok(true) => {},
                Ok(false) => {
                    let output = proc.wait_with_output()?;
                    dump_output(&output);

                    return Ok(false);
                }
                Err(err) => {
                    let output = proc.wait_with_output()?;
                    dump_output(&output);

                    println!("FAILED ({err})");
                    return Ok(false);
                }
            }
        }

        proc.stdin = Some(stdin);
        proc.stdout = Some(stdout);
        let output = proc.wait_with_output()?;
        dump_output(&output);

        let completed = if output.status.success() {
            println!("OK");

            true
        } else {
            let code = output.status
                .code()
                .map(|code| code.to_string())
                .unwrap_or_else(|| String::from("no return code"));

            println!("ERROR {}", code);

            false
        };

        Ok(completed)
    }
    
    pub fn run(&self, opts: &Opts) -> bool {
        let ok = self.try_run(opts).unwrap_or_else(|err| {
            println!("FAILED ({err})");
            false
        });
        
        println!();
        
        ok
    }
}



fn run_step(
    step: &TestScriptStep,
    stdin: &mut ChildStdin,
    stdout: &mut ChildStdout,
    line_buf: &mut Vec<u8>,
) -> io::Result<bool> {
    if let Some(input) = &step.input {
        println!("  >> {}", input.trim_end());
        stdin.write_all(input.as_bytes())?;
        stdin.flush()?;
    }

    if let Some(expect_output) = &step.output {
        line_buf.clear();
        read_to_line_feed(stdout, line_buf)?;

        let line = match String::from_utf8(line_buf.clone()) {
            Ok(string) => string.trim_end().to_string(),
            Err(err) => {
                println!("ERROR: unreadable output: {}", err);
                return Ok(false);
            }
        };

        println!("  << {}", line);

        if line != *expect_output {
            println!("ERROR: unexpected output");
            println!("EXPECTED: {}", expect_output.trim_end());
            println!("  ACTUAL: {}", line);

            return Ok(false);
        }
    }

    Ok(true)
}

fn read_to_line_feed(src: &mut impl Read, buf: &mut Vec<u8>) -> io::Result<()> {
    loop {
        let mut next = [0];
        src.read_exact(&mut next)?;

        if next[0] == b'\n' {
            break Ok(());
        }

        if next[0] != b'\r' {
            buf.push(next[0]);
        }
    }
}

fn try_run_command(command: &mut Command) -> io::Result<()> {
    let output = command.output()?;

    if output.status.success() {
        return Ok(());
    }

    dump_output(&output);
    Err(io::Error::new(io::ErrorKind::Other, output.status.to_string()))
}

fn is_target_outdated(target: &Path, source: &Path) -> bool {
    if !target.exists() {
        return true;
    }

    let get_target_modified = target.metadata()
        .and_then(|meta| meta.modified()).ok();
    let get_source_modified = source.metadata()
        .and_then(|meta| meta.modified()).ok();

    match (get_target_modified, get_source_modified) {
        (Some(target_modified), Some(source_modified)) =>
            source_modified > target_modified,
        _ => true,
    }
}

fn dump_output(output: &Output) {
    let stdout = String::from_utf8(output.stdout.clone()).unwrap();
    if stdout.len() > 0 {
        for line in stdout.lines() {
            println!("  >> {}", line.trim());
        }
    }

    let stderr = String::from_utf8(output.stderr.clone()).unwrap();
    if stderr.len() > 0 {
        for line in stderr.lines() {
            println!("  !! {}", line.trim());
        }
    }
}
