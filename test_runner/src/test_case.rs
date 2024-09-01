use std::env;
use std::fs::DirEntry;
use std::io::{self, BufRead, BufReader, Write};
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
        
        Self::try_run_command(Command::new(&opts.compiler)
            .arg(&self.path)
            .arg("-o").arg(&c_file_path))?;

        let exe_ext = match env::consts::OS {
            "windows" => "exe",
            _ => "",
        };
        
        let mut exe_path = c_file_path.clone();
        exe_path.set_extension(exe_ext);

        Self::try_run_command(Command::new("clang")
            .arg(c_file_path)
            .arg("-o").arg(&exe_path))?;
        
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

        let mut line_buf = String::new();
        for step in &self.script.steps {
            match Self::run_step(step, &mut stdin, &mut stdout, &mut line_buf) {
                Ok(true) => {},
                Ok(false) => return Ok(false),
                Err(err) => {
                    println!("FAILED ({err})");
                    return Ok(false);
                }
            }
        }

        proc.stdin = Some(stdin);
        proc.stdout = Some(stdout);
        let output = proc.wait_with_output()?;
        Self::dump_output(&output);

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

    fn run_step(
        step: &TestScriptStep,
        stdin: &mut ChildStdin,
        stdout: &mut ChildStdout,
        line_buf: &mut String,
    ) -> io::Result<bool> {
        if let Some(input) = &step.input {
            println!("  >> {}", input.trim_end());
            stdin.write_all(input.as_bytes())?;
            stdin.flush()?;
        }

        if let Some(expect_output) = &step.output {
            line_buf.clear();

            let mut reader = BufReader::new(stdout);
            reader.read_line(line_buf)?;

            if line_buf.trim() != expect_output.trim() {
                println!("ERROR: unexpected output");
                println!("EXPECTED: {}", expect_output.trim_end());
                println!("  ACTUAL: {}", line_buf.trim_end());

                return Ok(false);
            }

            println!("  << {}", line_buf.trim_end());
        }
        
        Ok(true)
    }

    fn try_run_command(command: &mut Command) -> io::Result<()> {
        let output = command.output()?;

        if output.status.success() {
            return Ok(());
        }

        Self::dump_output(&output);
        Err(io::Error::new(io::ErrorKind::Other, output.status.to_string()))
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
}
