use chrono::{DateTime, Utc};
use std::process::Command;
use structopt::StructOpt;

mod opts;
mod test_script;

use opts::Opts;
use test_case::TestCase;

mod test_case;

fn main() -> Result<(), i32> {
    let opts = Opts::from_args();

    let exists = opts.compiler.exists();
    if !exists {
        eprintln!("compiler not found! expected at {}", opts.compiler.display());
        return Err(1);
    }

    println!("using compiler: {}", opts.compiler.display());

    let timestamp = match opts.compiler.metadata().and_then(|metadata| metadata.modified()) {
        Ok(modified) => {
            let modified_date = DateTime::<Utc>::from(modified);
            format!("{}", modified_date.format("%Y-%m-%d %H:%M"))
        }
        Err(_) => "unknown timestamp".to_string(),
    };

    let version_check_out = Command::new(opts.compiler.clone())
        .arg("--version")
        .output()
        .map_err(|err| {
            eprintln!("version check failed: {}", err);
            1
        })?;

    println!("{} ({})", String::from_utf8(version_check_out.stdout).unwrap().trim(), timestamp);

    let test_files = TestCase::find_at_path(&opts.search_path);
    println!("found {} tests...", test_files.len());

    for test_file in test_files {
        if !test_file.run(&opts) && !opts.error_continue {
            break;
        }
    }
    
    Ok(())
}
