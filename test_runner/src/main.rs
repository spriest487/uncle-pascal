use chrono::{DateTime, Utc};
use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;
use structopt::StructOpt;

mod opts;
mod test_script;

use opts::Opts;
use test_case::TestCase;

mod test_case;

fn main() {
    let opts = Opts::from_args();
    let target = if opts.use_release { "release" } else { "debug" };

    let exe_ext = match env::consts::OS {
        "windows" => ".exe",
        _ => "",
    };

    let pascal2_path = PathBuf::from(&format!("target/{target}/pascal2{exe_ext}"));
    let exists = pascal2_path.exists();
    if !exists {
        panic!("compiler not found! expected at {}", pascal2_path.display());
    }

    println!("using compiler: {}", pascal2_path.display());

    let timestamp = match pascal2_path.metadata().and_then(|metadata| metadata.modified()) {
        Ok(modified) => {
            let modified_date = DateTime::<Utc>::from(modified);
            format!("{}", modified_date.format("%Y-%m-%d %H:%M"))
        }
        Err(_) => "unknown timestamp".to_string(),
    };

    let version_check_out = Command::new(pascal2_path.clone())
        .arg("--version")
        .output()
        .expect("version check failed");

    println!("{} ({})", String::from_utf8(version_check_out.stdout).unwrap().trim(), timestamp);

    let test_files = TestCase::find_at_path(&Path::new("demos"));
    println!("found {} tests...", test_files.len());

    for test_file in test_files {
        if !test_file.run(&pascal2_path) && !opts.error_continue {
            break;
        }
    }
}
