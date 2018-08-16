use std::{
    io::{self, Read, BufRead},
    fmt::{self, Write},
    collections::HashSet,
};

#[derive(Debug)]
pub enum PreprocessorError {
    SymbolNotDefined {
        name: String,
        filename: String,
        line: usize
    },
    IOError(io::Error),
}

impl fmt::Display for PreprocessorError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PreprocessorError::SymbolNotDefined { name, filename, line } =>
                write!(f, "preprocessor symbol `{}` was not defined ({}:{})", name, filename, line),
            PreprocessorError::IOError(err) =>
                write!(f, "{}", err),
        }
    }
}

impl From<io::Error> for PreprocessorError {
    fn from(err: io::Error) -> Self {
        PreprocessorError::IOError(err)
    }
}

impl From<fmt::Error> for PreprocessorError {
    fn from(err: fmt::Error) -> Self {
        PreprocessorError::IOError(io::Error::new(io::ErrorKind::Other, err.to_string()))
    }
}

pub fn preprocess(source: impl Read,
                  filename: &str,
                  mut symbols: HashSet<String>) -> Result<String, PreprocessorError> {
    let mut processed = String::new();

    let mut comment_block = String::new();

    let read_buf = io::BufReader::new(source);
    for (line_num, line) in read_buf.lines().enumerate() {
        let mut line_src = line?;

        /* line comments never contain pp directives, just discard them */
        if let Some(comment_pos) = line_src.find("//") {
            line_src.truncate(comment_pos);
        };

        for line_char in line_src.chars() {
            if comment_block.len() > 0 {
                if line_char == '}' {
                    //these should be case-insensitive
                    comment_block = comment_block.to_ascii_lowercase();

                    //process directive
                    if comment_block.starts_with("{$define ") {
                        let name = comment_block[7..].to_string();

                        symbols.insert(name);
                    } else if comment_block.starts_with("{$undef ") {
                        let name = comment_block[7..].to_string();
                        if !symbols.contains(&name) {
                            return Err(PreprocessorError::SymbolNotDefined {
                                name: name.to_string(),
                                filename: filename.to_string(),
                                line: line_num,
                            })
                        }
                    } else if comment_block.starts_with("{$ifdef ") {

                    } else if comment_block.starts_with("{$ifndef ") {

                    } else if comment_block.starts_with("{$") {
                        eprintln!("preprocessor encountered unrecognized directive: {}}}", comment_block);
                    }

                    comment_block.clear();
                } else {
                    comment_block.push(line_char);
                }
            } else if line_char == '{' {
                comment_block.push(line_char);
            } else {
                processed.push(line_char);
            }
        }

        processed.write_char('\n')?;
    }

    Ok(processed)
}