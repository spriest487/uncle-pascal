use std::{
    io::{self, Read, BufRead},
    fmt::{self, Write},
    collections::HashSet,
};
use regex::*;

#[derive(Debug)]
pub enum PreprocessorError {
    SymbolNotDefined {
        name: String,
        filename: String,
        line: usize,
    },
    IllegalDirective {
        directive: String,
        filename: String,
        line: usize,
    },
    UnexpectedEndIf {
        filename: String,
        line: usize,
    },
    UnterminatedCondition {
        filename: String,
        from_line: usize,
    },
    IOError(io::Error),
}

impl fmt::Display for PreprocessorError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PreprocessorError::SymbolNotDefined { name, filename, line } =>
                write!(f, "symbol `{}` was not defined ({}:{})", name, filename, line),

            PreprocessorError::IllegalDirective { directive, filename, line } =>
                write!(f, "unrecognized directive `{}` ({}:{})", directive, filename, line),

            PreprocessorError::UnexpectedEndIf { filename, line } =>
                write!(f, "else or endif without matching ifdef or ifndef ({}:{})", filename, line),

            PreprocessorError::UnterminatedCondition { filename, from_line } =>
                write!(f, "unterminated conditional block ({}:{})", filename, from_line),

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

enum Directive {
    Define(String),
    Undef(String),
    IfDef(String),
    IfNDef(String),
    EndIf,
    Else,
    ElseIf(String),
}

struct DirectiveParser {
    define_pattern: Regex,
    undef_pattern: Regex,
    ifdef_pattern: Regex,
    ifndef_pattern: Regex,
    endif_pattern: Regex,
    else_pattern: Regex,
    elseif_pattern: Regex,
}

impl DirectiveParser {
    fn new() -> Self {
        Self {
            define_pattern: Regex::new(r#"^define\s+(\w+)$"#).unwrap(),
            undef_pattern: Regex::new(r#"^undef\s+(\w+)$"#).unwrap(),
            ifdef_pattern: Regex::new(r#"^ifdef\s+(\w+)$"#).unwrap(),
            ifndef_pattern: Regex::new(r#"^ifndef\s+(\w+)$"#).unwrap(),
            endif_pattern: Regex::new("^endif$").unwrap(),
            else_pattern: Regex::new("^else$").unwrap(),
            elseif_pattern: Regex::new(r#"^elseif\s+(\w+)$"#).unwrap(),
        }
    }

    fn parse(&self, s: &str) -> Option<Directive> {
        if let Some(define_captures) = self.define_pattern.captures(s) {
            let symbol = define_captures[1].to_string();

            Some(Directive::Define(symbol))
        } else if let Some(undef_captures) = self.undef_pattern.captures(s) {
            let symbol = undef_captures[1].to_string();

            Some(Directive::Undef(symbol))
        } else if let Some(ifdef_captures) = self.ifdef_pattern.captures(s) {
            let symbol = ifdef_captures[1].to_string();

            Some(Directive::IfDef(symbol))
        } else if let Some(ifndef_captures) = self.ifndef_pattern.captures(s) {
            let symbol = ifndef_captures[1].to_string();

            Some(Directive::IfNDef(symbol))
        } else if self.endif_pattern.is_match(s) {
            Some(Directive::EndIf)
        } else if self.else_pattern.is_match(s) {
            Some(Directive::Else)
        } else if let Some(elseif_captures) = self.elseif_pattern.captures(s) {
            let symbol = elseif_captures[1].to_string();

            Some(Directive::ElseIf(symbol))
        } else {
            None
        }
    }
}

struct SymbolCondition {
    value: bool,
    start_line: usize,
}

pub struct Preprocessor {
    directive_parser: DirectiveParser,

    condition_stack: Vec<SymbolCondition>,

    symbols: HashSet<String>,

    filename: String,

    output: String,
    comment_block: String,

    current_line: usize,
}

impl Preprocessor {
    pub fn new(filename: &str, symbols: HashSet<String>) -> Self {
        Preprocessor {
            filename: filename.to_string(),

            directive_parser: DirectiveParser::new(),

            condition_stack: Vec::new(),

            symbols,

            output: String::new(),
            comment_block: String::new(),

            current_line: 0,
        }
    }

    pub fn preprocess(mut self, source: impl Read) -> Result<String, PreprocessorError> {
        let read_buf = io::BufReader::new(source);
        for (line_num, line) in read_buf.lines().enumerate() {
            self.current_line = line_num + 1;

            self.process_line(line?)?;
        }

        if let Some(condition) = self.condition_stack.last() {
            return Err(PreprocessorError::UnterminatedCondition {
                from_line: condition.start_line,
                filename: self.filename,
            });
        }

        Ok(self.output)
    }

    fn process_line(&mut self, mut line: String) -> Result<(), PreprocessorError> {
        /* line comments never contain pp directives, just discard them */
        if let Some(comment_pos) = line.find("//") {
            line.truncate(comment_pos);
        };

        for line_char in line.chars() {
            if self.comment_block.len() > 0 {
                // continuing an existing comment
                self.comment_block.push(line_char);

                // did it terminate?
                if line_char == '}' {
                    self.process_directive()?;
                    self.comment_block.clear();
                }
            } else if line_char == '{' {
                // start a new comment block
                self.comment_block.push(line_char);
            } else if self.condition_active() {
                self.output.push(line_char);
            }
        }

        self.output.write_char('\n')?;
        Ok(())
    }

    /* true when we haven't encountered any conditional compilation flags, or all conditional
    compilation flags in the current stack are true */
    fn condition_active(&self) -> bool {
        self.condition_stack.iter().fold(true, |active, symbol_condition| {
            active && symbol_condition.value
        })
    }

    fn push_condition(&mut self, symbol: String, positive: bool) -> Result<(), PreprocessorError> {
        self.condition_stack.push(SymbolCondition {
            value: {
                let has_symbol = self.symbols.contains(&symbol);

                if positive { has_symbol } else { !has_symbol }
            },
            start_line: self.current_line,
        });

        Ok(())
    }

    fn pop_condition(&mut self) -> Result<(), PreprocessorError> {
        match self.condition_stack.pop() {
            Some(_) => Ok(()),
            None => Err(PreprocessorError::UnexpectedEndIf {
                filename: self.filename.clone(),
                line: self.current_line,
            })
        }
    }

    fn process_directive(&mut self) -> Result<(), PreprocessorError> {
        if self.comment_block.chars().skip(1).next() != Some('$') {
            return Ok(());
        }

        /* skip 2 for `{$` and take 1 less for the closing `}` */
        let directive_name = self.comment_block.chars()
            .skip(2)
            .take(self.comment_block.len() - 3)
            .collect::<String>()
            .to_ascii_lowercase()
            .trim()
            .to_string();

        match self.directive_parser.parse(&directive_name) {
            Some(Directive::Define(symbol)) => {
                if self.condition_active() {
                    self.symbols.insert(symbol);
                }

                Ok(())
            }

            Some(Directive::Undef(symbol)) => {
                if !self.condition_active() || self.symbols.remove(&symbol) {
                    Ok(())
                } else {
                    Err(PreprocessorError::SymbolNotDefined {
                        name: symbol,
                        filename: self.filename.clone(),
                        line: self.current_line,
                    })
                }
            }

            Some(Directive::IfDef(symbol)) => {
                self.push_condition(symbol, true)
            }

            Some(Directive::IfNDef(symbol)) => {
                self.push_condition(symbol, false)
            }

            Some(Directive::Else) => {
                match self.condition_stack.last_mut() {
                    None => Err(PreprocessorError::UnexpectedEndIf {
                        filename: self.filename.clone(),
                        line: self.current_line,
                    }),
                    Some(condition) => {
                        condition.value = !condition.value;
                        Ok(())
                    }
                }
            }

            Some(Directive::ElseIf(symbol)) => {
                self.pop_condition()?;
                self.push_condition(symbol, true)
            }

            Some(Directive::EndIf) => {
                self.pop_condition()
            }

            None => return Err(PreprocessorError::IllegalDirective {
                directive: directive_name,
                filename: self.filename.clone(),
                line: self.current_line,
            })
        }
    }
}

