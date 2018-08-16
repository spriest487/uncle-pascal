use std::{
    io::{self, Read, BufRead},
    fmt::{self, Write},
};
use regex::*;
use opts::{
    CompileOptions,
    Mode,
};

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
    Mode(Mode),
    Switches(Vec<(String, bool)>),
}

struct DirectiveParser {
    define_pattern: Regex,
    undef_pattern: Regex,
    ifdef_pattern: Regex,
    ifndef_pattern: Regex,
    endif_pattern: Regex,
    else_pattern: Regex,
    elseif_pattern: Regex,
    mode_pattern: Regex,
    switch_pattern: Regex,
}

impl DirectiveParser {
    fn new() -> Self {
        Self {
            define_pattern: Regex::new(r"^define\s+(\w+)$").unwrap(),
            undef_pattern: Regex::new(r"^undef\s+(\w+)$").unwrap(),
            ifdef_pattern: Regex::new(r"^ifdef\s+(\w+)$").unwrap(),
            ifndef_pattern: Regex::new(r"^ifndef\s+(\w+)$").unwrap(),
            endif_pattern: Regex::new("^endif$").unwrap(),
            else_pattern: Regex::new("^else$").unwrap(),
            elseif_pattern: Regex::new(r"^elseif\s+(\w+)$").unwrap(),
            mode_pattern: Regex::new(r"^mode\s+(\w+)$").unwrap(),
            switch_pattern: Regex::new(r"^([a-zA-Z]+)([+-])$").unwrap(),
        }
    }

    fn match_switches(&self, s: &str) -> Option<Vec<(String, bool)>> {
        let switches: Vec<_> = s.split(',')
            .map(|switch| {
                if let Some(switch_capture) = self.switch_pattern.captures(&switch.trim()) {
                    let switch_name = switch_capture[1].to_string();
                    let switch_val = match &switch_capture[2] {
                        "+" => true,
                        "-" => false,
                        _ => unreachable!("excluded by pattern: {}", &switch_capture[2]),
                    };

                    Some((switch_name, switch_val))
                } else {
                    None
                }
            })
            .collect();

        if switches.iter().any(|switch| switch.is_none()) {
            None
        } else {
            Some(switches.into_iter()
                .map(|switch| switch.unwrap())
                .collect())
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
        } else if let Some(mode_captures) = self.mode_pattern.captures(s) {
            let mode = match mode_captures[1].to_uppercase().as_str() {
                "FPC" => Mode::Fpc,
                "UNCLE" => Mode::Uncle,
                "DELPHI" => Mode::Delphi,
                unrecognized @ _ => {
                    eprintln!("unrecognized mode: {}", unrecognized);
                    return None;
                }
            };

            Some(Directive::Mode(mode))
        } else if let Some(switches) = self.match_switches(s) {
            Some(Directive::Switches(switches))
        } else {
            None
        }
    }
}

#[derive(Debug)]
struct SymbolCondition {
    value: bool,
    start_line: usize,
}

pub struct Preprocessor {
    directive_parser: DirectiveParser,

    condition_stack: Vec<SymbolCondition>,

    filename: String,

    output: String,
    comment_block: String,

    current_line: usize,
    last_char: char,

    opts: CompileOptions,
}

#[derive(Clone, Debug)]
pub struct PreprocessedUnit {
    pub source: String,

    pub opts: CompileOptions,
}

impl Preprocessor {
    pub fn new(filename: &str, opts: CompileOptions) -> Self {
        Preprocessor {
            filename: filename.to_string(),

            directive_parser: DirectiveParser::new(),

            condition_stack: Vec::new(),

            opts,

            output: String::new(),
            comment_block: String::new(),

            current_line: 0,

            last_char: ' '
        }
    }

    pub fn preprocess(mut self, source: impl Read) -> Result<PreprocessedUnit, PreprocessorError> {
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

        Ok(PreprocessedUnit {
            source: self.output,
            opts: self.opts,
        })
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

                let comment_starter = self.comment_block.chars().next().unwrap();

                // did it terminate the comment this block was started with?
                let terminated = (line_char == '}' && comment_starter == '{')
                    || (line_char == ')' && self.last_char == '*' && comment_starter == '(');

                if terminated {
                    if line_char == '}' {
                        self.process_directive()?;
                    }
                    self.comment_block.clear();
                }
            } else if line_char == '{' {
                // start a new { } comment block
                self.comment_block.push(line_char);
            } else if self.last_char == '(' && line_char == '*' {
                self.comment_block.push_str("(*");
                self.output.pop();
            } else if self.condition_active() {
                self.output.push(line_char);
            }

            self.last_char = line_char;
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
                let has_symbol = self.opts.defined(&symbol);
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
                    self.opts.define(symbol);
                }

                Ok(())
            }

            Some(Directive::Undef(symbol)) => {
                if !self.condition_active() || self.opts.undef(&symbol) {
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

            Some(Directive::Switches(switches)) => {
                if self.condition_active() {
                    for (name, on) in switches {
                        self.opts.set_switch(&name, on);
                    }
                }
                Ok(())
            }

            Some(Directive::Mode(mode)) => {
                if self.condition_active() {
                    self.opts.set_mode(mode);
                }
                Ok(())
            }

            None => {
                if !self.condition_active() {
                    Ok(())
                } else if !self.opts.strict_switches() {
                    eprintln!("ignoring unrecognized directive {{${}}} (strict preprocessing not enabled for mode `{}`)",
                              directive_name,
                              self.opts.mode());
                    Ok(())
                } else {
                    Err(PreprocessorError::IllegalDirective {
                        directive: directive_name,
                        filename: self.filename.clone(),
                        line: self.current_line,
                    })
                }
            }
        }
    }
}

