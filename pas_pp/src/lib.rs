use pas_common::{
    span::*,
    BuildOptions,
    DiagnosticOutput,
    Mode,
};
use regex::*;
use std::{
    fmt,
    path::PathBuf,
};

#[derive(Debug)]
pub enum PreprocessorError {
    SymbolNotDefined { name: String, at: Span },
    IllegalDirective { directive: String, at: Span },
    UnexpectedEndIf(Span),
    UnterminatedCondition(Span),
}

impl fmt::Display for PreprocessorError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PreprocessorError::SymbolNotDefined { name, .. } => {
                write!(f, "symbol `{}` was not defined", name)
            },

            PreprocessorError::IllegalDirective { directive, .. } => {
                write!(f, "unrecognized directive `{}`", directive)
            },

            PreprocessorError::UnexpectedEndIf(_) => {
                write!(f, "else or endif without matching ifdef or ifndef")
            },

            PreprocessorError::UnterminatedCondition(_) => {
                write!(f, "unterminated conditional block")
            },
        }
    }
}

impl Spanned for PreprocessorError {
    fn span(&self) -> &Span {
        match self {
            PreprocessorError::SymbolNotDefined { at, .. } => at,
            PreprocessorError::IllegalDirective { at, .. } => at,
            PreprocessorError::UnexpectedEndIf(at) => at,
            PreprocessorError::UnterminatedCondition(at) => at,
        }
    }
}

impl DiagnosticOutput for PreprocessorError {
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
    LinkLib(String),
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
    linklib_pattern: Regex,
}

impl DirectiveParser {
    fn new() -> Self {
        Self {
            define_pattern: Regex::new(r"(?i)^define\s+(\w+)$").unwrap(),
            undef_pattern: Regex::new(r"(?i)^undef\s+(\w+)$").unwrap(),
            ifdef_pattern: Regex::new(r"(?i)^ifdef\s+(\w+)$").unwrap(),
            ifndef_pattern: Regex::new(r"(?i)^ifndef\s+(\w+)$").unwrap(),
            endif_pattern: Regex::new("(?i)^endif$").unwrap(),
            else_pattern: Regex::new("(?i)^else$").unwrap(),
            elseif_pattern: Regex::new(r"(?i)^elseif\s+(\w+)$").unwrap(),
            mode_pattern: Regex::new(r"(?i)^mode\s+(\w+)$").unwrap(),
            switch_pattern: Regex::new(r"(?i)^([a-zA-Z]+)([+-])$").unwrap(),
            linklib_pattern: Regex::new(r"(?i)linklib\s+(\w+)$").unwrap(),
        }
    }

    fn match_switches(&self, s: &str) -> Option<Vec<(String, bool)>> {
        let switches: Vec<_> = s
            .split(',')
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
            Some(switches.into_iter().map(|switch| switch.unwrap()).collect())
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
                "UNCLE" => Mode::Default,
                "DELPHI" => Mode::Delphi,
                unrecognized => {
                    eprintln!("unrecognized mode: {}", unrecognized);
                    return None;
                },
            };

            Some(Directive::Mode(mode))
        } else if let Some(switches) = self.match_switches(s) {
            Some(Directive::Switches(switches))
        } else if let Some(linklib) = self.linklib_pattern.captures(s) {
            Some(Directive::LinkLib(linklib[1].to_string()))
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

    filename: PathBuf,

    output: String,
    comment_block: String,

    current_line: usize,
    last_char: char,

    opts: BuildOptions,
}

#[derive(Clone, Debug)]
pub struct PreprocessedUnit {
    pub filename: PathBuf,
    pub source: String,

    pub opts: BuildOptions,
}

impl Preprocessor {
    pub fn new(filename: impl Into<PathBuf>, opts: BuildOptions) -> Self {
        Preprocessor {
            filename: filename.into(),

            directive_parser: DirectiveParser::new(),

            condition_stack: Vec::new(),

            opts,

            output: String::new(),
            comment_block: String::new(),

            current_line: 0,

            last_char: ' ',
        }
    }

    fn current_span(&self) -> Span {
        let loc = Location {
            line: self.current_line,
            col: 0,
        };
        Span::new(self.filename.clone(), loc, loc)
    }

    pub fn preprocess(mut self, source: &str) -> Result<PreprocessedUnit, PreprocessorError> {
        for (line_num, line) in source.lines().enumerate() {
            self.current_line = line_num + 1;

            self.process_line(line.to_string())?;
        }

        if let Some(condition) = self.condition_stack.last() {
            return Err(PreprocessorError::UnterminatedCondition(Span::new(
                self.filename.clone(),
                Location {
                    line: condition.start_line,
                    col: 0,
                },
                Location {
                    line: condition.start_line,
                    col: 0,
                },
            )));
        }

        Ok(PreprocessedUnit {
            filename: self.filename,
            source: self.output,
            opts: self.opts,
        })
    }

    fn process_line(&mut self, mut line: String) -> Result<(), PreprocessorError> {
        // line comments never contain pp directives, just discard them
        if let Some(comment_pos) = line.find("//") {
            line.truncate(comment_pos);
        };

        for line_char in line.chars() {
            if !self.comment_block.is_empty() {
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

        self.output.push('\n');
        Ok(())
    }

    // true when we haven't encountered any conditional compilation flags, or all conditional
    // compilation flags in the current stack are true
    fn condition_active(&self) -> bool {
        self.condition_stack
            .iter()
            .all(|symbol_condition| symbol_condition.value)
    }

    fn push_condition(&mut self, symbol: &str, positive: bool) -> Result<(), PreprocessorError> {
        self.condition_stack.push(SymbolCondition {
            value: {
                let has_symbol = self.opts.defined(symbol);
                if positive {
                    has_symbol
                } else {
                    !has_symbol
                }
            },
            start_line: self.current_line,
        });

        Ok(())
    }

    fn pop_condition(&mut self) -> Result<(), PreprocessorError> {
        match self.condition_stack.pop() {
            Some(_) => Ok(()),
            None => Err(PreprocessorError::UnexpectedEndIf(Span::new(
                self.filename.clone(),
                Location {
                    line: self.current_line,
                    col: 0,
                },
                Location {
                    line: self.current_line,
                    col: 0,
                },
            ))),
        }
    }

    fn process_directive(&mut self) -> Result<(), PreprocessorError> {
        if self.comment_block.chars().nth(1) != Some('$') {
            return Ok(());
        }

        // skip 2 for `{$` and take 1 less for the closing `}`
        let directive_name = self
            .comment_block
            .chars()
            .skip(2)
            .take(self.comment_block.len() - 3)
            .collect::<String>()
            .trim()
            .to_string();

        match self.directive_parser.parse(&directive_name) {
            Some(Directive::Define(symbol)) => {
                if self.condition_active() {
                    self.opts.define(symbol);
                }

                Ok(())
            },

            Some(Directive::Undef(symbol)) => {
                if !self.condition_active() || self.opts.undef(&symbol) {
                    Ok(())
                } else {
                    Err(PreprocessorError::SymbolNotDefined {
                        name: symbol,
                        at: self.current_span(),
                    })
                }
            },

            Some(Directive::IfDef(symbol)) => self.push_condition(&symbol, true),

            Some(Directive::IfNDef(symbol)) => self.push_condition(&symbol, false),

            Some(Directive::Else) => match self.condition_stack.last_mut() {
                None => Err(PreprocessorError::UnexpectedEndIf(self.current_span())),
                Some(condition) => {
                    condition.value = !condition.value;
                    Ok(())
                },
            },

            Some(Directive::ElseIf(symbol)) => {
                self.pop_condition()?;
                self.push_condition(&symbol, true)
            },

            Some(Directive::EndIf) => self.pop_condition(),

            Some(Directive::Switches(switches)) => {
                if self.condition_active() {
                    for (name, on) in switches {
                        self.opts.set_switch(&name, on);
                    }
                }
                Ok(())
            },

            Some(Directive::LinkLib(lib_name)) => {
                if self.condition_active() {
                    self.opts.link_lib(lib_name.clone());
                }
                Ok(())
            },

            Some(Directive::Mode(mode)) => {
                if self.condition_active() {
                    self.opts.mode = mode;
                }
                Ok(())
            },

            None => {
                if !self.condition_active() {
                    Ok(())
                } else if !self.opts.strict_switches() {
                    eprintln!("ignoring unrecognized directive {{${}}} (strict preprocessing not enabled for mode `{}`)",
                              directive_name,
                              self.opts.mode);
                    Ok(())
                } else {
                    Err(PreprocessorError::IllegalDirective {
                        directive: directive_name,
                        at: self.current_span(),
                    })
                }
            },
        }
    }
}
