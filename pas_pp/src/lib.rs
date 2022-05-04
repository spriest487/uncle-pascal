pub mod error;
mod directive;

use pas_common::{span::*, BuildOptions};
use std::{
    io::Read,
    io,
    path::PathBuf,
    fs::File,
    rc::Rc
};
use crate::{
    directive::{Directive, DirectiveParser},
    error::PreprocessorError
};

#[derive(Debug)]
struct SymbolCondition {
    value: bool,
    start_line: usize,
}

pub struct Preprocessor {
    directive_parser: DirectiveParser,

    condition_stack: Vec<SymbolCondition>,

    filename: Rc<PathBuf>,

    output: String,
    comment_block: Option<CommentBlock>,

    current_line: usize,
    last_char: char,

    opts: BuildOptions,
}

struct CommentBlock {
    text: String,
    span: Span,
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
            filename: Rc::new(filename.into()),

            directive_parser: DirectiveParser::new(),

            condition_stack: Vec::new(),

            opts,

            output: String::new(),
            comment_block: None,

            current_line: 0,

            last_char: ' ',
        }
    }

    fn current_line_span(&self, col: usize) -> Span {
        let loc = Location {
            line: self.current_line,
            col,
        };

        Span { file: self.filename.clone(), start: loc, end: loc }
    }

    pub fn preprocess(mut self, source: &str) -> Result<PreprocessedUnit, PreprocessorError> {
        for (line_num, line) in source.lines().enumerate() {
            self.process_line(line.to_string())?;

            self.current_line = line_num + 1;
        }

        if let Some(condition) = self.condition_stack.last() {
            return Err(PreprocessorError::UnterminatedCondition(Span {
                file: self.filename.clone(),
                start: Location {
                    line: condition.start_line,
                    col: 0,
                },
                end: Location {
                    line: condition.start_line,
                    col: 0,
                },
            }));
        }

        Ok(PreprocessedUnit {
            filename: (*self.filename).clone(),
            source: self.output,
            opts: self.opts,
        })
    }

    fn process_line(&mut self, mut line: String) -> Result<(), PreprocessorError> {
        // line comments never contain pp directives, just discard them
        if let Some(comment_pos) = line.find("//") {
            line.truncate(comment_pos);
        };

        for (col, line_char) in line.chars().enumerate() {
            if let Some(comment_block) = &mut self.comment_block {
                // continuing an existing comment
                comment_block.text.push(line_char);
                comment_block.span.end = Location {
                    col,
                    line: self.current_line,
                };

                let comment_starter = comment_block.text.chars().next().unwrap();

                // did it terminate the comment this block was started with?
                let terminated = (line_char == '}' && comment_starter == '{')
                    || (line_char == ')' && self.last_char == '*' && comment_starter == '(');

                if terminated {
                    let comment_block = self.comment_block.take().unwrap();
                    if line_char == '}' {
                        self.process_directive(comment_block, col)?;
                    }
                    self.comment_block = None;
                }
            } else if line_char == '{' {
                // start a new { } comment block
                self.comment_block = Some(CommentBlock {
                    text: "{".to_string(),
                    span: self.current_line_span(col),
                })
            } else if self.last_char == '(' && line_char == '*' {
                // start a new (* *) comment block
                self.comment_block = Some(CommentBlock {
                    text: "(*".to_string(),
                    span: self.current_line_span(col),
                });
                // it's a two character comment sequence so pop the (
                self.output.pop();
            } else if self.condition_active() {
                self.output.push(line_char);
            }

            self.last_char = line_char;
        }

        // new line at end of real line
        self.output.push('\n');

        // if we're currently building a comment, add the newline to the comment text too
        if let Some(comment_block) = &mut self.comment_block {
            comment_block.text.push('\n');
        }

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

    fn pop_condition(&mut self, col: usize) -> Result<(), PreprocessorError> {
        match self.condition_stack.pop() {
            Some(_) => Ok(()),
            None => Err(PreprocessorError::UnexpectedEndIf(self.current_line_span(col))),
        }
    }

    fn process_directive(&mut self, comment_block: CommentBlock, current_col: usize) -> Result<(), PreprocessorError> {
        if comment_block.text.chars().nth(1) != Some('$') {
            // no directive, just an empty brace comment
            return Ok(());
        }

        let directive_span = comment_block.span.clone();

        // skip 2 for `{$` and take 1 less for the closing `}`
        let directive_name = comment_block
            .text
            .chars()
            .skip(2)
            .take(comment_block.text.len() - 3)
            .collect::<String>()
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
                        at: comment_block.span.clone(),
                    })
                }
            }

            Some(Directive::IfDef(symbol)) => self.push_condition(&symbol, true),

            Some(Directive::IfNDef(symbol)) => self.push_condition(&symbol, false),

            Some(Directive::Else) => match self.condition_stack.last_mut() {
                None => Err(PreprocessorError::UnexpectedEndIf(directive_span)),
                Some(condition) => {
                    condition.value = !condition.value;
                    Ok(())
                }
            },

            Some(Directive::ElseIf(symbol)) => {
                self.pop_condition(current_col)?;
                self.push_condition(&symbol, true)
            }

            Some(Directive::EndIf) => self.pop_condition(current_col),

            Some(Directive::Switches(switches)) => {
                if self.condition_active() {
                    for (name, on) in switches {
                        self.opts.set_switch(&name, on);
                    }
                }
                Ok(())
            }

            Some(Directive::LinkLib(lib_name)) => {
                if self.condition_active() {
                    self.opts.link_lib(lib_name.clone());
                }
                Ok(())
            }

            Some(Directive::Mode(mode)) => {
                if self.condition_active() {
                    self.opts.mode = mode;
                }
                Ok(())
            }

            Some(Directive::Include(filename)) => {
                let full_path = match self.filename.parent() {
                    Some(parent) => parent.join(&filename),
                    None => PathBuf::from(&filename),
                };

                let include_src = self.read_include(&full_path)
                    .map_err(|err| PreprocessorError::IncludeError {
                        filename,
                        err,
                        at: directive_span,
                    })?;

                let pp = Preprocessor::new(full_path, self.opts.clone());
                let include_output = pp.preprocess(&include_src)?;

                self.output += include_output.source.as_str();
                Ok(())
            }

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
                        at: directive_span,
                    })
                }
            }
        }
    }

    fn read_include(&mut self, filename: &PathBuf) -> io::Result<String> {
        let mut file = File::open(filename)?;
        let mut src = String::new();
        file.read_to_string(&mut src)?;

        Ok(src)
    }
}
