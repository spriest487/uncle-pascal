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
use pas_common::source_map::{SourceMap, SourceMapBuilder};
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

    output_lines: Vec<String>,
    comment_block: Option<CommentBlock>,

    current_line: usize,
    last_char: char,

    opts: BuildOptions,

    pub source_map: SourceMapBuilder,
}

struct CommentBlock {
    text: String,
    span: Span,
}

#[derive(Clone, Debug)]
pub struct PreprocessedUnit {
    pub filename: Rc<PathBuf>,
    pub source: String,

    pub opts: BuildOptions,

    pub source_map: SourceMap,
}

impl Preprocessor {
    pub fn new(filename: impl Into<PathBuf>, opts: BuildOptions) -> Self {
        let filename = Rc::new(filename.into());
        Preprocessor {
            filename: filename.clone(),

            directive_parser: DirectiveParser::new(),

            condition_stack: Vec::new(),

            opts,

            output_lines: Vec::new(),
            comment_block: None,

            current_line: 0,

            last_char: ' ',

            source_map: SourceMapBuilder::new(filename),
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

        if let Some(comment) = self.comment_block {
            return Err(PreprocessorError::UnterminatedComment(comment.span));
        }

        Ok(PreprocessedUnit {
            filename: self.filename,
            source: self.output_lines.join("\n"),
            opts: self.opts,

            source_map: self.source_map.build(),
        })
    }

    fn process_line(&mut self, mut line: String) -> Result<(), PreprocessorError> {
        let mut output = String::new();

        // line comments never contain pp directives, just discard them
        if let Some(comment_pos) = line.find("//") {
            line.truncate(comment_pos);
        };

        for (col, line_char) in line.chars().enumerate() {
            let col = col + 1;
            if let Some(comment_block) = &mut self.comment_block {
                // continuing an existing comment
                comment_block.text.push(line_char);
                comment_block.span.end = Location::new(self.current_line, col + 1);

                let comment_starter = comment_block.text.chars().next().unwrap();

                // did it terminate the comment this block was started with?
                let terminated = (line_char == '}' && comment_starter == '{')
                    || (line_char == ')' && self.last_char == '*' && comment_starter == '(');

                if terminated {
                    let comment_block = self.comment_block.take().unwrap();
                    if line_char == '}' {
                        self.process_directive(comment_block, col, &mut output)?;
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
                    span: self.current_line_span(col.saturating_sub("(*".len())),
                });
                // it's a two character comment sequence so pop the (
                output.pop();
            } else if self.condition_active() {
                output.push(line_char);
            }

            self.last_char = line_char;
        }

        let output_line_num = self.output_lines.len();
        self.source_map.add(
            Location::new(output_line_num, 1),
            Location::new(output_line_num, output.len() + 1),
            Span {
                start: Location::new(self.current_line, 1),
                end: Location::new(self.current_line, line.len() + 1),
                file: self.filename.clone(),
            }
        );

        // new line at end of real line
        self.output_lines.push(output);

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

    fn process_directive(&mut self, comment_block: CommentBlock, current_col: usize, output: &mut String) -> Result<(), PreprocessorError> {
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

                let line_offset = self.output_lines.len();
                for (i, mapping) in include_output.source_map.into_iter().enumerate() {
                    let mut start = mapping.start;
                    let mut end = mapping.end;

                    start.line += line_offset;
                    end.line += line_offset;

                    if i == 0 {
                        start.col += current_col;
                    }

                    self.source_map.add(start, end, mapping.src);
                }

                for include_source_line in include_output.source.lines() {
                    output.push_str(include_source_line);
                    self.output_lines.push(output.clone());
                    output.clear();
                }

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
