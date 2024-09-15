use std::{fmt, slice};
use std::fmt::Formatter;
use common::span::Span;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StackTrace {
    frames: Vec<StackTraceFrame>,     
}

impl StackTrace {
    pub fn new(frames: impl IntoIterator<Item=StackTraceFrame>) -> Self {
        let mut stack_trace = Self { frames: frames.into_iter().collect() };
        
        if stack_trace.frames.is_empty() {
            stack_trace.frames.push(StackTraceFrame {
                location: Span::zero(""),
                name: String::new(),
            });
        }
        
        stack_trace
    }
    
    pub fn top(&self) -> &StackTraceFrame {
        &self.frames[0]
    }
    
    pub fn frames(&self) -> impl Iterator<Item=&StackTraceFrame> {
        self.frames.iter()
    }
    
    pub fn is_empty(&self) -> bool {
        self.frames.len() == 1 && self.frames[0].name == ""
    }
}

impl<'a> IntoIterator for &'a StackTrace {
    type Item = &'a StackTraceFrame;
    type IntoIter = slice::Iter<'a, StackTraceFrame>;

    fn into_iter(self) -> Self::IntoIter {
        self.frames.iter()
    }
}


impl fmt::Display for StackTrace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            return Ok(());
        }
        
        for i in 0..self.frames.len() {
            if i > 0 {
                writeln!(f)?;
            }

            write!(f, "{}", self.frames[i])?;
        }
        
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StackTraceFrame {
    pub name: String,
    pub location: Span,
}

impl fmt::Display for StackTraceFrame {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.name, self.location)
    }
}
