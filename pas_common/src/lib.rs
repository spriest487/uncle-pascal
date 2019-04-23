pub mod span;

use {
    crate::span::*,
    std::{
        fmt,
        ops::Deref,
        collections::{
            HashSet,
            hash_map::{
                HashMap,
            },
        },
    },
};

pub use {
    backtrace::Backtrace,
};

pub trait DiagnosticOutput : Spanned {
    fn title(&self) -> String {
        self.to_string()
    }

    fn label(&self) -> Option<String> {
        None
    }

    fn main(&self) -> DiagnosticMessage {
        DiagnosticMessage {
            title: self.title(),
            label: self.label(),
            span: self.span().clone(),
        }
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        Vec::new()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        None
    }
}

#[derive(Clone, Debug)]
pub struct DiagnosticMessage {
    pub title: String,
    pub label: Option<String>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TracedError<T> {
    pub err: T,
    pub bt: Backtrace,
}

impl<T> TracedError<T> {
    pub fn trace(err: T) -> Self {
        const SKIP_FRAMES: usize = 0;

        let mut frames: Vec<_> = Backtrace::new().into();
        frames.rotate_left(SKIP_FRAMES);
        frames.truncate(frames.len() - SKIP_FRAMES);

        Self {
            err,
            bt: frames.into(),
        }
    }

    pub fn chain<TNext: From<T>>(self) -> TracedError<TNext> {
        TracedError {
            err: self.err.into(),
            bt: self.bt
        }
    }
}

impl<T> Deref for TracedError<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.err
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BuildOptions {
    pub case_sensitive: bool,
    pub mode: Mode,

    pp_symbols: HashSet<String>,
    switches: HashMap<String, bool>,
}

impl Default for BuildOptions {
    fn default() -> Self {
        BuildOptions {
            case_sensitive: true,
            mode: Mode::Default,
            pp_symbols: HashSet::new(),
            switches: HashMap::new(),
        }
    }
}

impl BuildOptions {
    pub fn defined(&self, pp_symbol: &str) -> bool {
        self.pp_symbols.contains(pp_symbol)
    }

    pub fn define(&mut self, pp_symbol: String) {
        self.pp_symbols.insert(pp_symbol);
    }

    pub fn undef(&mut self, sym: &str) -> bool {
        self.pp_symbols.remove(sym)
    }

    pub fn set_switch(&mut self, switch: &str, on: bool) {
        self.switches.insert(switch.to_string(), on);
    }

    pub fn strict_switches(&self) -> bool {
        match self.mode {
            Mode::Default => true,
            _ => false,
        }
    }

    pub fn link_lib(&mut self, _lib: String) {
        unimplemented!()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Mode {
    Default,
    Fpc,
    Delphi,
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Mode::Default => "Default",
            Mode::Fpc => "FPC-compatible",
            Mode::Delphi => "Delphi-compatible",
        })
    }
}