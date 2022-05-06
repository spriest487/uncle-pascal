pub mod span;
pub mod source_map;

use crate::span::*;
use std::{
    cmp::Ordering,
    collections::{hash_map::HashMap, HashSet},
    env, fmt,
    ops::Deref,
    path::Path,
};

pub use backtrace::Backtrace;

pub trait DiagnosticOutput: fmt::Display {
    fn title(&self) -> String {
        self.to_string()
    }

    fn label(&self) -> Option<DiagnosticLabel> {
        None
    }

    fn notes(&self) -> Vec<String> {
        Vec::new()
    }

    fn main(&self) -> DiagnosticMessage {
        let title = self.title();
        let label = self.label();
        let notes = self.notes().to_vec();

        DiagnosticMessage {
            title,
            label,
            notes,
        }
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        Vec::new()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        None
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DiagnosticLabel {
    pub text: Option<String>,
    pub span: Span,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DiagnosticMessage {
    pub title: String,
    pub notes: Vec<String>,
    pub label: Option<DiagnosticLabel>,
}

impl Ord for DiagnosticLabel {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.span.file.as_ref().cmp(other.span.file.as_ref()) {
            Ordering::Equal => match self.span.end.cmp(&other.span.end) {
                Ordering::Equal => self.span.start.cmp(&other.span.start),
                end_ord => end_ord,
            },
            file_ord => file_ord,
        }
    }
}

impl PartialOrd for DiagnosticLabel {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for DiagnosticMessage {
    fn cmp(&self, other: &Self) -> Ordering {
        self.label.cmp(&other.label)
    }
}

impl PartialOrd for DiagnosticMessage {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
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
            bt: self.bt,
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
    pub lang_mode: LanguageMode,

    pub verbose: bool,

    pp_symbols: HashSet<String>,
    switches: HashMap<String, bool>,
}

impl Default for BuildOptions {
    fn default() -> Self {
        BuildOptions {
            verbose: false,
            case_sensitive: true,
            lang_mode: LanguageMode::Default,
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
        match self.lang_mode {
            LanguageMode::Default => true,
            _ => false,
        }
    }

    pub fn link_lib(&mut self, _lib: String) {
        unimplemented!()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum LanguageMode {
    Default,
    Fpc,
    Delphi,
}

impl fmt::Display for LanguageMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LanguageMode::Default => "Default",
                LanguageMode::Fpc => "FPC-compatible",
                LanguageMode::Delphi => "Delphi-compatible",
            }
        )
    }
}

pub fn path_relative_to_cwd(path: &Path) -> &Path {
    env::current_dir()
        .ok()
        .and_then(|cwd| cwd.canonicalize().ok())
        .and_then(|cwd| path.strip_prefix(cwd).ok())
        .unwrap_or_else(|| path)
}
