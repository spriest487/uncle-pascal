use std::{
    collections::{
        HashMap,
        HashSet},
    fmt,
};
use getopts;

#[derive(Clone, Debug)]
struct ModeProfile {
    pub mode: Mode,
    pub default_switches: HashSet<String>,
    pub default_symbols: HashSet<String>,
    pub case_sensitive: bool,
    pub strict_switches: bool,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Mode {
    Uncle,
    Fpc,
    Delphi,
}

impl Mode {
    fn profile(self) -> ModeProfile {
        match self {
            Mode::Uncle => ModeProfile {
                mode: self,
                default_switches: HashSet::new(),
                default_symbols: {
                    let mut symbols = HashSet::new();
                    symbols.insert("UNCLE".to_string());
                    symbols
                },
                strict_switches: true,
                case_sensitive: true,
            },

            Mode::Fpc => ModeProfile {
                mode: self,
                default_symbols: {
                    let mut symbols = HashSet::new();
                    symbols.insert("FPC".to_string());
                    symbols
                },
                default_switches: HashSet::new(),
                strict_switches: false,
                case_sensitive: false,
            },

            Mode::Delphi => ModeProfile {
                mode: self,
                default_symbols: {
                    let mut symbols = HashSet::new();
                    symbols.insert("DCC".to_string());
                    symbols
                },
                default_switches: HashSet::new(),
                strict_switches: false,
                case_sensitive: false,
            },
        }
    }
}


impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Mode::Uncle => "uncle",
            Mode::Fpc => "fpc",
            Mode::Delphi => "delphi",
        })
    }
}

#[derive(Clone, Debug)]
pub struct CompileOptions {
    mode: ModeProfile,

    switches: HashMap<String, bool>,
    symbols: HashMap<String, bool>,
}

impl CompileOptions {
    pub fn new(mode: Mode) -> Self {
        CompileOptions {
            mode: mode.profile(),
            switches: HashMap::new(),
            symbols: HashMap::new(),
        }
    }

    pub fn case_sensitive(&self) -> bool {
        self.mode.case_sensitive
    }

    pub fn strict_switches(&self) -> bool {
        self.mode.strict_switches
    }

    pub fn mode(&self) -> Mode {
        self.mode.mode
    }

    pub fn set_mode(&mut self, mode: Mode) {
        self.mode = mode.profile();
    }

    #[allow(dead_code)]
    pub fn switch(&self, switch: &str) -> bool {
        match self.switches.get(switch) {
            None => self.mode.default_switches.contains(switch),
            Some(on) => *on,
        }
    }

    pub fn set_switch(&mut self, switch: &str, on: bool) {
        self.switches.insert(switch.to_string(), on);
    }

    pub fn defined(&self, symbol: &str) -> bool {
        match self.symbols.get(symbol) {
            None => self.mode.default_symbols.contains(symbol),
            Some(defined) => *defined,
        }
    }

    pub fn define(&mut self, symbol: String) {
        self.symbols.insert(symbol, true);
    }

    pub fn undef(&mut self, symbol: &str) -> bool {
        self.symbols.remove(symbol) == Some(true)
    }

    pub fn from_getopts(matches: &getopts::Matches) -> Self {
        let mode = match matches.opt_str("mode") {
            Some(mode_opt) => match mode_opt.as_str() {
                "fpc" => Mode::Fpc,
                "delphi" => Mode::Delphi,
                "uncle" => Mode::Uncle,

                unrecognized @ _ => {
                    eprintln!("unrecognized -mode option: {}", unrecognized);
                    Mode::Uncle
                },
            },
            None =>
                Mode::Uncle,
        };

        let mut opts = CompileOptions::new(mode);

        for defined in matches.opt_strs("D") {
            opts.define(defined);
        }

        opts
    }
}

impl Default for CompileOptions {
    fn default() -> Self {
        CompileOptions::new(Mode::Uncle)
    }
}