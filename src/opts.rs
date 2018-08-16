use std::{
    collections::{
        HashMap,
        HashSet},
    fmt,
};
use linked_hash_set::LinkedHashSet;
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
        /* todo: this should be based on the target platform rather than the compiler platform */
        /* we use to_uppercase for case insensitivity in modes where the preprocessor is
        case-insensitive, so make sure everything added at this stage is uppercase already */
        let mut symbols = HashSet::new();
        if cfg!(target_os = "macos") {
            symbols.insert("DARWIN".to_string());
        } else if cfg!(target_os = "windows") {
            symbols.insert("MSWINDOWS".to_string());
            if cfg!(target_arch = "x86_64") {
                symbols.insert("WIN64".to_string());
            } else {
                symbols.insert("WIN32".to_string());
            }
        } else if cfg!(target_os = "linux") {
            symbols.insert("LINUX".to_string());
        } else {
            eprintln!("warning: unsupported target platform");
        }

        match self {
            Mode::Uncle => ModeProfile {
                mode: self,
                default_switches: HashSet::new(),
                default_symbols: {
                    symbols.insert("UNCLE".to_string());
                    symbols
                },
                strict_switches: true,
                case_sensitive: true,
            },

            Mode::Fpc => ModeProfile {
                mode: self,
                default_symbols: {
                    symbols.insert("FPC".to_string());

                    if cfg!(target_arch = "x86_64") {
                        symbols.insert("CPU64".to_string());
                    }

                    symbols
                },
                default_switches: HashSet::new(),
                strict_switches: false,
                case_sensitive: false,
            },

            Mode::Delphi => ModeProfile {
                mode: self,
                default_symbols: {
                    symbols.insert("DCC".to_string());

                    if cfg!(target_os = "macos") {
                        symbols.insert("MACOS".to_string());
                    }

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

    link_libs: LinkedHashSet<String>,
    lib_paths: LinkedHashSet<String>,
}

impl CompileOptions {
    pub fn new(mode: Mode) -> Self {
        CompileOptions {
            mode: mode.profile(),

            switches: HashMap::new(),
            symbols: HashMap::new(),

            link_libs: LinkedHashSet::new(),
            lib_paths: LinkedHashSet::new(),
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

    fn is_switch(&self, switch: &str) -> bool {
        match self.switches.get(switch) {
            None => self.mode.default_switches.contains(switch),
            Some(on) => *on,
        }
    }

    #[allow(dead_code)]
    pub fn switch(&self, switch: &str) -> bool {
        match self.case_sensitive() {
            true => self.is_switch(switch),
            false => self.is_switch(&switch.to_uppercase()),
        }
    }

    pub fn set_switch(&mut self, switch: &str, on: bool) {
        match self.case_sensitive() {
            true => { self.switches.insert(switch.to_string(), on); },
            false => { self.switches.insert(switch.to_uppercase(), on); },
        }
    }

    fn is_defined(&self, symbol: &str) -> bool {
        match self.symbols.get(symbol) {
            None => self.mode.default_symbols.contains(symbol),
            Some(defined) => *defined,
        }
    }

    pub fn defined(&self, symbol: &str) -> bool {
        match self.case_sensitive() {
            true => self.is_defined(symbol),
            false => self.is_defined(&symbol.to_uppercase())
        }
    }

    pub fn define(&mut self, symbol: String) {
        match self.case_sensitive() {
            true => { self.symbols.insert(symbol, true); },
            false => { self.symbols.insert(symbol.to_uppercase(), true); },
        }
    }

    pub fn undef(&mut self, symbol: &str) -> bool {
        match self.case_sensitive() {
            true => self.symbols.remove(symbol) == Some(true),
            false => self.symbols.remove(&symbol.to_uppercase()) == Some(true),
        }
    }

    pub fn link_lib(&mut self, lib_name: String) {
        self.link_libs.insert(lib_name);
    }

    pub fn link_libs(&self) -> impl Iterator<Item=&String> {
        self.link_libs.iter()
    }

    pub fn lib_path(&mut self, lib_path: String) {
        self.lib_paths.insert(lib_path);
    }

    pub fn lib_paths(&self) -> impl Iterator<Item=&String> {
        self.lib_paths.iter()
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

        for link_lib in matches.opt_strs("l") {
            opts.link_lib(link_lib);
        }

        for lib_path in matches.opt_strs("L") {
            opts.lib_path(lib_path);
        }

        opts
    }
}

impl Default for CompileOptions {
    fn default() -> Self {
        CompileOptions::new(Mode::Uncle)
    }
}
