use {
    std::{
        fmt,
        collections::HashMap,
    },
    crate::{
        Instruction,
        Type,
        Value,
    },
};

mod builtin {
    use super::MemCell;

    pub(super) fn write_ln(args: &[MemCell]) -> Option<MemCell> {
        assert_eq!(1, args.len(), "writeln expected 1 argument");
        match &args[0] {
            MemCell::I32(int) => {
                println!("{}", int);
                None
            },
            _ => panic!("write_ln expected i32 argument")
        }
    }
}

#[derive(Clone)]
enum Function {
    Builtin(fn(args: &[MemCell]) -> Option<MemCell>)
}

impl Function {
    fn invoke(&self, args: &[MemCell]) -> Option<MemCell> {
        match self {
            Function::Builtin(builtin_fn) => builtin_fn(args),
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Builtin(_) => write!(f, "<native code>"),
        }
    }
}

#[derive(Debug, Clone)]
enum MemCell {
    I32(i32),
    Function(Function),
}

//const NONE_CELL: Option<MemCell> = None;

#[derive(Debug)]
pub struct Interpreter {
    locals: Vec<Option<MemCell>>,
    globals: HashMap<String, MemCell>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert("WriteLn".to_string(), MemCell::Function(
            Function::Builtin(builtin::write_ln)));

        Self {
            globals,
            locals: Vec::new(),
        }
    }

    fn store(&mut self, at: &Value, val: MemCell) {
        match at {
            Value::LiteralI32(_) => {
                panic!("literal is not a valid storage location")
            },

            Value::Local(id) => {
                match self.locals.get_mut(*id) {
                    Some(Some(cell)) => { *cell = val; },
                    None | Some(None) => panic!("local cell {} is not allocated", id),
                }
            }

            Value::Global(name) => {
                if self.globals.contains_key(name) {
                    panic!("global call {} is already allocated");
                }
                self.globals.insert(name.clone(), val);
            }
        }
    }

    fn load(&self, at: &Value) -> MemCell {
        match at {
            Value::Local(id) => match self.locals.get(*id) {
                Some(Some(cell)) => cell.clone(),
                None | Some(None) => panic!("local cell {} is not allocated", id),
            }

            Value::LiteralI32(i) => MemCell::I32(*i),

            Value::Global(name) => match self.globals.get(name) {
                Some(cell) => cell.clone(),
                None => panic!("global cell {} is not allocated", name),
            }
        }
    }

    pub fn execute(&mut self, instructions: &[Instruction]) {
        for instruction in instructions.iter() {
            match instruction {
                Instruction::LocalAlloc(id, ty) => {
                    while self.locals.len() <= *id {
                        self.locals.push(None);
                    }

                    match self.locals[*id]  {
                        None => {
                            let default_val = match ty {
                                Type::I32 => MemCell::I32(0),
                                _ => panic!("can't allocate local cell of type `{:?}`", ty),
                            };

                            self.locals[*id] = Some(default_val);
                        }

                        _ => panic!("local cell {} is already allocated"),
                    }
                }

                Instruction::LocalDelete(id) => {
                    match self.locals[*id] {
                        None => panic!("local cell {} is not allocated", id),
                        Some(_) => self.locals[*id] = None,
                    }
                }

                Instruction::Add { out, a, b } => {
                    let out_val = match (self.load(a), self.load(b)) {
                        (MemCell::I32(a), MemCell::I32(b)) => MemCell::I32(a + b),
                        _ => panic!("Add is not valid for {:?} + {:?}", a, b)
                    };

                    self.store(out, out_val);
                }

                Instruction::Set { out, new_val } => {
                    self.store(out, self.load(new_val));
                }

                Instruction::Call { out, function, args } => {
                    let arg_cells: Vec<_> = args.iter()
                        .map(|arg_val| self.load(arg_val))
                        .collect();

                    let result = match self.load(function) {
                        MemCell::Function(function) => {
                            function.invoke(&arg_cells)
                        }

                        _ => panic!("{} does not reference a function"),
                    };

                    if let Some(out_val) = out {
                        let result_cell = result.expect("called function must return something if Call instruction references an out value");
                        self.store(out_val, result_cell);
                    }
                }
            }
        }
    }
}