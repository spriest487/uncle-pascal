use {
    std::{
        fmt,
        collections::HashMap,
    },
    crate::{
        Function as FunctionIR,
        Instruction,
        Type,
        Value,
        Unit,
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
            }
            _ => panic!("write_ln expected i32 argument")
        }
    }
}

#[derive(Clone)]
enum Function {
    Builtin(fn(args: &[MemCell]) -> Option<MemCell>),
    IR(FunctionIR),
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Builtin(_) => write!(f, "<native code>"),
            Function::IR(func) => write!(f, "<function with {} instructions>", func.body.len())
        }
    }
}

#[derive(Debug, Clone)]
enum MemCell {
    I32(i32),
    Function(Function),
}

impl MemCell {
    fn default_of(ty: &Type) -> MemCell {
        match ty {
            Type::I32 => MemCell::I32(-1),
            _ => panic!("can't initialize default cell of type `{:?}`", ty),
        }
    }
}

#[derive(Debug)]
struct StackFrame {
    locals: Vec<Option<MemCell>>,
}

#[derive(Debug)]
pub struct Interpreter {
    stack: Vec<StackFrame>,
    globals: HashMap<String, MemCell>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert("WriteLn".to_string(), MemCell::Function(
            Function::Builtin(builtin::write_ln)));

        Self {
            globals,
            stack: Vec::new(),
        }
    }

    fn store(&mut self, at: &Value, val: MemCell) {
        match at {
            Value::LiteralI32(_) => {
                panic!("literal is not a valid storage location")
            }

            Value::Local(id) => {
                match self.current_frame_mut().locals.get_mut(*id) {
                    Some(Some(cell)) => { *cell = val; }
                    None | Some(None) => panic!("local cell {} is not allocated", id),
                }
            }

            Value::Global(name) => {
                if self.globals.contains_key(name) {
                    panic!("global cell {} is already allocated");
                }
                self.globals.insert(name.clone(), val);
            }
        }
    }

    fn load(&self, at: &Value) -> MemCell {
        match at {
            Value::Local(id) => match self.current_frame().locals.get(*id) {
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

    fn push_stack(&mut self) {
        self.stack.push(StackFrame {
            locals: Vec::new()
        });
    }

    fn pop_stack(&mut self) {
        self.stack.pop().expect("popped stack with no stackframes");
    }

    fn current_frame(&self) -> &StackFrame {
        self.stack.last().expect("called current_frame without no stackframes")
    }

    fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.stack.last_mut().expect("called current_frame without no stackframes")
    }

    fn invoke(&mut self, func: &Function, args: &[MemCell], out: Option<&Value>) {
        let result_cell = match func {
            Function::Builtin(builtin_fn) => builtin_fn(args),

            Function::IR(ir_func) => {
                self.push_stack();

                // store empty result at $0 if needed
                if let Some(return_ty) = &ir_func.return_ty {
                    self.current_frame_mut().locals.push(Some(MemCell::default_of(return_ty)));
                }

                // store params in either $0.. or $1..
                self.current_frame_mut().locals.extend(args.iter()
                    .cloned()
                    .map(Some));

                self.execute(&ir_func.body);

                let result = match &ir_func.return_ty {
                    Some(_) => Some(self.load(&Value::Local(0))),
                    None => None,
                };

                self.pop_stack();
                result
            }
        };

        match (result_cell, out) {
            (Some(result_cell), Some(out_val)) => {
                self.store(&out_val, result_cell);
            }

            (None, Some(_)) => {
                panic!("called function which has no return type in a context where a return value was expected");
            }

            // ok, no output expected, ignore result if there is one
            (_, None) => {}
        }
    }

    pub fn execute(&mut self, instructions: &[Instruction]) {
        for instruction in instructions.iter() {
            match instruction {
                Instruction::LocalAlloc(id, ty) => {
                    while self.current_frame().locals.len() <= *id {
                        self.current_frame_mut().locals.push(None);
                    }

                    match self.current_frame().locals[*id] {
                        None => self.current_frame_mut().locals[*id] = Some(MemCell::default_of(ty)),
                        _ => panic!("local cell {} is already allocated"),
                    }
                }

                Instruction::LocalDelete(id) => {
                    match self.current_frame().locals[*id] {
                        None => panic!("local cell {} is not allocated", id),
                        Some(_) => self.current_frame_mut().locals[*id] = None,
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

                    match self.load(function) {
                        MemCell::Function(function) => {
                            self.invoke(&function, &arg_cells, out.as_ref())
                        }

                        _ => panic!("{} does not reference a function"),
                    };
                }

                Instruction::Member { out, of, struct_name, member, } => {
                    unimplemented!("member {} {} {}.{}", out, of, struct_name, member)
                }
            }
        }
    }

    pub fn load_unit(&mut self, unit: &Unit) {
        for (func_name, func) in &unit.functions {
            let func_loc = Value::Global(func_name.clone());
            let func_cell = MemCell::Function(Function::IR(func.clone()));
            self.store(&func_loc, func_cell);
        }

        self.push_stack();
        self.execute(&unit.init);
        self.pop_stack();
    }
}