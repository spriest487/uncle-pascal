use {
    crate::{
        Function as FunctionIR,
        Instruction,
        metadata::*,
        Ref,
        Type,
        Unit,
        Value,
    },
    std::{
        collections::HashMap,
        fmt,
        f32,
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
enum Pointer {
    Uninit(Type),
    Rc(Type, usize),
//    Stack { frame: usize, local: usize, },
}

#[derive(Debug, Clone)]
enum MemCell {
    Bool(bool),
    I32(i32),
    F32(f32),
    Function(Function),
    Structure(Vec<MemCell>),
    Pointer(Pointer),
}

#[derive(Debug, Clone)]
struct HeapCell {
    ref_count: usize,
    val: MemCell,
}

#[derive(Debug)]
struct StackFrame {
    locals: Vec<Option<MemCell>>,
}

#[derive(Debug)]
pub struct Interpreter {
    metadata: Metadata,
    stack: Vec<StackFrame>,
    globals: HashMap<String, MemCell>,
    heap: Vec<Option<HeapCell>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        globals.insert("WriteLn".to_string(), MemCell::Function(
            Function::Builtin(builtin::write_ln)));

        Self {
            metadata: Metadata::new(),
            globals,
            stack: Vec::new(),
            heap: Vec::new(),
        }
    }

    fn init_cell(&self, ty: &Type) -> MemCell {
        match ty {
            Type::I32 => MemCell::I32(-1),
            Type::Bool => MemCell::Bool(false),
            Type::F32 => MemCell::F32(f32::INFINITY),
            Type::Struct(id) => {
                let struct_def = &self.metadata.structs()[id];

                let mut field_cells = Vec::new();
                for (&id, field) in &struct_def.fields {
                    // include padding of -1s for non-contiguous IDs
                    if id >= field_cells.len() {
                        field_cells.resize(id + 1, MemCell::I32(-1));
                    }
                    field_cells[id] = self.init_cell(&field.ty);
                }

                MemCell::Structure(field_cells)
            }

            Type::Pointer(target_ty) => {
                let ptr = Pointer::Uninit((**target_ty).clone());
                MemCell::Pointer(ptr)
            },

            _ => panic!("can't initialize default cell of type `{:?}`", ty),
        }
    }

    fn store(&mut self, at: &Ref, val: MemCell) {
        match at {
            Ref::Local(id) => {
                match self.current_frame_mut().locals.get_mut(*id) {
                    Some(Some(cell)) => { *cell = val; }
                    None | Some(None) => panic!("local cell {} is not allocated", id),
                }
            }

            Ref::Global(name) => {
                if self.globals.contains_key(name) {
                    panic!("global cell {} is already allocated");
                }
                self.globals.insert(name.clone(), val);
            }

            Ref::Deref(inner) => {
                match self.load(inner) {
                    MemCell::Pointer(ptr) => match ptr.clone() {
                        Pointer::Rc(_ty, slot) => {
                            let heap_cell = &mut self.heap.get_mut(slot)
                                .and_then(|cell_slot| cell_slot.as_mut())
                                .unwrap_or_else(|| panic!("heap cell {} is not allocated", slot));
                            heap_cell.val = val;
                        }

                        Pointer::Uninit(_) => {
                            panic!("dereferencing {:?} which contains an uninitialized pointer", inner);
                        }
                    },

                    x => panic!("can't deref non-pointer cell with value {:?}", x),
                }
            }
        }
    }

    fn load(&self, at: &Ref) -> &MemCell {
        match at {
            Ref::Local(id) => match self.current_frame().locals.get(*id) {
                Some(Some(cell)) => cell,
                None | Some(None) => panic!("local cell {} is not allocated", id),
            }

            Ref::Global(name) => match self.globals.get(name) {
                Some(cell) => cell,
                None => panic!("global cell {} is not allocated", name),
            }

            Ref::Deref(inner) => {
                match self.load(inner) {
                    MemCell::Pointer(ptr) => match ptr {
                        Pointer::Rc(_ty, slot) => {
                            let rc_cell = self.heap.get(*slot)
                                .and_then(|slot_val| slot_val.as_ref())
                                .unwrap_or_else(|| panic!("heap cell {} is not allocated"));

                            &rc_cell.val
                        }

                        Pointer::Uninit(_ty) => {
                            panic!("dereferencing {:?} which contains an uninitialized pointer", inner)
                        }
                    },

                    x => panic!("can't derefence cell {:?}", x),
                }
            }
        }
    }

    fn evaluate(&self, val: &Value) -> MemCell {
        match val {
            Value::Ref(r) => self.load(r).clone(),

            Value::LiteralI32(i) => MemCell::I32(*i),
            Value::LiteralF32(f) => MemCell::F32(*f),
            Value::LiteralBool(b) => MemCell::Bool(*b),
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

    fn invoke(&mut self, func: &Function, args: &[MemCell], out: Option<&Ref>) {
        let result_cell = match func {
            Function::Builtin(builtin_fn) => builtin_fn(args),

            Function::IR(ir_func) => {
                self.push_stack();

                // store empty result at $0 if needed
                if let Some(return_ty) = &ir_func.return_ty {
                    let result_cell = self.init_cell(return_ty);
                    self.current_frame_mut().locals.push(Some(result_cell));
                }

                // store params in either $0.. or $1..
                self.current_frame_mut().locals.extend(args.iter()
                    .cloned()
                    .map(Some));

                self.execute(&ir_func.body);

                let result = match &ir_func.return_ty {
                    Some(_) => {
                        let return_val = self.evaluate(&Value::Ref(Ref::Local(0)));
                        Some(return_val)
                    }
                    None => None,
                };

                self.pop_stack();
                result
            }
        };

        match (result_cell, out) {
            (Some(result_cell), Some(out_at)) => {
                self.store(&out_at, result_cell);
            }

            (None, Some(_)) => {
                panic!("called function which has no return type in a context where a return value was expected");
            }

            // ok, no output expected, ignore result if there is one
            (_, None) => {}
        }
    }

    pub fn execute(&mut self, instructions: &[Instruction]) {
        let labels: HashMap<_, _> = instructions.iter().enumerate()
            .filter_map(|(off, instruction)| match instruction {
                Instruction::Label(label) => Some((*label, off)),
                _ => None,
            })
            .collect();

        let mut pc = 0;
        while pc < instructions.len() {
            match &instructions[pc] {
                Instruction::LocalAlloc(id, ty) => {
                    while self.current_frame().locals.len() <= *id {
                        self.current_frame_mut().locals.push(None);
                    }

                    match self.current_frame().locals[*id] {
                        None => self.current_frame_mut().locals[*id] = Some(self.init_cell(ty)),
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
                    let out_val = match (self.evaluate(a), self.evaluate(b)) {
                        (MemCell::I32(a), MemCell::I32(b)) => MemCell::I32(a + b),
                        _ => panic!("Add is not valid for {:?} + {:?}", a, b)
                    };

                    self.store(out, out_val);
                }

                Instruction::Set { out, new_val } => {
                    self.store(out, self.evaluate(new_val));
                }

                Instruction::Call { out, function, args } => {
                    let arg_cells: Vec<_> = args.iter()
                        .map(|arg_val| self.evaluate(arg_val))
                        .collect();

                    match self.evaluate(function) {
                        MemCell::Function(function) => {
                            self.invoke(&function, &arg_cells, out.as_ref())
                        }

                        _ => panic!("{} does not reference a function"),
                    };
                }

                Instruction::GetField { out, of, struct_id: _, field_id, } => {
                    match self.load(of).clone() {
                        MemCell::Structure(cells) => {
                            let field_cell = cells.into_iter().skip(*field_id).next().unwrap();
                            self.store(out, field_cell);
                        }
                        _ => panic!("GetField instruction targeting non-structure cell"),
                    }
                }

                Instruction::SetField { of, new_val, struct_id: _, field_id, } => {
                    match self.load(of).clone() {
                        MemCell::Structure(mut cells) => {
                            cells[*field_id] = self.evaluate(new_val);
                            self.store(of, MemCell::Structure(cells))
                        }

                        _ => panic!("SetField instruction targeting non-structure cell"),
                    }
                }

                Instruction::Label(_) => {
                    // noop
                }

                Instruction::Jump { dest, } => {
                    pc = labels[dest];
                    continue;
                }

                Instruction::JumpIf { dest, test } => {
                    match self.evaluate(test) {
                        MemCell::Bool(true) => { pc = labels[dest]; }
                        MemCell::Bool(false) => {}
                        _ => panic!("JumpIf instruction testing non-boolean cell"),
                    }
                }

                Instruction::MemAlloc { out, ty, } => {
                    let heap_cell = HeapCell {
                        ref_count: 1,
                        val: self.init_cell(ty),
                    };

                    let empty_cell = self.heap.iter().enumerate()
                        .find_map(|(i, slot)| match slot {
                            None => Some(i),
                            Some(_) => None,
                        });

                    let slot = match empty_cell {
                        Some(empty) => {
                            self.heap[empty] = Some(heap_cell);
                            empty
                        },
                        None => {
                            self.heap.push(Some(heap_cell));
                            self.heap.len() - 1
                        }
                    };

                    let ptr = Pointer::Rc(ty.clone(), slot);
                    self.store(out, MemCell::Pointer(ptr));
                }
            }

            pc += 1;
        }
    }

    pub fn load_unit(&mut self, unit: &Unit) {
        self.metadata.extend(&unit.metadata);

        for (func_name, func) in &unit.functions {
            let func_loc = Ref::Global(func_name.clone());
            let func_cell = MemCell::Function(Function::IR(func.clone()));
            self.store(&func_loc, func_cell);
        }

        self.push_stack();
        self.execute(&unit.init);
        self.pop_stack();
    }
}