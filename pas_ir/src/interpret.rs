mod heap;
mod builtin;

use {
    crate::{
        Function as FunctionIR,
        Instruction,
        metadata::*,
        Ref,
        GlobalRef,
        Type,
        Module,
        Value,
    },
    std::{
        collections::HashMap,
        fmt,
        f32,
        ops::Add,
    },
    self::heap::{
        RcHeap,
        RcAddress,
    },
};

#[derive(Clone)]
pub enum Function {
    Builtin {
        func: fn(state: &mut Interpreter),
        ret: Type,
    },
    IR(FunctionIR),
}

impl Function {
    pub fn return_ty(&self) -> &Type {
        match self {
            Function::Builtin { ret, .. } => ret,
            Function::IR(func) => &func.return_ty,
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Builtin { .. } => write!(f, "<native code>"),
            Function::IR(func) => write!(f, "<function with {} instructions>", func.body.len())
        }
    }
}

//#[allow(unused)]
#[derive(Debug, Clone)]
pub enum Pointer {
    Uninit(Type),
    Rc(Type, RcAddress),
}

impl Pointer {
    pub fn ty(&self) -> &Type {
        match self {
            Pointer::Uninit(ty) => ty,
            Pointer::Rc(ty, _) => ty,
        }
    }

    pub fn as_rc_addr(&self) -> Option<RcAddress> {
        match self {
            Pointer::Rc(_ty, addr) => Some(*addr),
            _ => None,
        }
    }
}

impl Add<usize> for Pointer {
    type Output = Self;

    fn add(self, rhs: usize) -> Self {
        match self {
            Pointer::Uninit(ty) => Pointer::Uninit(ty),
            Pointer::Rc(ty, RcAddress(addr)) => Pointer::Rc(ty, RcAddress(addr + rhs)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum MemCell {
    Bool(bool),
    I32(i32),
    F32(f32),
    Function(Function),
    Structure(Vec<MemCell>),
    Pointer(Pointer),
}

impl MemCell {
    pub fn as_struct(&self) -> Option<&Vec<MemCell>> {
        match self {
            MemCell::Structure(fields) => Some(fields),
            _ => None,
        }
    }

    pub fn as_i32(&self) -> Option<i32> {
        match self {
            MemCell::I32(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_pointer(&self) -> Option<&Pointer> {
        match self {
            MemCell::Pointer(ptr) => Some(ptr),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct StackFrame {
    locals: Vec<Option<MemCell>>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InterpreterOpts {
    pub trace_heap: bool,
    pub trace_rc: bool,
    pub trace_ir: bool,
}

#[derive(Debug)]
pub struct Interpreter {
    metadata: Metadata,
    stack: Vec<StackFrame>,
    globals: HashMap<GlobalRef, MemCell>,
    heap: RcHeap,

    trace_rc: bool,
    trace_ir: bool,
}

impl Interpreter {
    pub fn new(opts: &InterpreterOpts) -> Self {
        let mut globals = HashMap::new();

        let int_to_str = Function::Builtin {
            func: builtin::int_to_str,
            ret: Type::Struct(STRING_ID).ptr(),
        };
        globals.insert(GlobalRef::Function("IntToStr".to_string()), MemCell::Function(int_to_str));

        let write_ln = Function::Builtin {
            func: builtin::write_ln,
            ret: Type::Nothing,
        };
        globals.insert(GlobalRef::Function("WriteLn".to_string()), MemCell::Function(write_ln));

        let mut heap = RcHeap::new();
        heap.trace = opts.trace_heap;

        Self {
            metadata: Metadata::new(),
            globals,
            stack: Vec::new(),
            heap,

            trace_rc: opts.trace_rc,
            trace_ir: opts.trace_ir,
        }
    }

    fn init_struct(&mut self, id: StructId) -> MemCell {
        let struct_def = self.metadata.structs()[&id].clone();

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

    fn init_cell(&mut self, ty: &Type) -> MemCell {
        match ty {
            Type::I32 => MemCell::I32(-1),
            Type::Bool => MemCell::Bool(false),
            Type::F32 => MemCell::F32(f32::INFINITY),
            Type::Struct(id) => {
                self.init_struct(*id)
            }

            Type::Pointer(target_ty) => {
                let ptr = Pointer::Uninit((**target_ty).clone());
                MemCell::Pointer(ptr)
            }

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
                    panic!("global cell `{}` is already allocated", name);
                }
                self.globals.insert(name.clone(), val);
            }

            Ref::Deref(inner) => {
                match self.load(inner) {
                    MemCell::Pointer(ptr) => match ptr.clone() {
                        Pointer::Rc(_ty, slot) => {
                            let heap_cell = self.heap.get_mut(slot)
                                .unwrap_or_else(|| panic!("heap cell {} is not allocated", slot));
                            *heap_cell = val;
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
                None | Some(None) => {
                    panic!("local cell {} is not allocated", id);
                }
            }

            Ref::Global(name) => match self.globals.get(name) {
                Some(cell) => cell,
                None => {
                    panic!("global cell `{}` is not allocated", name);
                }
            }

            Ref::Deref(inner) => {
                match self.load(inner) {
                    MemCell::Pointer(ptr) => match ptr {
                        Pointer::Rc(_ty, addr) => {
                            self.heap.get(*addr)
                                .unwrap_or_else(|| panic!("heap cell {} is not allocated", addr))
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

    fn call(&mut self, func: &Function, args: &[MemCell], out: Option<&Ref>) {
        self.push_stack();

        // store empty result at $0 if needed
        let return_ty = func.return_ty();
        if *return_ty != Type::Nothing {
            let result_cell = self.init_cell(return_ty);
            self.current_frame_mut().locals.push(Some(result_cell));
        }

        // store params in either $0.. or $1..
        self.current_frame_mut().locals.extend(args.iter()
            .cloned()
            .map(Some));

        match func {
            Function::Builtin { func, .. } => func(self),
            Function::IR(ir_func) => self.execute(&ir_func.body),
        };

        let result_cell = match return_ty {
            Type::Nothing => None,
            _ => {
                let return_val = self.evaluate(&Value::Ref(Ref::Local(0)));
                Some(return_val)
            }
        };

        self.pop_stack();

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

    fn drop_at(&mut self, at: &Ref) {
        let dropped = self.load(at).clone();
        self.drop_cell(dropped);
    }

    fn drop_cell(&mut self, cell: MemCell) {
        match cell {
            MemCell::Structure(cells) => {
                for cell in cells {
                    self.drop_cell(cell);
                }
            }

            MemCell::Pointer(Pointer::Rc(ty, addr)) => {
                let dead = self.heap.get_rc(addr) == 1;

                if dead {
                    let heap_cell = self.heap.get(addr).cloned().unwrap();
                    self.drop_cell(heap_cell)
                }

                if self.trace_rc {
                    eprintln!("rc: release {} @ {}", ty, addr)
                }
                self.heap.release(addr);
            }

            // can't contain rc pointers
            _ => {}
        }
    }

    fn retain_cell(&mut self, cell: &MemCell) {
        match cell {
            MemCell::Pointer(Pointer::Rc(ty, addr)) => {
                if self.trace_rc {
                    eprintln!("rc: retain {} @ {}", ty, addr);
                }

                self.heap.retain(*addr);
            }

            MemCell::Structure(field_cells) => {
                for field_cell in field_cells {
                    self.retain_cell(field_cell);
                }
            }

            // can't contain rc pointers
            _ => {},
        }
    }

    fn assign(&mut self, at: &Ref, new_val: MemCell) {
        // retain new value of cell
        self.retain_cell(&new_val);

        // release old value of cell
        self.drop_at(at);
        self.store(at, new_val);
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
            if self.trace_ir {
                eprintln!("interpreter: {}", instructions[pc]);
            }

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
                    if *id >= self.current_frame().locals.len() {
                        panic!("local cell {} is not allocated");
                    }

                    self.drop_at(&Ref::Local(*id));
                    self.current_frame_mut().locals[*id] = None;
                }

                Instruction::MemAlloc { out, ty, count, } => {
                    let mut init_cells = Vec::with_capacity(*count);
                    for _ in 0..*count {
                        init_cells.push(self.init_cell(ty));
                    }

                    let heap_addr = self.heap.alloc(init_cells);

                    self.store(out, MemCell::Pointer(Pointer::Rc(ty.clone(), heap_addr)))
                }

                Instruction::Add { out, a, b } => {
                    let out_val = match (self.evaluate(a), self.evaluate(b)) {
                        (MemCell::I32(a), MemCell::I32(b)) => {
                            MemCell::I32(a + b)
                        }

                        (MemCell::F32(a), MemCell::F32(b)) => {
                            MemCell::F32(a + b)
                        }

                        (MemCell::Pointer(Pointer::Rc(ref a_ty, ref a)),
                            MemCell::Pointer(Pointer::Rc(ref b_ty, ref b)))
                        if a_ty == b_ty => {
                            MemCell::Pointer(Pointer::Rc(a_ty.clone(), RcAddress(a.0 + b.0)))
                        }
                        _ => panic!("Add is not valid for {:?} + {:?}", a, b)
                    };

                    self.assign(out, out_val);
                }

                Instruction::Set { out, new_val } => {
                    self.assign(out, self.evaluate(new_val));
                }

                Instruction::Call { out, function, args } => {
                    let arg_cells: Vec<_> = args.iter()
                        .map(|arg_val| self.evaluate(arg_val))
                        .collect();

                    match self.evaluate(function) {
                        MemCell::Function(function) => {
                            self.call(&function, &arg_cells, out.as_ref())
                        }

                        _ => panic!("{} does not reference a function"),
                    };
                }

                Instruction::GetField { out, of, struct_id: _, field_id, } => {
                    match self.load(of).clone() {
                        MemCell::Structure(cells) => {
                            let field_cell = cells.into_iter().skip(*field_id).next().unwrap();
                            self.assign(out, field_cell);
                        }
                        _ => panic!("GetField instruction targeting non-structure cell"),
                    }
                }

                Instruction::SetField { of, new_val, struct_id: _, field_id, } => {
                    match self.load(of).clone() {
                        MemCell::Structure(mut cells) => {
                            cells[*field_id] = self.evaluate(new_val);
                            self.assign(of, MemCell::Structure(cells))
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
            }

            pc += 1;
        }
    }

    pub fn load_module(&mut self, module: &Module) {
        self.metadata.extend(&module.metadata);

        for (func_name, func) in &module.functions {
            let func_cell = MemCell::Function(Function::IR(func.clone()));
            let func_ref = GlobalRef::Function(func_name.clone());

            self.globals.insert(func_ref, func_cell);
        }

        for (id, literal) in module.metadata.strings() {
            let str_cell = self.create_string(literal);
            let str_ref = GlobalRef::StringLiteral(id);

            self.globals.insert(str_ref, str_cell);
        }

        self.push_stack();
        self.execute(&module.init);
        self.pop_stack();
    }

    fn create_string(&mut self, content: &str) -> MemCell {
        let chars: Vec<_> = content.chars()
            .map(|c| MemCell::I32(c as i32))
            .collect();
        let chars_ptr = self.heap.alloc(chars);

        let str_cell = MemCell::Structure(vec![
            MemCell::Pointer(Pointer::Rc(Type::I32, chars_ptr)),
            MemCell::I32(content.len() as i32),
        ]);

        let str_addr = self.heap.alloc(vec![str_cell]);
        let str_ptr = Pointer::Rc(Type::Struct(STRING_ID), str_addr);

        MemCell::Pointer(str_ptr)
    }

    fn read_string(&self, str_ref: &Ref) -> String {
        let (_, string_def) = self.metadata.find_struct("String")
            .expect("String type must exist");

        //todo: we should be reading args from local refs not args array

        let str_fields = self.load(str_ref)
            .as_struct()
            .unwrap_or_else(|| panic!("called read_string on {:?} which didn't hold a struct", str_ref));

        let len = &str_fields[string_def.find_field("len").unwrap()]
            .as_i32().unwrap();

        let chars_addr = &str_fields[string_def.find_field("chars").unwrap()]
            .as_pointer()
            .and_then(|ptr| ptr.as_rc_addr())
            .unwrap_or_else(|| panic!("string contained non-heap-alloced `chars` pointer"));

        let mut chars = Vec::new();
        for i in 0..*len as usize {
            let char_addr = RcAddress(chars_addr.0 + i);
            let char_val = self.heap.get(char_addr)
                .unwrap()
                .as_i32()
                .unwrap_or_else(|| panic!("expected string char @ {}", char_addr));

            chars.push(char_val as u8 as char);
        }

        chars.into_iter().collect()
    }
}

impl Drop for Interpreter {
    fn drop(&mut self) {
        let globals: Vec<_> = self.globals.keys().cloned().collect();
        for global in globals {
            self.drop_at(&Ref::Global(global));
        }
    }
}