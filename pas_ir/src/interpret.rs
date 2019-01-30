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
        ops::{
            Add,
            Sub,
        },
    },
    self::heap::{
        Heap,
        HeapAddress,
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
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Pointer {
    Null,
    Uninit(Type),
    Heap(Type, HeapAddress),
    Local { frame: usize, id: usize, ty: Type },
}

impl Pointer {
    pub fn ty(&self) -> &Type {
        match self {
            Pointer::Null => &Type::Nothing,
            Pointer::Uninit(ty) => ty,
            Pointer::Heap(ty, _) => ty,
            Pointer::Local { ty, .. } => ty,
        }
    }

    pub fn as_heap_addr(&self) -> Option<HeapAddress> {
        match self {
            Pointer::Heap(_ty, addr) => Some(*addr),
            _ => None,
        }
    }
}

impl Add<usize> for Pointer {
    type Output = Self;

    fn add(self, rhs: usize) -> Self {
        match self {
            Pointer::Null => Pointer::Null,
            Pointer::Uninit(ty) => Pointer::Uninit(ty),
            Pointer::Local { frame, id, ty } => Pointer::Local { frame, id: id + rhs, ty },
            Pointer::Heap(ty, HeapAddress(addr)) => Pointer::Heap(ty, HeapAddress(addr + rhs)),
        }
    }
}

impl Sub<usize> for Pointer {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self {
        match self {
            Pointer::Null => Pointer::Null,
            Pointer::Uninit(ty) => Pointer::Uninit(ty),
            Pointer::Local { frame, id, ty } => Pointer::Local { frame, id: id - rhs, ty },
            Pointer::Heap(ty, HeapAddress(addr)) => Pointer::Heap(ty, HeapAddress(addr - rhs)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructCell {
    pub id: StructId,
    pub fields: Vec<MemCell>,
}

impl PartialEq<Self> for StructCell {
    fn eq(&self, other: &Self) -> bool {
        if self.id != other.id || self.fields.len() != other.fields.len() {
            return false;
        }

        self.fields.iter().zip(other.fields.iter())
            .all(|(a, b)| match a.try_eq(b) {
                Some(eq) => eq,
                None => panic!("structs can only contain comparable fields")
            })
    }
}

#[derive(Debug, Clone)]
pub enum MemCell {
    Bool(bool),
    U8(u8),
    I32(i32),
    F32(f32),
    Function(Function),
    Structure(StructCell),
    Pointer(Pointer),
}

impl MemCell {
    pub fn try_eq(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (MemCell::Bool(a), MemCell::Bool(b)) => Some(a == b),
            (MemCell::U8(a), MemCell::U8(b)) => Some(a == b),
            (MemCell::I32(a), MemCell::I32(b)) => Some(a == b),
            (MemCell::F32(a), MemCell::F32(b)) => Some(a == b),
            (MemCell::Pointer(a), MemCell::Pointer(b)) => Some(a == b),
            (MemCell::Structure(a), MemCell::Structure(b)) => Some(a == b),
            _ => None,
        }
    }

    pub fn try_add(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => {
                Some(MemCell::I32(a + b))
            }

            (MemCell::F32(a), MemCell::F32(b)) => {
                Some(MemCell::F32(a + b))
            }

            (MemCell::Pointer(ptr), MemCell::I32(offset)) => {
                Some(MemCell::Pointer(ptr.clone() + *offset as usize))
            }

            (MemCell::Pointer(Pointer::Heap(ref a_ty, ref a)),
                MemCell::Pointer(Pointer::Heap(ref b_ty, ref b)))
            if a_ty == b_ty => {
                Some(MemCell::Pointer(Pointer::Heap(a_ty.clone(), HeapAddress(a.0 + b.0))))
            }

            _ => None,
        }
    }

    pub fn try_sub(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => {
                Some(MemCell::I32(a - b))
            }

            (MemCell::F32(a), MemCell::F32(b)) => {
                Some(MemCell::F32(a - b))
            }

            (MemCell::Pointer(ptr), MemCell::I32(offset)) => {
                Some(MemCell::Pointer(ptr.clone() - *offset as usize))
            }

            (MemCell::Pointer(Pointer::Heap(ref a_ty, ref a)),
                MemCell::Pointer(Pointer::Heap(ref b_ty, ref b)))
            if a_ty == b_ty => {
                Some(MemCell::Pointer(Pointer::Heap(a_ty.clone(), HeapAddress(a.0 - b.0))))
            }

            _ => None,
        }
    }

    pub fn as_struct_mut(&mut self, struct_id: StructId) -> Option<&mut StructCell> {
        match self {
            MemCell::Structure(struct_cell) if struct_id == struct_cell.id => Some(struct_cell),
            _ => None,
        }
    }

    pub fn as_struct(&self, struct_id: StructId) -> Option<&StructCell> {
        match self {
            MemCell::Structure(struct_cell) if struct_id == struct_cell.id => Some(struct_cell),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            MemCell::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_u8(&self) -> Option<u8> {
        match self {
            MemCell::U8(x) => Some(*x),
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

    fn value_ty(&self) -> Type {
        match self {
            MemCell::Pointer(ptr) => ptr.ty().clone().ptr(),
            MemCell::Bool(_) => Type::Bool,
            MemCell::U8(_) => Type::U8,
            MemCell::I32(_) => Type::I32,
            MemCell::F32(_) => Type::F32,
            MemCell::Structure(StructCell { id, .. }) => Type::Struct(*id),

            MemCell::Function(_) => Type::Nothing,
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
    heap: Heap,

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

        let get_mem = Function::Builtin {
            func: builtin::get_mem,
            ret: Type::U8.ptr(),
        };
        globals.insert(GlobalRef::Function("GetMem".to_string()), MemCell::Function(get_mem));

        let free_mem = Function::Builtin {
            func: builtin::free_mem,
            ret: Type::Nothing,
        };
        globals.insert(GlobalRef::Function("FreeMem".to_string()), MemCell::Function(free_mem));

        let mut heap = Heap::new();
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

        let mut fields = Vec::new();
        for (&id, field) in &struct_def.fields {
            // include padding of -1s for non-contiguous IDs
            if id >= fields.len() {
                fields.resize(id + 1, MemCell::I32(-1));
            }
            fields[id] = self.init_cell(&field.ty);
        }

        MemCell::Structure(StructCell { id, fields })
    }

    fn init_cell(&mut self, ty: &Type) -> MemCell {
        match ty {
            Type::I32 => MemCell::I32(-1),
            Type::Bool => MemCell::Bool(false),
            Type::F32 => MemCell::F32(f32::INFINITY),
            Type::Struct(id) => {
                self.init_struct(*id)
            }

            Type::Rc(_) => {
                self.init_struct(RC_ID)
            }

            Type::Pointer(target) => {
                let ptr = Pointer::Uninit((**target).clone());
                MemCell::Pointer(ptr)
            }

            _ => panic!("can't initialize default cell of type `{:?}`", ty),
        }
    }

    fn deref_ptr(&self, ptr: &Pointer) -> &MemCell {
        match ptr {
            Pointer::Heap(_ty, slot) => {
                self.heap.get(*slot)
                    .unwrap_or_else(|| panic!("heap cell {} is not allocated", slot))
            }

            Pointer::Local { frame, id, .. } => {
                let locals = &self.stack[*frame].locals;
                locals[*id].as_ref()
                    .unwrap_or_else(|| panic!("local cell {}.{} is not allocated", frame, id))
            }

            Pointer::Null => {
                panic!("derefencing null pointer");
            }

            Pointer::Uninit(ty) => {
                panic!("dereferencing uninitialized {} pointer", ty);
            }
        }
    }

    fn deref_ptr_mut(&mut self, ptr: &Pointer) -> &mut MemCell {
        match ptr {
            Pointer::Heap(_ty, slot) => {
                self.heap.get_mut(*slot)
                    .unwrap_or_else(|| panic!("heap cell {} is not allocated", slot))
            }

            Pointer::Local { frame, id, .. } => {
                let locals = &mut self.stack[*frame].locals;
                locals[*id].as_mut()
                    .unwrap_or_else(|| panic!("local cell {}.{} is not allocated", frame, id))
            }

            Pointer::Uninit(ty) => {
                panic!("dereferencing uninitialized {} pointer", ty);
            }

            Pointer::Null => {
                panic!("dereferencing null pointer")
            }
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
                match self.evaluate(inner) {
                    MemCell::Pointer(ptr) => {
                        *self.deref_ptr_mut(&ptr) = val;
                    }

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
                match self.evaluate(inner) {
                    MemCell::Pointer(ptr) => self.deref_ptr(&ptr),
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
            Value::LiteralNull => MemCell::Pointer(Pointer::Null),
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

    // todo: this should be handled in the IR so we don't need to know cell types
    fn release_cell(&mut self, cell: &MemCell) {
        match cell {
            MemCell::Structure(StructCell { fields, .. }) => {
                for cell in fields {
                    self.release_cell(cell);
                }
            }

            MemCell::Pointer(Pointer::Heap(ty, addr)) => {
                let rc_cell = self.heap.get_mut(*addr)
                    .and_then(|cell| cell.as_struct_mut(RC_ID));

                if let Some(rc_cell) = rc_cell {
                    let rc = rc_cell.fields[RC_REF_COUNT_FIELD].as_i32().unwrap();

                    if rc == 1 {
                        let val_addr = rc_cell.fields[RC_VALUE_FIELD].as_pointer()
                            .and_then(|ptr| ptr.as_heap_addr())
                            .unwrap();

                        let heap_cell = self.heap.get(val_addr).cloned().unwrap();
                        self.release_cell(&heap_cell);

                        if self.trace_rc {
                            eprintln!("rc: free {} @ {}", ty, addr)
                        }

                        self.heap.free(val_addr);
                        self.heap.free(*addr);
                    } else {
                        assert!(rc > 1);
                        if self.trace_rc {
                            eprintln!("rc: release {} @ {} ({} more refs)", ty, addr, rc - 1)
                        }

                        rc_cell.fields[RC_REF_COUNT_FIELD] = MemCell::I32(rc - 1);
                    }
                }
            }

            // can't contain rc pointers
            _ => {}
        }
    }

    // todo: this should be handled in the IR
    fn retain_cell(&mut self, cell: &MemCell) {
        match cell {
            MemCell::Pointer(Pointer::Heap(ty, addr)) => {
                let rc_cell = self.heap.get_mut(*addr)
                    .and_then(|cell| cell.as_struct_mut(RC_ID));

                if let Some(rc_cell) = rc_cell {
                    if self.trace_rc {
                        eprintln!("rc: retain {} @ {}", ty, addr);
                    }

                    let rc = rc_cell.fields[RC_REF_COUNT_FIELD].as_i32().unwrap();
                    rc_cell.fields[RC_REF_COUNT_FIELD] = MemCell::I32(rc + 1);
                }
            }

            MemCell::Structure(StructCell { fields, .. }) => {
                for field_cell in fields {
                    self.retain_cell(field_cell);
                }
            }

            // can't contain rc pointers
            _ => {}
        }
    }

    fn addr_of_ref(&self, target: &Ref) -> Pointer {
        match target {
            // let int := 1;
            // let intPtr := @int;
            // @(intPtr^) -> address of int behind intPtr
            Ref::Deref(deref_target) => {
                match self.evaluate(deref_target.as_ref()) {
                    MemCell::Pointer(ptr) => ptr.clone(),
                    _ => panic!("deref of non-pointer value @ {}", deref_target)
                }
            }

            // let int := 1;
            // @int -> stack address of int cell
            Ref::Local(id) => {
                let type_at = self.load(target).value_ty();
                Pointer::Local {
                    frame: self.stack.len() - 1,
                    id: *id,
                    ty: type_at,
                }
            }

            Ref::Global(global) => {
                panic!("can't take address of global ref {:?}", global)
            }
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
                        _ => panic!("local cell {} is already allocated", Ref::Local(*id)),
                    }
                }

                Instruction::LocalDelete(id) => {
                    if *id >= self.current_frame().locals.len() {
                        panic!("local cell {} is not allocated");
                    }

                    self.current_frame_mut().locals[*id] = None;
                }

                Instruction::RcNew { out, struct_id, } => {
                    let struct_ty = Type::Struct(*struct_id);
                    let init_cells = vec![self.init_cell(&struct_ty)];

                    let rc_ptr = self.rc_alloc(init_cells, *struct_id);

                    self.store(out, MemCell::Pointer(rc_ptr));
                }

                Instruction::Add { out, a, b } => {
                    match self.evaluate(a).try_add(&self.evaluate(b)) {
                        Some(result) => self.store(out, result),
                        None => panic!("Add is not valid for {:?} + {:?}", self.evaluate(a), self.evaluate(b)),
                    }
                }

                Instruction::Sub { out, a, b } => {
                    match self.evaluate(a).try_sub(&self.evaluate(b)) {
                        Some(result) => self.store(out, result),
                        None => panic!("Sub is not valid for {:?} - {:?}", self.evaluate(a), self.evaluate(b)),
                    }
                }

                Instruction::Eq { out, a, b } => {
                    let eq = match self.evaluate(a).try_eq(&self.evaluate(b)) {
                        Some(eq) => eq,
                        None => panic!("Eq is not valid for {:?} = {:?}", self.evaluate(a), self.evaluate(b)),
                    };

                    self.store(out, MemCell::Bool(eq))
                }

                Instruction::Gt { out, a, b } => {
                    let gt = match (self.evaluate(a), self.evaluate(b)) {
                        (MemCell::I32(a), MemCell::I32(b)) => a > b,
                        (MemCell::U8(a), MemCell::U8(b)) => a > b,
                        (MemCell::F32(a), MemCell::F32(b)) => a > b,
                        _ => panic!("Gt is not valid for {:?} > {:?}", a, b),
                    };

                    self.store(out, MemCell::Bool(gt));
                }

                Instruction::Not { out, a } => {
                    let val = self.evaluate(a).as_bool()
                        .unwrap_or_else(|| panic!("Not instruction is not valid for {:?}", a));

                    self.store(out, MemCell::Bool(!val));
                }

                Instruction::And { out, a, b } => {
                    let a_val = self.evaluate(a).as_bool()
                        .unwrap_or_else(|| panic!("operand a of And instruction must be bool, got {:?}", a));

                    let b_val = self.evaluate(b).as_bool()
                        .unwrap_or_else(|| panic!("operand b of And instruction must be bool, got {:?}", b));

                    self.store(out, MemCell::Bool(a_val && b_val));
                }

                Instruction::Or { out, a, b } => {
                    let a_val = self.evaluate(a).as_bool()
                        .unwrap_or_else(|| panic!("operand a of Or instruction must be bool, got {:?}", a));

                    let b_val = self.evaluate(b).as_bool()
                        .unwrap_or_else(|| panic!("operand b of Or instruction must be bool, got {:?}", b));

                    self.store(out, MemCell::Bool(a_val || b_val));
                }

                Instruction::Move { out, new_val } => {
                    self.store(out, self.evaluate(new_val));
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
                    }
                }

                Instruction::AddrOf { out, a } => {
                    let a_ptr = self.addr_of_ref(a);
                    self.store(out, MemCell::Pointer(a_ptr));
                }

                Instruction::GetField { out, of, struct_id, field_id, } => {
                    let field_val = match self.load(of) {
                        MemCell::Structure(StructCell { fields, id: cell_struct_id }) => {
                            if *cell_struct_id == RC_ID {
                                let val_ptr = fields[RC_VALUE_FIELD].as_pointer()
                                    .and_then(|ptr| ptr.as_heap_addr())
                                    .unwrap();
                                let val_cell = self.heap.get(val_ptr)
                                    .and_then(|cell| cell.as_struct(*struct_id))
                                    .unwrap();

                                &val_cell.fields[*field_id]
                            } else {
                                assert_eq!(struct_id, cell_struct_id);

                                &fields[*field_id]
                            }
                        }
                        _ => panic!("GetField instruction targeting non-structure cell"),
                    };

                    self.store(out, field_val.clone());
                }

                Instruction::SetField { of, new_val, struct_id, field_id, } => {
                    let new_val = self.evaluate(new_val);

                    match self.load(of).clone() {
                        MemCell::Structure(mut struct_cell) => {
                            if struct_cell.id == RC_ID {
                                let val_addr = struct_cell.fields[RC_VALUE_FIELD].as_pointer()
                                    .and_then(|ptr| ptr.as_heap_addr())
                                    .unwrap();

                                let rc_val = self.heap.get_mut(val_addr)
                                    .and_then(|cell| cell.as_struct_mut(*struct_id))
                                    .unwrap();
                                rc_val.fields[*field_id] = new_val;
                            } else {
                                assert_eq!(
                                    *struct_id, struct_cell.id,
                                    "SetField instruction expected {} at {}, found {}",
                                    struct_id, of, struct_cell.id
                                );

                                struct_cell.fields[*field_id] = new_val;
                                self.store(of, MemCell::Structure(struct_cell))
                            }
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

                Instruction::Release(at) => {
                    let cell = self.load(at).clone();
                    self.release_cell(&cell);
                }

                Instruction::Retain(at) => {
                    let cell = self.load(at).clone();
                    self.retain_cell(&cell);
                }
            }

            pc += 1;
        }
    }

    fn rc_alloc(&mut self, vals: Vec<MemCell>, struct_id: StructId) -> Pointer {
        let addr = self.heap.alloc(vals);
        let ptr = Pointer::Heap(Type::Struct(struct_id), addr);

        let rc_cell = MemCell::Structure(StructCell {
            id: RC_ID,
            fields: vec![MemCell::I32(1), MemCell::Pointer(ptr)],
        });

        let addr = self.heap.alloc(vec![rc_cell]);
        Pointer::Heap(Type::Struct(RC_ID), addr)
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

    fn get_rc_val(&self, rc_cell: &MemCell) -> &MemCell {
        let rc_struct = rc_cell.as_struct(RC_ID)
            .unwrap();

        let val_ptr = rc_struct.fields[RC_VALUE_FIELD]
            .as_pointer()
            .and_then(|ptr| ptr.as_heap_addr())
            .unwrap();

        self.heap.get(val_ptr).unwrap()
    }

    fn create_string(&mut self, content: &str) -> MemCell {
        let chars: Vec<_> = content.chars()
            .map(|c| MemCell::U8(c as u8))
            .collect();
        let chars_ptr = self.heap.alloc(chars);

        let str_fields = vec![
            //field 0: `chars: ^Byte`
            MemCell::Pointer(Pointer::Heap(Type::U8, chars_ptr)),

            //field 1: `len: Integer`
            MemCell::I32(content.len() as i32),
        ];

        let str_cell = MemCell::Structure(StructCell { id: STRING_ID, fields: str_fields });

        let str_ptr = self.rc_alloc(vec![str_cell], STRING_ID);
        MemCell::Pointer(str_ptr)
    }

    fn read_string(&self, str_ref: &Ref) -> String {
        let str_cell = self.get_rc_val(self.load(str_ref))
            .as_struct(STRING_ID)
            .unwrap();

        let len = &str_cell.fields[STRING_LEN_FIELD]
            .as_i32().unwrap();

        let chars_addr = &str_cell.fields[STRING_CHARS_FIELD]
            .as_pointer()
            .and_then(|ptr| ptr.as_heap_addr())
            .unwrap_or_else(|| panic!("string contained non-heap-alloced `chars` pointer"));

        let mut chars = Vec::new();
        for i in 0..*len as usize {
            let char_addr = HeapAddress(chars_addr.0 + i);
            let char_val = self.heap.get(char_addr)
                .unwrap()
                .as_u8()
                .unwrap_or_else(|| panic!("expected string char @ {}", char_addr));

            chars.push(char_val as char);
        }

        chars.into_iter().collect()
    }
}

impl Drop for Interpreter {
    fn drop(&mut self) {
        let globals: Vec<_> = self.globals.values().cloned().collect();

        for global in globals {
            self.release_cell(&global);
        }
    }
}