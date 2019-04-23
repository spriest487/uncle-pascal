use self::heap::{Heap, HeapAddress};
use crate::{
    metadata::*, Function as FunctionIR, GlobalRef, Instruction, LocalID, Module, Ref, Type, Value,
};
use std::{
    collections::HashMap,
    f32, fmt,
    ops::{Add, Index, IndexMut, Sub},
    rc::Rc,
};

mod builtin;
mod heap;

#[derive(Clone)]
pub enum Function {
    Builtin {
        func: fn(state: &mut Interpreter),
        ret: Type,
    },
    IR(Rc<FunctionIR>),
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
            Function::IR(func) => write!(f, "<function with {} instructions>", func.body.len()),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Pointer {
    Null,
    Uninit,
    Heap(HeapAddress),
    Local {
        frame: usize,
        id: usize,
    },
    IntoArray {
        array: Box<Pointer>,
        offset: usize,
    },
    IntoStruct {
        structure: Box<Pointer>,
        field: FieldID,
    },
}

impl Pointer {
    pub fn as_heap_addr(&self) -> Option<HeapAddress> {
        match self {
            Pointer::Heap(addr) => Some(*addr),
            _ => None,
        }
    }
}

impl Add<usize> for Pointer {
    type Output = Self;

    fn add(self, rhs: usize) -> Self {
        match self {
            Pointer::Null => Pointer::Null,
            Pointer::Uninit => Pointer::Uninit,
            Pointer::Local { frame, id } => Pointer::Local {
                frame,
                id: id + rhs,
            },
            Pointer::Heap(HeapAddress(addr)) => Pointer::Heap(HeapAddress(addr + rhs)),
            Pointer::IntoArray {
                array,
                offset,
            } => Pointer::IntoArray {
                array,
                offset: offset + rhs,
            },
            Pointer::IntoStruct { .. } => {
                panic!("pointer arithmetic on struct pointers is illegal")
            }
        }
    }
}

impl Sub<usize> for Pointer {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self {
        match self {
            Pointer::Null => Pointer::Null,
            Pointer::Uninit => Pointer::Uninit,
            Pointer::Local { frame, id } => Pointer::Local {
                frame,
                id: id - rhs,
            },
            Pointer::Heap(HeapAddress(addr)) => Pointer::Heap(HeapAddress(addr - rhs)),
            Pointer::IntoArray {
                array,
                offset,
            } => Pointer::IntoArray {
                array,
                offset: offset - rhs,
            },
            Pointer::IntoStruct { .. } => {
                panic!("pointer arithmetic on struct pointers is illegal")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructCell {
    pub id: StructID,
    pub fields: Vec<MemCell>,
}

impl Index<FieldID> for StructCell {
    type Output = MemCell;

    fn index(&self, index: FieldID) -> &MemCell {
        &self.fields[index.0]
    }
}

impl IndexMut<FieldID> for StructCell {
    fn index_mut(&mut self, index: FieldID) -> &mut MemCell {
        &mut self.fields[index.0]
    }
}

impl PartialEq<Self> for StructCell {
    fn eq(&self, other: &Self) -> bool {
        if self.id != other.id || self.fields.len() != other.fields.len() {
            return false;
        }

        self.fields
            .iter()
            .zip(other.fields.iter())
            .all(|(a, b)| match a.try_eq(b) {
                Some(eq) => eq,
                None => panic!("structs can only contain comparable fields"),
            })
    }
}

#[derive(Debug, Clone)]
pub struct RcCell {
    pub resource_addr: HeapAddress,
    pub ref_count: usize,
    pub ty_id: StructID,
}

impl RcCell {
    fn uninitialized() -> Self {
        Self {
            resource_addr: HeapAddress(0),
            ref_count: 0,
            ty_id: StructID(0),
        }
    }
}

enum ReleaseTarget {
    RcPointer,
    Struct(StructID),
}

impl ReleaseTarget {
    fn for_ty(ty: &Type) -> Option<Self> {
        match ty {
            Type::RcPointer(..) => Some(ReleaseTarget::RcPointer),
            Type::Struct(id) => Some(ReleaseTarget::Struct(*id)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayCell {
    pub el_ty: Type,
    pub elements: Vec<MemCell>,
}

impl ArrayCell {
    pub fn try_eq(&self, other: &Self) -> Option<bool> {
        if self.elements.len() == other.elements.len() {
            let mut all_same = true;
            for (mine, theirs) in self.elements.iter().zip(other.elements.iter()) {
                all_same &= mine.try_eq(theirs)?;
            }
            Some(all_same)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub enum MemCell {
    Bool(bool),
    U8(u8),
    I32(i32),
    F32(f32),
    RcCell(RcCell),
    Function(Function),
    Structure(StructCell),
    Pointer(Pointer),
    Array(ArrayCell),
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
            (MemCell::Array(a), MemCell::Array(b)) => a.try_eq(b),
            _ => None,
        }
    }

    pub fn try_add(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => Some(MemCell::I32(a + b)),

            (MemCell::F32(a), MemCell::F32(b)) => Some(MemCell::F32(a + b)),

            (MemCell::Pointer(ptr), MemCell::I32(offset)) => {
                Some(MemCell::Pointer(ptr.clone() + *offset as usize))
            }

            (
                MemCell::Pointer(Pointer::Heap(ref a)),
                MemCell::Pointer(Pointer::Heap(ref b)),
            ) => Some(MemCell::Pointer(Pointer::Heap(
                HeapAddress(a.0 + b.0),
            ))),

            _ => None,
        }
    }

    pub fn try_sub(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => Some(MemCell::I32(a - b)),

            (MemCell::F32(a), MemCell::F32(b)) => Some(MemCell::F32(a - b)),

            (MemCell::Pointer(ptr), MemCell::I32(offset)) => {
                Some(MemCell::Pointer(ptr.clone() - *offset as usize))
            }

            (
                MemCell::Pointer(Pointer::Heap(ref a)),
                MemCell::Pointer(Pointer::Heap(ref b)),
            ) => Some(MemCell::Pointer(Pointer::Heap(
                HeapAddress(a.0 - b.0),
            ))),

            _ => None,
        }
    }

    pub fn as_struct_mut(&mut self, struct_id: StructID) -> Option<&mut StructCell> {
        match self {
            MemCell::Structure(struct_cell) if struct_id == struct_cell.id => Some(struct_cell),
            _ => None,
        }
    }

    pub fn as_struct(&self, struct_id: StructID) -> Option<&StructCell> {
        match self {
            MemCell::Structure(struct_cell) if struct_id == struct_cell.id => Some(struct_cell),
            _ => None,
        }
    }

    pub fn as_rc_mut(&mut self) -> Option<&mut RcCell> {
        match self {
            MemCell::RcCell(rc) => Some(rc),
            _ => None,
        }
    }

    pub fn as_rc(&self) -> Option<&RcCell> {
        match self {
            MemCell::RcCell(rc) => Some(rc),
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

#[derive(Debug, Clone)]
struct GlobalCell {
    value: MemCell,
    ty: Type,
}

#[derive(Debug)]
pub struct Interpreter {
    metadata: Metadata,
    stack: Vec<StackFrame>,
    globals: HashMap<GlobalRef, GlobalCell>,
    heap: Heap,

    trace_rc: bool,
    trace_ir: bool,
}

impl Interpreter {
    pub fn new(opts: &InterpreterOpts) -> Self {
        let globals = HashMap::new();

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

    fn init_struct(&mut self, id: StructID) -> MemCell {
        let struct_def = self.metadata.structs()[&id].clone();

        let mut fields = Vec::new();
        for (&id, field) in &struct_def.fields {
            // include padding of -1s for non-contiguous IDs
            if id.0 >= fields.len() {
                fields.resize(id.0 + 1, MemCell::I32(-1));
            }
            fields[id.0] = self.init_cell(&field.ty);
        }

        MemCell::Structure(StructCell { id, fields })
    }

    fn init_cell(&mut self, ty: &Type) -> MemCell {
        match ty {
            Type::I32 => MemCell::I32(-1),
            Type::Bool => MemCell::Bool(false),
            Type::F32 => MemCell::F32(f32::INFINITY),
            Type::Struct(id) => self.init_struct(*id),

            Type::RcPointer(_) => MemCell::RcCell(RcCell::uninitialized()),

            Type::Pointer(_target) => {
                MemCell::Pointer(Pointer::Uninit)
            }

            Type::Array { element, dim } => {
                let mut elements = Vec::new();
                for _ in 0..*dim {
                    elements.push(self.init_cell(element.as_ref()));
                }

                MemCell::Array(ArrayCell {
                    el_ty: (**element).clone(),
                    elements,
                })
            }

            _ => panic!("can't initialize default cell of type `{:?}`", ty),
        }
    }

    fn deref_ptr(&self, ptr: &Pointer) -> &MemCell {
        match ptr {
            Pointer::Heap(slot) => self
                .heap
                .get(*slot)
                .unwrap_or_else(|| panic!("heap cell {} is not allocated: {:?}", slot, pas_common::Backtrace::new())),

            Pointer::Local { frame, id, .. } => {
                let locals = &self.stack[*frame].locals;
                locals[*id]
                    .as_ref()
                    .unwrap_or_else(|| panic!("local cell {}.{} is not allocated", frame, id))
            }

            Pointer::IntoArray { array, offset, .. } => {
                match self.deref_ptr(array) {
                    MemCell::Array(array_cell) => {
                        &array_cell.elements[*offset]
                    }

                    other => panic!("dereferencing array pointer which doesn't point to an array cell (was: {:#?})", other),
                }
            }

            Pointer::IntoStruct { structure, field, .. } => {
                match self.deref_ptr(structure) {
                    MemCell::Structure(struct_cell) => {
                        &struct_cell[*field]
                    }

                    other => panic!("dereferencing struct member pointer which doesn't point into a struct (was: {:#?})", other)
                }
            }

            Pointer::Null => {
                panic!("dereferencing null pointer");
            }

            Pointer::Uninit => {
                panic!("dereferencing uninitialized pointer");
            }
        }
    }

    fn deref_ptr_mut(&mut self, ptr: &Pointer) -> &mut MemCell {
        match ptr {
            Pointer::Heap(slot) => self
                .heap
                .get_mut(*slot)
                .unwrap_or_else(|| panic!("heap cell {} is not allocated", slot)),

            Pointer::Local { frame, id, .. } => {
                let locals = &mut self.stack[*frame].locals;
                locals[*id]
                    .as_mut()
                    .unwrap_or_else(|| panic!("local cell {}.{} is not allocated", frame, id))
            }

            Pointer::IntoArray { array, offset, .. } => {
                match self.deref_ptr_mut(array) {
                    MemCell::Array(array_cell) => {
                        &mut array_cell.elements[*offset]
                    }

                    other => panic!("dereferencing array pointer which doesn't point to an array cell (was: {:#?})", other),
                }
            }

            Pointer::IntoStruct { structure, field, .. } => {
                match self.deref_ptr_mut(structure) {
                    MemCell::Structure(struct_cell) => {
                        &mut struct_cell[*field]
                    }

                    other => panic!("dereferencing struct member pointer which doesn't point into a struct (was: {:#?})", other),
                }
            }

            Pointer::Uninit => {
                panic!("dereferencing uninitialized pointer");
            }

            Pointer::Null => panic!("dereferencing null pointer"),
        }
    }

    fn store(&mut self, at: &Ref, val: MemCell) {
        match at {
            Ref::Local(LocalID(id)) => match self.current_frame_mut().locals.get_mut(*id) {
                Some(Some(cell)) => {
                    *cell = val;
                }
                None | Some(None) => panic!("local cell {} is not allocated", id),
            },

            Ref::Global(name) => {
                if self.globals.contains_key(name) {
                    panic!("global cell `{}` is already allocated", name);
                }
                self.globals.insert(
                    name.clone(),
                    GlobalCell {
                        value: val,
                        //todo: for the moment this means no RC for global cells stored after init
                        ty: Type::Nothing,
                    },
                );
            }

            Ref::Deref(inner) => match self.evaluate(inner) {
                MemCell::Pointer(ptr) => {
                    *self.deref_ptr_mut(&ptr) = val;
                }

                x => panic!("can't dereference non-pointer cell with value {:?}", x),
            },
        }
    }

    fn load(&self, at: &Ref) -> &MemCell {
        match at {
            Ref::Local(LocalID(id)) => match self.current_frame().locals.get(*id) {
                Some(Some(cell)) => cell,
                None | Some(None) => {
                    panic!("local cell {} is not allocated", id);
                }
            },

            Ref::Global(name) => match self.globals.get(name) {
                Some(cell) => &cell.value,
                None => {
                    panic!("global cell `{}` is not allocated", name);
                }
            },

            Ref::Deref(inner) => match self.evaluate(inner) {
                MemCell::Pointer(ptr) => self.deref_ptr(&ptr),
                x => panic!("can't dereference cell {:?}", x),
            },
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
        self.stack.push(StackFrame { locals: Vec::new() });
    }

    fn pop_stack(&mut self) {
        self.stack.pop().expect("popped stack with no stackframes");
    }

    fn current_frame(&self) -> &StackFrame {
        self.stack
            .last()
            .expect("called current_frame without no stackframes")
    }

    fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.stack
            .last_mut()
            .expect("called current_frame without no stackframes")
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
        self.current_frame_mut()
            .locals
            .extend(args.iter().cloned().map(Some));

        match func {
            Function::Builtin { func, .. } => func(self),
            Function::IR(ir_func) => self.execute(&ir_func.body),
        };

        let result_cell = match return_ty {
            Type::Nothing => None,
            _ => {
                let return_val = self.evaluate(&Value::Ref(Ref::Local(LocalID(0))));
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

    fn invoke_disposer(&mut self, cell: &MemCell, ty: &Type) {
        let dispose_impl_id = self.metadata.find_impl(ty, DISPOSABLE_ID, "Dispose");

        if let Some(dispose_func) = dispose_impl_id {
            let dispose_desc = self.metadata.func_desc(dispose_func);
            let disposed_name = self.metadata.pretty_ty_name(ty);

            let dispose_ref = GlobalRef::Function(dispose_func);
            let func = match self.globals.get(&dispose_ref).map(|c| &c.value) {
                Some(MemCell::Function(func)) => func.clone(),
                _ => panic!("missing {} for {}", dispose_desc, disposed_name),
            };

            if self.trace_rc {
                eprintln!("rc: invoking {}", dispose_desc);
            }

            self.call(&func, &[cell.clone()], None);
        } else {
            if self.trace_rc {
                eprintln!("rc: no disposer for {}", self.metadata.pretty_ty_name(ty));
            }
        }
    }

    fn release_cell(&mut self, cell: &MemCell, target: ReleaseTarget) {
        match target {
            ReleaseTarget::Struct(struct_id) => {
                let struct_cell = cell.as_struct(struct_id).unwrap();
                let cell_ty = Type::Struct(struct_id);

                self.invoke_disposer(&cell, &cell_ty);
                let struct_def: Struct = self.metadata.structs()[&struct_id].clone();

                for (field_id, field_def) in &struct_def.fields {
                    let field_cell = &struct_cell[*field_id];
                    let field_ty = &field_def.ty;

                    if let Some(field_release_target) = ReleaseTarget::for_ty(field_ty) {
                        self.release_cell(field_cell, field_release_target);
                    }
                }
            }

            ReleaseTarget::RcPointer => {
                let rc_addr = cell.as_pointer().and_then(|p| p.as_heap_addr()).unwrap();

                let rc_cell = self.heap[rc_addr].as_rc().unwrap().clone();
                let resource_ty = Type::Struct(rc_cell.ty_id);

                if rc_cell.ref_count == 1 {
                    if self.trace_rc {
                        println!(
                            "rc: delete cell {:?} of resource ty {}",
                             cell,
                             self.metadata.pretty_ty_name(&resource_ty)
                        );
                    }

                    // Dispose() the inner resource. For an RC type, interfaces are implemented
                    // for the RC pointer type, not the resource type
                    self.invoke_disposer(&cell, &resource_ty.clone().rc());

                    let resource_cell = self.heap[rc_cell.resource_addr].clone();

                    // Now release the fields of the resource struct as if it was a normal record.
                    // This could technically invoke another disposer, but it won't, because there's
                    // no way to declare a disposer for the inner resource type of an RC type
                    self.release_cell(&resource_cell, ReleaseTarget::Struct(rc_cell.ty_id));

                    if self.trace_rc {
                        eprintln!("rc: free {} @ {}", self.metadata.pretty_ty_name(&resource_ty), rc_addr)
                    }

                    self.heap.free(rc_cell.resource_addr);
                    self.heap.free(rc_addr);
                } else {
                    assert!(rc_cell.ref_count > 1);
                    if self.trace_rc {
                        eprintln!(
                            "rc: release {} @ {} ({} more refs)",
                            self.metadata.pretty_ty_name(&resource_ty),
                            rc_cell.resource_addr,
                            rc_cell.ref_count - 1
                        )
                    }

                    self.heap[rc_addr] = MemCell::RcCell(RcCell {
                        ref_count: rc_cell.ref_count - 1,
                        ..rc_cell
                    });
                }
            }
        }
    }

    // todo: this should be handled in the IR
    fn retain_cell(&mut self, cell: &MemCell) {
        match cell {
            MemCell::Pointer(Pointer::Heap(addr)) => {
                let rc_cell = self.heap.get_mut(*addr).and_then(|cell| cell.as_rc_mut());

                if let Some(rc_cell) = rc_cell {
                    if self.trace_rc {
                        eprintln!("rc: retain @ {}", addr);
                    }

                    rc_cell.ref_count += 1;
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
            Ref::Deref(deref_val) => {
                match deref_val.as_ref() {
                    // recursively handle multiple deref levels (i^^^)
                    Value::Ref(Ref::Deref(inner_deref)) => match inner_deref.as_ref() {
                        Value::Ref(ref_val) => self.addr_of_ref(ref_val),
                        _ => panic!("deref of non-reference value"),
                    },

                    val => match self.evaluate(val) {
                        MemCell::Pointer(ptr) => ptr.clone(),
                        _ => panic!("deref of non-pointer value @ {}", val),
                    },
                }
            }

            // let int := 1;
            // @int -> stack address of int cell
            Ref::Local(id) => {
                // find the highest frame in which this cell id is allocated
                let frame_id = self
                    .stack
                    .iter()
                    .enumerate()
                    .rev()
                    .filter_map(|(frame_id, stack_frame)| {
                        if stack_frame.locals.len() > id.0 {
                            Some(frame_id)
                        } else {
                            None
                        }
                    })
                    .next()
                    .unwrap();

                Pointer::Local {
                    frame: frame_id,
                    id: id.0,
                }
            }

            Ref::Global(global) => panic!("can't take address of global ref {:?}", global),
        }
    }

    pub fn execute(&mut self, instructions: &[Instruction]) {
        let labels: HashMap<_, _> = instructions
            .iter()
            .enumerate()
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
                Instruction::Comment(_) => {
                    // noop
                }

                Instruction::LocalAlloc(id, ty) => {
                    while self.current_frame().locals.len() <= id.0 {
                        self.current_frame_mut().locals.push(None);
                    }

                    match self.current_frame().locals[id.0] {
                        None => self.current_frame_mut().locals[id.0] = Some(self.init_cell(ty)),
                        _ => panic!("local cell {} is already allocated", id),
                    }
                }

                Instruction::LocalDelete(id) => {
                    if id.0 >= self.current_frame().locals.len() {
                        panic!("local cell {} is not allocated", id);
                    }

                    self.current_frame_mut().locals[id.0] = None;
                }

                Instruction::RcNew { out, struct_id } => {
                    let struct_ty = Type::Struct(*struct_id);
                    let init_cells = vec![self.init_cell(&struct_ty)];

                    let rc_ptr = self.rc_alloc(init_cells, *struct_id);

                    self.store(out, MemCell::Pointer(rc_ptr));
                }

                Instruction::Add { out, a, b } => {
                    match self.evaluate(a).try_add(&self.evaluate(b)) {
                        Some(result) => self.store(out, result),
                        None => panic!(
                            "Add is not valid for {:?} + {:?}",
                            self.evaluate(a),
                            self.evaluate(b)
                        ),
                    }
                }

                Instruction::Sub { out, a, b } => {
                    match self.evaluate(a).try_sub(&self.evaluate(b)) {
                        Some(result) => self.store(out, result),
                        None => panic!(
                            "Sub is not valid for {:?} - {:?}",
                            self.evaluate(a),
                            self.evaluate(b)
                        ),
                    }
                }

                Instruction::Eq { out, a, b } => {
                    let eq = match self.evaluate(a).try_eq(&self.evaluate(b)) {
                        Some(eq) => eq,
                        None => panic!(
                            "Eq is not valid for {:?} = {:?}",
                            self.evaluate(a),
                            self.evaluate(b)
                        ),
                    };

                    self.store(out, MemCell::Bool(eq))
                }

                Instruction::Gt { out, a, b } => {
                    let gt = match (self.evaluate(a), self.evaluate(b)) {
                        (MemCell::I32(a), MemCell::I32(b)) => a > b,
                        (MemCell::U8(a), MemCell::U8(b)) => a > b,
                        (MemCell::F32(a), MemCell::F32(b)) => a > b,
                        (cell_a, cell_b) => panic!(
                            "Gt is not valid for {} ({:?}) > {} ({:?})",
                            a, cell_a, b, cell_b
                        ),
                    };

                    self.store(out, MemCell::Bool(gt));
                }

                Instruction::Not { out, a } => {
                    let val = self
                        .evaluate(a)
                        .as_bool()
                        .unwrap_or_else(|| panic!("Not instruction is not valid for {:?}", a));

                    self.store(out, MemCell::Bool(!val));
                }

                Instruction::And { out, a, b } => {
                    let a_val = self.evaluate(a).as_bool().unwrap_or_else(|| {
                        panic!("operand a of And instruction must be bool, got {:?}", a)
                    });

                    let b_val = self.evaluate(b).as_bool().unwrap_or_else(|| {
                        panic!("operand b of And instruction must be bool, got {:?}", b)
                    });

                    self.store(out, MemCell::Bool(a_val && b_val));
                }

                Instruction::Or { out, a, b } => {
                    let a_val = self.evaluate(a).as_bool().unwrap_or_else(|| {
                        panic!("operand a of Or instruction must be bool, got {:?}", a)
                    });

                    let b_val = self.evaluate(b).as_bool().unwrap_or_else(|| {
                        panic!("operand b of Or instruction must be bool, got {:?}", b)
                    });

                    self.store(out, MemCell::Bool(a_val || b_val));
                }

                Instruction::Move { out, new_val } => {
                    self.store(out, self.evaluate(new_val));
                }

                Instruction::Call {
                    out,
                    function,
                    args,
                } => {
                    let arg_cells: Vec<_> =
                        args.iter().map(|arg_val| self.evaluate(arg_val)).collect();

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

                Instruction::Field {
                    out,
                    a,
                    field,
                    of_ty,
                } => {
                    let struct_ptr = match of_ty {
                        Type::Struct(_) => self.addr_of_ref(a),

                        Type::RcPointer(rc_ty) if rc_ty.as_struct().is_some() => {
                            let rc_addr = self
                                .load(&a.clone().deref())
                                .as_rc()
                                .map(|rc_cell| rc_cell.resource_addr)
                                .unwrap_or_else(|| {
                                    panic!("failed to read value pointer of rc pointer @ {}", a);
                                });

                            Pointer::Heap(rc_addr)
                        }

                        _ => {
                            panic!(
                                "invalid base type referenced in Field instruction: {}.{}",
                                of_ty, field
                            );
                        }
                    };

                    let field_ptr = Pointer::IntoStruct {
                        field: *field,
                        structure: Box::new(struct_ptr),
                    };

                    self.store(out, MemCell::Pointer(field_ptr));
                }

                Instruction::Element {
                    out,
                    a,
                    // not used because the interpreter doesn't need to know element type to
                    // calculate the pointer offset
                    element: _el,
                    index,
                } => {
                    let array = self.addr_of_ref(a);

                    // todo: use a real usize
                    let offset = self.evaluate(index).as_i32().unwrap() as usize;

                    self.store(
                        out,
                        MemCell::Pointer(Pointer::IntoArray {
                            offset,
                            array: Box::new(array),
                        }),
                    );
                }

                Instruction::Label(_) => {
                    // noop
                }

                Instruction::Jump { dest } => {
                    pc = labels[dest];
                    continue;
                }

                Instruction::JumpIf { dest, test } => match self.evaluate(test) {
                    MemCell::Bool(true) => {
                        pc = labels[dest];
                    }
                    MemCell::Bool(false) => {}
                    _ => panic!("JumpIf instruction testing non-boolean cell"),
                },

                Instruction::Release { at } => {
                    let cell = self.load(at).clone();
                    self.release_cell(&cell, ReleaseTarget::RcPointer);
                }

                Instruction::Retain { at } => {
                    let cell = self.load(at).clone();
                    self.retain_cell(&cell);
                }
            }

            pc += 1;
        }
    }

    fn rc_alloc(&mut self, vals: Vec<MemCell>, ty_id: StructID) -> Pointer {
        let resource_addr = self.heap.alloc(vals);

        let rc_cell = MemCell::RcCell(RcCell {
            ref_count: 1,
            resource_addr,
            ty_id,
        });

        let addr = self.heap.alloc(vec![rc_cell]);
        Pointer::Heap(addr)
    }

    fn init_globals(&mut self) {
        let inttostr_name = GlobalName::new("IntToStr", vec!["System"]);
        if let Some(inttostr_id) = self.metadata.find_function(&inttostr_name) {
            self.globals.insert(
                GlobalRef::Function(inttostr_id),
                GlobalCell {
                    value: MemCell::Function(Function::Builtin {
                        func: builtin::int_to_str,
                        ret: Type::Struct(STRING_ID).ptr(),
                    }),
                    ty: Type::Nothing,
                },
            );
        }

        let writeln_name = GlobalName::new("WriteLn", vec!["System"]);
        if let Some(writeln_id) = self.metadata.find_function(&writeln_name) {
            self.globals.insert(
                GlobalRef::Function(writeln_id),
                GlobalCell {
                    value: MemCell::Function(Function::Builtin {
                        func: builtin::write_ln,
                        ret: Type::Nothing,
                    }),
                    ty: Type::Nothing,
                },
            );
        }

        let getmem_name = GlobalName::new("GetMem", vec!["System"]);
        if let Some(getmem_id) = self.metadata.find_function(&getmem_name) {
            self.globals.insert(
                GlobalRef::Function(getmem_id),
                GlobalCell {
                    value: MemCell::Function(Function::Builtin {
                        func: builtin::get_mem,
                        ret: Type::U8.ptr(),
                    }),
                    ty: Type::Nothing,
                },
            );
        }

        let freemem_name = GlobalName::new("FreeMem", vec!["System"]);
        if let Some(freemem_id) = self.metadata.find_function(&freemem_name) {
            self.globals.insert(
                GlobalRef::Function(freemem_id),
                GlobalCell {
                    value: MemCell::Function(Function::Builtin {
                        func: builtin::free_mem,
                        ret: Type::Nothing,
                    }),
                    ty: Type::Nothing,
                },
            );
        }
    }

    pub fn load_module(&mut self, module: &Module) {
        self.metadata.extend(&module.metadata);

        for (func_name, func) in &module.functions {
            let func_cell = MemCell::Function(Function::IR(Rc::new(func.clone())));
            let func_ref = GlobalRef::Function(func_name.clone());

            self.globals.insert(
                func_ref,
                GlobalCell {
                    value: func_cell,
                    ty: Type::Nothing,
                },
            );
        }

        for (id, literal) in module.metadata.strings() {
            let str_cell = self.create_string(literal);
            let str_ref = GlobalRef::StringLiteral(id);

            self.globals.insert(
                str_ref,
                GlobalCell {
                    value: str_cell,
                    ty: Type::Struct(STRING_ID).rc(),
                },
            );
        }

        self.init_globals();

        self.push_stack();
        self.execute(&module.init);
        self.pop_stack();
    }

    fn deref_rc(&self, rc_cell: &MemCell) -> &MemCell {
        let rc = rc_cell
            .as_rc()
            .unwrap_or_else(|| panic!("trying to dereference RC pointer with an invalid value"));

        self.heap.get(rc.resource_addr).unwrap()
    }

    fn create_string(&mut self, content: &str) -> MemCell {
        let chars: Vec<_> = content.chars().map(|c| MemCell::U8(c as u8)).collect();
        let chars_len = chars.len();

        let chars_ptr = if chars_len > 0 {
            let heap_addr = self.heap.alloc(chars);
            Pointer::Heap(heap_addr)
        } else {
            Pointer::Null
        };

        let str_fields = vec![
            // field 0: `chars: ^Byte`
            MemCell::Pointer(chars_ptr),
            // field 1: `len: Integer`
            MemCell::I32(chars_len as i32),
        ];

        let str_cell = MemCell::Structure(StructCell {
            id: STRING_ID,
            fields: str_fields,
        });

        let str_ptr = self.rc_alloc(vec![str_cell], STRING_ID);
        MemCell::Pointer(str_ptr)
    }

    fn read_string(&self, str_ref: &Ref) -> String {
        let str_cell = self
            .deref_rc(self.load(str_ref))
            .as_struct(STRING_ID)
            .unwrap();

        let len = &str_cell[STRING_LEN_FIELD].as_i32().unwrap();

        if *len == 0 {
            return String::new();
        }

        let chars_addr = &str_cell[STRING_CHARS_FIELD]
            .as_pointer()
            .and_then(|ptr| ptr.as_heap_addr())
            .unwrap_or_else(|| {
                panic!(
                    "string contained non-heap-alloced `chars` pointer: {:?}",
                    str_cell
                )
            });

        let mut chars = Vec::new();
        for i in 0..*len as usize {
            let char_addr = HeapAddress(chars_addr.0 + i);
            let char_val = self
                .heap
                .get(char_addr)
                .unwrap()
                .as_u8()
                .unwrap_or_else(|| panic!("expected string char @ {}", char_addr));

            chars.push(char_val as char);
        }

        chars.into_iter().collect()
    }

    pub fn shutdown(mut self) {
        let globals: Vec<_> = self.globals.values().cloned().collect();

        for GlobalCell { value, ty } in globals {
            if let Some(release_target) = ReleaseTarget::for_ty(&ty) {
                self.release_cell(&value, release_target);
            }
        }

        self.heap.finalize()
    }
}
