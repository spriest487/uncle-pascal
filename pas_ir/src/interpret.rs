use std::{
    collections::HashMap,
    f32, fmt,
    ops::{Add, Index, IndexMut, Sub},
    rc::Rc,
};

use crate::{
    metadata::*, Function as IRFunction, FunctionDef, GlobalRef, Instruction, InstructionFormatter, Label,
    LocalID, Module, Ref, Type, Value,
};

use self::heap::{Heap, HeapAddress};

mod builtin;
mod heap;

pub type BuiltinFn = fn(state: &mut Interpreter);

#[derive(Clone)]
pub enum Function {
    Builtin { func: BuiltinFn, ret: Type },
    IR(Rc<FunctionDef>),
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
    VariantTag {
        variant: Box<Pointer>,
    },
    VariantData {
        variant: Box<Pointer>,
        tag: usize,
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
            Pointer::IntoArray { array, offset } => Pointer::IntoArray {
                array,
                offset: offset + rhs,
            },
            Pointer::VariantData { .. }
            | Pointer::VariantTag { .. }
            | Pointer::IntoStruct { .. } => {
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
            Pointer::IntoArray { array, offset } => Pointer::IntoArray {
                array,
                offset: offset - rhs,
            },
            Pointer::VariantData { .. }
            | Pointer::VariantTag { .. }
            | Pointer::IntoStruct { .. } => {
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
pub struct VariantCell {
    pub id: StructID,
    pub tag: Box<MemCell>,
    pub data: Box<MemCell>,
}

#[derive(Debug, Clone)]
pub struct RcCell {
    pub resource_addr: HeapAddress,
    pub ref_count: usize,
    pub struct_id: StructID,
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
    RcCell(Box<RcCell>),
    Function(Function),
    Structure(Box<StructCell>),
    Variant(Box<VariantCell>),
    Pointer(Pointer),
    Array(Box<ArrayCell>),
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
            (MemCell::U8(a), MemCell::U8(b)) => Some(MemCell::U8(a + b)),
            (MemCell::F32(a), MemCell::F32(b)) => Some(MemCell::F32(a + b)),

            (MemCell::Pointer(ptr), MemCell::I32(offset)) => {
                Some(MemCell::Pointer(ptr.clone() + *offset as usize))
            }

            (MemCell::Pointer(Pointer::Heap(ref a)), MemCell::Pointer(Pointer::Heap(ref b))) => {
                Some(MemCell::Pointer(Pointer::Heap(HeapAddress(a.0 + b.0))))
            }

            _ => None,
        }
    }

    pub fn try_sub(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => Some(MemCell::I32(a - b)),
            (MemCell::U8(a), MemCell::U8(b)) => Some(MemCell::U8(a - b)),
            (MemCell::F32(a), MemCell::F32(b)) => Some(MemCell::F32(a - b)),

            (MemCell::Pointer(ptr), MemCell::I32(offset)) => {
                Some(MemCell::Pointer(ptr.clone() - *offset as usize))
            }

            (MemCell::Pointer(Pointer::Heap(ref a)), MemCell::Pointer(Pointer::Heap(ref b))) => {
                Some(MemCell::Pointer(Pointer::Heap(HeapAddress(a.0 - b.0))))
            }

            _ => None,
        }
    }

    pub fn try_mul(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => Some(MemCell::I32(a * b)),
            (MemCell::U8(a), MemCell::U8(b)) => Some(MemCell::U8(a * b)),
            (MemCell::F32(a), MemCell::F32(b)) => Some(MemCell::F32(a * b)),

            _ => None,
        }
    }

    pub fn try_idiv(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (MemCell::I32(a), MemCell::I32(b)) => Some(MemCell::I32(a / b)),
            (MemCell::U8(a), MemCell::U8(b)) => Some(MemCell::U8(a / b)),
            (MemCell::F32(a), MemCell::F32(b)) => Some(MemCell::F32(a / b)),

            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<&Function> {
        match self {
            MemCell::Function(f) => Some(f),
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

    pub fn as_variant(&self, struct_id: StructID) -> Option<&VariantCell> {
        match self {
            MemCell::Variant(var_cell) if struct_id == var_cell.id => Some(var_cell),
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
struct Block {
    decls: Vec<LocalID>,
}

#[derive(Debug)]
struct LocalCell {
    // local cells are allocated on the fly as the interpreter passes LocalAlloc
    // instructions. we want to prevent IR code from alloc-ing two locals with the same
    // ID, but we might also legally run the same alloc instruction more than once if flow control
    // takes us back over it.
    // therefore we need to remember where a local allocation was made,
    // so the duplicate check can tell whether it's two allocations with the same ID
    // function param allocs don't have an alloc locatoin
    alloc_pc: Option<usize>,

    value: MemCell,
}

impl LocalCell {
    fn is_alloc_pc(&self, pc: usize) -> bool {
        match self.alloc_pc {
            None => false,
            Some(alloc_pc) => alloc_pc == pc,
        }
    }
}

#[derive(Debug)]
struct StackFrame {
    locals: Vec<Option<LocalCell>>,
    block_stack: Vec<Block>,
}

impl StackFrame {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            block_stack: vec![Block { decls: Vec::new() }],
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InterpreterOpts {
    pub trace_heap: bool,
    pub trace_rc: bool,
    pub trace_ir: bool,

    pub no_stdlib: bool,
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
            metadata: Metadata::default(),
            globals,
            stack: Vec::new(),
            heap,

            trace_rc: opts.trace_rc,
            trace_ir: opts.trace_ir,
        }
    }

    fn init_struct(&mut self, id: StructID) -> MemCell {
        let struct_def = self.metadata.get_struct_def(id).unwrap().clone();

        let mut fields = Vec::new();
        for (&id, field) in &struct_def.fields {
            // include padding of -1s for non-contiguous IDs
            if id.0 >= fields.len() {
                fields.resize(id.0 + 1, MemCell::I32(-1));
            }
            fields[id.0] = self.default_init_cell(&field.ty);
        }

        MemCell::Structure(Box::new(StructCell { id, fields }))
    }

    fn default_init_cell(&mut self, ty: &Type) -> MemCell {
        match ty {
            Type::I32 => MemCell::I32(-1),
            Type::U8 => MemCell::U8(255),
            Type::Bool => MemCell::Bool(false),
            Type::F32 => MemCell::F32(f32::NAN),

            Type::Struct(id) => self.init_struct(*id),

            Type::RcPointer(_) => MemCell::Pointer(Pointer::Null),

            Type::Pointer(_target) => MemCell::Pointer(Pointer::Uninit),

            Type::Array { element, dim } => {
                let mut elements = Vec::new();
                for _ in 0..*dim {
                    elements.push(self.default_init_cell(element.as_ref()));
                }

                MemCell::Array(Box::new(ArrayCell {
                    el_ty: (**element).clone(),
                    elements,
                }))
            }

            Type::Variant(id) => MemCell::Variant(Box::new(VariantCell {
                id: *id,
                tag: Box::new(MemCell::I32(0)),
                data: Box::new(MemCell::Pointer(Pointer::Null)),
            })),

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
                    .map(|cell| &cell.value)
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

            Pointer::VariantTag { variant } => {
                match self.deref_ptr(variant) {
                    MemCell::Variant(variant_cell) => variant_cell.tag.as_ref(),
                    other => panic!("dereferencing variant tag pointer which doens't point to a variant cell (was: {:#?}", other),
                }
            }

            Pointer::VariantData { variant, tag } => {
                let expect_tag = *tag as i32; //todo proper size type
                match self.deref_ptr(variant) {
                    MemCell::Variant(variant_cell) => {
                        assert_eq!(variant_cell.tag.as_i32().unwrap(), expect_tag);
                        variant_cell.data.as_ref()
                    },
                    other => panic!("dereferencing variant tag pointer which doens't point to a variant cell (was: {:#?}", other),
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
                    .map(|cell| &mut cell.value)
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

            Pointer::VariantTag { variant } => {
                match self.deref_ptr_mut(variant) {
                    MemCell::Variant(variant_cell) => variant_cell.tag.as_mut(),
                    other => panic!("dereferencing variant tag pointer which doens't point to a variant cell (was: {:#?}", other),
                }
            }

            Pointer::VariantData { variant, tag } => {
                let expect_tag = *tag as i32; //todo proper size type
                match self.deref_ptr_mut(variant) {
                    MemCell::Variant(variant_cell) => {
                        assert_eq!(variant_cell.tag.as_i32().unwrap(), expect_tag);
                        variant_cell.data.as_mut()
                    },
                    other => panic!("dereferencing variant tag pointer which doens't point to a variant cell (was: {:#?}", other),
                }
            }

            Pointer::Uninit => {
                panic!("dereferencing uninitialized pointer");
            }

            Pointer::Null => panic!("dereferencing null pointer"),
        }
    }

    fn store(&mut self, at: &Ref, val: MemCell) {
        //        println!("{} <- {:#?}", at, val);

        match at {
            Ref::Local(LocalID(id)) => match self.current_frame_mut().locals.get_mut(*id) {
                Some(Some(cell)) => {
                    cell.value = val;
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
                        // todo: for the moment this means no RC for global cells stored after init
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
                Some(Some(cell)) => &cell.value,
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
            Value::LiteralByte(i) => MemCell::U8(*i),
            Value::LiteralF32(f) => MemCell::F32(*f),
            Value::LiteralBool(b) => MemCell::Bool(*b),
            Value::LiteralNull => MemCell::Pointer(Pointer::Null),
        }
    }

    fn push_stack(&mut self) {
        self.stack.push(StackFrame::new());
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

    fn vcall_lookup(
        &self,
        self_cell: &MemCell,
        iface_id: InterfaceID,
        method: MethodID,
    ) -> FunctionID {
        let self_rc = self_cell
            .as_pointer()
            .and_then(|rc_ptr| {
                let rc_addr = rc_ptr.as_heap_addr()?;
                let rc_cell = self.heap.get(rc_addr)?;
                rc_cell.as_rc()
            })
            .unwrap_or_else(|| {
                panic!(
                    "expected target of virtual call {}.{} to be an rc cell, but found {:?}",
                    iface_id, method.0, self_cell
                )
            });

        let instance_ty = Type::RcPointer(Some(ClassID::Class(self_rc.struct_id)));

        self.metadata
            .find_impl(&instance_ty, iface_id, method)
            .unwrap_or_else(|| {
                let mut err = "virtual call ".to_string();

                let iface_ty = Type::RcPointer(Some(ClassID::Interface(iface_id)));
                let _ = self.metadata.format_type(&iface_ty, &mut err);
                err.push('.');
                let _ = self.metadata.format_method(iface_id, method, &mut err);
                err.push_str(" missing implementation for ");
                let _ = self.metadata.format_type(&instance_ty, &mut err);

                panic!(err)
            })
    }

    fn call(&mut self, func: &Function, args: &[MemCell], out: Option<&Ref>) {
        self.push_stack();

        // store empty result at $0 if needed
        let return_ty = func.return_ty();
        if *return_ty != Type::Nothing {
            let result_cell = LocalCell {
                alloc_pc: None,
                value: self.default_init_cell(return_ty),
            };
            self.current_frame_mut().locals.push(Some(result_cell));
        }

        // store params in either $0.. or $1..
        self.current_frame_mut()
            .locals
            .extend(args.iter().cloned().map(|arg| {
                Some(LocalCell {
                    value: arg,
                    alloc_pc: None,
                })
            }));

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
        let dispose_impl_id = self
            .metadata
            .find_impl(ty, DISPOSABLE_ID, DISPOSABLE_DISPOSE_INDEX);

        if let Some(dispose_func) = dispose_impl_id {
            let dispose_desc = self
                .metadata
                .func_desc(dispose_func)
                .unwrap_or_else(|| dispose_func.to_string());
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
        } else if self.trace_rc {
            eprintln!("rc: no disposer for {}", self.metadata.pretty_ty_name(ty));
        }
    }

    fn release_cell(&mut self, cell: &MemCell) {
        let ptr = cell
            .as_pointer()
            .unwrap_or_else(|| panic!("released cell was not a pointer, found: {:?}", cell));
        // NULL is a valid release target because we release uninitialized local RC pointers
        // just do nothing
        if *ptr == Pointer::Null {
            return;
        }

        let rc_addr = ptr
            .as_heap_addr()
            .unwrap_or_else(|| panic!("released cell was not a heap pointer, found: {:?}", cell));

        let rc_cell = match &self.heap[rc_addr] {
            MemCell::RcCell(rc_cell) => rc_cell.clone(),
            other => panic!("released cell was not an rc value, found: {:?}", other),
        };

        let resource_ty = Type::Struct(rc_cell.struct_id);

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
            let resource_id = ClassID::Class(rc_cell.struct_id);
            self.invoke_disposer(&cell, &Type::RcPointer(Some(resource_id)));

            // Now release the fields of the resource struct as if it was a normal record.
            // This could technically invoke another disposer, but it won't, because there's
            // no way to declare a disposer for the inner resource type of an RC type
            let rc_funcs = self
                .metadata
                .find_rc_boilerplate(&resource_ty)
                .unwrap_or_else(|| {
                    let name = self.metadata.pretty_ty_name(&resource_ty);
                    let funcs = self
                        .metadata
                        .rc_boilerplate_funcs()
                        .map(|(ty, funcs)| {
                            format!(
                                "  {}: release={}, retain={}",
                                self.metadata.pretty_ty_name(ty),
                                funcs.release,
                                funcs.retain,
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("\n");

                    panic!("missing rc boilerplate for class {} in:\n{}", name, funcs);
                });

            // todo: should function cells be Rc<Function> so we don't need to clone them here?
            let release_func = self.globals[&GlobalRef::Function(rc_funcs.release)]
                .value
                .as_function()
                .cloned()
                .unwrap();

            let release_ptr = MemCell::Pointer(Pointer::Heap(rc_cell.resource_addr));

            self.call(&release_func, &[release_ptr], None);

            if self.trace_rc {
                eprintln!(
                    "rc: free {} @ {}",
                    self.metadata.pretty_ty_name(&resource_ty),
                    rc_addr
                )
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

            self.heap[rc_addr] = MemCell::RcCell(Box::new(RcCell {
                ref_count: rc_cell.ref_count - 1,
                ..*rc_cell
            }));
        }
    }

    fn retain_cell(&mut self, cell: &MemCell) {
        match cell {
            MemCell::Pointer(Pointer::Heap(addr)) => {
                let rc_cell = self.heap[*addr]
                    .as_rc_mut()
                    .expect("retained cell must point to an rc cell");

                if self.trace_rc {
                    eprintln!("rc: retain @ {}", addr);
                }

                rc_cell.ref_count += 1;
            }

            _ => panic!("{:?} cannot be retained"),
        }
    }

    fn addr_of_ref(&self, target: &Ref) -> Pointer {
        match target {
            // let int := 1;
            // let intPtr := @int;
            // @(intPtr^) -> address of int behind intPtr
            Ref::Deref(val) => match self.evaluate(val) {
                MemCell::Pointer(ptr) => ptr.clone(),

                _ => panic!("deref of non-pointer value @ {}", val),
            },

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
        let labels = find_labels(instructions);
        let line_count_width = instructions.len().to_string().len();

        let mut pc = 0;
        while pc < instructions.len() {
            if self.trace_ir {
                let mut instruction_str = String::new();
                self.metadata
                    .format_instruction(&instructions[pc], &mut instruction_str)
                    .unwrap();
                eprintln!(
                    "{:>width$}| {}",
                    pc,
                    instruction_str,
                    width = line_count_width
                );
            }

            self.execute_instruction(&instructions[pc], &mut pc, &labels);

            pc += 1;
        }
    }

    fn execute_instruction(
        &mut self,
        instruction: &Instruction,
        pc: &mut usize,
        labels: &HashMap<Label, LabelLocation>,
    ) {
        match instruction {
            Instruction::Comment(_) => {
                // noop
            }

            Instruction::LocalAlloc(id, ty) => {
                let uninit_cell = self.default_init_cell(ty);

                let frame = self.current_frame_mut();
                while frame.locals.len() <= id.0 {
                    frame.locals.push(None);
                }

                match &mut frame.locals[id.0] {
                    None => {
                        frame.locals[id.0] = Some(LocalCell {
                            value: uninit_cell,
                            alloc_pc: Some(*pc),
                        })
                    }
                    Some(already_allocated) => {
                        // the same ID can only be reused if this is the same instruction that
                        // allocated it in the first place
                        if !already_allocated.is_alloc_pc(*pc) {
                            panic!("local cell {} is already allocated", id)
                        }
                        already_allocated.value = uninit_cell;
                    }
                }

                frame
                    .block_stack
                    .last_mut()
                    .expect("block stack must never be empty")
                    .decls
                    .push(*id);
            }

            Instruction::LocalBegin => {
                let frame = self.current_frame_mut();
                frame.block_stack.push(Block { decls: Vec::new() });
            }

            Instruction::LocalEnd => {
                let frame = self.current_frame_mut();

                let popped_block = frame
                    .block_stack
                    .pop()
                    .expect("block stack must never be empty");

                for LocalID(id) in popped_block.decls {
                    if id >= frame.locals.len() {
                        panic!("local cell {} is not allocated", id);
                    }
                    frame.locals[id] = None;
                }
            }

            Instruction::RcNew { out, struct_id } => {
                let struct_ty = Type::Struct(*struct_id);
                let init_cells = vec![self.default_init_cell(&struct_ty)];

                let rc_ptr = self.rc_alloc(init_cells, *struct_id);

                self.store(out, MemCell::Pointer(rc_ptr));
            }

            Instruction::Add { out, a, b } => match self.evaluate(a).try_add(&self.evaluate(b)) {
                Some(result) => self.store(out, result),
                None => panic!(
                    "Add is not valid for {:?} + {:?}",
                    self.evaluate(a),
                    self.evaluate(b)
                ),
            },

            Instruction::Mul { out, a, b } => match self.evaluate(a).try_mul(&self.evaluate(b)) {
                Some(result) => self.store(out, result),
                None => panic!(
                    "Mul is not valid for {:?} * {:?}",
                    self.evaluate(a),
                    self.evaluate(b)
                ),
            }

            Instruction::IDiv { out, a, b } => match self.evaluate(a).try_idiv(&self.evaluate(b)) {
                Some(result) => self.store(out, result),
                None => panic!(
                    "IDiv is not valid for {:?} div {:?}",
                    self.evaluate(a),
                    self.evaluate(b)
                ),
            },

            Instruction::Sub { out, a, b } => match self.evaluate(a).try_sub(&self.evaluate(b)) {
                Some(result) => self.store(out, result),
                None => panic!(
                    "Sub is not valid for {:?} - {:?}",
                    self.evaluate(a),
                    self.evaluate(b)
                ),
            },

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
                let arg_cells: Vec<_> = args.iter().map(|arg_val| self.evaluate(arg_val)).collect();

                match self.evaluate(function) {
                    MemCell::Function(function) => self.call(&function, &arg_cells, out.as_ref()),

                    _ => panic!("{} does not reference a function", function),
                }
            }

            Instruction::VirtualCall {
                out,
                iface_id,
                method,
                self_arg,
                rest_args,
            } => {
                let self_cell = self.evaluate(&self_arg);
                let func = self.vcall_lookup(&self_cell, *iface_id, *method);

                let mut arg_cells = vec![self_cell];
                arg_cells.extend(rest_args.iter().map(|arg_val| self.evaluate(arg_val)));

                let func_ref = Ref::Global(GlobalRef::Function(func));
                let func = match self.evaluate(&Value::Ref(func_ref)) {
                    MemCell::Function(func) => func,
                    unexpected => panic!("invalid function cell: {:?}", unexpected),
                };

                self.call(&func, &arg_cells, out.as_ref())
            }

            Instruction::ClassIs { out, a, class_id } => {
                let rc_addr = self
                    .evaluate(a)
                    .as_pointer()
                    .and_then(Pointer::as_heap_addr)
                    .expect("argument a of ClassIs instruction must evaluate to a heap pointer");
                let rc_cell =
                    self.heap.get(rc_addr).and_then(MemCell::as_rc).expect(
                        "rc pointer target of ClassIs instruction must point to an rc cell",
                    );

                let is = match class_id {
                    ClassID::Class(struct_id) => rc_cell.struct_id == *struct_id,
                    ClassID::Interface(iface_id) => {
                        let resource_id = ClassID::Class(rc_cell.struct_id);
                        let actual_ty = Type::RcPointer(Some(resource_id));

                        self.metadata.is_impl(&actual_ty, *iface_id)
                    }
                };

                self.store(out, MemCell::Bool(is));
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
                let field_ptr = match of_ty {
                    Type::Struct(..) => {
                        let struct_ptr = self.addr_of_ref(a);

                        Pointer::IntoStruct {
                            field: *field,
                            structure: Box::new(struct_ptr),
                        }
                    }

                    Type::RcPointer(..) => {
                        let struct_ptr = self
                            .load(&a.clone().deref())
                            .as_rc()
                            .map(|rc_cell| Pointer::Heap(rc_cell.resource_addr))
                            .unwrap_or_else(|| {
                                panic!("trying to read field pointer of rc type but target wasn't an rc cell @ {}", a);
                            });

                        Pointer::IntoStruct {
                            field: *field,
                            structure: Box::new(struct_ptr),
                        }
                    }

                    _ => panic!(
                        "invalid base type referenced in Field instruction: {}.{}",
                        of_ty, field
                    ),
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

            Instruction::VariantTag { out, a, .. } => {
                let a_ptr = self.addr_of_ref(a);
                self.store(
                    out,
                    MemCell::Pointer(Pointer::VariantTag {
                        variant: Box::new(a_ptr),
                    }),
                );
            }

            Instruction::VariantData { out, a, tag, .. } => {
                let a_ptr = self.addr_of_ref(a);
                self.store(
                    out,
                    MemCell::Pointer(Pointer::VariantData {
                        variant: Box::new(a_ptr),
                        tag: *tag,
                    }),
                );
            }

            Instruction::Label(_) => {
                // noop
            }

            Instruction::Jump { dest } => {
                self.jump(pc, &labels[dest]);
            }

            Instruction::JumpIf { dest, test } => match self.evaluate(test) {
                MemCell::Bool(true) => {
                    self.jump(pc, &labels[dest]);
                }
                MemCell::Bool(false) => {}
                _ => panic!("JumpIf instruction testing non-boolean cell"),
            },

            Instruction::Release { at } => {
                let cell = self.load(at).clone();
                self.release_cell(&cell);
            }

            Instruction::Retain { at } => {
                let cell = self.load(at).clone();
                self.retain_cell(&cell);
            }

            Instruction::DynAlloc {
                out,
                element_ty,
                len,
            } => {
                let len = self
                    .evaluate(len)
                    .as_i32()
                    .expect("len of DynAlloc must be i32");

                let mut cells = Vec::new();
                for _ in 0..len {
                    cells.push(self.default_init_cell(element_ty));
                }

                let addr = self.heap.alloc(cells);
                let ptr = Pointer::Heap(addr);

                self.store(out, MemCell::Pointer(ptr));
            }

            Instruction::DynFree { at } => match self.load(at) {
                MemCell::Pointer(Pointer::Heap(addr)) => {
                    let addr = *addr;
                    self.heap.free(addr);
                }

                x => panic!("target of DynFree at {} must be pointer, was: {:?}", at, x),
            },
        }
    }

    fn jump(&mut self, pc: &mut usize, label: &LabelLocation) {
        *pc = label.pc_offset;

        // assume all jumps are either upwards or to the same level
        let frame = self.current_frame_mut();
        let current_block = frame.block_stack.len() - 1;

        assert!(
            current_block >= label.block_depth,
            "jmp from block level {} to {} is invalid, can only jmp upwards in the block stack",
            current_block,
            label.block_depth
        );

        let pop_blocks = current_block - label.block_depth;

        for _ in 0..pop_blocks {
            let popped_block = frame.block_stack.pop().unwrap();
            for decl in popped_block.decls {
                frame.locals[decl.0] = None;
            }
        }
    }

    fn rc_alloc(&mut self, vals: Vec<MemCell>, ty_id: StructID) -> Pointer {
        let resource_addr = self.heap.alloc(vals);

        let rc_cell = MemCell::RcCell(Box::new(RcCell {
            ref_count: 1,
            resource_addr,
            struct_id: ty_id,
        }));

        let addr = self.heap.alloc(vec![rc_cell]);
        Pointer::Heap(addr)
    }

    fn define_builtin(&mut self, name: GlobalName, func: BuiltinFn, ret: Type) {
        if let Some(func_id) = self.metadata.find_function(&name) {
            self.globals.insert(
                GlobalRef::Function(func_id),
                GlobalCell {
                    value: MemCell::Function(Function::Builtin { func, ret }),
                    ty: Type::Nothing,
                },
            );
        } else {
            eprintln!(
                "define_builtin: no declaration for defined function `{}`",
                name
            );
        }
    }

    fn init_stdlib_globals(&mut self) {
        let system_funcs: &[(&str, BuiltinFn, Type)] = &[
            ("IntToStr", builtin::int_to_str, Type::string_ptr()),
            ("StrToInt", builtin::str_to_int, Type::I32),
            ("CompareStr", builtin::compare_str, Type::I32),
            ("WriteLn", builtin::write_ln, Type::Nothing),
            ("ReadLn", builtin::read_ln, Type::string_ptr()),
            ("GetMem", builtin::get_mem, Type::U8.ptr()),
            ("FreeMem", builtin::free_mem, Type::Nothing),
        ];

        for (ident, func, ret) in system_funcs {
            let name = GlobalName::new(ident.to_string(), vec!["System"]);
            self.define_builtin(name, *func, ret.clone());
        }
    }

    pub fn load_module(&mut self, module: &Module, init_stdlib: bool) {
        self.metadata.extend(&module.metadata);

        for (func_name, func) in &module.functions {
            if let IRFunction::Local(func_def) = func {
                let func_cell = MemCell::Function(Function::IR(Rc::new(func_def.clone())));
                let func_ref = GlobalRef::Function(*func_name);

                self.globals.insert(
                    func_ref,
                    GlobalCell {
                        value: func_cell,
                        ty: Type::Nothing,
                    },
                );
            }
        }

        for (id, literal) in module.metadata.strings() {
            let str_cell = self.create_string(literal);
            let str_ref = GlobalRef::StringLiteral(id);

            self.globals.insert(
                str_ref,
                GlobalCell {
                    value: str_cell,
                    ty: Type::RcPointer(Some(ClassID::Class(STRING_ID))),
                },
            );
        }

        if init_stdlib {
            self.init_stdlib_globals();
        }

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

        let str_cell = MemCell::Structure(Box::new(StructCell {
            id: STRING_ID,
            fields: str_fields,
        }));

        let str_ptr = self.rc_alloc(vec![str_cell], STRING_ID);
        MemCell::Pointer(str_ptr)
    }

    fn read_string(&self, str_ref: &Ref) -> String {
        let str_cell = self.deref_rc(self.load(str_ref));

        let str_cell = match str_cell.as_struct(STRING_ID) {
            Some(struct_cell) => struct_cell,
            None => panic!(
                "tried to read string value from rc cell which didn't contain a string struct: {:?}",
                str_cell,
            )
        };

        let len = &str_cell[STRING_LEN_FIELD].as_i32().unwrap();

        if *len == 0 {
            return String::new();
        }

        let chars_addr = &str_cell[STRING_CHARS_FIELD]
            .as_pointer()
            .and_then(Pointer::as_heap_addr)
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
            if ty.is_rc() {
                self.release_cell(&value);
            }
        }

        self.heap.finalize()
    }
}

struct LabelLocation {
    pc_offset: usize,
    block_depth: usize,
}

fn find_labels(instructions: &[Instruction]) -> HashMap<Label, LabelLocation> {
    let mut block_depth = 0;
    let mut locations = HashMap::new();

    for (pc_offset, instruction) in instructions.iter().enumerate() {
        match instruction {
            Instruction::LocalBegin => block_depth += 1,
            Instruction::LocalEnd => block_depth -= 1,
            Instruction::Label(label) => {
                locations.insert(
                    label.clone(),
                    LabelLocation {
                        block_depth,
                        pc_offset,
                    },
                );
            }
            _ => continue,
        }
    }

    locations
}
