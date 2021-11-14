pub use self::{
    heap::{Heap, HeapAddress},
    memcell::*,
    ptr::Pointer,
};
use crate::{
    func::ffi::FfiCache,
    func::{BuiltinFn, BuiltinFunction, Function},
    stack::StackFrame,
};
use pas_ir::{
    metadata::*, Function as IRFunction, GlobalRef, Instruction, InstructionFormatter, Label,
    LocalID, Module, Ref, Type, Value,
};
use std::{collections::HashMap, rc::Rc};

use crate::result::{ExecError, ExecResult};
use pas_common::span::Span;
use pas_ir::metadata::ty::{ClassID, FieldID};

mod builtin;
mod func;
mod heap;
mod memcell;
mod ptr;
mod stack;
pub mod result;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InterpreterOpts {
    pub trace_heap: bool,
    pub trace_rc: bool,
    pub trace_ir: bool,

    pub no_stdlib: bool,
}

#[derive(Debug, Clone)]
pub struct GlobalCell {
    pub value: MemCell,
    ty: Type,
}

#[derive(Debug)]
pub struct Interpreter {
    metadata: Metadata,
    stack: Vec<StackFrame>,
    globals: HashMap<GlobalRef, GlobalCell>,
    heap: Heap,

    ffi_cache: FfiCache,

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

            ffi_cache: FfiCache::new(),

            trace_rc: opts.trace_rc,
            trace_ir: opts.trace_ir,
        }
    }

    fn init_struct(&self, id: StructID) -> ExecResult<MemCell> {
        let struct_def = self.metadata.get_struct_def(id).cloned()
            .ok_or_else(|| {
                let msg = format!("missing struct definition in metadata: {}", id);
                ExecError::illegal_state(msg)
            })?;

        let mut fields = Vec::new();
        for (&id, field) in &struct_def.fields {
            // include padding of -1s for non-contiguous IDs
            if id.0 >= fields.len() {
                fields.resize(id.0 + 1, MemCell::I32(-1));
            }
            fields[id.0] = self.default_init_cell(&field.ty)?;
        }

        let struct_cell = StructCell { id, fields };

        Ok(MemCell::Structure(Box::new(struct_cell)))
    }

    fn default_init_cell(&self, ty: &Type) -> ExecResult<MemCell> {
        let cell = match ty {
            Type::I32 => MemCell::I32(-1),
            Type::U8 => MemCell::U8(255),
            Type::Bool => MemCell::Bool(false),
            Type::F32 => MemCell::F32(f32::NAN),

            Type::Struct(id) => self.init_struct(*id)?,

            Type::RcPointer(_) => MemCell::Pointer(Pointer::Uninit),

            Type::Pointer(_target) => MemCell::Pointer(Pointer::Uninit),

            Type::Array { element, dim } => {
                let mut elements = Vec::new();
                for _ in 0..*dim {
                    let el = self.default_init_cell(element.as_ref())?;
                    elements.push(el);
                }

                MemCell::Array(Box::new(ArrayCell {
                    el_ty: (**element).clone(),
                    elements,
                }))
            }

            Type::Variant(id) => MemCell::Variant(Box::new(VariantCell {
                id: *id,
                tag: Box::new(MemCell::I32(0)),
                data: Box::new(MemCell::Pointer(Pointer::Uninit)),
            })),

            _ => {
                let msg = format!("can't initialize default cell of type `{:?}`", ty);
                return Err(ExecError::illegal_state(msg));
            }
        };

        Ok(cell)
    }

    fn deref_ptr(&self, ptr: &Pointer) -> ExecResult<&MemCell> {
        match ptr {
            Pointer::Heap(slot) => {
                let heap_cell = self
                    .heap
                    .get(*slot)
                    .unwrap_or_else(|| panic!("heap cell {} is not allocated: {:?}", slot, pas_common::Backtrace::new()));

                Ok(heap_cell)
            },

            Pointer::Local { frame, id, .. } => {
                self.stack[*frame].deref_local(*id)
            }

            Pointer::IntoArray { array, offset, .. } => {
                match self.deref_ptr(array)? {
                    MemCell::Array(array_cell) => {
                        Ok(&array_cell.elements[*offset])
                    }

                    invalid => {
                        let msg = "referencing array element";
                        return Err(ExecError::expected_ty(msg, MemCellKind::Array, invalid.kind()));
                    },
                }
            }

            Pointer::IntoStruct { structure, field, .. } => {
                match self.deref_ptr(structure)? {
                    MemCell::Structure(struct_cell) => {
                        Ok(&struct_cell[*field])
                    }

                    invalid => {
                        let msg = "referencing struct field";
                        return Err(ExecError::expected_ty(msg, MemCellKind::Structure, invalid.kind()));
                    }
                }
            }

            Pointer::VariantTag { variant } => {
                match self.deref_ptr(variant)? {
                    MemCell::Variant(variant_cell) => Ok(variant_cell.tag.as_ref()),
                    invalid => {
                        let msg = "dereferencing variant tag pointer which doesn't point to a variant cell";
                        return Err(ExecError::expected_ty(msg, MemCellKind::Variant, invalid.kind()));
                    },
                }
            }

            Pointer::VariantData { variant, tag } => {
                let expect_tag = *tag as i32; //todo proper size type
                match self.deref_ptr(variant)? {
                    MemCell::Variant(variant_cell) => {
                        assert_eq!(variant_cell.tag.as_i32(), Some(expect_tag));
                        Ok(variant_cell.data.as_ref())
                    },

                    invalid => {
                        let msg = "dereferencing variant tag pointer which doesn't point to a variant cell";
                        return Err(ExecError::expected_ty(msg, MemCellKind::Variant, invalid.kind()));
                    },
                }
            }

            _ => {
                return Err(ExecError::IllegalDereference(ptr.kind()))
            }
        }
    }

    fn deref_ptr_mut(&mut self, ptr: &Pointer) -> ExecResult<&mut MemCell> {
        match ptr {
            Pointer::Heap(slot) => {
                let heap_cell = self
                    .heap
                    .get_mut(*slot)
                    .unwrap_or_else(|| panic!("heap cell {} is not allocated: {:?}", slot, pas_common::Backtrace::new()));

                Ok(heap_cell)
            },

            Pointer::Local { frame, id, .. } => {
                self.stack[*frame].deref_local_mut(*id)
            }

            Pointer::IntoArray { array, offset, .. } => {
                match self.deref_ptr_mut(array)? {
                    MemCell::Array(array_cell) => {
                        Ok(&mut array_cell.elements[*offset])
                    }

                    invalid => {
                        let msg = "referencing array element";
                        return Err(ExecError::expected_ty(msg, MemCellKind::Array, invalid.kind()));
                    },
                }
            }

            Pointer::IntoStruct { structure, field, .. } => {
                match self.deref_ptr_mut(structure)? {
                    MemCell::Structure(struct_cell) => {
                        Ok(&mut struct_cell[*field])
                    }

                    invalid => {
                        let msg = "referencing struct field";
                        return Err(ExecError::expected_ty(msg, MemCellKind::Structure, invalid.kind()));
                    }
                }
            }

            Pointer::VariantTag { variant } => {
                match self.deref_ptr_mut(variant)? {
                    MemCell::Variant(variant_cell) => Ok(variant_cell.tag.as_mut()),
                    invalid => {
                        let msg = "dereferencing variant tag pointer which doesn't point to a variant cell";
                        return Err(ExecError::expected_ty(msg, MemCellKind::Variant, invalid.kind()));
                    },
                }
            }

            Pointer::VariantData { variant, tag } => {
                let expect_tag = *tag as i32; //todo proper size type
                match self.deref_ptr_mut(variant)? {
                    MemCell::Variant(variant_cell) => {
                        assert_eq!(variant_cell.tag.as_i32(), Some(expect_tag));
                        Ok(variant_cell.data.as_mut())
                    },

                    invalid => {
                        let msg = "dereferencing variant tag pointer which doesn't point to a variant cell";
                        return Err(ExecError::expected_ty(msg, MemCellKind::Variant, invalid.kind()));
                    },
                }
            }

            _ => {
                return Err(ExecError::IllegalDereference(ptr.kind()))
            }
        }
    }

    fn store(&mut self, at: &Ref, val: MemCell) -> ExecResult<()> {
        match at {
            Ref::Discard => {
                // do nothing with this value
                Ok(())
            }

            Ref::Local(id) => {
                self.current_frame_mut()?.store_local(*id, val)
            },

            Ref::Global(name) => {
                if self.globals.contains_key(name) {
                    return Err(ExecError::illegal_state(format!("global cell `{}` is already allocated", name)));
                }

                self.globals.insert(
                    name.clone(),
                    GlobalCell {
                        value: val,
                        // todo: for the moment this means no RC for global cells stored after init
                        ty: Type::Nothing,
                    },
                );

                Ok(())
            }

            Ref::Deref(inner) => match self.evaluate(inner)? {
                MemCell::Pointer(ptr) => {
                    let ptr_deref_cell = self.deref_ptr_mut(&ptr)?;
                    *ptr_deref_cell = val;

                    Ok(())
                }

                x => {
                    let msg = format!("can't dereference non-pointer cell with value {:?}", x);
                    Err(ExecError::illegal_state(msg))
                },
            },
        }
    }

    fn load(&self, at: &Ref) -> ExecResult<&MemCell> {
        match at {
            Ref::Discard => Err(ExecError::illegal_state("can't read value from discard ref")),

            Ref::Local(id) => self.current_frame()?.load_local(*id),

            Ref::Global(name) => match self.globals.get(name) {
                Some(cell) => Ok(&cell.value),
                None => {
                    let msg = format!("global cell `{}` is not allocated", name);
                    Err(ExecError::illegal_state(msg))
                }
            },

            Ref::Deref(inner) => match self.evaluate(inner)? {
                MemCell::Pointer(ptr) => self.deref_ptr(&ptr),
                x => {
                    let msg = format!("can't dereference cell {:?}", x);
                    Err(ExecError::illegal_state(msg))
                },
            },
        }
    }

    fn evaluate(&self, val: &Value) -> ExecResult<MemCell> {
        match val {
            Value::Ref(r) => {
                let ref_val = self.load(r)?;
                Ok(ref_val.clone())
            },

            Value::LiteralI32(i) => Ok(MemCell::I32(*i)),
            Value::LiteralByte(i) => Ok(MemCell::U8(*i)),
            Value::LiteralF32(f) => Ok(MemCell::F32(*f)),
            Value::LiteralBool(b) => Ok(MemCell::Bool(*b)),
            Value::LiteralNull => Ok(MemCell::Pointer(Pointer::Null)),
        }
    }

    fn push_stack(&mut self, name: impl Into<String>) {
        let stack_frame = StackFrame::new(name);
        self.stack.push(stack_frame);
    }

    fn pop_stack(&mut self) -> ExecResult<()> {
        self.stack.pop().ok_or_else(|| ExecError::illegal_state("popped stack with no stackframes"))?;
        Ok(())
    }

    fn current_frame(&self) -> ExecResult<&StackFrame> {
        self.stack
            .last()
            .ok_or_else(|| ExecError::illegal_state("called current_frame without no stackframes"))
    }

    fn current_frame_mut(&mut self) -> ExecResult<&mut StackFrame> {
        self.stack
            .last_mut()
            .ok_or_else(|| ExecError::illegal_state("called current_frame without no stackframes"))
    }

    fn vcall_lookup(
        &self,
        self_cell: &MemCell,
        iface_id: InterfaceID,
        method: MethodID,
    ) -> ExecResult<FunctionID> {
        let self_rc = self_cell
            .as_pointer()
            .and_then(|rc_ptr| {
                let rc_addr = rc_ptr.as_heap_addr()?;
                let rc_cell = self.heap.get(rc_addr)?;
                rc_cell.as_rc()
            })
            .ok_or_else(|| {
                ExecError::illegal_state(format!(
                    "expected target of virtual call {}.{} to be an rc cell, but found {:?}",
                    iface_id, method.0, self_cell
                ))
            })?;

        let instance_ty = Type::RcPointer(Some(ClassID::Class(self_rc.struct_id)));

        self.metadata
            .find_impl(&instance_ty, iface_id, method)
            .ok_or_else(|| {
                let mut err = "virtual call ".to_string();

                let iface_ty = Type::RcPointer(Some(ClassID::Interface(iface_id)));
                let _ = self.metadata.format_type(&iface_ty, &mut err);
                err.push('.');
                let _ = self.metadata.format_method(iface_id, method, &mut err);
                err.push_str(" missing implementation for ");
                let _ = self.metadata.format_type(&instance_ty, &mut err);

                ExecError::illegal_state(format!("{}", err))
            })
    }

    fn call(&mut self, func: &Function, args: &[MemCell], out: Option<&Ref>) -> ExecResult<()> {
        self.push_stack(func.debug_name());

        // store empty result at $0 if needed
        let return_ty = func.return_ty();
        if *return_ty != Type::Nothing {
            let result_cell = self.default_init_cell(return_ty)?;
            self.current_frame_mut()?.push_local(result_cell);
        }

        // store params in either $0.. or $1..
        for arg_cell in args.iter().cloned() {
            self.current_frame_mut()?.push_local(arg_cell);
        }

        func.invoke(self)?;

        let result_cell = match return_ty {
            Type::Nothing => None,
            _ => {
                let ref_local_0 = Ref::Local(LocalID(0));
                let return_val = self.evaluate(&Value::Ref(ref_local_0))?;
                Some(return_val)
            }
        };

        self.pop_stack()?;

        match (result_cell, out) {
            (Some(result_cell), Some(out_at)) => {
                self.store(&out_at, result_cell)?;
            }

            (None, Some(_)) => {
                let msg = "called function which has no return type in a context where a return value was expected";
                return Err(ExecError::illegal_state(msg));
            }

            // ok, no output expected, ignore result if there is one
            (_, None) => {}
        }

        Ok(())
    }

    fn invoke_disposer(&mut self, cell: &MemCell, ty: &Type) -> ExecResult<()> {
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

            self.call(&func, &[cell.clone()], None)?;
        } else if self.trace_rc {
            eprintln!("rc: no disposer for {}", self.metadata.pretty_ty_name(ty));
        }

        Ok(())
    }

    fn release_cell(&mut self, cell: &MemCell) -> ExecResult<()> {
        let ptr = cell
            .as_pointer()
            .unwrap_or_else(|| panic!("released cell was not a pointer, found: {:?}", cell));
        // NULL is a valid release target because we release uninitialized local RC pointers
        // just do nothing
        if *ptr == Pointer::Null {
            return Ok(());
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
            self.invoke_disposer(&cell, &Type::RcPointer(Some(resource_id)))?;

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

                    panic!("missing rc boilerplate for {} in:\n{}", name, funcs);
                });

            // todo: should function cells be Rc<Function> so we don't need to clone them here?
            let release_func = self.globals[&GlobalRef::Function(rc_funcs.release)]
                .value
                .as_function()
                .cloned()
                .unwrap();

            let release_ptr = MemCell::Pointer(Pointer::Heap(rc_cell.resource_addr));

            self.call(&release_func, &[release_ptr], None)?;

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

        Ok(())
    }

    fn retain_cell(&mut self, cell: &MemCell) -> ExecResult<()> {
        match cell {
            MemCell::Pointer(Pointer::Heap(addr)) => {
                let rc_cell = match &mut self.heap[*addr] {
                    MemCell::RcCell(rc_cell) => rc_cell.as_mut(),
                    other => {
                        let msg = format!("retained cell must point to an rc cell, found: {:?}", other);
                        return Err(ExecError::illegal_state(msg));
                    }
                };

                if self.trace_rc {
                    eprintln!("rc: retain @ {}", addr);
                }

                rc_cell.ref_count += 1;

                Ok(())
            }

            _ => {
                Err(ExecError::illegal_state(format!("{:?} cannot be retained", cell)))
            },
        }
    }

    fn addr_of_ref(&self, target: &Ref) -> ExecResult<Pointer> {
        match target {
            Ref::Discard => panic!("can't take address of discard ref"),

            // let int := 1;
            // let intPtr := @int;
            // @(intPtr^) -> address of int behind intPtr
            Ref::Deref(val) => match self.evaluate(val)? {
                MemCell::Pointer(ptr) => Ok(ptr.clone()),

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
                        if stack_frame.is_allocated(*id) {
                            Some(frame_id)
                        } else {
                            None
                        }
                    })
                    .next()
                    .ok_or_else(|| {
                        let msg = format!("id {} is not allocated in any stack frames", id);
                        ExecError::illegal_state(msg)
                    })?;

                Ok(Pointer::Local {
                    frame: frame_id,
                    id: *id,
                })
            }

            Ref::Global(global) => {
                let msg = format!("can't take address of global ref {:?}", global);
                Err(ExecError::illegal_state(msg))
            },
        }
    }

    pub fn execute(&mut self, instructions: &[Instruction]) -> ExecResult<()> {
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

            self.exec_instruction(&instructions[pc], &mut pc, &labels)?;

            pc += 1;
        }

        Ok(())
    }

    fn exec_instruction(
        &mut self,
        instruction: &Instruction,
        pc: &mut usize,
        labels: &HashMap<Label, LabelLocation>,
    ) -> ExecResult<()> {
        match instruction {
            Instruction::Comment(_) => {
                // noop
            }

            Instruction::LocalAlloc(id, ty) => self.exec_local_alloc(*pc, *id, ty)?,

            Instruction::LocalBegin => self.exec_local_begin()?,
            Instruction::LocalEnd => self.exec_local_end()?,

            Instruction::RcNew { out, struct_id } => self.exec_rc_new(out, struct_id)?,

            Instruction::Add { out, a, b } => self.exec_add(out, a, b)?,

            Instruction::Mul { out, a, b } => self.exec_mul(out, a, b)?,
            Instruction::IDiv { out, a, b } => self.exec_idiv(out, a, b)?,
            Instruction::Sub { out, a, b } => self.exec_sub(out, a, b)?,
            Instruction::Shl { out, a, b } => self.exec_shl(out, a, b)?,
            Instruction::Shr { out, a, b } => self.exec_shr(out, a, b)?,
            Instruction::Eq { out, a, b } => self.exec_eq(out, a, b)?,
            Instruction::Gt { out, a, b } => self.exec_gt(out, a, b)?,
            Instruction::Not { out, a } => self.exec_not(out, a)?,
            Instruction::And { out, a, b } => self.exec_and(out, a, b)?,
            Instruction::Or { out, a, b } => self.exec_or(out, a, b)?,
            Instruction::Move { out, new_val } => {
                let val = self.evaluate(new_val)?;
                self.store(out, val)?
            },
            Instruction::Call {
                out,
                function,
                args,
            } => self.exec_call(out, function, args)?,

            Instruction::VirtualCall {
                out,
                iface_id,
                method,
                self_arg,
                rest_args,
            } => self.exec_virtual_call(out.as_ref(), *iface_id, *method, &self_arg, rest_args)?,

            Instruction::ClassIs { out, a, class_id } => self.exec_class_is(out, a, class_id)?,

            Instruction::AddrOf { out, a } => {
                let a_ptr = self.addr_of_ref(a)?;
                self.store(out, MemCell::Pointer(a_ptr))?;
            }

            Instruction::Field {
                out,
                a,
                field,
                of_ty,
            } => self.exec_field(out, &a, field, of_ty)?,

            Instruction::Element {
                out,
                a,
                // not used because the interpreter doesn't need to know element type to
                // calculate the pointer offset
                element: _el,
                index,
            } => {
                self.exec_element(out, a, index)?;
            }

            Instruction::VariantTag { out, a, .. } => self.exec_variant_tag(out, a)?,

            Instruction::VariantData { out, a, tag, .. } => self.exec_variant_data(out, a, tag)?,

            Instruction::Label(_) => {
                // noop
            }

            Instruction::Jump { dest } => self.exec_jump(pc, &labels[dest])?,
            Instruction::JumpIf { dest, test } => self.exec_jmpif(pc, &labels, *dest, test)?,

            Instruction::Release { at } => self.exec_release(at)?,
            Instruction::Retain { at } => self.exec_retain(at)?,

            Instruction::DynAlloc {
                out,
                element_ty,
                len,
            } => self.exec_dynalloc(out, element_ty, len)?,

            Instruction::DynFree { at } => self.exec_dynfree(at)?,
            Instruction::Raise { val } => self.exec_raise(&val)?,

            // all value are one cell in the interpreter
            Instruction::SizeOf { out, .. } => {
                self.store(out, MemCell::I32(1))?;
            }
        }

        Ok(())
    }

    fn exec_local_alloc(&mut self, pc: usize, id: LocalID, ty: &Type) -> ExecResult<()> {
        let uninit_cell = self.default_init_cell(ty)?;

        self.current_frame_mut()?.alloc_local(id, pc, uninit_cell)?;

        Ok(())
    }

    fn exec_local_begin(&mut self) -> ExecResult<()> {
        self.current_frame_mut()?.push_block();

        Ok(())
    }

    fn exec_local_end(&mut self) -> ExecResult<()> {
        self.current_frame_mut()?.pop_block()?;

        Ok(())
    }

    fn exec_rc_new(&mut self, out: &Ref, struct_id: &StructID) -> ExecResult<()> {
        let struct_ty = Type::Struct(*struct_id);

        let default_vall = self.default_init_cell(&struct_ty)?;
        let init_cells = vec![default_vall];

        let rc_ptr = self.rc_alloc(init_cells, *struct_id);

        self.store(out, MemCell::Pointer(rc_ptr))?;

        Ok(())
    }

    fn exec_add(&mut self, out: &Ref, a: &Value, b: &Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        match a_val.try_add(&b_val) {
            Some(result) => {
                self.store(out, result)?;
                Ok(())
            },
            None => {
                let msg = format!("Add is not valid for {:?} + {:?}", a_val, b_val);
                Err(ExecError::illegal_state(msg))
            },
        }
    }

    fn exec_raise(&mut self, val: &Ref) -> ExecResult<()> {
        let msg = self.read_string(&val.clone().to_deref())?;

        return Err(ExecError::Raised {
            msg,
            //todo
            span: Span::zero("<interpreter>"),
        });
    }

    fn exec_dynfree(&mut self, at: &Ref) -> ExecResult<()> {
        match self.load(at)? {
            MemCell::Pointer(Pointer::Heap(addr)) => {
                let addr = *addr;
                self.heap.free(addr);

                Ok(())
            }

            x => {
                let msg = format!("target of DynFree at {} must be pointer, was: {:?}", at, x);
                Err(ExecError::illegal_state(msg))
            },
        }
    }

    fn exec_dynalloc(&mut self, out: &Ref, element_ty: &Type, len: &Value) -> ExecResult<()> {
        let len = self
            .evaluate(len)?
            .as_i32()
            .ok_or_else(|| ExecError::illegal_state("len of DynAlloc must be i32"))?;

        let mut cells = Vec::new();
        for _ in 0..len {
            let default_val = self.default_init_cell(element_ty)?;
            cells.push(default_val);
        }

        let addr = self.heap.alloc(cells);
        let ptr = Pointer::Heap(addr);

        self.store(out, MemCell::Pointer(ptr))?;

        Ok(())
    }

    fn exec_retain(&mut self, at: &Ref) -> ExecResult<()> {
        let cell = self.load(at)?.clone();
        self.retain_cell(&cell)?;

        Ok(())
    }

    fn exec_release(&mut self, at: &Ref) -> ExecResult<()> {
        let cell = self.load(at)?.clone();
        self.release_cell(&cell)?;

        Ok(())
    }

    fn exec_jmpif(
        &mut self,
        pc: &mut usize,
        labels: &HashMap<Label, LabelLocation>,
        dest: Label,
        test: &Value,
    ) -> ExecResult<()> {
        match self.evaluate(test)? {
            MemCell::Bool(true) => {
                self.exec_jump(pc, &labels[&dest])
            }
            MemCell::Bool(false) => {
                Ok(())
            }
            _ => Err(ExecError::illegal_state("JumpIf instruction testing non-boolean cell")),
        }
    }

    fn exec_variant_data(&mut self, out: &Ref, a: &Ref, tag: &usize) -> ExecResult<()> {
        let a_ptr = self.addr_of_ref(a)?;
        self.store(
            out,
            MemCell::Pointer(Pointer::VariantData {
                variant: Box::new(a_ptr),
                tag: *tag,
            }),
        )?;

        Ok(())
    }

    fn exec_variant_tag(&mut self, out: &Ref, a: &Ref) -> ExecResult<()> {
        let a_ptr = self.addr_of_ref(a)?;
        self.store(
            out,
            MemCell::Pointer(Pointer::VariantTag {
                variant: Box::new(a_ptr),
            }),
        )?;

        Ok(())
    }

    fn exec_element(&mut self, out: &Ref, a: &Ref, index: &Value) -> ExecResult<()> {
        let array = self.addr_of_ref(a)?;

        // todo: use a real usize
        let offset = self.evaluate(index)?
            .as_i32()
            .map(|i| i as usize)
            .ok_or_else(|| {
                let msg = "element instruction has non-integer illegal index value";
                ExecError::illegal_state(msg)
            })?;

        self.store(
            out,
            MemCell::Pointer(Pointer::IntoArray {
                offset,
                array: Box::new(array),
            }),
        )?;

        Ok(())
    }

    fn exec_mul(&mut self, out: &Ref, a: &Value, b: &Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        match a_val.try_mul(&b_val) {
            Some(result) => self.store(out, result),
            None => panic!(
                "Mul is not valid for {:?} * {:?}",
                a_val,
                b_val,
            ),
        }
    }

    fn exec_idiv(&mut self, out: &Ref, a: &Value, b: &Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        match a_val.try_idiv(&b_val) {
            Some(result) => self.store(out, result),
            None => panic!(
                "IDiv is not valid for {:?} div {:?}",
                a_val,
                b_val,
            ),
        }
    }

    fn exec_sub(&mut self, out: &Ref, a: &Value, b: &Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        match a_val.try_sub(&b_val) {
            Some(result) => self.store(out, result),
            None => panic!(
                "Sub is not valid for {:?} - {:?}",
                a_val,
                b_val,
            ),
        }
    }

    fn exec_shl(&mut self, out: &Ref, a: &Value, b: &Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        match a_val.try_shl(&b_val) {
            Some(result) => self.store(out, result),
            None => panic!(
                "Shl is not valid for {:?} shl {:?}",
                a_val,
                b_val,
            ),
        }
    }

    fn exec_shr(&mut self, out: &Ref, a: &Value, b: &Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        match a_val.try_shr(&b_val) {
            Some(result) => self.store(out, result)?,
            None => {
                let msg = format!("Shr is not valid for {:?} shr {:?}", a_val, b_val);
                return Err(ExecError::illegal_state(msg));
            },
        }

        Ok(())
    }

    fn exec_eq(&mut self, out: &Ref, a: &Value, b: &Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        let eq = match a_val.try_eq(&b_val) {
            Some(eq) => eq,
            None => {
                let msg = format!("Eq is not valid for {:?} = {:?}", a_val, b_val);
                return Err(ExecError::illegal_state(msg));
            },
        };

        self.store(out, MemCell::Bool(eq))?;

        Ok(())
    }

    fn exec_gt(&mut self, out: &Ref, a: &Value, b: &Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        let gt = a_val.try_gt(&b_val)
            .ok_or_else(|| {
                let msg = format!("Gt is not valid for {} ({:?}) > {} ({:?})", a, a_val, b, b_val);
                ExecError::illegal_state(msg)
            })?;

        self.store(out, MemCell::Bool(gt))?;

        Ok(())
    }

    fn exec_not(&mut self, out: &Ref, a: &Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;

        let not = match a_val.try_not() {
            Some(not) => not,
            None => {
                let msg = format!("Not instruction is not valid for {:?}", a);
                return Err(ExecError::illegal_state(msg))
            }
        };

        self.store(out, MemCell::Bool(not))?;

        Ok(())
    }

    fn exec_and(&mut self, out: &Ref, a: &Value, b: &Value) -> ExecResult<()> {
        let a_val = self
            .evaluate(a)?
            .as_bool()
            .ok_or_else(|| {
                let msg = format!("operand a of And instruction must be bool, got {:?}", a);
                ExecError::illegal_state(msg)
            })?;

        let b_val = self
            .evaluate(b)?
            .as_bool()
            .ok_or_else(|| {
                let msg = format!("operand b of And instruction must be bool, got {:?}", b);
                ExecError::illegal_state(msg)
            })?;

        self.store(out, MemCell::Bool(a_val && b_val))?;

        Ok(())
    }

    fn exec_or(&mut self, out: &Ref, a: &Value, b: &Value) -> ExecResult<()> {
        let a_val = self
            .evaluate(a)?
            .as_bool()
            .ok_or_else(|| {
                let msg = format!("operand a of Or instruction must be bool, got {:?}", a);
                ExecError::illegal_state(msg)
            })?;

        let b_val = self
            .evaluate(b)?
            .as_bool()
            .ok_or_else(|| {
                let msg = format!("operand b of Or instruction must be bool, got {:?}", b);
                ExecError::illegal_state(msg)
            })?;

        self.store(out, MemCell::Bool(a_val || b_val))?;

        Ok(())
    }

    fn exec_call(
        &mut self,
        out: &Option<Ref>,
        function: &Value,
        args: &Vec<Value>,
    ) -> ExecResult<()> {
        let arg_cells: Vec<_> = args
            .iter()
            .map(|arg_val| self.evaluate(arg_val))
            .collect::<ExecResult<_>>()?;

        match self.evaluate(function)? {
            MemCell::Function(function) => self.call(&function, &arg_cells, out.as_ref())?,

            _ => {
                let msg = format!("{} does not reference a function", function);
                return Err(ExecError::illegal_state(msg));
            }
        }

        Ok(())
    }

    fn exec_virtual_call(
        &mut self,
        out: Option<&Ref>,
        iface_id: InterfaceID,
        method: MethodID,
        self_arg: &Value,
        rest_args: &[Value],
    ) -> ExecResult<()> {
        let self_cell = self.evaluate(&self_arg)?;
        let func = self.vcall_lookup(&self_cell, iface_id, method)?;

        let mut arg_cells = vec![self_cell];
        for arg_val in rest_args {
            let arg_cell = self.evaluate(arg_val)?;
            arg_cells.push(arg_cell);
        }

        let func_ref = Ref::Global(GlobalRef::Function(func));
        let func = match self.evaluate(&Value::Ref(func_ref))? {
            MemCell::Function(func) => func,
            unexpected => {
                let msg = format!("invalid function cell: {:?}", unexpected);
                return Err(ExecError::illegal_state(msg));
            },
        };

        self.call(&func, &arg_cells, out)?;

        Ok(())
    }

    fn exec_class_is(&mut self, out: &Ref, a: &Value, class_id: &ClassID) -> ExecResult<()> {
        let rc_addr = self
            .evaluate(a)?
            .as_pointer()
            .and_then(Pointer::as_heap_addr)
            .ok_or_else(|| {
                let msg = "argument a of ClassIs instruction must evaluate to a heap pointer";
                ExecError::illegal_state(msg)
            })?;
        let rc_cell = self
            .heap
            .get(rc_addr)
            .and_then(MemCell::as_rc)
            .ok_or_else(|| {
                let msg = "rc pointer target of ClassIs instruction must point to an rc cell";
                ExecError::illegal_state(msg)
            })?;

        let is = match class_id {
            ClassID::Class(struct_id) => rc_cell.struct_id == *struct_id,
            ClassID::Interface(iface_id) => {
                let resource_id = ClassID::Class(rc_cell.struct_id);
                let actual_ty = Type::RcPointer(Some(resource_id));

                self.metadata.is_impl(&actual_ty, *iface_id)
            }
        };

        self.store(out, MemCell::Bool(is))?;

        Ok(())
    }

    fn exec_field(&mut self, out: &Ref, a: &Ref, field: &FieldID, of_ty: &Type) -> ExecResult<()> {
        let field_ptr = match of_ty {
            Type::Struct(..) => {
                let struct_ptr = self.addr_of_ref(a)?;

                Pointer::IntoStruct {
                    field: *field,
                    structure: Box::new(struct_ptr),
                }
            }

            Type::RcPointer(..) => {
                let target = self.load(&a.clone().to_deref())?;
                let struct_ptr = match target.as_rc() {
                    Some(rc_cell) => Pointer::Heap(rc_cell.resource_addr),
                    None => {
                        let msg = format!("trying to read field pointer of rc type but target wasn't an rc cell @ {} (target was: {:?}", a, target);
                        return Err(ExecError::illegal_state(msg));
                    }
                };

                Pointer::IntoStruct {
                    field: *field,
                    structure: Box::new(struct_ptr),
                }
            }

            _ => {
                let msg = format!(
                    "invalid base type referenced in Field instruction: {}.{}",
                    of_ty, field
                );
                return Err(ExecError::illegal_state(msg));
            }
        };

        self.store(out, MemCell::Pointer(field_ptr))?;

        Ok(())
    }

    fn exec_jump(&mut self, pc: &mut usize, label: &LabelLocation) -> ExecResult<()> {
        *pc = label.pc_offset;

        // assume all jumps are either upwards or to the same level
        self.current_frame_mut()?.pop_block_to(label.block_depth)?;

        Ok(())
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

    fn define_builtin(&mut self, name: Symbol, func: BuiltinFn, ret: Type) {
        if let Some(func_id) = self.metadata.find_function(&name) {
            self.globals.insert(
                GlobalRef::Function(func_id),
                GlobalCell {
                    value: MemCell::Function(Rc::new(Function::Builtin(BuiltinFunction {
                        func,
                        return_ty: ret,
                        debug_name: name.to_string(),
                    }))),
                    ty: Type::Nothing,
                },
            );
        }
    }

    fn init_stdlib_globals(&mut self) {
        let system_funcs: &[(&str, BuiltinFn, Type)] = &[
            ("IntToStr", builtin::int_to_str, Type::string_ptr()),
            ("StrToInt", builtin::str_to_int, Type::I32),
            ("WriteLn", builtin::write_ln, Type::Nothing),
            ("ReadLn", builtin::read_ln, Type::string_ptr()),
            ("GetMem", builtin::get_mem, Type::U8.ptr()),
            ("FreeMem", builtin::free_mem, Type::Nothing),
            ("ArrayLengthInternal", builtin::array_length, Type::I32),
            (
                "ArraySetLengthInternal",
                builtin::set_length,
                Type::RcPointer(None),
            ),
        ];

        for (ident, func, ret) in system_funcs {
            let name = Symbol::new(ident.to_string(), vec!["System"]);
            self.define_builtin(name, *func, ret.clone());
        }
    }

    pub fn load_module(&mut self, module: &Module, init_stdlib: bool) -> ExecResult<()> {
        self.metadata.extend(&module.metadata);

        for (func_name, ir_func) in &module.functions {
            let func_ref = GlobalRef::Function(*func_name);

            let func = match ir_func {
                IRFunction::Local(func_def) => {
                    let ir_func = Function::IR(func_def.clone());
                    Some(ir_func)
                },

                IRFunction::External(external_ref) if external_ref.src == "rt" => {
                    None
                }

                IRFunction::External(external_ref) => {
                    let ffi_func = Function::new_ffi(external_ref, &mut self.ffi_cache, &self.metadata)?;
                    Some(ffi_func)
                }
            };

            if let Some(func) = func {
                self.globals.insert(
                    func_ref,
                    GlobalCell {
                        value: MemCell::Function(Rc::new(func)),
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

        self.push_stack("<init>");
        self.execute(&module.init)?;
        self.pop_stack()?;

        Ok(())
    }

    fn deref_rc(&self, rc_cell: &MemCell) ->  ExecResult<&MemCell> {
        let rc = rc_cell.as_rc().ok_or_else(|| {
            ExecError::illegal_state(format!("trying to deref RC ref but value was {}", rc_cell.kind()))
        })?;

        let res_addr = rc.resource_addr;
        self.heap.get(res_addr).ok_or_else(|| {
            let msg = format!("heap address not allocated: {}", res_addr);
            ExecError::illegal_state(msg)
        })
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

    fn read_string(&self, str_ref: &Ref) -> ExecResult<String> {
        let str_cell = self.deref_rc(self.load(str_ref)?)?;

        let str_cell = match str_cell.as_struct(STRING_ID) {
            Some(struct_cell) => struct_cell,
            None => return Err(ExecError::illegal_state(format!(
                "tried to read string value from rc cell which didn't contain a string struct: {:?}",
                str_cell,
            ))),
        };

        let len = &str_cell[STRING_LEN_FIELD].as_i32().unwrap();

        if *len == 0 {
            return Ok(String::new());
        }

        let chars_addr = str_cell[STRING_CHARS_FIELD]
            .as_pointer()
            .and_then(Pointer::as_heap_addr)
            .ok_or_else(|| {
                ExecError::illegal_state(format!(
                    "string contained non-heap-alloced `chars` pointer: {:?}",
                    str_cell
                ))
            })?;

        let mut chars = Vec::new();
        for i in 0..*len as usize {
            let char_addr = HeapAddress(chars_addr.0 + i);
            let char_val = self
                .heap
                .get(char_addr)
                .unwrap()
                .as_u8()
                .ok_or_else(|| {
                    ExecError::illegal_state(format!("expected string char @ {}", char_addr))
                })?;

            chars.push(char_val as char);
        }

        Ok(chars.into_iter().collect())
    }

    pub fn shutdown(mut self) -> ExecResult<()> {
        let globals: Vec<_> = self.globals.values().cloned().collect();

        for GlobalCell { value, ty } in globals {
            if ty.is_rc() {
                self.release_cell(&value)?;
            }
        }

        self.heap.finalize();
        Ok(())
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
