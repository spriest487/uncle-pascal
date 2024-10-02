mod builtin;
mod dyn_value;
mod func;
mod heap;
mod marshal;
mod ptr;
pub mod result;
mod stack;
mod diag;

pub use self::dyn_value::*;
pub use self::ptr::Pointer;
use crate::func::BuiltinFn;
use crate::func::BuiltinFunction;
use crate::func::Function;
use crate::heap::NativeHeap;
use crate::marshal::Marshaller;
use crate::result::ExecError;
use crate::result::ExecResult;
use crate::stack::StackFrame;
use crate::stack::StackTrace;
use crate::stack::StackTraceFrame;
use ir_lang as ir;
use ir_lang::InstructionFormatter;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::BitXor;
use std::rc::Rc;
use crate::diag::{DiagnosticOutput, DiagnosticWorker};

#[derive(Debug)]
pub struct Interpreter {
    metadata: ir::Metadata,
    stack: Vec<StackFrame>,
    globals: HashMap<ir::GlobalRef, GlobalValue>,

    native_heap: NativeHeap,

    marshaller: Rc<Marshaller>,

    opts: InterpreterOpts,

    functions: BTreeMap<ir::FunctionID, Rc<Function>>,
    
    diag_worker: Option<DiagnosticWorker>,
}

impl Interpreter {
    pub fn new(opts: InterpreterOpts) -> Self {
        let globals = HashMap::new();

        let marshaller = Rc::new(Marshaller::new());

        let native_heap = NativeHeap::new(marshaller.clone(), opts.trace_heap);
        
        let diag_worker = match opts.diag_port {
            0 => None,
            port => DiagnosticWorker::new(port),
        };

        Self {
            metadata: ir::Metadata::default(),
            globals,
            stack: Vec::new(),

            native_heap,

            marshaller,

            opts,
            
            functions: BTreeMap::new(),
            
            diag_worker,
        }
    }

    fn add_stack_trace(&self, err: ExecError) -> ExecError {
        match err {
            err @ ExecError::WithStackTrace { .. } => err,
            err => ExecError::WithStackTrace {
                err: Box::new(err),
                stack_trace: self.stack_trace(),
            },
        }
    }

    fn stack_trace(&self) -> StackTrace {
        let frames = self
            .stack
            .iter()
            .rev()
            .map(|s|
                StackTraceFrame::new(s.name().to_string(), &s.debug_location())
            );

        StackTrace::new(frames)
    }

    fn stack_trace_formatted(&self) -> String {
        self.stack_trace()
            .into_iter()
            .map(|frame| format!("\t at {}", frame))
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn marshaller(&self) -> &Marshaller {
        &self.marshaller
    }

    fn init_struct(&self, id: ir::TypeDefID) -> ExecResult<StructValue> {
        let struct_def = self.metadata.get_struct_def(id).cloned().ok_or_else(|| {
            let msg = format!("missing struct definition in metadata: {}", id);
            ExecError::illegal_state(msg)
        })?;

        let mut fields = Vec::with_capacity(struct_def.fields.len());
        for (&id, field) in &struct_def.fields {
            // include padding of -1s for non-contiguous IDs
            if id.0 >= fields.len() {
                fields.resize(id.0 + 1, DynValue::I32(-1));
            }
            fields[id.0] = self.default_init_dyn_val(&field.ty)?;
        }

        let struct_val = StructValue {
            type_id: id,
            fields,
            rc: None,
        };

        Ok(struct_val)
    }

    fn default_init_dyn_val(&self, ty: &ir::Type) -> ExecResult<DynValue> {
        let val = match ty {
            ir::Type::I8 => DynValue::I8(i8::MIN),
            ir::Type::U8 => DynValue::U8(u8::MAX),
            ir::Type::I16 => DynValue::I16(i16::MIN),
            ir::Type::U16 => DynValue::U16(u16::MAX),
            ir::Type::I32 => DynValue::I32(i32::MIN),
            ir::Type::U32 => DynValue::U32(u32::MAX),
            ir::Type::I64 => DynValue::I64(i64::MIN),
            ir::Type::U64 => DynValue::U64(u64::MAX),
            ir::Type::ISize => DynValue::ISize(isize::MIN),
            ir::Type::USize => DynValue::USize(usize::MAX),

            ir::Type::Bool => DynValue::Bool(false),
            ir::Type::F32 => DynValue::F32(f32::NAN),

            ir::Type::Struct(id) => DynValue::from(self.init_struct(*id)?),

            ir::Type::RcPointer(class_id) => DynValue::Pointer(Pointer::null(match class_id {
                ir::VirtualTypeID::Class(struct_id) => ir::Type::Struct(*struct_id),
                ir::VirtualTypeID::Closure(..)
                | ir::VirtualTypeID::Any
                | ir::VirtualTypeID::Interface(..) => ir::Type::Nothing,
            })),

            ir::Type::Pointer(target) => DynValue::Pointer(Pointer::null((**target).clone())),

            ir::Type::Array { element, dim } => {
                let mut elements = Vec::with_capacity(*dim);
                let el = self.default_init_dyn_val(element.as_ref())?;

                for _ in 0..*dim {
                    elements.push(el.clone());
                }

                DynValue::Array(Box::new(ArrayValue {
                    el_ty: (**element).clone(),
                    elements,
                }))
            },

            ir::Type::Variant(id) => {
                let default_tag = 0;
                let cases = &self
                    .metadata
                    .get_variant_def(*id)
                    .ok_or_else(|| {
                        ExecError::illegal_state(format!("missing variant definition {}", *id))
                    })?
                    .cases;
                let case_ty = &cases
                    .get(default_tag as usize)
                    .ok_or_else(|| {
                        ExecError::illegal_state(format!(
                            "missing default case definition for variant {}",
                            *id
                        ))
                    })?
                    .ty;

                let default_val = match case_ty {
                    Some(case_ty) => self.default_init_dyn_val(case_ty)?,
                    None => DynValue::Pointer(Pointer::null(ir::Type::Nothing)),
                };

                DynValue::Variant(Box::new(VariantValue {
                    id: *id,
                    tag: Box::new(DynValue::I32(default_tag)),
                    data: Box::new(default_val),
                }))
            },

            ir::Type::Function(..) => DynValue::Function(ir::FunctionID(usize::MAX)),

            _ => {
                let msg = format!("can't initialize default value of type `{:?}`", ty);
                return Err(ExecError::illegal_state(msg));
            },
        };

        Ok(val)
    }

    fn load_indirect(&self, ptr: &Pointer) -> ExecResult<Cow<DynValue>> {
        let val = self.native_heap.load(ptr)?;
        Ok(Cow::Owned(val))
    }

    /// dereference a pointer and set the value it points to
    fn store_indirect(&mut self, ptr: &Pointer, val: DynValue) -> ExecResult<()> {
        self.native_heap.store(ptr, val)?;
        Ok(())
    }

    fn store_local(&mut self, id: ir::LocalID, val: DynValue) -> ExecResult<()> {
        let current_frame = self.current_frame_mut()?;
        let local_ptr = current_frame
            .get_local_ptr(id)
            .map_err(|err| self.add_stack_trace(err.into()))?;

        self.marshaller.marshal_into(&val, &local_ptr)?;

        Ok(())
    }

    fn load_local(&self, id: ir::LocalID) -> ExecResult<DynValue> {
        let current_frame = self.current_frame()?;
        let local_ptr = current_frame
            .get_local_ptr(id)
            .map_err(|err| self.add_stack_trace(err.into()))?;

        let value = self.marshaller.unmarshal_from_ptr(&local_ptr)?;

        Ok(value)
    }

    fn store(&mut self, at: &ir::Ref, val: DynValue) -> ExecResult<()> {
        match at {
            ir::Ref::Discard => {
                // do nothing with this value
                Ok(())
            },

            ir::Ref::Local(id) => self.store_local(*id, val),

            ir::Ref::Global(name) => {
                if self.globals.contains_key(name) {
                    return Err(ExecError::illegal_state(format!(
                        "global `{}` is already allocated",
                        name
                    )));
                }

                self.globals.insert(
                    name.clone(),
                    GlobalValue {
                        value: val,
                        // todo: for the moment this means no RC for global vals stored after init
                        ty: ir::Type::Nothing,
                    },
                );

                Ok(())
            },

            ir::Ref::Deref(inner) => match self.evaluate(inner)? {
                DynValue::Pointer(ptr) => {
                    self.store_indirect(&ptr, val)?;

                    Ok(())
                },

                x => {
                    let msg = format!("can't dereference non-pointer val with value {:?}", x);
                    Err(ExecError::illegal_state(msg))
                },
            },
        }
    }

    fn load(&self, at: &ir::Ref) -> ExecResult<Cow<DynValue>> {
        match at {
            ir::Ref::Discard => {
                let msg = "can't read value from discard ref";
                Err(ExecError::illegal_state(msg))
            },

            ir::Ref::Local(id) => self.load_local(*id).map(Cow::Owned),

            ir::Ref::Global(name) => match self.globals.get(name) {
                Some(global_val) => Ok(Cow::Borrowed(&global_val.value)),
                None => {
                    let msg = format!("global val `{}` is not allocated", name);
                    Err(ExecError::illegal_state(msg))
                },
            },

            ir::Ref::Deref(inner) => match self.evaluate(inner)? {
                DynValue::Pointer(ptr) => self.load_indirect(&ptr),
                x => {
                    let msg = format!("can't dereference val {:?}", x);
                    Err(ExecError::illegal_state(msg))
                },
            },
        }
    }

    fn evaluate(&self, val: &ir::Value) -> ExecResult<DynValue> {
        match val {
            ir::Value::Ref(r) => {
                let ref_val = self.load(r)?;
                Ok(ref_val.into_owned())
            },

            ir::Value::SizeOf(ty) => {
                let marshal_ty = self.marshaller.get_ty(ty)?;
                let size = cast::i32(marshal_ty.size()).map_err(|_| {
                    ExecError::illegal_state(format!(
                        "type has illegal size: {}",
                        marshal_ty.size()
                    ))
                })?;

                Ok(DynValue::I32(size))
            },

            ir::Value::LiteralU8(i) => Ok(DynValue::U8(*i)),
            ir::Value::LiteralI8(i) => Ok(DynValue::I8(*i)),
            ir::Value::LiteralI16(i) => Ok(DynValue::I16(*i)),
            ir::Value::LiteralU16(i) => Ok(DynValue::U16(*i)),
            ir::Value::LiteralI32(i) => Ok(DynValue::I32(*i)),
            ir::Value::LiteralU32(i) => Ok(DynValue::U32(*i)),
            ir::Value::LiteralI64(i) => Ok(DynValue::I64(*i)),
            ir::Value::LiteralU64(i) => Ok(DynValue::U64(*i)),
            ir::Value::LiteralISize(i) => Ok(DynValue::ISize(*i)),
            ir::Value::LiteralUSize(i) => Ok(DynValue::USize(*i)),
            ir::Value::LiteralF32(f) => Ok(DynValue::F32(*f)),
            ir::Value::LiteralBool(b) => Ok(DynValue::Bool(*b)),
            ir::Value::LiteralNull => Ok(DynValue::Pointer(Pointer::null(ir::Type::Nothing))),
        }
    }

    fn push_stack(&mut self, name: impl Into<String>, stack_size: usize) {
        let stack_frame = StackFrame::new(name, self.marshaller.clone(), stack_size);
        self.stack.push(stack_frame);
    }

    fn pop_stack(&mut self) -> ExecResult<()> {
        let popped = self
            .stack
            .pop()
            .ok_or_else(|| ExecError::illegal_state("popped stack with no stackframes"))?;
        popped.check_sentinel()?;
        Ok(())
    }

    fn current_frame(&self) -> ExecResult<&StackFrame> {
        self.stack
            .last()
            .ok_or_else(|| ExecError::illegal_state("called current_frame without no stackframes"))
    }

    fn current_frame_mut(&mut self) -> ExecResult<&mut StackFrame> {
        match self.stack.last_mut() {
            Some(frame) => Ok(frame),
            None => Err(ExecError::illegal_state(
                "called current_frame without no stackframes",
            )),
        }
    }

    fn vcall_lookup(
        &self,
        self_val: &DynValue,
        iface_id: ir::InterfaceID,
        method: ir::MethodID,
    ) -> ExecResult<ir::FunctionID> {
        let self_ptr = self_val.as_pointer().ok_or_else(|| {
            let msg = "expected target of vcall to be a pointer";
            ExecError::illegal_state(msg)
        })?;

        let self_val = self.load_indirect(self_ptr)?;
        let self_class_id = match self_val.as_ref() {
            DynValue::Structure(struct_val) => Ok(struct_val.type_id),
            _ => {
                let msg = format!(
                    "expected target of vcall {}.{} to be an rc value, but found {:?}",
                    iface_id, method.0, self_val,
                );
                Err(ExecError::illegal_state(msg))
            },
        }?;

        let instance_ty = ir::Type::RcPointer(ir::VirtualTypeID::Class(self_class_id));

        self.metadata
            .find_virtual_impl(&instance_ty, iface_id, method)
            .ok_or_else(|| {
                let mut err = "virtual call ".to_string();

                let iface_ty = ir::Type::RcPointer(ir::VirtualTypeID::Interface(iface_id));
                let _ = self.metadata.format_type(&iface_ty, &mut err);
                err.push('.');
                let _ = self.metadata.format_method(iface_id, method, &mut err);
                err.push_str(" missing implementation for ");
                let _ = self.metadata.format_type(&instance_ty, &mut err);

                ExecError::illegal_state(format!("{}", err))
            })
    }

    fn call(
        &mut self,
        id: ir::FunctionID,
        args: &[DynValue],
        out: Option<&ir::Ref>,
    ) -> ExecResult<()> {
        let func = self
            .functions
            .get(&id)
            .ok_or_else(|| {
                let msg = format!("missing function: {id}");
                ExecError::illegal_state(msg)
            })?
            .clone();

        let stack_size = func.stack_alloc_size(self.marshaller())?;
        self.push_stack(func.debug_name(), stack_size);

        // store empty result at $0 if needed
        let return_ty = func.return_ty();
        let ret_id = if *return_ty != ir::Type::Nothing {
            let result_val = self.default_init_dyn_val(return_ty)?;

            let ret_id = self
                .current_frame_mut()?
                .add_undeclared_local(return_ty.clone(), &result_val)?;
            assert_eq!(ret_id, ir::LocalID(0));
            Some(ret_id)
        } else {
            None
        };

        if args.len() != func.param_tys().len() {
            let msg = format!(
                "arguments provided for function call are invalid (expected {} args, got {})",
                func.param_tys().len(),
                args.len()
            );
            return Err(ExecError::illegal_state(msg));
        }

        // store params in either $0.. or $1..
        let first_arg_id = match ret_id {
            Some(ret_id) => ir::LocalID(ret_id.0 + 1),
            None => ir::LocalID(0),
        };

        for (arg_index, (arg_val, param_ty)) in args.iter().zip(func.param_tys()).enumerate() {
            let arg_id = self
                .current_frame_mut()?
                .add_undeclared_local(param_ty.clone(), arg_val)?;

            assert_eq!(ir::LocalID(first_arg_id.0 + arg_index), arg_id);
        }

        func.invoke(self)?;

        let result_val = match ret_id {
            None => None,
            Some(ret_id) => {
                let ret_ref = ir::Ref::Local(ret_id);
                let return_val = self.evaluate(&ir::Value::Ref(ret_ref))?;
                Some(return_val)
            },
        };

        self.pop_stack()?;

        match (result_val, out) {
            (Some(v), Some(out_at)) => {
                self.store(&out_at, v)?;
            },

            (None, Some(_)) => {
                let msg = "called function which has no return type in a context where a return value was expected";
                return Err(ExecError::illegal_state(msg));
            },

            // ok, no output expected, ignore result if there is one
            (_, None) => {},
        }

        Ok(())
    }

    fn invoke_disposer(&mut self, val: &DynValue, ty: &ir::Type) -> ExecResult<()> {
        let dispose_impl_id = self
            .metadata
            .find_virtual_impl(ty, ir::DISPOSABLE_ID, ir::DISPOSABLE_DISPOSE_INDEX);

        if let Some(dispose_func_id) = dispose_impl_id {
            let dispose_desc = self
                .metadata
                .func_desc(dispose_func_id)
                .unwrap_or_else(|| dispose_func_id.to_string());

            if self.opts.trace_rc {
                eprintln!("rc: invoking {}", dispose_desc);
            }

            self.call(dispose_func_id, &[val.clone()], None)?;
        } else if self.opts.trace_rc {
            eprintln!("rc: no disposer for {}", self.metadata.pretty_ty_name(ty));
        }

        Ok(())
    }

    fn find_rc_boilerplate(&self, resource_ty: &ir::Type) -> ExecResult<ir::RuntimeType> {
        self.metadata.get_runtime_type(&resource_ty).ok_or_else(|| {
            let name = self.metadata.pretty_ty_name(&resource_ty);
            let funcs = self
                .metadata
                .runtime_types()
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

            let msg = format!("missing rc boilerplate for {} in:\n{}", name, funcs);
            ExecError::illegal_state(msg)
        })
    }

    fn release_dyn_val(&mut self, val: &DynValue) -> ExecResult<bool> {
        let ptr = val.as_pointer().ok_or_else(|| {
            let msg = format!("released val was not a pointer, found: {:?}", val);
            ExecError::illegal_state(msg)
        })?;

        // NULL is a valid release target because we release uninitialized local RC pointers
        // just do nothing
        if ptr.is_null() {
            return Ok(false);
        }

        let mut struct_val = match self.load_indirect(&ptr)?.into_owned() {
            DynValue::Structure(struct_val) => *struct_val,
            other => {
                let msg = format!("released val was not a structure, found: {:?}", other);
                return Err(ExecError::illegal_state(msg));
            },
        };

        let mut struct_rc = struct_val.rc.as_ref().cloned().ok_or_else(|| {
            let msg = "unable to access rc state of released structure".to_string();
            ExecError::illegal_state(msg)
        })?;

        // release calls are totally ignored for immortal refs
        if struct_rc.strong_count < 0 {
            return Ok(false);
        }

        if struct_rc.strong_count == 0 {
            panic!(
                "releasing with no strong refs remaining @ {} (+{} weak refs remain)\n{}",
                ptr.to_pretty_string(&self.metadata),
                struct_rc.weak_count,
                self.stack_trace_formatted(),
            );
        }

        struct_rc.strong_count -= 1;

        if self.opts.trace_rc {
            eprintln!(
                "rc: release @ {} ({}+{} refs to {} remain)",
                ptr.to_pretty_string(&self.metadata),
                struct_rc.strong_count,
                struct_rc.weak_count,
                self.struct_debug_string(&struct_val),
            )
        }

        // are we releasing the last strong ref here?
        if struct_rc.strong_count == 0 {
            if self.opts.trace_rc {
                println!(
                    "rc: dispose @ {} ({}+{} refs remain)",
                    ptr.to_pretty_string(&self.metadata),
                    struct_rc.strong_count,
                    struct_rc.weak_count,
                );
            }

            // Dispose() the inner resource. For an RC type, interfaces are implemented
            // for the RC pointer type, not the resource type
            let class_id = ir::VirtualTypeID::Class(struct_val.type_id);
            self.invoke_disposer(&val, &ir::Type::RcPointer(class_id))?;

            // Now release the fields of the struct as if it was a normal record
            let rc_funcs = self.find_rc_boilerplate(&ir::Type::Struct(struct_val.type_id))?;
            self.call(rc_funcs.release, &[val.clone()], None)?;

            if struct_rc.weak_count == 0 {
                // no more weak refs, free the object
                if self.opts.trace_rc {
                    eprintln!(
                        "rc: free @ {} of {}",
                        ptr.to_pretty_string(&self.metadata),
                        self.struct_debug_string(&struct_val),
                    )
                }

                self.dynfree(ptr)?;
            }

            Ok(true)
        } else {
            // update the object's ref count and store it
            struct_val.rc = Some(struct_rc);

            self.store_indirect(&ptr, DynValue::Structure(Box::new(struct_val)))?;

            Ok(false)
        }
    }

    fn retain_dyn_val(&mut self, val: &DynValue) -> ExecResult<()> {
        match val {
            DynValue::Pointer(ptr) => {
                let mut rc_val = self.load_indirect(ptr)?.into_owned();

                match &mut rc_val {
                    DynValue::Structure(struct_val) if struct_val.rc.is_some() => {
                        let struct_rc = struct_val.rc.as_mut().unwrap();
                        struct_rc.strong_count += 1;

                        if self.opts.trace_rc {
                            eprintln!(
                                "rc: retain @ {} ({}+{} refs)",
                                ptr.to_pretty_string(&self.metadata),
                                struct_rc.strong_count,
                                struct_rc.weak_count
                            );
                        }
                    },

                    other => {
                        let msg =
                            format!("retained val must point to an rc val, found: {:?}", other);
                        return Err(ExecError::illegal_state(msg));
                    },
                };

                self.store_indirect(ptr, rc_val)?;

                Ok(())
            },

            _ => Err(ExecError::illegal_state(format!(
                "{:?} cannot be retained",
                val
            ))),
        }
    }

    fn struct_debug_string(&self, struct_val: &StructValue) -> String {
        // special case useful for debugging - print string values in rc trace output
        let val_as_debug_str = if struct_val.type_id == ir::STRING_ID {
            self.read_string_struct(struct_val).ok()
        } else {
            None
        };

        if let Some(debug_str) = val_as_debug_str {
            format!("string '{}'", debug_str)
        } else {
            let ty_name = self
                .metadata
                .pretty_ty_name(&ir::Type::Struct(struct_val.type_id));
            format!("struct of type {}", ty_name)
        }
    }

    fn addr_of_ref(&self, target: &ir::Ref) -> ExecResult<Pointer> {
        match target {
            ir::Ref::Discard => {
                let msg = "can't take address of discard ref";
                Err(ExecError::illegal_state(msg))
            },

            // let int := 1;
            // let intPtr := @int;
            // @(intPtr^) -> address of int behind intPtr
            ir::Ref::Deref(val) => match self.evaluate(val)? {
                DynValue::Pointer(ptr) => Ok(ptr.clone()),

                _ => {
                    let msg = format!("deref of non-pointer value @ {}", val);
                    Err(ExecError::illegal_state(msg))
                },
            },

            // let int := 1;
            // @int -> stack address of int val
            ir::Ref::Local(id) => self.current_frame()?.get_local_ptr(*id).map_err(|err| {
                let msg = err.to_string();
                ExecError::illegal_state(msg)
            }),

            ir::Ref::Global(global) => {
                let msg = format!("can't take address of global ref {:?}", global);
                Err(ExecError::illegal_state(msg))
            },
        }
    }

    pub fn opts(&self) -> &InterpreterOpts {
        &self.opts
    }

    pub fn execute(&mut self, instructions: &[ir::Instruction]) -> ExecResult<()> {
        let labels = find_labels(instructions);
        let line_count_width = instructions.len().to_string().len();

        let mut pc = 0;
        while pc < instructions.len() {
            if self.opts.trace_ir {
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

            self.exec_instruction(&instructions[pc], &mut pc, &labels)
                .map_err(|err| self.add_stack_trace(err))?;
            
            self.update_diagnostics();

            pc += 1;
        }

        Ok(())
    }

    fn exec_instruction(
        &mut self,
        instruction: &ir::Instruction,
        pc: &mut usize,
        labels: &HashMap<ir::Label, LabelLocation>,
    ) -> ExecResult<()> {
        match instruction {
            ir::Instruction::Comment(_) => {
                // noop
            },

            ir::Instruction::DebugPush(ctx) => {
                self.current_frame_mut()?.debug_push(ctx.clone());
            },

            ir::Instruction::DebugPop => {
                self.current_frame_mut()?.debug_pop();
            },

            ir::Instruction::LocalAlloc(id, ty) => {
                self.exec_local_alloc(*id, *pc, ty)?;
            },

            ir::Instruction::LocalBegin => self.exec_local_begin()?,
            ir::Instruction::LocalEnd => self.exec_local_end()?,

            ir::Instruction::RcNew { out, struct_id } => self.exec_rc_new(out, *struct_id)?,

            ir::Instruction::Add { out, a, b } => self.exec_add(out, a, b)?,

            ir::Instruction::Mul { out, a, b } => self.exec_mul(out, a, b)?,
            ir::Instruction::Div { out, a, b } => self.exec_div(out, a, b)?,
            ir::Instruction::Mod { out, a, b } => self.exec_mod(out, a, b)?,
            ir::Instruction::Sub { out, a, b } => self.exec_sub(out, a, b)?,
            ir::Instruction::Shl { out, a, b } => self.exec_shl(out, a, b)?,
            ir::Instruction::Shr { out, a, b } => self.exec_shr(out, a, b)?,
            ir::Instruction::Eq { out, a, b } => self.exec_eq(out, a, b)?,
            ir::Instruction::Gt { out, a, b } => self.exec_gt(out, a, b)?,
            ir::Instruction::Not { out, a } => self.exec_not(out, a)?,
            ir::Instruction::And { out, a, b } => self.exec_and(out, a, b)?,
            ir::Instruction::Or { out, a, b } => self.exec_or(out, a, b)?,

            ir::Instruction::BitAnd { out, a, b } => {
                self.exec_bitwise(out, a, b, u64::bitand, instruction)?
            },
            ir::Instruction::BitOr { out, a, b } => {
                self.exec_bitwise(out, a, b, u64::bitor, instruction)?
            },
            ir::Instruction::BitXor { out, a, b } => {
                self.exec_bitwise(out, a, b, u64::bitxor, instruction)?
            },
            ir::Instruction::BitNot { out, a } => self.exec_bitwise_not(out, a, instruction)?,

            ir::Instruction::Move { out, new_val } => {
                let val = self.evaluate(new_val)?;
                self.store(out, val)?
            },
            ir::Instruction::Call {
                out,
                function,
                args,
            } => self.exec_call(out, function, args)?,

            ir::Instruction::VirtualCall {
                out,
                iface_id,
                method,
                self_arg,
                rest_args,
            } => self.exec_virtual_call(out.as_ref(), *iface_id, *method, &self_arg, rest_args)?,

            ir::Instruction::ClassIs { out, a, class_id } => {
                self.exec_class_is(out, a, class_id)?
            },

            ir::Instruction::AddrOf { out, a } => {
                let a_ptr = self.addr_of_ref(a)?;
                self.store(out, DynValue::Pointer(a_ptr))?;
            },

            ir::Instruction::Field {
                out,
                a,
                field,
                of_ty,
            } => self.exec_field(out, &a, field, of_ty)?,

            ir::Instruction::Element {
                out,
                a,
                // not used because the interpreter doesn't need to know element type to
                // calculate the pointer offset
                element,
                index,
            } => {
                self.exec_element(out, a, index, element)?;
            },

            ir::Instruction::VariantTag { out, a, .. } => self.exec_variant_tag(out, a)?,

            ir::Instruction::VariantData { out, a, tag, of_ty } => {
                self.exec_variant_data(out, a, tag, of_ty)?
            },

            ir::Instruction::Label(_) => {
                // noop
            },

            ir::Instruction::Jump { dest } => self.exec_jump(pc, &labels[dest])?,
            ir::Instruction::JumpIf { dest, test } => self.exec_jmpif(pc, &labels, *dest, test)?,

            ir::Instruction::Release { at } => self.exec_release(at)?,
            ir::Instruction::Retain { at } => self.exec_retain(at)?,

            ir::Instruction::Raise { val } => self.exec_raise(&val)?,

            ir::Instruction::Cast { out, ty, a } => self.exec_cast(out, ty, a)?,
        }

        Ok(())
    }

    fn exec_local_alloc(&mut self, id: ir::LocalID, pc: usize, ty: &ir::Type) -> ExecResult<()> {
        let uninit_val = self.default_init_dyn_val(ty)?;

        let current_frame = self.current_frame_mut()?;
        current_frame
            .declare_local(id, ty.clone(), &uninit_val, pc)
            .map_err(|err| self.add_stack_trace(err.into()))?;

        Ok(())
    }

    fn exec_local_begin(&mut self) -> ExecResult<()> {
        self.current_frame_mut()?.push_block();

        Ok(())
    }

    fn exec_local_end(&mut self) -> ExecResult<()> {
        self.current_frame_mut()
            .and_then(|f| {
                f.pop_block()?;
                Ok(())
            })
            .map_err(|err| self.add_stack_trace(err.into()))?;

        Ok(())
    }

    fn exec_rc_new(&mut self, out: &ir::Ref, struct_id: ir::TypeDefID) -> ExecResult<()> {
        let struct_val = self.init_struct(struct_id)?;
        let rc_ptr = self.rc_alloc(struct_val)?;

        self.store(out, DynValue::Pointer(rc_ptr))?;

        Ok(())
    }

    fn exec_add(&mut self, out: &ir::Ref, a: &ir::Value, b: &ir::Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        let result = match (a_val, b_val) {
            // pointer arithmetic
            (DynValue::Pointer(ptr), DynValue::I32(offset))
            | (DynValue::I32(offset), DynValue::Pointer(ptr)) => {
                let offset_ptr = self.offset_ptr(ptr, offset as isize)?;

                Ok(DynValue::Pointer(offset_ptr))
            },

            // value addition
            (a_val, b_val) => match a_val.try_add(&b_val) {
                Some(result) => Ok(result),
                None => Err(ExecError::IllegalInstruction(ir::Instruction::Add {
                    a: a.clone(),
                    b: b.clone(),
                    out: out.clone(),
                })),
            },
        }?;

        self.store(out, result)?;
        Ok(())
    }

    fn offset_ptr(&self, ptr: Pointer, offset: isize) -> ExecResult<Pointer> {
        let marshal_ty = self.marshaller.get_ty(&ptr.ty)?;
        let ty_size = marshal_ty.size() as isize;

        Ok(Pointer {
            ty: ptr.ty,
            addr: (ptr.addr as isize + offset * ty_size) as usize,
        })
    }

    fn exec_raise(&mut self, val: &ir::Ref) -> ExecResult<()> {
        let msg = self.read_string(&val.clone())?;

        Err(ExecError::Raised { msg })
    }

    fn exec_cast(&mut self, out: &ir::Ref, ty: &ir::Type, a: &ir::Value) -> ExecResult<()> {
        let val = self.evaluate(a)?;
        match val.try_cast(ty) {
            Some(new_val) => {
                self.store(out, new_val)?;
                Ok(())
            },

            None => Err(ExecError::IllegalInstruction(ir::Instruction::Cast {
                out: out.clone(),
                a: a.clone(),
                ty: ty.clone(),
            })),
        }
    }

    pub fn dynfree(&mut self, ptr: &Pointer) -> ExecResult<()> {
        self.native_heap.free(&ptr.clone())?;
        Ok(())
    }

    pub fn dynalloc_init<ValuesIter, Values>(
        &mut self,
        ty: &ir::Type,
        values: Values,
    ) -> ExecResult<Pointer>
    where
        Values: IntoIterator<Item = DynValue, IntoIter = ValuesIter>,
        ValuesIter: ExactSizeIterator<Item = DynValue>,
    {
        let values = values.into_iter();
        if values.len() == 0 {
            return Err(ExecError::ZeroLengthAllocation);
        }

        let marshal_ty = self.marshaller.get_ty(&ty)?;
        let marshal_size = marshal_ty.size();

        let alloc_len = marshal_size * values.len();
        let alloc_ptr = self.dynalloc(ty, alloc_len)?;

        for (i, value) in values.enumerate() {
            let element_offset = i * marshal_size;
            let val_dst = Pointer {
                addr: alloc_ptr.addr + element_offset,
                ty: ty.clone(),
            };
            self.store_indirect(&val_dst, value)?;
        }

        Ok(alloc_ptr)
    }

    pub fn dynalloc(&mut self, ty: &ir::Type, len: usize) -> ExecResult<Pointer> {
        if len == 0 {
            return Err(ExecError::ZeroLengthAllocation);
        }

        let ptr = self.native_heap.alloc(ty.clone(), len)?;

        Ok(ptr)
    }

    fn exec_retain(&mut self, at: &ir::Ref) -> ExecResult<()> {
        let val = self.load(at)?.into_owned();
        self.retain_dyn_val(&val)?;

        Ok(())
    }

    fn exec_release(&mut self, at: &ir::Ref) -> ExecResult<()> {
        let val = self.load(at)?.into_owned();

        // to aid with debugging, set freed RC pointers to a recognizable value
        if self.release_dyn_val(&val)? {
            self.store(
                at,
                DynValue::Pointer(Pointer {
                    ty: ir::Type::Nothing,
                    addr: usize::MAX,
                }),
            )?;
        }

        Ok(())
    }

    fn exec_jmpif(
        &mut self,
        pc: &mut usize,
        labels: &HashMap<ir::Label, LabelLocation>,
        dest: ir::Label,
        cond: &ir::Value,
    ) -> ExecResult<()> {
        let cond_val = self.evaluate(cond)?;
        match cond_val {
            DynValue::Bool(true) => self.exec_jump(pc, &labels[&dest]),
            DynValue::Bool(false) => Ok(()),
            _ => Err(ExecError::illegal_state(
                "JumpIf instruction testing non-boolean value",
            )),
        }
    }

    fn exec_variant_data(
        &mut self,
        out: &ir::Ref,
        a: &ir::Ref,
        tag: &usize,
        of_ty: &ir::Type,
    ) -> ExecResult<()> {
        let variant_id = match of_ty {
            ir::Type::Variant(id) => *id,
            other => {
                let msg = format!(
                    "cannot execute variant data instruction for non-variant type: {}",
                    other
                );
                return Err(ExecError::illegal_state(msg));
            },
        };

        let a_ptr = self.addr_of_ref(a)?;

        let case_def = self
            .metadata
            .get_variant_def(variant_id)
            .and_then(|def| def.cases.get(*tag))
            .ok_or_else(|| {
                let msg = format!(
                    "missing definition for variant case {}.{}",
                    variant_id, *tag
                );
                ExecError::illegal_state(msg)
            })?;
        let case_ty = case_def.ty.clone().unwrap_or(ir::Type::Nothing);

        // we don't need to actually look this up, the variant data always appears right
        // after the fixed-size tag
        let data_offset = self.marshaller.variant_tag_type().size();

        self.store(
            out,
            DynValue::Pointer(Pointer {
                addr: a_ptr.addr + data_offset,
                ty: case_ty,
            }),
        )?;

        Ok(())
    }

    fn exec_variant_tag(&mut self, out: &ir::Ref, a: &ir::Ref) -> ExecResult<()> {
        // the variant tag is actually just the first member of the variant, so we just need to
        // output a pointer ot hte variant
        let tag_ptr = self.addr_of_ref(a)?;

        self.store(out, DynValue::Pointer(tag_ptr))?;

        Ok(())
    }

    fn exec_element(
        &mut self,
        out: &ir::Ref,
        a: &ir::Ref,
        index: &ir::Value,
        element: &ir::Type,
    ) -> ExecResult<()> {
        let array_ptr = self.addr_of_ref(a)?;

        let el_marshal_ty = self.marshaller.get_ty(element)?;

        let index_value = self
            .evaluate(index)?
            .as_i32()
            .map(|i| i as usize)
            .ok_or_else(|| {
                let msg = "element instruction has non-integer illegal index value";
                ExecError::illegal_state(msg)
            })?;

        let index_offset = el_marshal_ty.size() * index_value;

        self.store(
            out,
            DynValue::Pointer(Pointer {
                addr: array_ptr.addr + index_offset,
                ty: element.clone(),
            }),
        )?;

        Ok(())
    }

    fn exec_div(&mut self, out: &ir::Ref, a: &ir::Value, b: &ir::Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        match a_val.try_div(&b_val) {
            Some(result) => self.store(out, result),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Div {
                a: a.clone(),
                b: b.clone(),
                out: out.clone(),
            })),
        }
    }

    fn exec_mod(&mut self, out: &ir::Ref, a: &ir::Value, b: &ir::Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        match a_val.try_mod(&b_val) {
            Some(result) => self.store(out, result),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Mod {
                a: a.clone(),
                b: b.clone(),
                out: out.clone(),
            })),
        }
    }

    fn exec_mul(&mut self, out: &ir::Ref, a: &ir::Value, b: &ir::Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        match a_val.try_mul(&b_val) {
            Some(result) => self.store(out, result),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Mul {
                a: a.clone(),
                b: b.clone(),
                out: out.clone(),
            })),
        }
    }

    fn exec_sub(&mut self, out: &ir::Ref, a: &ir::Value, b: &ir::Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        let result = match (a_val, b_val) {
            // pointer arithmetic
            (DynValue::Pointer(ptr), DynValue::I32(offset))
            | (DynValue::I32(offset), DynValue::Pointer(ptr)) => {
                let ptr = self.offset_ptr(ptr, -offset as isize)?;

                Ok(DynValue::Pointer(ptr))
            },

            // value addition
            (a_val, b_val) => match a_val.try_sub(&b_val) {
                Some(result) => Ok(result),
                None => Err(ExecError::IllegalInstruction(ir::Instruction::Sub {
                    a: a.clone(),
                    b: b.clone(),
                    out: out.clone(),
                })),
            },
        }?;

        self.store(out, result)?;
        Ok(())
    }

    fn exec_shl(&mut self, out: &ir::Ref, a: &ir::Value, b: &ir::Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        match a_val.try_shl(&b_val) {
            Some(result) => self.store(out, result),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Shl {
                a: a.clone(),
                b: b.clone(),
                out: out.clone(),
            })),
        }
    }

    fn exec_shr(&mut self, out: &ir::Ref, a: &ir::Value, b: &ir::Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        match a_val.try_shr(&b_val) {
            Some(result) => self.store(out, result),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Shr {
                a: a.clone(),
                b: b.clone(),
                out: out.clone(),
            })),
        }
    }

    fn exec_eq(&mut self, out: &ir::Ref, a: &ir::Value, b: &ir::Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        match a_val.try_eq(&b_val) {
            Some(eq) => self.store(out, DynValue::Bool(eq)),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Eq {
                a: a.clone(),
                b: b.clone(),
                out: out.clone(),
            })),
        }
    }

    fn exec_gt(&mut self, out: &ir::Ref, a: &ir::Value, b: &ir::Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;
        let b_val = self.evaluate(b)?;

        match a_val.try_gt(&b_val) {
            Some(gt) => self.store(out, DynValue::Bool(gt)),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Gt {
                a: a.clone(),
                b: b.clone(),
                out: out.clone(),
            })),
        }
    }

    fn exec_not(&mut self, out: &ir::Ref, a: &ir::Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?;

        match a_val.try_not() {
            Some(not) => self.store(out, DynValue::Bool(not)),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Not {
                a: a.clone(),
                out: out.clone(),
            })),
        }
    }

    fn exec_and(&mut self, out: &ir::Ref, a: &ir::Value, b: &ir::Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?.as_bool().ok_or_else(|| {
            let msg = format!("operand a of And instruction must be bool, got {:?}", a);
            ExecError::illegal_state(msg)
        })?;

        let b_val = self.evaluate(b)?.as_bool().ok_or_else(|| {
            let msg = format!("operand b of And instruction must be bool, got {:?}", b);
            ExecError::illegal_state(msg)
        })?;

        self.store(out, DynValue::Bool(a_val && b_val))?;

        Ok(())
    }

    fn exec_or(&mut self, out: &ir::Ref, a: &ir::Value, b: &ir::Value) -> ExecResult<()> {
        let a_val = self.evaluate(a)?.as_bool().ok_or_else(|| {
            let msg = format!("operand a of Or instruction must be bool, got {:?}", a);
            ExecError::illegal_state(msg)
        })?;

        let b_val = self.evaluate(b)?.as_bool().ok_or_else(|| {
            let msg = format!("operand b of Or instruction must be bool, got {:?}", b);
            ExecError::illegal_state(msg)
        })?;

        self.store(out, DynValue::Bool(a_val || b_val))?;

        Ok(())
    }

    fn exec_bitwise<Op>(
        &mut self,
        out: &ir::Ref,
        a: &ir::Value,
        b: &ir::Value,
        op: Op,
        instruction: &ir::Instruction,
    ) -> ExecResult<()>
    where
        Op: Fn(u64, u64) -> u64,
    {
        let a_cell = self.evaluate(a)?;

        let a_val = a_cell
            .try_cast(&ir::Type::U64)
            .and_then(|val| val.as_u64())
            .ok_or_else(|| ExecError::IllegalInstruction(instruction.clone()))?;

        let b_val = self
            .evaluate(b)?
            .try_cast(&ir::Type::U64)
            .and_then(|val| val.as_u64())
            .ok_or_else(|| ExecError::IllegalInstruction(instruction.clone()))?;

        let result = op(a_val, b_val);
        self.store(
            out,
            match a_cell {
                DynValue::U8(_) => DynValue::U8(result as u8),
                DynValue::U16(_) => DynValue::U16(result as u16),
                DynValue::U32(_) => DynValue::U32(result as u32),
                DynValue::USize(_) => DynValue::USize(result as usize),
                _ => DynValue::U64(result),
            },
        )
    }

    fn exec_bitwise_not(
        &mut self,
        out: &ir::Ref,
        a: &ir::Value,
        instruction: &ir::Instruction,
    ) -> ExecResult<()> {
        let a_cell = self.evaluate(a)?;
        let a_val = a_cell
            .try_cast(&ir::Type::U64)
            .and_then(|val| val.as_u64())
            .ok_or_else(|| ExecError::IllegalInstruction(instruction.clone()))?;

        let result = !a_val;
        self.store(
            out,
            match a_cell {
                DynValue::U8(_) => DynValue::U8(result as u8),
                DynValue::U16(_) => DynValue::U16(result as u16),
                DynValue::U32(_) => DynValue::U32(result as u32),
                DynValue::USize(_) => DynValue::USize(result as usize),
                _ => DynValue::U64(result),
            },
        )
    }

    fn exec_call(
        &mut self,
        out: &Option<ir::Ref>,
        function: &ir::Value,
        args: &Vec<ir::Value>,
    ) -> ExecResult<()> {
        let arg_vals: Vec<_> = args
            .iter()
            .map(|arg_val| self.evaluate(arg_val))
            .collect::<ExecResult<_>>()?;

        match self.evaluate(function)? {
            DynValue::Function(function) => self.call(function, &arg_vals, out.as_ref())?,

            _ => {
                let msg = format!("{} does not reference a function", function);
                return Err(ExecError::illegal_state(msg));
            },
        }

        Ok(())
    }

    fn exec_virtual_call(
        &mut self,
        out: Option<&ir::Ref>,
        iface_id: ir::InterfaceID,
        method: ir::MethodID,
        self_arg: &ir::Value,
        rest_args: &[ir::Value],
    ) -> ExecResult<()> {
        let self_val = self.evaluate(&self_arg)?;
        let func = self.vcall_lookup(&self_val, iface_id, method)?;

        let mut arg_vals = vec![self_val];
        for arg_val in rest_args {
            let arg_val = self.evaluate(arg_val)?;
            arg_vals.push(arg_val);
        }

        let func_ref = ir::Ref::Global(ir::GlobalRef::Function(func));
        let func = match self.evaluate(&ir::Value::Ref(func_ref))? {
            DynValue::Function(func) => func,
            unexpected => {
                let msg = format!("invalid function val: {:?}", unexpected);
                return Err(ExecError::illegal_state(msg));
            },
        };

        self.call(func, &arg_vals, out)?;

        Ok(())
    }

    fn exec_class_is(
        &mut self,
        out: &ir::Ref,
        a: &ir::Value,
        class_id: &ir::VirtualTypeID,
    ) -> ExecResult<()> {
        let a_ptr = self.evaluate(a)?.as_pointer().cloned().ok_or_else(|| {
            let msg = "argument a of ClassIs instruction must evaluate to a pointer";
            ExecError::illegal_state(msg)
        })?;

        let a_val = match self.load_indirect(&a_ptr)?.into_owned() {
            DynValue::Structure(struct_val) => Ok(struct_val),
            _ => {
                let msg = "pointer target of ClassIs instruction must point to a class type";
                Err(ExecError::illegal_state(msg))
            },
        }?;

        let is = match class_id {
            ir::VirtualTypeID::Any => true,

            ir::VirtualTypeID::Class(type_id) => a_val.type_id == *type_id,

            ir::VirtualTypeID::Interface(iface_id) => {
                let resource_id = ir::VirtualTypeID::Class(a_val.type_id);
                let actual_ty = ir::Type::RcPointer(resource_id);

                self.metadata.is_impl(&actual_ty, *iface_id)
            },

            // todo: can `is` be used with closures?
            ir::VirtualTypeID::Closure(..) => false,
        };

        self.store(out, DynValue::Bool(is))?;

        Ok(())
    }

    fn exec_field(
        &mut self,
        out: &ir::Ref,
        a: &ir::Ref,
        field: &ir::FieldID,
        of_ty: &ir::Type,
    ) -> ExecResult<()> {
        let field_ptr = match of_ty {
            ir::Type::Struct(struct_id) => {
                let struct_ptr = self.addr_of_ref(a)?;

                let field_info = self.marshaller.get_field_info(*struct_id, *field)?;

                Pointer {
                    ty: field_info.ty,
                    addr: struct_ptr.addr + field_info.offset,
                }
            },

            // assume the statically-provided type ID is correct, no need to load the actual value
            // and check dynamically
            ir::Type::RcPointer(ir::VirtualTypeID::Class(type_id)) => {
                let val_ptr = self.addr_of_ref(&a.clone().to_deref())?;

                let field_info = self.marshaller.get_field_info(*type_id, *field)?;

                Pointer {
                    ty: field_info.ty,
                    addr: val_ptr.addr + field_info.offset,
                }
            },

            // virtual reference, we need to load the actual value behind the pointer to get the
            // concrete type ID
            ir::Type::RcPointer(..) => {
                let val_ptr = self.addr_of_ref(&a.clone().to_deref())?;

                let val = self.load_indirect(&val_ptr)?;
                let struct_val = match val.as_ref() {
                    DynValue::Structure(struct_val) => struct_val,
                    val => {
                        let msg = format!("trying to read field pointer of rc type but target wasn't an rc value @ {} (target was: {:?}", a, val);
                        return Err(ExecError::illegal_state(msg));
                    },
                };

                let field_info = self.marshaller.get_field_info(struct_val.type_id, *field)?;

                Pointer {
                    ty: field_info.ty,
                    addr: val_ptr.addr + field_info.offset,
                }
            },

            _ => {
                let msg = format!(
                    "invalid base type referenced in Field instruction: {}.{}",
                    of_ty, field
                );
                return Err(ExecError::illegal_state(msg));
            },
        };

        self.store(out, DynValue::Pointer(field_ptr))?;

        Ok(())
    }

    fn exec_jump(&mut self, pc: &mut usize, label: &LabelLocation) -> ExecResult<()> {
        *pc = label.pc_offset;

        // assume all jumps are either upwards or to the same level
        self.current_frame_mut()?.pop_block_to(label.block_depth)?;

        Ok(())
    }

    fn define_builtin(
        &mut self,
        name: ir::NamePath,
        func: BuiltinFn,
        ret: ir::Type,
        params: Vec<ir::Type>,
    ) {
        if let Some(func_id) = self.metadata.find_function(&name) {
            self.globals.insert(
                ir::GlobalRef::Function(func_id),
                GlobalValue {
                    value: DynValue::Function(func_id),
                    ty: ir::Type::Nothing,
                },
            );

            let func = Function::Builtin(BuiltinFunction {
                func,
                return_ty: ret,
                param_tys: params,
                debug_name: name.to_string(),
            });

            self.functions.insert(func_id, Rc::new(func));
        }
    }

    fn rc_alloc(&mut self, mut value: StructValue) -> ExecResult<Pointer> {
        let res_ty = ir::Type::Struct(value.type_id);

        value.rc = Some(RcState {
            strong_count: 1,
            weak_count: 0,
        });

        let res_ptr = self.dynalloc_init(&res_ty, [DynValue::from(value)])?;

        Ok(res_ptr)
    }

    fn init_stdlib_globals(&mut self) {
        let system_funcs: &[(&str, BuiltinFn, ir::Type, Vec<ir::Type>)] = &[
            (
                "Int8ToStr",
                builtin::i8_to_str,
                ir::Type::string_ptr(),
                vec![ir::Type::I8],
            ),
            (
                "UInt8ToStr",
                builtin::u8_to_str,
                ir::Type::string_ptr(),
                vec![ir::Type::U8],
            ),
            (
                "Int16ToStr",
                builtin::i16_to_str,
                ir::Type::string_ptr(),
                vec![ir::Type::I16],
            ),
            (
                "UInt16ToStr",
                builtin::u16_to_str,
                ir::Type::string_ptr(),
                vec![ir::Type::U16],
            ),
            (
                "Int32ToStr",
                builtin::i32_to_str,
                ir::Type::string_ptr(),
                vec![ir::Type::I32],
            ),
            (
                "UInt32ToStr",
                builtin::u32_to_str,
                ir::Type::string_ptr(),
                vec![ir::Type::U32],
            ),
            (
                "Int64ToStr",
                builtin::i64_to_str,
                ir::Type::string_ptr(),
                vec![ir::Type::I64],
            ),
            (
                "UInt64ToStr",
                builtin::u64_to_str,
                ir::Type::string_ptr(),
                vec![ir::Type::U64],
            ),
            (
                "NativeIntToStr",
                builtin::isize_to_str,
                ir::Type::string_ptr(),
                vec![ir::Type::ISize],
            ),
            (
                "NativeUIntToStr",
                builtin::usize_to_str,
                ir::Type::string_ptr(),
                vec![ir::Type::USize],
            ),
            (
                "StrToInt",
                builtin::str_to_int,
                ir::Type::I32,
                vec![ir::STRING_TYPE],
            ),
            (
                "Write",
                builtin::write,
                ir::Type::Nothing,
                vec![ir::STRING_TYPE],
            ),
            (
                "WriteLn",
                builtin::write_ln,
                ir::Type::Nothing,
                vec![ir::STRING_TYPE],
            ),
            ("ReadLn", builtin::read_ln, ir::Type::string_ptr(), vec![]),
            (
                "GetMem",
                builtin::get_mem,
                ir::Type::U8.ptr(),
                vec![ir::Type::I32],
            ),
            (
                "FreeMem",
                builtin::free_mem,
                ir::Type::Nothing,
                vec![ir::Type::U8.ptr()],
            ),
            (
                "ArrayLengthInternal",
                builtin::array_length,
                ir::Type::I32,
                vec![ir::Type::any()],
            ),
            (
                "ArraySetLengthInternal",
                builtin::set_length,
                ir::Type::any(),
                vec![ir::Type::any(), ir::Type::I32, ir::Type::any()],
            ),
            (
                "RandomInteger",
                builtin::random_integer,
                ir::Type::I32,
                vec![ir::Type::I32, ir::Type::I32]
            ),
            (
                "RandomSingle",
                builtin::random_single,
                ir::Type::F32,
                vec![ir::Type::F32, ir::Type::F32]
            )
            ,(
                "Pow",
                builtin::pow,
                ir::Type::F32,
                vec![ir::Type::F32, ir::Type::F32]
            ),
            (
                "Sqrt",
                builtin::sqrt,
                ir::Type::F32,
                vec![ir::Type::F32]
            ),
            (
                "Sin",
                builtin::sin,
                ir::Type::F32,
                vec![ir::Type::F32]
            ),
            (
                "ArcSin",
                builtin::arc_sin,
                ir::Type::F32,
                vec![ir::Type::F32]
            ),
            (
                "Cos",
                builtin::cos,
                ir::Type::F32,
                vec![ir::Type::F32]
            ),
            (
                "ArcCos",
                builtin::arc_cos,
                ir::Type::F32,
                vec![ir::Type::F32]
            ),
            (
                "Tan",
                builtin::tan,
                ir::Type::F32,
                vec![ir::Type::F32]
            ),
            (
                "ArcTan",
                builtin::arc_tan,
                ir::Type::F32,
                vec![ir::Type::F32]
            )
        ];

        for (ident, func, ret, params) in system_funcs {
            let name = ir::NamePath::new(vec!["System".to_string()], ident.to_string());
            self.define_builtin(name, *func, ret.clone(), params.to_vec());
        }
    }

    pub fn load_module(&mut self, module: &ir::Module) -> ExecResult<()> {
        self.metadata.extend(&module.metadata());

        let mut marshaller = (*self.marshaller).clone();

        for (id, type_def) in module.metadata().type_defs() {
            let def_result = match type_def {
                ir::TypeDef::Struct(struct_def) => marshaller
                    .add_struct(id, struct_def, module.metadata())
                    .map(Some),
                ir::TypeDef::Variant(variant_def) => marshaller
                    .add_variant(id, variant_def, module.metadata())
                    .map(Some),
                ir::TypeDef::Function(_func_def) => {
                    // functions don't need special marshalling, we only marshal pointers to them
                    Ok(None)
                },
            };

            def_result.map_err(|err| self.add_stack_trace(err.into()))?;
        }

        for (func_id, ir_func) in module.functions() {
            let func_ref = ir::GlobalRef::Function(*func_id);

            let func = match ir_func {
                ir::Function::Local(ir_func_def) => {
                    let ir_func = Function::IR(ir_func_def.clone());
                    Some(ir_func)
                },

                ir::Function::External(external_ref) if external_ref.src == ir::BUILTIN_SRC => None,

                ir::Function::External(external_ref) => {
                    let ffi_func = Function::new_ffi(external_ref, &mut marshaller, &self.metadata)
                        .map_err(|err| ExecError::WithStackTrace {
                            err: Box::new(ExecError::MarshalError(err)),
                            stack_trace: self.stack_trace(),
                        })?;
                    Some(ffi_func)
                },
            };

            if let Some(func) = func {
                self.globals.insert(
                    func_ref,
                    GlobalValue {
                        value: DynValue::Function(*func_id),
                        ty: ir::Type::Nothing,
                    },
                );

                self.functions.insert(*func_id, Rc::new(func));
            }
        }

        self.marshaller = Rc::new(marshaller);
        self.native_heap.set_marshaller(self.marshaller.clone());

        for (id, literal) in module.metadata().strings() {
            let str_val = self
                .create_string(literal)
                .map_err(|err| ExecError::WithStackTrace {
                    err: err.into(),
                    stack_trace: self.stack_trace(),
                })?;

            let str_ref = ir::GlobalRef::StringLiteral(id);

            self.globals.insert(
                str_ref,
                GlobalValue {
                    value: str_val,
                    ty: ir::Type::RcPointer(ir::VirtualTypeID::Class(ir::STRING_ID)),
                },
            );
        }

        self.init_stdlib_globals();

        let init_stack_size = self
            .marshaller
            .stack_alloc_size(module.init())
            .map_err(|err| self.add_stack_trace(err.into()))?;

        self.push_stack("<init>", init_stack_size);

        if let Some(module_span) = module.span() {
            self.current_frame_mut()?.debug_push(module_span.clone());
        }

        self.execute(module.init())?;
        self.pop_stack()?;

        Ok(())
    }

    fn create_string(&mut self, content: &str) -> ExecResult<DynValue> {
        let chars: Vec<_> = content.chars().map(|c| DynValue::U8(c as u8)).collect();
        let chars_len = cast::i32(chars.len()).map_err(|_| {
            let msg = format!("string length out of range: {}", chars.len());
            ExecError::illegal_state(msg)
        })?;

        let chars_ptr = if chars_len > 0 {
            self.dynalloc_init(&ir::Type::U8, chars)?
        } else {
            Pointer::null(ir::Type::U8)
        };

        let mut string_struct = self.init_struct(ir::STRING_ID)?;
        string_struct[ir::STRING_LEN_FIELD] = DynValue::I32(chars_len);
        string_struct[ir::STRING_CHARS_FIELD] = DynValue::Pointer(chars_ptr);

        let str_ptr = self.rc_alloc(string_struct)?;

        Ok(DynValue::Pointer(str_ptr))
    }

    // reads the string value stored in the string object that `str_ref` is a pointer to
    fn read_string(&self, str_ref: &ir::Ref) -> ExecResult<String> {
        let str_ptr = self.load(&str_ref.clone().to_deref())?;

        let str_struct = match str_ptr.as_struct(ir::STRING_ID) {
            Some(struct_val) if struct_val.type_id == ir::STRING_ID => struct_val,
            _ => {
                let msg = format!(
                    "tried to read string value from rc val which didn't contain a string struct: {:?}",
                    str_ptr,
                );
                return Err(ExecError::illegal_state(msg));
            },
        };

        self.read_string_struct(str_struct)
    }

    fn read_string_struct(&self, str_struct: &StructValue) -> ExecResult<String> {
        let len_val = &str_struct[ir::STRING_LEN_FIELD];
        let len = len_val
            .as_i32()
            .and_then(|len| cast::usize(len).ok())
            .ok_or_else(|| {
                let msg = format!("string length value contained invalid value: {:?}", len_val);
                ExecError::illegal_state(msg)
            })?;

        if len == 0 {
            return Ok(String::new());
        }

        let chars_ptr = str_struct[ir::STRING_CHARS_FIELD]
            .as_pointer()
            .ok_or_else(|| {
                ExecError::illegal_state(format!(
                    "string contained invalid `chars` pointer value: {:?}",
                    str_struct
                ))
            })?;

        let mut chars = Vec::new();
        for i in 0..len {
            let char_ptr = Pointer {
                addr: chars_ptr.addr + i,
                ty: chars_ptr.ty.clone(),
            };

            let char_val = self.load_indirect(&char_ptr)?.as_u8().ok_or_else(|| {
                ExecError::illegal_state(format!("expected string char @ {}", char_ptr))
            })?;

            chars.push(char_val as char);
        }

        Ok(chars.into_iter().collect())
    }
    
    fn update_diagnostics(&self) {
        if let Some(diag_worker) = &self.diag_worker {
            diag_worker.update(|| DiagnosticOutput {
                stack_trace: self.stack_trace(),
                heap_stats: self.native_heap.stats(),
            });
        }
    }

    pub fn shutdown(mut self) -> ExecResult<()> {
        let globals: Vec<_> = self.globals.values().cloned().collect();

        for GlobalValue { value, ty } in globals {
            if ty.is_rc() {
                self.release_dyn_val(&value)?;
            }
        }

        if self.opts.trace_heap {
            self.native_heap.print_trace_stats();
        }
        
        if let Some(worker) = self.diag_worker.take() {
            worker.shutdown();
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InterpreterOpts {
    pub trace_heap: bool,
    pub trace_rc: bool,
    pub trace_ir: bool,
    
    pub diag_port: u16,
}

#[derive(Debug, Clone)]
pub struct GlobalValue {
    pub value: DynValue,
    ty: ir::Type,
}

struct LabelLocation {
    pc_offset: usize,
    block_depth: usize,
}

fn find_labels(instructions: &[ir::Instruction]) -> HashMap<ir::Label, LabelLocation> {
    let mut block_depth = 0;
    let mut locations = HashMap::new();

    for (pc_offset, instruction) in instructions.iter().enumerate() {
        match instruction {
            ir::Instruction::LocalBegin => block_depth += 1,
            ir::Instruction::LocalEnd => block_depth -= 1,
            ir::Instruction::Label(label) => {
                locations.insert(
                    label.clone(),
                    LabelLocation {
                        block_depth,
                        pc_offset,
                    },
                );
            },
            _ => continue,
        }
    }

    locations
}
