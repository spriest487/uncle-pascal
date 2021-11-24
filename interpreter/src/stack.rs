use std::fmt;
use std::rc::Rc;
use pas_ir::{LocalID, Type};
use crate::{FfiCache, ValueCell};
use crate::func::ffi::{MarshalError, MarshalResult};
use crate::heap::NativePointer;

#[derive(Debug)]
struct Block {
    decls: Vec<LocalID>,

    initial_stack_offset: usize,
}

#[derive(Debug)]
struct StackAlloc {
    // local cells are allocated on the fly as the interpreter passes LocalAlloc
    // instructions. we want to prevent IR code from alloc-ing two locals with the same
    // ID, but we might also legally run the same alloc instruction more than once if flow control
    // takes us back over it.
    // therefore we need to remember where a local allocation was made,
    // so the duplicate check can tell whether it's two allocations with the same ID
    // function param allocs don't have an alloc locatoin
    alloc_pc: Option<usize>,

    ty: Type,

    stack_offset: usize,
}

#[derive(Debug)]
pub(super) struct StackFrame {
    name: String,

    locals: Vec<StackAlloc>,
    block_stack: Vec<Block>,

    stack_mem: Box<[u8]>,
    stack_offset: usize,

    marshaller: Rc<FfiCache>,
}

impl StackFrame {
    pub fn new(name: impl Into<String>, marshaller: Rc<FfiCache>, stack_size: usize) -> Self {
        Self {
            name: name.into(),

            locals: Vec::new(),
            block_stack: vec![Block {
                decls: Vec::new(),
                initial_stack_offset: 0,
            }],

            marshaller,

            stack_mem: vec![0; stack_size].into_boxed_slice(),
            stack_offset: 0,
        }
    }

    pub fn push_local(&mut self, ty: Type, value: &ValueCell, alloc_pc: Option<usize>) -> MarshalResult<LocalID> {
        let start_offset = self.stack_offset;
        let alloc_slice = &mut self.stack_mem[start_offset..];
        let size = self.marshaller.marshal(value, alloc_slice)?;
        self.stack_offset += size;

        self.locals.push(StackAlloc {
            alloc_pc,
            ty,
            stack_offset: start_offset,
        });

        let id = LocalID(self.locals.len() - 1);
        Ok(id)
    }

    pub fn get_local_ptr(&self, id: LocalID) -> StackResult<NativePointer> {
        let alloc = self.locals.get(id.0).ok_or_else(|| StackError::LocalNotAllocated(id))?;

        Ok(NativePointer {
            ty: alloc.ty.clone(),
            addr: self.stack_mem.as_ptr() as usize + alloc.stack_offset,
        })
    }

    pub fn push_block(&mut self) {
        self.block_stack.push(Block {
            decls: Vec::new(),
            initial_stack_offset: self.stack_offset,
        });
    }

    pub fn pop_block(&mut self) -> StackResult<()> {
        let popped_block = self
            .block_stack
            .pop()
            .ok_or_else(|| StackError::EmptyBlockStack)?;

        let new_stack_offset = popped_block.initial_stack_offset;
        self.stack_offset = new_stack_offset;
        self.locals.retain(|l| l.stack_offset < new_stack_offset);

        Ok(())
    }

    pub fn pop_block_to(&mut self, block_depth: usize) -> StackResult<()> {
        let current_block = self.block_stack.len() - 1;

        if current_block < block_depth {
            return Err(StackError::IllegalJmp {
                current_block,
                dest_block: block_depth,
            });
        }

        let pop_block_count = current_block - block_depth;

        for _ in 0..pop_block_count {
            match self.block_stack.pop() {
                Some(popped_block) => {
                    self.stack_offset = popped_block.initial_stack_offset;
                },

                None => {
                    return Err(StackError::EmptyBlockStack)
                }
            }
        }

        let new_stack_offset = self.stack_offset;
        self.locals.retain(|l| l.stack_offset < new_stack_offset);

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum StackError {
    LocalNotAllocated(LocalID),
    IllegalJmp {
        current_block: usize,
        dest_block: usize,
    },
    EmptyBlockStack,
    MarshalError(MarshalError),
}

impl From<MarshalError> for StackError {
    fn from(err: MarshalError) -> Self {
        StackError::MarshalError(err)
    }
}

impl fmt::Display for StackError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StackError::LocalNotAllocated(id) => {
                write!(f, "local is not allocated: {}", id)
            }
            StackError::EmptyBlockStack => {
                write!(f, "unbalanced block delimiters: popping empty block stack")
            }
            StackError::IllegalJmp { current_block, dest_block } => {
                write!(f, "illegal jump from block {} to block {}", current_block, dest_block)
            }
            StackError::MarshalError(err) => {
                write!(f, "{}", err)
            }
        }
    }
}

pub type StackResult<T> = Result<T, StackError>;