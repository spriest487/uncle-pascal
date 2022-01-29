use std::convert::TryInto;
use std::fmt;
use std::mem::size_of;
use std::rc::Rc;
use pas_ir::{LocalID, Type};
use crate::{ValueCell, marshal::{Marshaller, MarshalError, MarshalResult}, Pointer};

const SENTINEL: usize = 12345678;

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

    marshaller: Rc<Marshaller>,
}

impl StackFrame {
    pub fn new(name: impl Into<String>, marshaller: Rc<Marshaller>, stack_size: usize) -> Self {
        let sentinel_size = size_of::<usize>();
        let mut stack_mem = vec![0; stack_size + sentinel_size];
        stack_mem[stack_size..].copy_from_slice(&SENTINEL.to_ne_bytes());

        Self {
            name: name.into(),

            locals: Vec::new(),
            block_stack: vec![Block {
                decls: Vec::new(),
                initial_stack_offset: 0,
            }],

            marshaller,

            stack_mem: stack_mem.into_boxed_slice(),
            stack_offset: 0,
        }
    }

    /// add a local to the stack without a local variable declaration. function params and return
    /// values get allocated by this mechanism because they aren't ever declared as locals in the
    /// body of the function
    pub fn add_undeclared_local(&mut self, ty: Type, value: &ValueCell) -> MarshalResult<LocalID> {
        let stack_offset = self.stack_alloc(value)?;

        assert_eq!(self.marshaller.get_ty(&ty)?.size(), self.stack_offset - stack_offset);

        self.locals.push(StackAlloc {
            alloc_pc: None,
            ty,
            stack_offset,
        });

        let id = LocalID(self.locals.len() - 1);
        Ok(id)
    }

    pub fn declare_local(&mut self, id: LocalID, ty: Type, value: &ValueCell, alloc_pc: usize) -> StackResult<()> {
        if self.locals.len() != id.0 {
            return Err(StackError::IllegalAlloc(id));
        }

        // we only need to allocate new variables the first time the block is executed, so if
        // we try to allocate twice from the same instruction, just do nothing
        // todo: this could be cleaned up by allocating everything at the start of the block
        // instead of doing it as we encounter new locals
        for (existing_id, local) in self.locals.iter().enumerate() {
            if existing_id == id.0 {
                if local.alloc_pc != Some(alloc_pc) {
                    return Err(StackError::DuplicateLocalAlloc {
                        stack_frame: self.name.clone(),
                        id,
                        first_pc: local.alloc_pc,
                        next_pc: alloc_pc,
                    });
                }

                return Ok(());
            }
        }

        let stack_offset = self.stack_alloc(value)?;

        self.locals.push(StackAlloc {
            alloc_pc: Some(alloc_pc),
            ty,
            stack_offset,
        });

        Ok(())
    }

    pub fn check_sentinel(&self) -> StackResult<()> {
        let sentinel_bytes = self.stack_mem[self.stack_mem.len() - size_of::<usize>()..]
            .try_into()
            .unwrap();

        let sentinel = usize::from_ne_bytes(sentinel_bytes);

        if sentinel == SENTINEL {
            Ok(())
        } else {
            Err(StackError::BadSentinel(sentinel))
        }
    }

    fn stack_alloc(&mut self, value: &ValueCell) -> MarshalResult<usize> {
        let start_offset = self.stack_offset;
        let alloc_slice = &mut self.stack_mem[start_offset..];
        let size = self.marshaller.marshal(value, alloc_slice)?;
        self.stack_offset += size;

        Ok(start_offset)
    }

    pub fn get_local_ptr(&self, id: LocalID) -> StackResult<Pointer> {
        match self.locals.get(id.0) {
            Some(alloc) => {
                Ok(Pointer {
                    ty: alloc.ty.clone(),
                    addr: self.stack_mem.as_ptr() as usize + alloc.stack_offset,
                })
            }

            None => {
                Err(StackError::LocalNotAllocated(id))
            }
        }
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
    DuplicateLocalAlloc {
        stack_frame: String,
        id: LocalID,
        first_pc: Option<usize>,
        next_pc: usize,
    },
    IllegalJmp {
        current_block: usize,
        dest_block: usize,
    },
    IllegalAlloc(LocalID),
    EmptyBlockStack,
    MarshalError(MarshalError),
    BadSentinel(usize),
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
            StackError::DuplicateLocalAlloc { id, first_pc, next_pc, stack_frame } => {
                write!(f, "{}: local {} was reallocated by a separate instruction (", stack_frame, id)?;

                match first_pc {
                    Some(first_pc) => write!(f, "first alloc @ instruction {}, next alloc @ instruction {}", first_pc, next_pc)?,
                    None => write!(f, " (reallocation @ instruction {})", next_pc)?,
                }

                write!(f, ")")
            }
            StackError::EmptyBlockStack => {
                write!(f, "unbalanced block delimiters: popping empty block stack")
            }
            StackError::IllegalJmp { current_block, dest_block } => {
                write!(f, "illegal jump from block {} to block {}", current_block, dest_block)
            }
            StackError::IllegalAlloc(id) => {
                write!(f, "allocation of local cell {} is not legal here", id)
            }
            StackError::MarshalError(err) => {
                write!(f, "{}", err)
            }
            StackError::BadSentinel(sentinel) => {
                write!(f, "bad sentinel value: {}", sentinel)
            }
        }
    }
}

pub type StackResult<T> = Result<T, StackError>;