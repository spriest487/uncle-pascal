use std::fmt;
use pas_ir::LocalID;
use crate::ValueCell;

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

    value: ValueCell,
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
pub(super) struct StackFrame {
    name: String,

    locals: Vec<Option<LocalCell>>,
    block_stack: Vec<Block>,
}

impl StackFrame {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),

            locals: Vec::new(),
            block_stack: vec![Block { decls: Vec::new() }],
        }
    }

    pub fn deref_local(&self, id: LocalID) -> StackResult<&ValueCell> {
        match self.locals[id.0].as_ref() {
            Some(local_val) => {
                Ok(&local_val.value)
            }
            None => Err(StackError::LocalNotAllocated(id)),
        }
    }

    pub fn deref_local_mut(&mut self, id: LocalID) -> StackResult<&mut ValueCell> {
        match self.locals[id.0].as_mut() {
            Some(local_val) => {
                Ok(&mut local_val.value)
            }
            None => Err(StackError::LocalNotAllocated(id)),
        }
    }

    pub fn push_local(&mut self, value: ValueCell) {
        self.locals.push(Some(LocalCell {
            alloc_pc: None,
            value,
        }));
    }

    pub fn alloc_local(&mut self, id: LocalID, alloc_pc: usize, value: ValueCell) -> StackResult<()> {
        while self.locals.len() <= id.0 {
            self.locals.push(None);
        }

        match &mut self.locals[id.0] {
            None => {
                self.locals[id.0] = Some(LocalCell {
                    value,
                    alloc_pc: Some(alloc_pc),
                })
            }
            Some(already_allocated) => {
                // the same ID can only be reused if this is the same instruction that
                // allocated it in the first place
                if !already_allocated.is_alloc_pc(alloc_pc) {
                    panic!("local cell {} is already allocated", id)
                }
                already_allocated.value = value;
            }
        }

        self.push_block_decl(id)?;

        Ok(())
    }

    pub fn is_allocated(&self, id: LocalID) -> bool {
        id.0 < self.locals.len()
    }

    pub fn store_local(&mut self, id: LocalID, value: ValueCell) -> StackResult<()> {
        match self.locals.get_mut(id.0) {
            Some(Some(cell)) => {
                cell.value = value;
                Ok(())
            }
            None | Some(None) => {
                Err(StackError::LocalNotAllocated(id))
            },
        }
    }

    pub fn load_local(&self, id: LocalID) -> StackResult<&ValueCell> {
        match self.locals.get(id.0) {
            Some(Some(cell)) => Ok(&cell.value),
            None | Some(None) => {
                Err(StackError::LocalNotAllocated(id))
            }
        }
    }

    pub fn push_block(&mut self) {
        self.block_stack.push(Block {
            decls: Vec::new(),
        });
    }

    pub fn pop_block(&mut self) -> StackResult<()> {
        let popped_block = self
            .block_stack
            .pop()
            .ok_or_else(|| StackError::EmptyBlockStack)?;

        for id in popped_block.decls {
            self.locals[id.0] = None;
        }

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

        let pop_blocks = current_block - block_depth;

        for _ in 0..pop_blocks {
            match self.block_stack.pop() {
                Some(popped_block) => {
                    for decl in popped_block.decls {
                        self.locals[decl.0] = None;
                    }
                },

                None => {
                    return Err(StackError::EmptyBlockStack)
                }
            }

        }

        Ok(())
    }

    fn push_block_decl(&mut self, id: LocalID) -> StackResult<()> {
        match self.block_stack.last_mut() {
            Some(current_block) => {
                current_block.decls.push(id);
                Ok(())
            }

            None => Err(StackError::EmptyBlockStack)
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum StackError {
    LocalNotAllocated(LocalID),
    IllegalJmp {
        current_block: usize,
        dest_block: usize,
    },
    EmptyBlockStack,
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
        }
    }
}

pub type StackResult<T> = Result<T, StackError>;