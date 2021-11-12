use pas_ir::LocalID;
use crate::MemCell;

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

    pub fn deref_local(&self, id: LocalID) -> &MemCell {
        self.locals[id.0]
            .as_ref()
            .map(|cell| &cell.value)
            .unwrap_or_else(|| panic!("local cell {} is not allocated in stack frame \"{}\"", id, self.name))
    }

    pub fn deref_local_mut(&mut self, id: LocalID) -> &mut MemCell {
        let cell_val = self.locals[id.0].as_mut().map(|cell| &mut cell.value);
        match cell_val {
            Some(cell_val) => cell_val,
            None => {
                panic!("local cell {} is not allocated in stack frame \"{}\"", id, self.name);
            }
        }
    }

    pub fn push_local(&mut self, value: MemCell) {
        self.locals.push(Some(LocalCell {
            alloc_pc: None,
            value,
        }));
    }

    pub fn alloc_local(&mut self, id: LocalID, alloc_pc: usize, value: MemCell) {
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

        self.push_block_decl(id);
    }

    pub fn is_allocated(&self, id: LocalID) -> bool {
        id.0 < self.locals.len()
    }

    pub fn store_local(&mut self, id: LocalID, value: MemCell) {
        match self.locals.get_mut(id.0) {
            Some(Some(cell)) => {
                cell.value = value;
            }
            None | Some(None) => panic!("local cell {} is not allocated", id),
        }
    }

    pub fn load_local(&self, id: LocalID) -> &MemCell {
        match self.locals.get(id.0) {
            Some(Some(cell)) => &cell.value,
            None | Some(None) => {
                panic!("local cell {} is not allocated", id);
            }
        }
    }

    pub fn push_block(&mut self) {
        self.block_stack.push(Block {
            decls: Vec::new(),
        });
    }

    pub fn pop_block(&mut self) {
        let popped_block = self
            .block_stack
            .pop()
            .expect("block stack must never be empty");

        for id in popped_block.decls {
            if !self.is_allocated(id) {
                panic!("local cell {} is not allocated", id);
            }
            self.locals[id.0] = None;
        }
    }

    pub fn pop_block_to(&mut self, block_depth: usize) {
        let current_block = self.block_stack.len() - 1;

        assert!(
            current_block >= block_depth,
            "jmp from block level {} to {} is invalid, can only jmp upwards in the block stack",
            current_block,
            block_depth
        );

        let pop_blocks = current_block - block_depth;

        for _ in 0..pop_blocks {
            let popped_block = self.block_stack.pop().unwrap();
            for decl in popped_block.decls {
                self.locals[decl.0] = None;
            }
        }
    }

    fn push_block_decl(&mut self, id: LocalID) {
        let current_block = self.block_stack.last_mut().expect("block stack must never be empty");
        current_block.decls.push(id);
    }
}
