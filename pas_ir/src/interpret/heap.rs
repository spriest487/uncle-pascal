use {
    std::{
        fmt,
        mem::size_of,
    },
    super::{
        MemCell,
    },
};

#[derive(Debug, Clone)]
struct HeapCell {
    ref_count: usize,
    val: MemCell,
}

#[derive(Debug, Clone)]
pub struct RcHeap {
    slots: Vec<Option<HeapCell>>,

    pub trace: bool,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct RcAddress(usize);

impl fmt::Display for RcAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:0width$x}", self.0, width = (size_of::<usize>() * 2))
    }
}

impl RcHeap {
    pub fn new() -> Self {
        Self {
            slots: Vec::new(),
            trace: false,
        }
    }

    pub fn alloc(&mut self, val: MemCell) -> RcAddress {
        let heap_cell = HeapCell {
            ref_count: 1,
            val,
        };

        let empty_cell = self.slots.iter().enumerate()
            .find_map(|(i, slot)| match slot {
                None => Some(i),
                Some(_) => None,
            });

        let slot = match empty_cell {
            Some(empty) => {
                self.slots[empty] = Some(heap_cell);
                empty
            },
            None => {
                self.slots.push(Some(heap_cell));
                self.slots.len() - 1
            }
        };

        let addr = RcAddress(slot);
        if self.trace {
            eprintln!("heap: {} allocated with {:?}", addr, self.get(addr).unwrap());
        }
        addr
    }

    pub fn get(&self, addr: RcAddress) -> Option<&MemCell> {
        self.slots.get(addr.0)
            .and_then(|slot| slot.as_ref())
            .map(|slot| &slot.val)
    }

    pub fn get_mut(&mut self, addr: RcAddress) -> Option<&mut MemCell> {
        self.slots.get_mut(addr.0)
            .and_then(|slot| slot.as_mut())
            .map(|slot| &mut slot.val)
    }

    pub fn retain(&mut self, addr: RcAddress) {
        let slot: &mut HeapCell = self.slots.get_mut(addr.0)
            .and_then(|slot| slot.as_mut())
            .unwrap_or_else(|| panic!("attempting to retain unallocated slot {}", addr));

        slot.ref_count += 1;

        if self.trace {
            eprintln!("heap: {} retained ({} refs)", addr, slot.ref_count);
        }
    }

    pub fn get_rc(&self, addr: RcAddress) -> usize {
        self.slots.get(addr.0)
            .and_then(|slot| slot.as_ref())
            .map(|slot| slot.ref_count)
            .unwrap_or_else(|| panic!("attempting to get refcount of unallocated slot {}", addr))
    }

    pub fn release(&mut self, addr: RcAddress) {
        let slot: &mut HeapCell = self.slots.get_mut(addr.0)
            .and_then(|slot| slot.as_mut())
            .unwrap_or_else(|| panic!("attempting to release unallocated slot {}", addr));

        if slot.ref_count == 1 {
            if self.trace {
                eprintln!("heap: {} deallocated", addr);
            }
            self.slots[addr.0] = None;
        } else {
            slot.ref_count -= 1;
            if self.trace {
                eprintln!("heap: {} released ({} refs)", addr, slot.ref_count);
            }
        }
    }
}

impl Drop for RcHeap {
    fn drop(&mut self) {
        if self.trace {
            let leaked_addrs = self.slots.iter()
                .enumerate()
                .filter_map(|(addr, s)| match s {
                    Some(_) => Some(RcAddress(addr)),
                    None => None,
                });

            for addr in leaked_addrs {
                println!("heap: leaked cell {}", addr);
            }
        }
    }
}