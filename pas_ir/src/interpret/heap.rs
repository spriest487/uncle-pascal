use crate::interpret::MemCell;
use std::{
    collections::HashMap,
    fmt,
    mem::size_of,
    ops::{Add, Index, IndexMut},
};

#[derive(Debug, Clone)]
pub struct Heap {
    slots: Vec<Option<MemCell>>,
    alloc_lens: HashMap<HeapAddress, usize>,

    pub trace: bool,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct HeapAddress(pub usize);

impl fmt::Display for HeapAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:0width$x}", self.0, width = (size_of::<usize>() * 2))
    }
}

impl Add<usize> for HeapAddress {
    type Output = Self;

    fn add(self, rhs: usize) -> Self {
        HeapAddress(self.0 + rhs)
    }
}

impl HeapAddress {
    pub fn range_to(self, other: HeapAddress) -> impl Iterator<Item = Self> {
        (self.0..other.0).map(HeapAddress)
    }
}

impl Heap {
    pub fn new() -> Self {
        Self {
            slots: Vec::new(),
            alloc_lens: HashMap::new(),
            trace: false,
        }
    }

    pub fn alloc(&mut self, vals: Vec<MemCell>) -> HeapAddress {
        let count = vals.len();

        if count == 0 {
            panic!("allocation of length 0");
        }

        let free_addr = self
            .slots
            .windows(count)
            .enumerate()
            .find_map(|(addr, window)| {
                if window.iter().all(|cell| cell.is_none()) {
                    Some(addr)
                } else {
                    None
                }
            })
            .unwrap_or_else(|| {
                let addr = self.slots.len();
                self.slots.resize(self.slots.len() + count, None);
                addr
            });

        let addr = HeapAddress(free_addr);
        assert!(!self.alloc_lens.contains_key(&HeapAddress(free_addr)));
        self.alloc_lens.insert(HeapAddress(free_addr), count);

        let addr_range = free_addr..free_addr + count;
        let addr_vals = addr_range.zip(vals.into_iter());

        for (val_addr, val) in addr_vals {
            self.slots[val_addr] = Some(val);
        }

        if self.trace {
            eprintln!("heap: {} allocated with length {}", addr, count);
        }
        addr
    }

    pub fn free(&mut self, addr: HeapAddress) {
        let free_len = self
            .alloc_lens
            .remove(&addr)
            .expect("must have an alloc len for freed cell");

        for addr in addr.range_to(addr + free_len) {
            self.slots[addr.0] = None;
        }

        if self.trace {
            eprintln!("heap: {} freed ({} cells)", addr, free_len);
        }
    }

    pub fn get(&self, addr: HeapAddress) -> Option<&MemCell> {
        self.slots.get(addr.0).and_then(|slot| slot.as_ref())
    }

    pub fn get_mut(&mut self, addr: HeapAddress) -> Option<&mut MemCell> {
        self.slots.get_mut(addr.0).and_then(|slot| slot.as_mut())
    }

    fn expect_empty(&mut self) {
        if self.trace {
            let leaked_addrs = self
                .slots
                .iter()
                .enumerate()
                .filter_map(|(addr, s)| match s {
                    Some(_) => Some(HeapAddress(addr)),
                    None => None,
                });

            for addr in leaked_addrs {
                let val = self.get(addr).unwrap();
                println!("heap: leaked cell {}: {:?}", addr, val);
            }
        }
    }

    pub fn finalize(mut self) {
        self.expect_empty()
    }
}

impl Index<HeapAddress> for Heap {
    type Output = MemCell;

    fn index(&self, addr: HeapAddress) -> &MemCell {
        self.get(addr)
            .unwrap_or_else(|| panic!("trying to access unallocated heap location {}", addr))
    }
}

impl IndexMut<HeapAddress> for Heap {
    fn index_mut(&mut self, addr: HeapAddress) -> &mut MemCell {
        self.get_mut(addr).unwrap_or_else(|| {
            panic!(
                "trying to mutably access unallocated heap location {}",
                addr
            )
        })
    }
}
