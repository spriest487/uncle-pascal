use {
    std::{
        fmt,
        mem::size_of,
        collections::HashMap,
        ops::{
            Add,
        }
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
    alloc_lens: HashMap<RcAddress, usize>,

    pub trace: bool,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct RcAddress(pub usize);

impl fmt::Display for RcAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:0width$x}", self.0, width = (size_of::<usize>() * 2))
    }
}

impl Add<usize> for RcAddress {
    type Output = Self;

    fn add(self, rhs: usize) -> Self {
        RcAddress(self.0 + rhs)
    }
}

impl RcAddress {
    pub fn range_to(self, other: RcAddress) -> impl Iterator<Item=Self> {
        (self.0..other.0).map(RcAddress)
    }
}

impl RcHeap {
    pub fn new() -> Self {
        Self {
            slots: Vec::new(),
            alloc_lens: HashMap::new(),
            trace: false,
        }
    }

    pub fn alloc(&mut self, vals: Vec<MemCell>) -> RcAddress {
        let count = vals.len();

        if count == 0 {
            panic!("allocation of length 0");
        }

        let free_addr = self.slots.windows(count)
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

        let addr = RcAddress(free_addr);
        assert!(!self.alloc_lens.contains_key(&RcAddress(free_addr)));
        self.alloc_lens.insert(RcAddress(free_addr), count);

        let addr_range = free_addr..free_addr + count;
        let addr_vals = addr_range.zip(vals.into_iter());

        for (val_addr, val) in addr_vals {
            self.slots[val_addr] = Some(HeapCell {
                ref_count: 1,
                val,
            });
        }

        if self.trace {
            eprintln!("heap: {} allocated with length {}", addr, count);
        }
        addr
    }

    pub fn free(&mut self, addr: RcAddress) {
        let free_len = self.alloc_lens.remove(&addr)
            .expect("must have an alloc len for freed cell");

        for addr in addr.range_to(addr + free_len) {
            self.slots[addr.0] = None;
        }

        if self.trace {
            eprintln!("heap: {} freed ({} cells)", addr, free_len);
        }
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
            .unwrap_or_else(|| {
                panic!("attempting to get refcount of unallocated slot {}", addr);
            })
    }

    pub fn release(&mut self, addr: RcAddress) {
        let slot: &mut HeapCell = self.slots.get_mut(addr.0)
            .and_then(|slot| slot.as_mut())
            .unwrap_or_else(|| panic!("attempting to release unallocated slot {}", addr));

        if slot.ref_count == 1 {
            self.free(addr);
        } else if slot.ref_count > 0 {
            slot.ref_count -= 1;
            if self.trace {
                eprintln!("heap: {} released ({} refs)", addr, slot.ref_count);
            }
        } else {
            panic!("heap: can't release {}, this slot is part of another allocation", addr);
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
                let val = self.get(addr).unwrap();
                let rc = self.get_rc(addr);
                println!("heap: leaked cell {} with {} refs: {:?}", addr, rc, val);
            }
        }
    }
}