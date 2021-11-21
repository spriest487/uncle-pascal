use std::borrow::Cow;
use std::collections::HashMap;
use crate::{HeapAddress, ValueCell};

#[derive(Debug, Clone)]
pub struct ValueHeap {
    slots: Vec<Option<ValueCell>>,
    alloc_lens: HashMap<HeapAddress, usize>,

    pub trace: bool,
}

impl ValueHeap {
    pub fn new() -> Self {
        Self {
            slots: Vec::new(),
            alloc_lens: HashMap::new(),
            trace: false,
        }
    }

    pub fn alloc(&mut self, vals: Vec<ValueCell>) -> HeapAddress {
        let count = vals.len();

        if count == 0 {
            return HeapAddress(0);
        }

        let free_addr = self
            .slots
            .windows(count)
            .enumerate()
            .find_map(|(addr, window)| {
                if window.iter().all(Option::is_none) {
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

    pub fn load(&self, addr: HeapAddress) -> Result<Cow<ValueCell>, HeapAddress> {
        match self.slots.get(addr.0) {
            Some(Some(slot)) => Ok(Cow::Borrowed(slot)),
            _ => Err(addr)
        }
    }

    pub fn store(&mut self, addr: HeapAddress, val: ValueCell) -> Result<(), HeapAddress> {
        match self.slots.get_mut(addr.0) {
            Some(Some(slot)) => {
                *slot = val;
                Ok(())
            }
            _ => Err(addr)
        }
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

            let mut leak_count = 0;

            for addr in leaked_addrs {
                let val = self.load(addr).unwrap();
                println!("heap: leaked cell {}: {:?}", addr, val);

                leak_count += 1;
            }

            if leak_count == 0 {
                println!("heap: no leaks detected");
            }
        }
    }

    pub fn finalize(mut self) {
        self.expect_empty()
    }
}
