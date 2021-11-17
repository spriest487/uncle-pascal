use std::{
    fmt,
    mem::size_of,
    ops::Add,
};
use crate::ValueCell;

pub mod value_heap;

pub trait Heap {
    fn alloc(&mut self, vals: Vec<ValueCell>) -> HeapAddress;
    fn free(&mut self, addr: HeapAddress);

    fn load(&self, addr: HeapAddress) -> Result<ValueCell, HeapAddress>;
    fn store(&mut self, addr: HeapAddress, val: ValueCell) -> Result<(), HeapAddress>;

    fn finalize(self);
}

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct HeapAddress(pub usize);

impl fmt::Display for HeapAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:0width$x}", self.0, width = (size_of::<usize>() * 2))
    }
}

impl fmt::Debug for HeapAddress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "HeapAddress({})", self)
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