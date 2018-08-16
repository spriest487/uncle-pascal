pub struct WrapIter<TItem> {
    wrapped: Box<Iterator<Item=TItem>>
}

impl<TItem> WrapIter<TItem> {
    pub fn new<TIter>(iter: TIter) -> Self
        where TIter: IntoIterator<Item=TItem> + 'static
    {
        Self { wrapped: Box::from(iter.into_iter()) }
    }
}

impl<TItem> Iterator for WrapIter<TItem> {
    type Item = TItem;

    fn next(&mut self) -> Option<Self::Item> {
        self.wrapped.next()
    }
}

#[cfg(test)]
mod test {

}