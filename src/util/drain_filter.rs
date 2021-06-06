/// Creates an iterator which uses a closure to determine if an element should
/// be removed starting from the end of the vector.
///
/// If the closure returns true, then the element is removed and yielded. If the
/// closure returns false, the element will remain in the vector and will not be
/// yielded by the iterator.
pub fn rev_drain_filter<T, F>(vec: &mut Vec<T>, predicate: F) -> RevDrainFilter<T, F>
where
    F: FnMut(&mut T) -> bool,
{
    let current_offset = vec.len();
    RevDrainFilter {
        vec,
        predicate,
        current_offset,
    }
}

pub struct RevDrainFilter<'v, T, F> {
    vec: &'v mut Vec<T>,
    predicate: F,
    // The offset value will range from 0..=vec.len(). 0 will designate that the iterator is empty.
    // The iterator will always access the element in the index equal to `offset - 1`.
    current_offset: usize,
}

impl<'v, T, F> Iterator for RevDrainFilter<'v, T, F>
where
    F: FnMut(&mut T) -> bool,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while 0 != self.current_offset {
            if (self.predicate)(&mut self.vec[self.current_offset - 1]) {
                let val = Some(self.vec.remove(self.current_offset - 1));
                self.current_offset -= 1;
                return val;
            } else {
                self.current_offset -= 1;
            }
        }

        None
    }
}
