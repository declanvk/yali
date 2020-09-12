//! Common structures for reporting and selecting information from source code

use std::{
    marker::PhantomData,
    ops::{Bound, Range, RangeBounds},
};

/// A representation of a continuous block of source code
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub struct Span {
    /// The line number of the `Span`
    line: u32,
    /// The range in byte-indices that the `Span` occupies
    range: Range<usize>,

    // used to make the struct unconstructable
    _priv: PhantomData<()>,
}

impl Span {
    /// Create a new `Span` for the given line and source range
    pub fn new(line: u32, bounded: impl RangeBounds<usize>) -> Self {
        let start: usize = match bounded.start_bound() {
            Bound::Included(x) => *x,
            Bound::Excluded(x) => x.saturating_add(1),
            Bound::Unbounded => 0,
        };

        let end: usize = match bounded.end_bound() {
            Bound::Included(x) => x.saturating_sub(1),
            Bound::Excluded(x) => *x,
            Bound::Unbounded => panic!("Unable to create a Span with unbounded upper limit!"),
        };

        Span {
            line,
            range: start..end,
            _priv: PhantomData,
        }
    }

    /// Return the line of this `Span`
    pub fn line(&self) -> u32 {
        self.line
    }

    /// Return the range covered by this `Span`
    pub fn range(&self) -> &Range<usize> {
        &self.range
    }
}
