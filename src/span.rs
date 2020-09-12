//! Common structures for reporting and selecting information from source code

use std::ops::{Bound, Range, RangeBounds};

/// A representation of a continuous block of source code
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    /// The line number of the `Span`
    line: u32,
    /// The range in byte-indices that the `Span` occupies
    range: Range<usize>,
}

impl Span {
    /// Create an empty `Span` on an illogical line
    pub fn dummy() -> Self {
        Span {
            range: 0..0,
            line: u32::MAX,
        }
    }

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
        }
    }

    /// Create a `Span` which envelops all the provided sub-`Span`s
    pub fn envelop<'s>(subspans: impl IntoIterator<Item = &'s Span>) -> Self {
        let mut start: usize = usize::MAX;
        let mut end: usize = 0;
        let mut line = u32::MAX;

        for span in subspans {
            if span.range.start < start {
                start = span.range.start;
            }

            if span.range.end > end {
                end = span.range.end;
            }

            if span.line < line {
                line = span.line;
            }
        }

        Span {
            range: start..end,
            line,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn envelop_small_spans() {
        let s1 = Span::envelop(&[Span::new(0, 2..5), Span::dummy(), Span::new(0, 0..2)][..]);
        assert_eq!(s1.line, 0);
        assert_eq!(s1.range, 0..5);

        let s2 = Span::envelop(&[Span::new(22, 4567..8900), Span::new(20, 2345..6789)][..]);
        assert_eq!(s2.line, 20);
        assert_eq!(s2.range, 2345..8900);
    }
}
