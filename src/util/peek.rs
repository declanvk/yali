macro_rules! impl_peekable_n {
    ($name:ident, $len:expr) => {
        #[derive(Debug, Clone)]
        pub struct $name<I: Iterator> {
            iter: I,
            peeked: [Option<I::Item>; $len],
            filled: usize,
        }

        impl<I: Iterator> $name<I> {
            pub fn new(iter: I) -> Self {
                Self {
                    iter,
                    peeked: Default::default(),
                    filled: 0,
                }
            }

            pub fn peek(&mut self, lookahead: usize) -> Option<&I::Item> {
                // Cases:
                //  1. No previous `peek` call for `lookahead`, need to get next value
                //  2. Previous `peek` call for `lookahead`, return existing value

                // If the `lookahead` is greater then the backing array
                if lookahead >= self.peeked.len() {
                    panic!(
                        "Attempted to peek further than allowed! Max lookahead: [{}].",
                        self.peeked.len()
                    );
                }

                if lookahead >= self.filled {
                    for idx in self.filled..=lookahead {
                        self.peeked[idx] = self.iter.next();
                    }

                    self.filled += lookahead - self.filled + 1;
                }

                self.peeked[lookahead].as_ref()
            }

            pub fn next_if(&mut self, func: impl FnOnce(&I::Item) -> bool) -> Option<I::Item> {
                match self.next() {
                    Some(matched) if func(&matched) => Some(matched),
                    other => {
                        // There is at least one empty slot in the peeked array
                        assert!(self.filled < self.peeked.len() - 1);

                        self.peeked.rotate_right(1);
                        self.filled += 1;
                        self.peeked[0] = other;

                        None
                    },
                }
            }
        }

        impl<I: Iterator> Iterator for $name<I> {
            type Item = I::Item;

            fn next(&mut self) -> Option<Self::Item> {
                if self.filled == 0 {
                    self.iter.next()
                } else {
                    let res = self.peeked[0].take();
                    self.filled -= 1;
                    self.peeked.rotate_left(1);

                    res
                }
            }
        }
    };
}

impl_peekable_n!(Peekable2, 2);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_peek2_operations() {
        let mut it = Peekable2::new(0..20);

        assert_eq!(it.peek(0).copied(), Some(0));
        assert_eq!(it.peek(1).copied(), Some(1));

        assert_eq!(it.next(), Some(0));
        assert_eq!(it.next(), Some(1));

        assert_eq!(it.peek(0).copied(), Some(2));
        assert_eq!(it.next(), Some(2));

        assert_eq!(it.next(), Some(3));
        assert_eq!(it.next(), Some(4));
    }
}
