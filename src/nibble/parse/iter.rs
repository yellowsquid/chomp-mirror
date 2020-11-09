use std::iter::Peekable;

pub use self::take_while::PeekableTakeWhile;

/// An iterator with a `peek()` that returns an optional reference to the next
/// element.
///
/// This trait is an extension of the `Peekable` type in the standard library.
pub trait Peek: Iterator {
    /// Returns a reference to the [`next`] value without advancing the iterator.
    ///
    /// Like [`next`], if there is a value, it is wrapped in [`Some`]. But if
    /// iteration is over, [`None`] is returned.
    fn peek(&mut self) -> Option<&Self::Item>;
}

impl<I: Peek> Peek for &mut I {
    fn peek(&mut self) -> Option<&Self::Item> {
        I::peek(self)
    }
}

impl<I> Peek for Box<&mut dyn Peek<Item = I>> {
    fn peek(&mut self) -> Option<&Self::Item> {
        (**self).peek()
    }
}

impl<I: Iterator> Peek for Peekable<I> {
    fn peek(&mut self) -> Option<&Self::Item> {
        Peekable::peek(self)
    }
}

pub trait PeekExt: Peek {
    /// Returns the [`next`] value and advance the iterator only if `pred` is true.
    fn next_if<P: FnOnce(&Self::Item) -> bool>(
        &mut self,
        pred: P,
    ) -> Option<Result<Self::Item, &mut Self>> {
        let i = self.peek()?;

        if pred(i) {
            Some(Ok(self.next().unwrap()))
        } else {
            Some(Err(self))
        }
    }

    fn next_if_then<
        P: FnOnce(&Self::Item) -> bool,
        F: FnOnce(Self::Item) -> T,
        G: FnOnce(&mut Self) -> T,
        T,
    >(
        &mut self,
        pred: P,
        matched: F,
        unmatched: G,
    ) -> Option<T> {
        let i = self.peek()?;

        if pred(i) {
            Some(matched(self.next().unwrap()))
        } else {
            Some(unmatched(self))
        }
    }

    /// Advances the iterator only if the [`next`] item equals `val`.
    fn next_if_eq<T>(&mut self, val: &T) -> Option<Result<(), &Self::Item>>
    where
        Self::Item: PartialEq<T>,
    {
        self.next_if(|i| PartialEq::eq(i, val))
            .map(|r| r.map(|_| ()).map_err(|i| i.peek().unwrap()))
    }

    fn consume_if_next<T>(&mut self, val: &T) -> bool
    where
        Self::Item: PartialEq<T>,
    {
        self.next_if_eq(val).map(|r| r.is_ok()).unwrap_or(false)
    }

    /// Advance the iterator until `pred` is false, or `peek` returns [`None`].
    fn advance_while<P: FnMut(&Self::Item) -> bool>(&mut self, mut pred: P) {
        while self.next_if(&mut pred).map(|r| r.is_ok()).unwrap_or(false) {
            // Condition already loops for us
        }
    }
}

impl<I: Peek> PeekExt for I {}

mod take_while {
    use super::Peek;

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct PeekableTakeWhile<I, F> {
        inner: I,
        predicate: Option<F>,
    }

    impl<I, F> PeekableTakeWhile<I, F>
    where
        I: Peek,
        F: FnMut(&<I as Iterator>::Item) -> bool,
    {
        pub fn new(iter: I, predicate: F) -> Self {
            Self {
                inner: iter,
                predicate: Some(predicate),
            }
        }
    }

    impl<I, F> Iterator for PeekableTakeWhile<I, F>
    where
        I: Peek,
        F: FnMut(&<I as Iterator>::Item) -> bool,
    {
        type Item = <I as Iterator>::Item;

        fn next(&mut self) -> Option<Self::Item> {
            // Can't inline because of lifetimes
            let inner = &mut self.inner;
            if self.predicate.as_mut().and_then(|p| inner.peek().map(p))? {
                self.inner.next()
            } else {
                self.predicate = None;
                None
            }
        }
    }

    impl<I, F> Peek for PeekableTakeWhile<I, F>
    where
        I: Peek,
        F: FnMut(&<I as Iterator>::Item) -> bool,
    {
        fn peek(&mut self) -> Option<&Self::Item> {
            let inner = &mut self.inner;
            self.predicate
                .as_mut()
                .and_then(move |p| inner.peek().filter(|i| p(i)))
        }
    }
}
