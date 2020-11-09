pub mod iter;

use self::iter::Peek;
use self::iter::PeekExt;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ParseError<E> {
    EndOfFile,
    InvalidCharacter(char),
    Other(E),
}

impl<U> ParseError<U> {
    pub fn map<T: Into<U>>(other: ParseError<T>) -> Self {
        match other {
            ParseError::EndOfFile => Self::EndOfFile,
            ParseError::InvalidCharacter(c) => Self::InvalidCharacter(c),
            ParseError::Other(t) => Self::Other(t.into())
        }
    }
}

/// Parse a value from an iterator.
pub trait Parse: Sized {
    /// The associated error that can be returned from parsing.
    type Err;
    /// Parses `iter` to return a value of this type.
    ///
    /// This method may not reach the end of `iter`. If so, it must not consume any
    /// characters in the stream that do not form part of the value.
    ///
    /// If parsing succeeds, return the value inside [`Ok`]. Otherwise, when the
    /// iterator is ill-formatted, return an error specific to the inside [`Err`].
    /// The error type is specific to the implementation of the trait.
    fn parse<I: PeekExt<Item = char>>(iter: I) -> Result<Self, ParseError<Self::Err>>;
}

/// Consumes the next item in `iter` if it equals `c`. Otherwise, returns an
/// appropriate error without advancing the iterator.
///
/// # Examples
/// Basic usage
/// ```
/// use chomp::nibble::parse::{requires, Parse, ParseError};
/// use std::convert::Infallible;
///
/// let s = "hello".to_owned();
/// let mut iter = s.chars().peekable();
/// assert_eq!(requires::<_, Infallible>(&mut iter, 'h'), Ok(()));
/// assert_eq!(requires::<_, Infallible>(&mut iter, 'e'), Ok(()));
/// assert_eq!(requires::<_, Infallible>(&mut iter, 'z'), Err(ParseError::InvalidCharacter('l')));
/// assert_eq!(iter.next(), Some('l'));
/// assert_eq!(iter.next(), Some('l'));
/// assert_eq!(iter.next(), Some('o'));
/// assert_eq!(requires::<_, Infallible>(iter, '!'), Err(ParseError::EndOfFile));
/// ```
pub fn requires<I: Peek<Item = char>, E>(mut iter: I, c: char) -> Result<(), ParseError<E>> {
    iter.next_if_eq(&c)
        .ok_or(ParseError::EndOfFile)?
        .map_err(|&c| ParseError::InvalidCharacter(c))
}
