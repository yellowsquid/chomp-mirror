use std::fmt::Display;

use super::{
    error::{ParseError, TakeError},
    position::LineCol,
};

pub trait Parser {
    fn next(&mut self) -> Option<char>;

    fn peek(&mut self) -> Option<char>;

    fn pos(&self) -> LineCol;

    fn take<P: Parse>(&mut self) -> Result<P, TakeError> {
        P::take(self)
    }

    fn parse<P: Parse>(self) -> Result<P, ParseError>
    where
        Self: Sized,
    {
        P::parse(self)
    }

    fn consume_str(&mut self, s: &'static str) -> Result<(), TakeError> {
        let mut count = 0;

        for exp in s.chars() {
            if let Some(got) = self.peek() {
                if got == exp {
                    self.next();
                    count += 1
                } else {
                    let mut out = String::from(&s[..count]);
                    out.push(got);

                    return Err(TakeError::BadString(self.pos(), out, s));
                }
            } else {
                return Err(TakeError::EndOfStream(self.pos()));
            }
        }

        Ok(())
    }
}

pub struct IterWrapper<T: ?Sized> {
    pos: LineCol,
    next: Option<char>,
    iter: T,
}

impl<T: Iterator<Item = char>> IterWrapper<T> {
    pub fn new(iter: T) -> Self {
        Self {
            pos: LineCol::default(),
            next: None,
            iter,
        }
    }
}

impl<I: ?Sized + Iterator<Item = char>> Parser for IterWrapper<I> {
    fn next(&mut self) -> Option<char> {
        match self.next.take().or_else(|| self.iter.next()) {
            x @ Some('\n')
            | x @ Some('\x0B')
            | x @ Some('\x0C')
            | x @ Some('\u{85}')
            | x @ Some('\u{2028}')
            | x @ Some('\u{2029}') => {
                self.pos.line += 1;
                self.pos.col = 0;
                x
            }
            Some('\x0D') => {
                if self.peek() == Some('\n') {
                    self.pos.col += 1;
                } else {
                    self.pos.line += 1;
                    self.pos.col = 0;
                }
                Some('\x0D')
            }
            x => {
                self.pos.col += 1;
                x
            }
        }
    }

    fn peek(&mut self) -> Option<char> {
        if self.next.is_none() {
            self.next = self.iter.next();
        }

        self.next.as_ref().copied()
    }

    fn pos(&self) -> LineCol {
        self.pos
    }
}

pub trait Parse: Display + Sized {
    fn take<P: Parser + ?Sized>(input: &mut P) -> Result<Self, TakeError>;

    fn parse<P: Parser>(mut input: P) -> Result<Self, ParseError> {
        let res = Self::take(&mut input)?;

        if input.peek().is_some() {
            Err(ParseError::InputContinues(input.pos()))
        } else {
            Ok(res)
        }
    }

    fn take_str(input: &str) -> Result<Self, TakeError> {
        Self::take(&mut IterWrapper::new(input.chars()))
    }

    fn parse_str(input: &str) -> Result<Self, ParseError> {
        Self::parse(IterWrapper::new(input.chars()))
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub struct Epsilon;

impl Display for Epsilon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

impl Parse for Epsilon {
    fn take<P: Parser + ?Sized>(_: &mut P) -> Result<Self, TakeError> {
        Ok(Epsilon)
    }
}

impl<T: Parse + Sized> Parse for Box<T> {
    fn take<P: Parser + ?Sized>(input: &mut P) -> Result<Self, TakeError> {
        Ok(Box::new(input.take()?))
    }

    fn parse<P: Parser>(input: P) -> Result<Self, ParseError> {
        Ok(Box::new(input.parse()?))
    }
}
