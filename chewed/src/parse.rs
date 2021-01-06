use super::error::{ParseError, TakeError};

pub trait Parser: Iterator<Item = char> {
    fn peek(&mut self) -> Option<char>;

    fn take<P: Parse>(&mut self) -> Result<P, TakeError> {
        P::take(self)
    }

    fn parse<P: Parse>(self) -> Result<P, ParseError>
    where
        Self: Sized,
    {
        P::parse(self)
    }

    fn take_str(&mut self, s: &'static str) -> Result<(), TakeError> {
        let mut count = 0;

        for exp in s.chars() {
            if let Some(got) = self.peek() {
                if got == exp {
                    self.next();
                    count += 1
                } else {
                    let mut out = String::from(&s[..count]);
                    out.push(got);

                    return Err(TakeError::BadString(out, s));
                }
            } else {
                return Err(TakeError::EndOfStream);
            }
        }

        Ok(())
    }
}

pub trait Parse: Sized {
    fn take<P: Parser + ?Sized>(input: &mut P) -> Result<Self, TakeError>;

    fn parse<P: Parser>(mut input: P) -> Result<Self, ParseError> {
        let res = Self::take(&mut input)?;

        if input.peek().is_some() {
            Err(ParseError::InputContinues)
        } else {
            Ok(res)
        }
    }
}

impl Parse for () {
    fn take<P: Parser + ?Sized>(_: &mut P) -> Result<Self, TakeError> {
        Ok(())
    }
}

impl<A: Parse, B: Parse> Parse for (A, B) {
    fn take<P: Parser + ?Sized>(input: &mut P) -> Result<Self, TakeError> {
        let a = input.take()?;
        let b = input.take()?;
        Ok((a, b))
    }

    fn parse<P: Parser>(mut input: P) -> Result<Self, ParseError> {
        let a = A::take(&mut input)?;
        let b = B::parse(input)?;
        Ok((a, b))
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
