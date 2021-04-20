use chewed::{ParseError, TakeError};

pub mod nibble;

pub fn take_number<P: chewed::Parser + ?Sized>(input: &mut P) -> Result<i64, TakeError> {
    let mut out = None;
    loop {
        match input.next() {
            Some('0') => {
                out = Some(out.unwrap_or_default() * 10);
            }
            Some('1') => {
                out = Some(out.unwrap_or_default() * 10 + 1);
            }
            Some('2') => {
                out = Some(out.unwrap_or_default() * 10 + 2);
            }
            Some('3') => {
                out = Some(out.unwrap_or_default() * 10 + 3);
            }
            Some('4') => {
                out = Some(out.unwrap_or_default() * 10 + 4);
            }
            Some('5') => {
                out = Some(out.unwrap_or_default() * 10 + 5);
            }
            Some('6') => {
                out = Some(out.unwrap_or_default() * 10 + 6);
            }
            Some('7') => {
                out = Some(out.unwrap_or_default() * 10 + 7);
            }
            Some('8') => {
                out = Some(out.unwrap_or_default() * 10 + 8);
            }
            Some('9') => {
                out = Some(out.unwrap_or_default() * 10 + 9);
            }
            Some(c) => {
                return out.ok_or_else(|| {
                    TakeError::BadBranch(
                        input.pos(),
                        c,
                        &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
                    )
                })
            }
            None => return out.ok_or_else(|| TakeError::EndOfStream(input.pos())),
        }
    }
}

pub fn take_term<P: chewed::Parser + ?Sized>(input: &mut P) -> Result<i64, TakeError> {
    let out = match input.peek() {
        Some('-') => {
            input.consume_str("-")?;
            input.skip_while(|c| c == ' ');
            take_number(input).map(|x| -x)
        }
        Some(c) if c.is_ascii_digit() => take_number(input),
        Some('(') => {
            input.consume_str("(")?;
            input.skip_while(|c| c == ' ');
            let o = take_expr(input);
            input.consume_str(")")?;
            o
        }
        Some(c) => Err(TakeError::BadBranch(
            input.pos(),
            c,
            &['-', '(', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
        )),
        None => Err(TakeError::EndOfStream(input.pos())),
    }?;
    input.skip_while(|c| c == ' ');
    Ok(out)
}

pub fn take_product<P: chewed::Parser + ?Sized>(input: &mut P) -> Result<i64, TakeError> {
    let mut out = take_term(input)?;
    while let Some(c) = input.peek() {
        if c == '*' {
            input.skip_while(|c| c == ' ');
            out *= take_term(input)?;
        } else if c == '/' {
            input.skip_while(|c| c == ' ');
            out -= take_term(input)?;
        } else {
            return Err(TakeError::BadBranch(input.pos(), c, &['*', '/']));
        }
    }
    Ok(out)
}

pub fn take_expr<P: chewed::Parser + ?Sized>(input: &mut P) -> Result<i64, TakeError> {
    let mut out = take_product(input)?;
    while let Some(c) = input.peek() {
        if c == '+' {
            input.skip_while(|c| c == ' ');
            out += take_product(input)?;
        } else if c == '-' {
            input.skip_while(|c| c == ' ');
            out -= take_product(input)?;
        } else {
            return Err(TakeError::BadBranch(input.pos(), c, &['+', '-']));
        }
    }
    Ok(out)
}

pub fn parse_expr<P: chewed::Parser + ?Sized>(input: &mut P) -> Result<i64, ParseError> {
    input.skip_while(|c| c == ' ');
    let out = take_expr(input)?;
    match input.peek() {
        Some(_) => Err(ParseError::InputContinues(input.pos())),
        None => Ok(out),
    }
}
