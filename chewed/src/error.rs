use std::{error::Error, fmt};

use super::position::LineCol;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TakeError {
    BadBranch(LineCol, char, &'static [char]),
    BadString(LineCol, String, &'static str),
    EndOfStream(LineCol),
}

impl fmt::Display for TakeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BadBranch(pos, got, expected) => {
                write!(f, "{}: Unexpected character {:?}. ", pos, got)?;

                if expected.is_empty() {
                    write!(f, "Expected end of input.")
                } else if expected.len() == 1 {
                    write!(f, "Expected character {:?}.", expected[0])
                } else {
                    let mut iter = expected.iter();
                    write!(f, "Expected one of {:?}", iter.next().unwrap())?;

                    for c in iter {
                        write!(f, ", {:?}", c)?;
                    }

                    Ok(())
                }
            }
            Self::BadString(pos, got, expected) => write!(
                f,
                "{}: Unexpected string {:?}. Expected {:?}",
                pos, got, expected
            ),
            Self::EndOfStream(pos) => write!(f, "{}: Unexpected end of input", pos),
        }
    }
}

impl Error for TakeError {}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParseError {
    TakeError(TakeError),
    InputContinues(LineCol),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TakeError(e) => e.fmt(f),
            Self::InputContinues(pos) => write!(f, "{}: Expected end of input", pos),
        }
    }
}

impl Error for ParseError {}

impl From<TakeError> for ParseError {
    fn from(e: TakeError) -> Self {
        Self::TakeError(e)
    }
}
