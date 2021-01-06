use std::{error::Error, fmt};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TakeError {
    BadBranch(char, &'static [char]),
    BadString(String, &'static str),
    EndOfStream,
}

impl fmt::Display for TakeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BadBranch(got, expected) => {
                write!(f, "Unexpected character {:?}.", got)?;

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
            Self::BadString(got, expected) => write!(f, "Unexpected string {:?}. Expected {:?}", got, expected),
            Self::EndOfStream => write!(f, "Unexpected end of input"),
        }
    }
}

impl Error for TakeError {}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParseError {
    TakeError(TakeError),
    InputContinues,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TakeError(e) => e.fmt(f),
            Self::InputContinues => write!(f, "Expected end of input"),
        }
    }
}

impl From<TakeError> for ParseError {
    fn from(e: TakeError) -> Self {
        Self::TakeError(e)
    }
}
