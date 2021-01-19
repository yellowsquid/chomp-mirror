use std::{
    error::Error,
    fmt::{self, Display},
};

use proc_macro2::Span;

use crate::chomp::Name;

use super::{Call, Parameter};

#[derive(Debug)]
pub enum SubstituteError {
    FreeParameter {
        param: Parameter,
        span: Option<Span>,
        name: Option<Name>,
    },
    WrongArgCount {
        call: Call,
        expected: usize,
        span: Option<Span>,
    },
}

impl From<SubstituteError> for syn::Error {
    fn from(e: SubstituteError) -> Self {
        let msg = e.to_string();
        let span = match e {
            SubstituteError::FreeParameter { span, .. } => span,
            SubstituteError::WrongArgCount { span, .. } => span,
        };

        Self::new(span.unwrap_or_else(Span::call_site), msg)
    }
}

impl Display for SubstituteError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FreeParameter { param, span, name } => {
                if let Some(name) = name {
                    write!(f, "unbound parameter: `{}`", name)
                } else {
                    write!(f, "unbound parameter: '{}", param.index)
                }
            }
            Self::WrongArgCount {
                call,
                expected,
                span,
            } => {
                if call.args.len() == 1 {
                    write!(
                        f,
                        "this function takes {} arguments but 1 was supplied",
                        expected
                    )
                } else {
                    write!(
                        f,
                        "this function takes {} arguments but {} were supplied",
                        expected,
                        call.args.len()
                    )
                }
            }
        }
    }
}

impl Error for SubstituteError {}
