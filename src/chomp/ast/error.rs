use std::{error::Error, fmt::{self, Display}};

use proc_macro2::Span;

use crate::chomp::Name;

use super::{Call, Parameter};

#[derive(Debug)]
pub enum SubstituteError {
    FreeParameter { param: Parameter, span: Option<Span>, name: Option<Name> },
    WrongArgCount { call: Call, expected: usize, span: Option<Span> },
}

impl From<SubstituteError> for syn::Error {
    fn from(e: SubstituteError) -> Self {
        match e {
            SubstituteError::FreeParameter { span, .. } => {
                Self::new(span.unwrap_or_else(Span::call_site), "unbound parameter")
            }
            SubstituteError::WrongArgCount { call, expected, span } => {
                let msg = if call.args.len() == 1 {
                    format!("this function takes {} arguments but 1 was supplied", expected)
                } else {
                    format!("this function takes {} arguments but {} were supplied", expected, call.args.len())
                };

                Self::new(span.unwrap_or_else(Span::call_site), msg)
            }
        }
    }
}

impl Display for SubstituteError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FreeParameter { param, span, name } => {
                let start = span.unwrap_or_else(Span::call_site).start();
                if let Some(name) = name {
                    write!(f, "{}:{}: unbound parameter `{}`", start.line, start.column, name)
                } else {
                    write!(f, "{}:{}: unbound parameter '{}", start.line, start.column, param.index)
                }
            }
            Self::WrongArgCount { call, expected, span } => {
                let start = span.unwrap_or_else(Span::call_site).start();
                if call.args.len() == 1 {
                    write!(f, "{}:{}: this function takes {} arguments but 1 was supplied", start.line, start.column, expected)
                } else {
                    write!(f, "{}:{}: this function takes {} arguments but {} were supplied", start.line, start.column, expected, call.args.len())
                }
            }
        }
    }
}

impl Error for SubstituteError {}
