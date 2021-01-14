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
        todo!()
    }
}

impl Display for SubstituteError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Error for SubstituteError {}
