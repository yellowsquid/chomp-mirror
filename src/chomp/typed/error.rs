use std::{
    error::Error,
    fmt::{self, Display},
};

use proc_macro2::Span;

use crate::chomp::{ast::Variable, Name};

use super::{context::GetVariableError, TypedExpression};

/// A type error when using a fix point variable.
#[derive(Debug)]
pub struct VariableError {
    pub inner: GetVariableError,
    pub var: Variable,
    pub span: Option<Span>,
    pub name: Option<Name>,
}

impl PartialEq for VariableError {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

impl Eq for VariableError {}

impl From<VariableError> for syn::Error {
    fn from(other: VariableError) -> Self {
        todo!()
    }
}

impl Display for VariableError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Error for VariableError {}

/// A type error when concatenating two terms.
#[derive(Debug)]
pub enum CatError {
    /// The first term was unexpectedly nullable.
    FirstNullable(TypedExpression, Option<Span>),
    /// The flast set of the first term intersects the first set of the second.
    FirstFlastOverlap(Vec<TypedExpression>, Option<Span>, TypedExpression),
}

impl PartialEq for CatError {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

impl Eq for CatError {}

impl From<CatError> for syn::Error {
    fn from(other: CatError) -> Self {
        todo!()
    }
}

impl Display for CatError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Error for CatError {}

/// A type error when alternating two terms.
#[derive(Debug)]
pub enum AltError {
    /// Both terms are nullable.
    BothNullable(Vec<TypedExpression>, Option<Span>, TypedExpression),
    /// The first sets of the two terms intersect.
    FirstOverlap(Vec<TypedExpression>, Option<Span>, TypedExpression),
}

impl PartialEq for AltError {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

impl Eq for AltError {}

impl From<AltError> for syn::Error {
    fn from(other: AltError) -> Self {
        todo!()
    }
}

impl Display for AltError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl Error for AltError {}

#[derive(Debug, Eq, PartialEq)]
pub enum TypeError {
    Cat(CatError),
    Alt(AltError),
    Variable(VariableError),
}

impl From<CatError> for TypeError {
    fn from(other: CatError) -> Self {
        Self::Cat(other)
    }
}

impl From<AltError> for TypeError {
    fn from(other: AltError) -> Self {
        Self::Alt(other)
    }
}

impl From<VariableError> for TypeError {
    fn from(other: VariableError) -> Self {
        Self::Variable(other)
    }
}

impl From<TypeError> for syn::Error {
    fn from(other: TypeError) -> Self {
        match other {
            TypeError::Cat(e) => e.into(),
            TypeError::Alt(e) => e.into(),
            TypeError::Variable(e) => e.into(),
        }
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Cat(e) => e.fmt(f),
            Self::Alt(e) => e.fmt(f),
            Self::Variable(e) => e.fmt(f),
        }
    }
}

impl Error for TypeError {}
