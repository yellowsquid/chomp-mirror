use std::{
    error::Error,
    fmt::{self, Display},
};

use proc_macro2::Span;

use crate::chomp::{ast::Variable, Name};

use super::{context::GetParameterError, TypedExpression};

/// A type error when using a fix point variable.
#[derive(Debug)]
pub struct VariableError {
    pub inner: GetParameterError,
    pub var: Variable,
    pub span: Option<Span>,
    pub name: Option<Name>,
}

impl From<VariableError> for syn::Error {
    fn from(other: VariableError) -> Self {
        let msg = other.to_string();
        let span = other.span;
        Self::new(span.unwrap_or_else(Span::call_site), msg)
    }
}

impl Display for VariableError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            GetParameterError::FreeParameter => write!(f, "unbound variable: "),
            GetParameterError::GuardedParameter => write!(f, "usage of guarded variable: "),
        }?;

        if let Some(name) = &self.name {
            write!(f, "`{}`", name)
        } else {
            write!(f, "'{}", self.var.index)
        }
    }
}

impl Error for VariableError {}

#[derive(Debug)]
pub enum CatError {
    FirstNullable {
        expr: TypedExpression,
        punct: Option<Span>,
    },
    FirstFlastOverlap {
        first: Vec<TypedExpression>,
        punct: Option<Span>,
        next: TypedExpression,
    },
}

impl From<CatError> for syn::Error {
    fn from(other: CatError) -> Self {
        let msg = other.to_string();
        let span = match other {
            CatError::FirstNullable { punct, .. } | CatError::FirstFlastOverlap { punct, .. } => {
                punct
            }
        };
        Self::new(span.unwrap_or_else(Span::call_site), msg)
    }
}

impl Display for CatError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FirstNullable { .. } => write!(f, "first part of concatenation is nullable"),
            Self::FirstFlastOverlap { .. } => {
                write!(f, "first set overlaps with preceding flast set")
            }
        }
    }
}

impl Error for CatError {}

#[derive(Debug)]
pub enum AltError {
    BothNullable {
        left: Vec<TypedExpression>,
        punct: Option<Span>,
        right: TypedExpression,
    },
    FirstOverlap {
        left: Vec<TypedExpression>,
        punct: Option<Span>,
        right: TypedExpression,
    },
}

impl From<AltError> for syn::Error {
    fn from(other: AltError) -> Self {
        let msg = other.to_string();
        let span = match other {
            AltError::BothNullable { punct, .. } | AltError::FirstOverlap { punct, .. } => punct,
        };
        Self::new(span.unwrap_or_else(Span::call_site), msg)
    }
}

impl Display for AltError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BothNullable { .. } => write!(f, "both branches are nullable"),
            Self::FirstOverlap { .. } => write!(f, "first sets of both branches overlap"),
        }
    }
}

impl Error for AltError {}

#[derive(Debug)]
pub struct NeedGroundError {
    pub span: Option<Span>,
}

impl From<NeedGroundError> for syn::Error {
    fn from(other: NeedGroundError) -> Self {
        let msg = other.to_string();
        let span = other.span;
        Self::new(span.unwrap_or_else(Span::call_site), msg)
    }
}

impl Display for NeedGroundError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "expected ground type but actually function type")
    }
}

impl Error for NeedGroundError {}

#[derive(Debug)]
pub enum TypeError {
    Cat(CatError),
    Alt(AltError),
    Variable(VariableError),
    NeedGround(NeedGroundError),
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

impl From<NeedGroundError> for TypeError {
    fn from(other: NeedGroundError) -> Self {
        Self::NeedGround(other)
    }
}

impl From<TypeError> for syn::Error {
    fn from(other: TypeError) -> Self {
        match other {
            TypeError::Cat(e) => e.into(),
            TypeError::Alt(e) => e.into(),
            TypeError::Variable(e) => e.into(),
            TypeError::NeedGround(e) => e.into(),
        }
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Cat(e) => e.fmt(f),
            Self::Alt(e) => e.fmt(f),
            Self::Variable(e) => e.fmt(f),
            Self::NeedGround(e) => e.fmt(f),
        }
    }
}

impl Error for TypeError {}
