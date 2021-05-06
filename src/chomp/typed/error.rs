use std::{
    error::Error,
    fmt::{self, Display},
};

use proc_macro2::Span;

use crate::chomp::{name::Name, ast::{Call, Fix, Lambda, Let, Variable}};

use super::{Type, TypedExpression, context::GetVariableError};

/// A type error when using a fix point variable.
#[derive(Debug)]
pub struct VariableError {
    pub inner: GetVariableError,
    pub var: Variable,
    pub span: Span,
    pub name: Option<Name>,
}

impl Display for VariableError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            GetVariableError::FreeVariable => write!(f, "unbound variable: "),
            GetVariableError::GuardedVariable => write!(f, "usage of guarded variable: "),
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
        punct: Span,
    },
    FirstFlastOverlap {
        front_ty: Type,
        punct: Span,
        next: TypedExpression,
    },
}

impl CatError {
    pub fn span(&self) -> Span {
        match self {
            Self::FirstNullable { punct, .. } | Self::FirstFlastOverlap { punct, .. } => *punct,
        }
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
        left_ty: Type,
        punct: Span,
        right: TypedExpression,
    },
    FirstOverlap {
        left_ty: Type,
        punct: Span,
        right: TypedExpression,
    },
}

impl AltError {
    pub fn span(&self) -> Span {
        match self {
            Self::BothNullable { punct, .. } | Self::FirstOverlap { punct, .. } => *punct,
        }
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
pub enum TypeError {
    Cat(CatError),
    Alt(AltError),
    Variable(VariableError),
    UnexpectedCall { span: Span, call: Call },
    UnexpectedLambda { span: Span, lambda: Lambda },
    UnexpectedLet { span: Span, stmt: Let },
    ExpectedLambda {span: Span, fix: Fix },
}

impl TypeError {
    pub fn span(&self) -> Span {
        match self {
            TypeError::Cat(c) => c.span(),
            TypeError::Alt(a) => a.span(),
            TypeError::Variable(v) => v.span,
            TypeError::UnexpectedCall { span, .. }
            | TypeError::UnexpectedLambda { span, .. }
            | TypeError::UnexpectedLet { span, .. }
            | TypeError::ExpectedLambda { span, .. } => *span,
        }
    }
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
        let span = other.span();
        let msg = format!("{}", other);
        Self::new(span, msg)
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Cat(e) => e.fmt(f),
            Self::Alt(e) => e.fmt(f),
            Self::Variable(e) => e.fmt(f),

            Self::UnexpectedCall { .. } => {
                write!(f, "unexpected call")
            }
            Self::UnexpectedLambda { .. } => {
                write!(f, "unexpected lambda")
            }
            Self::UnexpectedLet { .. } => {
                write!(f, "unexpected let")
            }
            Self::ExpectedLambda { .. } => {
                write!(f, "expected a lambda expression")
            }
        }
    }
}

impl Error for TypeError {}
