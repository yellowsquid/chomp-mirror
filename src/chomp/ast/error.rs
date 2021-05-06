use super::{Expression, Lambda, NamedExpression};
use proc_macro2::Span;
use std::{
    error::Error,
    fmt::{self, Display},
};

#[derive(Debug)]
pub enum ReductionError {
    CallNotAFunction {
        on: Expression,
        span: Span,
    },
    WrongArgCount {
        lambda: Lambda,
        args: Vec<NamedExpression>,
        span: Span,
    },
}

impl From<ReductionError> for syn::Error {
    fn from(e: ReductionError) -> Self {
        let msg = e.to_string();
        let span = match e {
            ReductionError::CallNotAFunction { span, .. }
            | ReductionError::WrongArgCount { span, .. } => span,
        };

        Self::new(span, msg)
    }
}

impl Display for ReductionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CallNotAFunction { .. } => {
                write!(f, "call expected a function")
            }
            Self::WrongArgCount { lambda, args, .. } => match (lambda.args.len(), args.len()) {
                (1, n) => write!(f, "this function takes 1 argument but {} were supplied", n),
                (m, 1) => write!(f, "this function takes {} arguments but 1 was supplied", m),
                (m, n) => write!(
                    f,
                    "this function takes {} arguments but {} were supplied",
                    m, n
                ),
            },
        }
    }
}

impl Error for ReductionError {}
