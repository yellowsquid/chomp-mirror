use super::{Expression, Lambda, NamedExpression};
use proc_macro2::Span;
use std::{
    error::Error,
    fmt::{self, Display},
};

#[derive(Debug)]
pub enum TranslateError {
    CallNotAFunction {
        on: Expression,
        span: Option<Span>,
    },
    WrongArgCount {
        lambda: Lambda,
        args: Vec<NamedExpression>,
        span: Option<Span>,
    },
}

impl From<TranslateError> for syn::Error {
    fn from(e: TranslateError) -> Self {
        let msg = e.to_string();
        let span = match e {
            TranslateError::CallNotAFunction { span, .. }
            | TranslateError::WrongArgCount { span, .. } => span,
        };

        Self::new(span.unwrap_or_else(Span::call_site), msg)
    }
}

impl Display for TranslateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CallNotAFunction { .. } => {
                write!(f, "call expected a function")
            }
            Self::WrongArgCount { lambda, args, .. } => match (lambda.args.len(), args.len()) {
                (1, n) => write!(f, "this function takes 1 argument but {} were supplied", n),
                (m, _) => write!(f, "this function takes {} arguments but 1 was supplied", m),
                (m, n) => write!(
                    f,
                    "this function takes {} arguments but {} were supplied",
                    m, n
                ),
            },
        }
    }
}

impl Error for TranslateError {}
