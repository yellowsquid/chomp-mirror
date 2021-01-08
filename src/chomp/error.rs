use std::{
    error::Error,
    fmt::{self, Display},
};

use proc_macro2::Span;

use super::{
    ast::{Alt, Call, Cat, Fix, Parameter, Variable},
    check::Spanning,
    typed::Type,
    visit::Visitable,
};

/// A type error when using a fix point variable.
#[derive(Debug, Eq, PartialEq)]
pub enum VariableError {
    /// Usage of a free variable.
    FreeVariable(Variable),
    /// Usage of a guarded variable.
    GuardedVariable(Variable),
}

impl From<VariableError> for syn::Error {
    fn from(other: VariableError) -> Self {
        match other {
            VariableError::FreeVariable(_) => todo!(),
            VariableError::GuardedVariable(_) => todo!(),
        }
    }
}

impl Display for VariableError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FreeVariable(var) => {
                let start = var.name().span().unwrap_or_else(Span::call_site).start();
                write!(
                    f,
                    "{}:{}: unbound variable '{}'",
                    start.line,
                    start.column,
                    var.name()
                )
            }
            Self::GuardedVariable(var) => {
                let start = var.name().span().unwrap_or_else(Span::call_site).start();
                write!(
                    f,
                    "{}:{}: variable '{}' is guarded",
                    start.line,
                    start.column,
                    var.name()
                )
            }
        }
    }
}

impl Error for VariableError {}

/// A type error when concatenating two terms.
#[derive(Debug, Eq, PartialEq)]
pub enum CatError {
    /// The first term was unexpectedly nullable.
    FirstNullable(Cat),
    /// The flast set of the first term intersects the first set of the second.
    FirstFlastOverlap(Cat),
}

impl From<CatError> for syn::Error {
    fn from(other: CatError) -> Self {
        match other {
            CatError::FirstNullable(cat) => {
                let mut err = Self::new(
                    cat.punct().map_or_else(Span::call_site, |p| p.span),
                    "first item in sequence cannot accept the empty string",
                );
                err.combine(Self::new(
                    cat.first()
                        .visit(&mut Spanning)
                        .unwrap_or_else(Span::call_site),
                    "this can accept empty string",
                ));
                err
            }
            CatError::FirstFlastOverlap(cat) => {
                let mut err = Self::new(
                    cat.punct().map_or_else(Span::call_site, |p| p.span),
                    "cannot decide whether to repeat first or start accepting second",
                );
                err.combine(Self::new(
                    cat.first()
                        .visit(&mut Spanning)
                        .unwrap_or_else(Span::call_site),
                    "a repetition of this",
                ));
                err.combine(Self::new(
                    cat.second()
                        .visit(&mut Spanning)
                        .unwrap_or_else(Span::call_site),
                    "collides with the start of this",
                ));
                err
            }
        }
    }
}

impl Display for CatError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FirstNullable(cat) => {
                let start = cat.punct().map_or_else(Span::call_site, |p| p.span).start();
                let fst_start = cat
                    .first()
                    .visit(&mut Spanning)
                    .unwrap_or_else(Span::call_site)
                    .start();
                write!(
                    f,
                    "{}:{}: term `{}' ({}:{}) accepts the empty string",
                    start.line,
                    start.column,
                    cat.first(),
                    fst_start.line,
                    fst_start.column
                )
            }
            Self::FirstFlastOverlap(cat) => {
                let start = cat.punct().map_or_else(Span::call_site, |p| p.span).start();
                let fst_start = cat
                    .first()
                    .visit(&mut Spanning)
                    .unwrap_or_else(Span::call_site)
                    .start();
                let snd_start = cat
                    .second()
                    .visit(&mut Spanning)
                    .unwrap_or_else(Span::call_site)
                    .start();
                write!(
                    f,
                    "{}:{}: cannot decide whether to repeat `{}' ({}:{}) or start accepting `{}' ({}:{}).",
                    start.line,
                    start.column,
                    cat.first(),
                    fst_start.line,
                    fst_start.column,
                    cat.second(),
                    snd_start.line,
                    snd_start.column
                )
            }
        }
    }
}

impl Error for CatError {}

/// A type error when alternating two terms.
#[derive(Debug, Eq, PartialEq)]
pub enum AltError {
    /// Both terms are nullable.
    BothNullable(Alt),
    /// The first sets of the two terms intersect.
    FirstOverlap(Alt),
}

impl From<AltError> for syn::Error {
    fn from(other: AltError) -> Self {
        match other {
            AltError::BothNullable(alt) => {
                let mut err = Self::new(
                    alt.punct().map_or_else(Span::call_site, |p| p.span),
                    "both branches accept the empty string",
                );
                err.combine(Self::new(
                    alt.left()
                        .visit(&mut Spanning)
                        .unwrap_or_else(Span::call_site),
                    "left branch accepts the empty string",
                ));
                err.combine(Self::new(
                    alt.right()
                        .visit(&mut Spanning)
                        .unwrap_or_else(Span::call_site),
                    "right branch accepts the empty string",
                ));
                err
            }
            AltError::FirstOverlap(alt) => {
                let mut err = Self::new(
                    alt.punct().map_or_else(Span::call_site, |p| p.span),
                    "cannot decide whether to accept the left or right branch",
                );
                err.combine(Self::new(
                    alt.left()
                        .visit(&mut Spanning)
                        .unwrap_or_else(Span::call_site),
                    "left branch starts with a character",
                ));
                err.combine(Self::new(
                    alt.right()
                        .visit(&mut Spanning)
                        .unwrap_or_else(Span::call_site),
                    "right branch starts with the same character",
                ));
                err
            }
        }
    }
}

impl Display for AltError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BothNullable(alt) => {
                let start = alt.punct().map_or_else(Span::call_site, |p| p.span).start();
                let left_start = alt
                    .left()
                    .visit(&mut Spanning)
                    .unwrap_or_else(Span::call_site)
                    .start();
                let right_start = alt
                    .right()
                    .visit(&mut Spanning)
                    .unwrap_or_else(Span::call_site)
                    .start();
                write!(
                    f,
                    "{}:{}: both `{}' ({}:{}) and `{}' ({}:{}) accept the empty string.",
                    start.line,
                    start.column,
                    alt.left(),
                    left_start.line,
                    left_start.column,
                    alt.right(),
                    right_start.line,
                    right_start.column,
                )
            }
            Self::FirstOverlap(alt) => {
                let start = alt.punct().map_or_else(Span::call_site, |p| p.span).start();
                let left_start = alt
                    .left()
                    .visit(&mut Spanning)
                    .unwrap_or_else(Span::call_site)
                    .start();
                let right_start = alt
                    .right()
                    .visit(&mut Spanning)
                    .unwrap_or_else(Span::call_site)
                    .start();
                write!(
                    f,
                    "{}:{}: cannot decide whether to accept `{}' ({}:{}) or `{}' ({}:{}).",
                    start.line,
                    start.column,
                    alt.left(),
                    left_start.line,
                    left_start.column,
                    alt.right(),
                    right_start.line,
                    right_start.column,
                )
            }
        }
    }
}

impl Error for AltError {}

#[derive(Debug, Eq, PartialEq)]
pub struct FixError(pub Fix, pub Type, pub Box<TypeError>);

impl From<FixError> for syn::Error {
    fn from(e: FixError) -> Self {
        let mut error = Self::from(*e.2);
        error.combine(Self::new(
            e.0.span().unwrap_or_else(Span::call_site),
            format!("assuming `{}' has type {:?}.", e.0.arg(), e.1),
        ));
        error
    }
}

impl Display for FixError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.2.fmt(f)?;
        let start = self.0.span().unwrap_or_else(Span::call_site).start();
        write!(
            f,
            "\n{}:{}: assuming `{}' has type {:?}.",
            start.line,
            start.column,
            self.0.arg(),
            self.1
        )
    }
}

impl Error for FixError {}

#[derive(Debug, Eq, PartialEq)]
pub enum TypeError {
    Cat(CatError),
    Alt(AltError),
    Variable(VariableError),
    Fix(FixError),
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
            TypeError::Fix(e) => e.into(),
        }
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Cat(e) => e.fmt(f),
            Self::Alt(e) => e.fmt(f),
            Self::Variable(e) => e.fmt(f),
            Self::Fix(e) => e.fmt(f),
        }
    }
}

impl Error for TypeError {}

#[derive(Debug, Eq, PartialEq)]
pub enum SubstituteError {
    FreeParameter(Parameter),
    WrongArgCount { call: Call, expected: usize },
}

impl Display for SubstituteError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FreeParameter(param) => {
                let start = param.name().span().unwrap_or_else(Span::call_site).start();
                write!(
                    f,
                    "{}:{}: undeclared variable `{}'",
                    start.line,
                    start.column,
                    param.name()
                )
            }
            SubstituteError::WrongArgCount { call, expected } => {
                let start = call.span().unwrap_or_else(Span::call_site).start();
                write!(
                    f,
                    "{}:{}: wrong number of arguments. Expected {}, got {}",
                    start.line,
                    start.column,
                    call.args().len(),
                    expected
                )
            }
        }
    }
}

impl Error for SubstituteError {}
