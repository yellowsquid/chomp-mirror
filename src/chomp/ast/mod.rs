use std::fmt::{self, Display};

use proc_macro2::Span;

use super::Name;

pub mod error;
pub mod substitute;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Hash)]
pub struct Epsilon;

pub type Literal = String;

#[derive(Clone, Debug)]
pub struct Cat {
    pub first: Box<NamedExpression>,
    pub rest: Vec<(Option<Span>, NamedExpression)>,
}

impl Display for Cat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}", self.first)?;
        for (_, other) in &self.rest {
            write!(f, " . {}", other)?;
        }
        write!(f, ")")
    }
}

impl PartialEq for Cat {
    fn eq(&self, other: &Self) -> bool {
        self.first == other.first
            && self.rest.len() == other.rest.len()
            && self
                .rest
                .iter()
                .zip(other.rest.iter())
                .all(|((_, me), (_, them))| me == them)
    }
}

impl Eq for Cat {}

#[derive(Clone, Debug)]
pub struct Alt {
    pub first: Box<NamedExpression>,
    pub rest: Vec<(Option<Span>, NamedExpression)>
}

impl Display for Alt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}", self.first)?;
        for (_, other) in &self.rest {
            write!(f, " | {}", other)?;
        }
        write!(f, ")")
    }
}

impl PartialEq for Alt {
    fn eq(&self, other: &Self) -> bool {
        self.first == other.first
            && self.rest.len() == other.rest.len()
            && self
                .rest
                .iter()
                .zip(other.rest.iter())
                .all(|((_, me), (_, them))| me == them)
    }
}

impl Eq for Alt {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Fix {
    pub inner: Box<NamedExpression>,
}

impl Display for Fix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "!{}", self.inner)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Variable {
    pub index: usize,
}

impl Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}", self.index)
    }
}

/// A function invocation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Call {
    pub on: Box<NamedExpression>,
    pub args: Vec<NamedExpression>,
}

impl Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}", self.on)?;

        for arg in self.args {
            write!(f, " {}", arg)?;
        }

        write!(f, ")")
    }
}

/// A function abstraction.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Lambda {
    pub first: Name,
    pub rest: Vec<Name>,
    pub inner: Box<NamedExpression>,
}

impl Display for Lambda {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "/{}", self.first)?;
        for name in self.rest {
            write!(f, ", {}", name)?;
        }
        write!(f, "/ {}", self.inner)
    }
}

#[derive(Clone, Debug)]
pub enum Expression {
    /// Matches the empty string.
    Epsilon(Epsilon),
    /// Matches the literal string.
    Literal(Literal),
    /// Matches one term followed by another.
    Cat(Cat),
    /// Matches either one term or another.
    Alt(Alt),
    /// The least fix point of a term.
    Fix(Fix),
    /// A fixed point variable.
    Variable(Variable),
    /// A function invocation.
    Call(Call),
    /// A function abstraction.
    Lambda(Lambda),
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Epsilon(_) => write!(f, "_"),
            Self::Literal(l) => l.fmt(f),
            Self::Cat(c) => c.fmt(f),
            Self::Alt(a) => a.fmt(f),
            Self::Fix(x) => x.fmt(f),
            Self::Variable(v) => v.fmt(f),
            Self::Lambda(p) => p.fmt(f),
            Self::Call(c) => c.fmt(f),
        }
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Epsilon(_) => matches!(other, Self::Epsilon(_)),
            Self::Literal(l) => {
                if let Self::Literal(them) = other {
                    l == them
                } else {
                    false
                }
            }
            Self::Cat(c) => {
                if let Self::Cat(them) = other {
                    c == them
                } else {
                    false
                }
            }
            Self::Alt(a) => {
                if let Self::Alt(them) = other {
                    a == them
                } else {
                    false
                }
            }
            Self::Fix(f) => {
                if let Self::Fix(them) = other {
                    f == them
                } else {
                    false
                }
            }
            Self::Variable(v) => {
                if let Self::Variable(them) = other {
                    v == them
                } else {
                    false
                }
            }
            Self::Lambda(p) => {
                if let Self::Lambda(them) = other {
                    p == them
                } else {
                    false
                }
            }
            Self::Call(c) => {
                if let Self::Call(them) = other {
                    c == them
                } else {
                    false
                }
            }
        }
    }
}

impl Eq for Expression {}

impl From<Epsilon> for Expression {
    fn from(eps: Epsilon) -> Self {
        Self::Epsilon(eps)
    }
}

impl From<Literal> for Expression {
    fn from(lit: Literal) -> Self {
        Self::Literal(lit)
    }
}

impl From<Cat> for Expression {
    fn from(cat: Cat) -> Self {
        Self::Cat(cat)
    }
}

impl From<Alt> for Expression {
    fn from(alt: Alt) -> Self {
        Self::Alt(alt)
    }
}

impl From<Fix> for Expression {
    fn from(fix: Fix) -> Self {
        Self::Fix(fix)
    }
}

impl From<Variable> for Expression {
    fn from(var: Variable) -> Self {
        Self::Variable(var)
    }
}

impl From<Lambda> for Expression {
    fn from(lambda: Lambda) -> Self {
        Self::Lambda(lambda)
    }
}

impl From<Call> for Expression {
    fn from(call: Call) -> Self {
        Self::Call(call)
    }
}

#[derive(Clone, Debug)]
pub struct NamedExpression {
    pub name: Option<Name>,
    pub expr: Expression,
    pub span: Option<Span>,
}

impl Display for NamedExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.name {
            Some(name) => write!(f, "({} : {})", self.expr, name),
            None => self.expr.fmt(f),
        }
    }
}

impl PartialEq for NamedExpression {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

impl Eq for NamedExpression {}

#[derive(Clone, Debug)]
pub struct Let {
    pub name: Name,
    pub val: NamedExpression,
    pub inner: Box<TopLevel>,
}

#[derive(Clone, Debug)]
pub enum TopLevel {
    Let(Let),
    Goal(NamedExpression),
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.params == other.params && self.expr == other.expr
    }
}

impl Eq for Function {}
