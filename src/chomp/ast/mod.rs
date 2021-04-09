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
    pub punct: Option<Span>,
    pub second: Box<NamedExpression>,
    pub rest: Vec<(Option<Span>, NamedExpression)>,
}

impl Display for Cat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} . {}", self.first, self.second)?;
        for (_, other) in &self.rest {
            write!(f, " . {}", other)?;
        }
        write!(f, ")")
    }
}

impl PartialEq for Cat {
    fn eq(&self, other: &Self) -> bool {
        self.first == other.first
            && self.second == other.second
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
    pub punct: Option<Span>,
    pub second: Box<NamedExpression>,
    pub rest: Vec<(Option<Span>, NamedExpression)>
}

impl Display for Alt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} | {}", self.first, self.second)?;
        for (_, other) in &self.rest {
            write!(f, " | {}", other)?;
        }
        write!(f, ")")
    }
}

impl PartialEq for Alt {
    fn eq(&self, other: &Self) -> bool {
        self.first == other.first
            && self.second == other.second
            && self.rest.len() == other.rest.len()
            && self
            .rest
            .iter()
            .zip(other.rest.iter())
            .all(|((_, me), (_, them))| me == them)
    }
}

impl Eq for Alt {}

#[derive(Clone, Debug)]
pub struct Fix {
    pub arg: Option<Name>,
    pub inner: Box<NamedExpression>,
}

impl Display for Fix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.arg {
            Some(arg) => write!(f, "[{}]({})", arg, self.inner),
            None => write!(f, "[]({})", self.inner),
        }
    }
}

impl PartialEq for Fix {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl Eq for Fix {}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Parameter {
    pub index: usize,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}>", self.index)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Global {
    pub name: Name,
}

impl Display for Global {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}", self.name)
    }
}

/// A macro invocation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Call {
    pub fun: Box<NamedExpression>,
    pub args: Vec<NamedExpression>,
}

impl Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})(", self.fun)?;

        let mut iter = self.args.iter();

        if let Some(arg) = iter.next() {
            write!(f, "{}", arg)?;

            for arg in iter {
                write!(f, ", {}", arg)?;
            }
        }

        write!(f, ")")
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Function {
    pub args: Vec<Name>,
    pub expr: Box<NamedExpression>,
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        let mut iter = self.args.iter();
        if let Some(arg) = iter.next() {
            write!(f, "{}", arg)?;
            for arg in iter {
                write!(f, ", {}", arg)?;
            }
        }

        write!(f, "]")
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
    /// A formal parameter.
    Parameter(Parameter),
    /// A global variable.
    Global(Global),
    /// A function invocation.
    Call(Call),
    /// A function definition.
    Function(Function)
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Epsilon(_) => write!(f, "_"),
            Self::Literal(l) => l.fmt(f),
            Self::Cat(c) => c.fmt(f),
            Self::Alt(a) => a.fmt(f),
            Self::Fix(x) => x.fmt(f),
            Self::Global(g) => g.fmt(f),
            Self::Parameter(p) => p.fmt(f),
            Self::Call(c) => c.fmt(f),
            Self::Function(n) => n.fmt(f),
        }
    }
}

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

impl From<Parameter> for Expression {
    fn from(param: Parameter) -> Self {
        Self::Parameter(param)
    }
}

impl From<Global> for Expression {
    fn from(global: Global) -> Self {
        Self::Global(global)
    }
}

impl From<Call> for Expression {
    fn from(call: Call) -> Self {
        Self::Call(call)
    }
}

impl From<Function> for Expression {
    fn from(fun: Function) -> Self {
        Self::Function(fun)
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
