use std::{
    fmt::{self, Display},
    hash,
};

use proc_macro2::Span;
use syn::{Ident, LitStr, Token};

use super::Name;

pub type Epsilon = Option<Token![_]>;

#[derive(Clone, Debug)]
pub enum Literal {
    Spanned(LitStr),
    Spanless(String),
}

impl Literal {
    pub fn value(&self) -> String {
        match self {
            Self::Spanned(l) => l.value(),
            Self::Spanless(s) => s.clone(),
        }
    }

    pub fn span(&self) -> Option<Span> {
        match self {
            Self::Spanned(l) => Some(l.span()),
            Self::Spanless(_) => None,
        }
    }

    pub fn as_litstr(self, span: Span) -> LitStr {
        match self {
            Self::Spanned(l) => l,
            Self::Spanless(s) => LitStr::new(&s, span),
        }
    }
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Spanned(me), Self::Spanned(them)) => me == them,
            (Self::Spanned(me), Self::Spanless(them)) => &me.value() == them,
            (Self::Spanless(me), Self::Spanned(them)) => me == &them.value(),
            (Self::Spanless(me), Self::Spanless(them)) => me == them,
        }
    }
}

impl Eq for Literal {}

impl hash::Hash for Literal {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.value().hash(state)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.value())
    }
}

impl From<LitStr> for Literal {
    fn from(l: LitStr) -> Self {
        Self::Spanned(l)
    }
}

impl From<String> for Literal {
    fn from(s: String) -> Self {
        Self::Spanless(s)
    }
}

#[derive(Clone, Debug)]
pub struct Cat {
    pub fst: Box<Expression>,
    pub punct: Option<Token![.]>,
    pub snd: Box<Expression>,
}

impl Cat {
    pub fn new(fst: Expression, punct: Option<Token![.]>, snd: Expression) -> Self {
        Self {
            fst: Box::new(fst),
            punct,
            snd: Box::new(snd),
        }
    }

    pub fn first(&self) -> &Expression {
        &self.fst
    }

    pub fn first_mut(&mut self) -> &mut Expression {
        &mut self.fst
    }

    pub fn punct(&self) -> Option<Token![.]> {
        self.punct
    }

    pub fn second(&self) -> &Expression {
        &self.snd
    }

    pub fn second_mut(&mut self) -> &mut Expression {
        &mut self.snd
    }
}

impl Display for Cat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}.{})", self.fst, self.snd)
    }
}

impl PartialEq for Cat {
    fn eq(&self, other: &Self) -> bool {
        self.first() == other.first() && self.second() == other.second()
    }
}

impl Eq for Cat {}

#[derive(Clone, Debug)]
pub struct Alt {
    pub left: Box<Expression>,
    pub punct: Option<Token![|]>,
    pub right: Box<Expression>,
}

impl Alt {
    pub fn new(left: Expression, punct: Option<Token![|]>, right: Expression) -> Self {
        Self {
            left: Box::new(left),
            punct,
            right: Box::new(right),
        }
    }

    pub fn left(&self) -> &Expression {
        &self.left
    }

    pub fn left_mut(&mut self) -> &mut Expression {
        &mut self.left
    }

    pub fn punct(&self) -> Option<Token![|]> {
        self.punct
    }

    pub fn right(&self) -> &Expression {
        &self.right
    }

    pub fn right_mut(&mut self) -> &mut Expression {
        &mut self.right
    }
}

impl Display for Alt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}|{})", self.left, self.right)
    }
}

impl PartialEq for Alt {
    fn eq(&self, other: &Self) -> bool {
        self.left() == other.left() && self.right() == other.right()
    }
}

impl Eq for Alt {}

#[derive(Clone, Debug)]
pub struct Fix {
    pub arg: Name,
    pub inner: Box<Expression>,
    pub span: Option<Span>,
}

impl Fix {
    pub fn new(arg: Name, inner: Expression, span: Option<Span>) -> Self {
        Self {
            arg,
            inner: Box::new(inner),
            span,
        }
    }

    pub fn arg(&self) -> &Name {
        &self.arg
    }

    pub fn inner(&self) -> &Expression {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut Expression {
        &mut self.inner
    }

    pub fn span(&self) -> Option<Span> {
        self.span
    }
}

impl Display for Fix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]({})", self.arg, self.inner)
    }
}

impl PartialEq for Fix {
    fn eq(&self, other: &Self) -> bool {
        self.inner() == other.inner()
    }
}

impl Eq for Fix {}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Variable {
    pub name: Name,
    pub index: usize,
}

impl Variable {
    pub fn new(name: Name, index: usize) -> Self {
        Self { name, index }
    }

    pub fn name(&self) -> &Name {
        &self.name
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn index_mut(&mut self) -> &mut usize {
        &mut self.index
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Parameter {
    pub name: Name,
    pub index: usize,
}

impl Parameter {
    pub fn new(name: Name, index: usize) -> Self {
        Self { name, index }
    }

    pub fn name(&self) -> &Name {
        &self.name
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn index_mut(&mut self) -> &mut usize {
        &mut self.index
    }
}

impl Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// A macro invocation.
#[derive(Clone, Debug)]
pub struct Call {
    pub name: Ident,
    pub args: Vec<Expression>,
    pub span: Option<Span>,
}

impl Call {
    pub fn new(name: Ident, args: Vec<Expression>, span: Option<Span>) -> Self {
        Self { name, args, span }
    }

    pub fn name(&self) -> &Ident {
        &self.name
    }

    pub fn args(&self) -> &[Expression] {
        &self.args
    }

    pub fn args_mut(&mut self) -> &mut [Expression] {
        &mut self.args
    }

    pub fn span(&self) -> Option<Span> {
        self.span
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;

        let mut iter = self.args.iter();

        if let Some(arg) = iter.next() {
            write!(f, "({}", arg)?;

            for arg in iter {
                write!(f, ", {}", arg)?;
            }

            write!(f, ")")
        } else {
            Ok(())
        }
    }
}

impl PartialEq for Call {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name() && self.args() == other.args()
    }
}

impl Eq for Call {}

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
    /// A formal parameter.
    Parameter(Parameter),
    /// A macro invocation.
    Call(Call),
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
            Self::Parameter(p) => p.fmt(f),
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
            Self::Parameter(p) => {
                if let Self::Parameter(them) = other {
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

impl From<Parameter> for Expression {
    fn from(param: Parameter) -> Self {
        Self::Parameter(param)
    }
}

impl From<Call> for Expression {
    fn from(call: Call) -> Self {
        Self::Call(call)
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Ident,
    pub params: usize,
    pub expr: Expression,
    pub span: Option<Span>,
}

impl Function {
    pub const fn new(name: Ident, params: usize, expr: Expression, span: Option<Span>) -> Self {
        Self {
            name,
            params,
            expr,
            span,
        }
    }
}
