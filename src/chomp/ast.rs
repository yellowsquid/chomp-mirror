use std::fmt::{self, Display};

use proc_macro2::Span;
use syn::{Ident, LitStr, Token};

pub type Epsilon = Token![_];

pub type Literal = LitStr;

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

#[derive(Clone, Debug)]
pub struct Fix {
    pub arg: Ident,
    pub inner: Box<Expression>,
    pub span: Option<Span>,
}

impl Fix {
    pub fn new(arg: Ident, inner: Expression, span: Option<Span>) -> Self {
        Self {
            arg,
            inner: Box::new(inner),
            span,
        }
    }

    pub fn arg(&self) -> &Ident {
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

#[derive(Clone, Debug)]
pub struct Variable {
    pub name: Ident,
    pub index: usize,
}

impl Variable {
    pub fn new(name: Ident, index: usize) -> Self {
        Self { name, index }
    }

    pub fn name(&self) -> &Ident {
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

#[derive(Clone, Debug)]
pub struct Parameter {
    pub name: Ident,
    pub index: usize,
}

impl Parameter {
    pub fn new(name: Ident, index: usize) -> Self {
        Self { name, index }
    }

    pub fn name(&self) -> &Ident {
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
        self.name.fmt(f)
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
            Self::Literal(l) => write!(f, "{:?}", l.value()),
            Self::Cat(c) => c.fmt(f),
            Self::Alt(a) => a.fmt(f),
            Self::Fix(x) => x.fmt(f),
            Self::Variable(v) => v.fmt(f),
            Self::Parameter(p) => p.fmt(f),
            Self::Call(c) => c.fmt(f),
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
    pub fn new(name: Ident, params: usize, expr: Expression, span: Option<Span>) -> Self {
        Self {
            name,
            params,
            expr,
            span,
        }
    }
}
