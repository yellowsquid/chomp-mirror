use std::{fmt, mem};

use proc_macro2::Span;
use syn::{punctuated::Pair, Token};

use crate::chomp::{
    ast::{self, NamedExpression},
    Name,
};

use super::{Alt, Call, Cat, Expression, Fix, Ident, Labelled, Lambda, ParenExpression, Term};

#[derive(Debug, Default)]
pub struct Context {
    bindings: Vec<Name>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn lookup(&self, name: &Name) -> Option<usize> {
        self.bindings
            .iter()
            .enumerate()
            .rfind(|(_, n)| *n == name)
            .map(|(idx, _)| idx)
    }

    pub fn push_variable(&mut self, name: Name) {
        self.bindings.push(name);
    }

    pub fn with_variable<F: FnOnce(&mut Self) -> R, R>(&mut self, name: Name, f: F) -> R {
        self.bindings.push(name);
        let res = f(self);
        self.bindings.pop();
        res
    }

    pub fn with_variables<I: IntoIterator<Item = Name>, F: FnOnce(&mut Self) -> R, R>(
        &mut self,
        names: I,
        f: F,
    ) -> R {
        let len = self.bindings.len();
        self.bindings.extend(names);
        let res = f(self);
        self.bindings.resize_with(len, || unreachable!());
        res
    }
}

#[derive(Clone, Debug)]
pub enum ConvertError {
    UndeclaredName(Box<Name>),
    EmptyCat(Option<Span>),
    EmptyAlt(Option<Span>),
    EmptyCall(Option<Span>),
    MissingArgs(Option<Span>),
}

impl From<ConvertError> for syn::Error {
    fn from(e: ConvertError) -> Self {
        let msg = e.to_string();
        let span = match e {
            ConvertError::UndeclaredName(name) => name.span(),
            ConvertError::EmptyCat(span)
            | ConvertError::EmptyAlt(span)
            | ConvertError::EmptyCall(span)
            | ConvertError::MissingArgs(span) => span,
        };

        Self::new(span.unwrap_or_else(Span::call_site), msg)
    }
}

impl fmt::Display for ConvertError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UndeclaredName(name) => {
                write!(f, "undeclared name: `{}`", name)
            }
            Self::EmptyCat(_) => {
                write!(f, "concatenation has no elements")
            }
            Self::EmptyAlt(_) => {
                write!(f, "alternation has no elements")
            }
            Self::EmptyCall(_) => {
                write!(f, "call has no function")
            }
            Self::MissingArgs(_) => {
                write!(f, "call has no arguments")
            }
        }
    }
}

impl std::error::Error for ConvertError {}

pub trait Convert {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError>;
}

impl Convert for Ident {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let span = Some(self.span());
        let name = self.into();

        let index = context
            .lookup(&name)
            .ok_or_else(|| ConvertError::UndeclaredName(Box::new(name.clone())))?;

        Ok(NamedExpression {
            name: Some(name),
            expr: ast::Variable { index }.into(),
            span,
        })
    }
}

impl Convert for Fix {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let span = self.span();
        let inner = self.expr.convert(context)?;
        Ok(NamedExpression {
            name: None,
            expr: ast::Fix {
                inner: Box::new(inner),
            }
            .into(),
            span,
        })
    }
}

impl Convert for ParenExpression {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        self.expr.convert(context)
    }
}

impl Convert for Term {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        match self {
            Self::Epsilon(e) => Ok(NamedExpression {
                name: None,
                expr: ast::Epsilon.into(),
                span: Some(e.span),
            }),
            Self::Ident(i) => i.convert(context),
            Self::Literal(l) => Ok(NamedExpression {
                name: None,
                expr: l.value().into(),
                span: Some(l.span()),
            }),
            Self::Fix(f) => f.convert(context),
            Self::Parens(p) => p.convert(context),
        }
    }
}

impl Convert for Call {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let span = self.span();
        let mut iter = self.0.into_iter();
        let on = iter
            .next()
            .ok_or_else(|| ConvertError::EmptyCall(span))?
            .convert(context)?;
        let args = iter
            .map(|arg| arg.convert(context))
            .collect::<Result<Vec<_>, _>>()?;
        if args.is_empty() {
            Err(ConvertError::MissingArgs(span))
        } else {
            Ok(NamedExpression {
                name: None,
                expr: ast::Call {
                    on: Box::new(on),
                    args,
                }
                .into(),
                span,
            })
        }
    }
}

impl Convert for Cat {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        fn convert_pair(
            pair: Pair<Call, Token![.]>,
            context: &mut Context,
        ) -> Result<(NamedExpression, Option<Span>), ConvertError> {
            match pair {
                Pair::Punctuated(t, p) => t.convert(context).map(|expr| (expr, Some(p.span))),
                Pair::End(t) => t.convert(context).map(|expr| (expr, None)),
            }
        }

        let span = self.span();
        let mut iter = self.0.into_pairs();

        let (first, mut punct) = iter
            .next()
            .ok_or(ConvertError::EmptyCat(span))
            .and_then(|pair| convert_pair(pair, context))?;

        let mut rest = iter
            .map(|pair| {
                convert_pair(pair, context).map(|(snd, p)| (mem::replace(&mut punct, p), snd))
            })
            .peekable();

        if let Some(_) = rest.peek() {
            Ok(NamedExpression {
                name: None,
                expr: ast::Cat {
                    first: Box::new(first),
                    rest: rest.collect::<Result<_, _>>()?,
                }
                .into(),
                span,
            })
        } else {
            Ok(first)
        }
    }
}

impl Convert for Labelled {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let span = self.span();
        let named = self.cat.convert(context)?;
        let name = self.label.map(|l| l.label.into()).or(named.name);

        Ok(NamedExpression {
            name,
            expr: named.expr,
            span,
        })
    }
}

impl Convert for Alt {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        fn convert_pair(
            pair: Pair<Labelled, Token![|]>,
            context: &mut Context,
        ) -> Result<(NamedExpression, Option<Span>), ConvertError> {
            match pair {
                Pair::Punctuated(t, p) => t.convert(context).map(|expr| (expr, Some(p.span))),
                Pair::End(t) => t.convert(context).map(|expr| (expr, None)),
            }
        }

        let span = self.span();
        let mut iter = self.0.into_pairs();

        let (first, mut punct) = iter
            .next()
            .ok_or(ConvertError::EmptyAlt(span))
            .and_then(|pair| convert_pair(pair, context))?;

        let mut rest = iter
            .map(|pair| {
                convert_pair(pair, context).map(|(snd, p)| (mem::replace(&mut punct, p), snd))
            })
            .peekable();

        if let Some(_) = rest.peek() {
            Ok(NamedExpression {
                name: None,
                expr: ast::Alt {
                    first: Box::new(first),
                    rest: rest.collect::<Result<_, _>>()?,
                }
                .into(),
                span,
            })
        } else {
            Ok(first)
        }
    }
}

impl Convert for Lambda {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let span = self.span();
        let mut args: Vec<_> = self.args.into_iter().map(Name::from).collect();
        let expr = self.expr;
        let inner = context.with_variables(args.clone(), |ctx| expr.convert(ctx))?;
        Ok(NamedExpression {
            name: None,
            expr: ast::Lambda {
                args,
                inner: Box::new(inner),
            }
            .into(),
            span,
        })
    }
}

impl Convert for Expression {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        match self {
            Expression::Alt(a) => a.convert(context),
            Expression::Lambda(l) => l.convert(context),
        }
    }
}
