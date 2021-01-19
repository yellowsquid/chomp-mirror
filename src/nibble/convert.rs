use std::{collections::HashMap, fmt};

use proc_macro2::Span;
use syn::punctuated::Pair;

use crate::chomp::{
    ast::{self, NamedExpression},
    Name,
};

use super::cst::{Alt, Call, Cat, Fix, Ident, Labelled, ParenExpression, Term};

#[derive(Clone, Copy, Debug)]
pub enum Binding {
    Variable(usize),
    Parameter(usize),
    Global,
}

#[derive(Debug, Default)]
pub struct Context {
    names: HashMap<String, Binding>,
    vars: usize,
}

impl Context {
    pub fn new<I: IntoIterator<Item = Name>>(globals: &[Name], params: I) -> Self {
        let mut names = HashMap::new();
        for global in globals {
            names.insert(global.to_string(), Binding::Global);
        }

        for (index, param) in params.into_iter().enumerate() {
            names.insert(param.to_string(), Binding::Parameter(index));
        }

        Self { names, vars: 0 }
    }

    pub fn lookup(&self, name: &Name) -> Option<Binding> {
        // we make variable binding cheaper by inserting wrong and pulling right.
        match self.names.get(&name.to_string()).copied() {
            Some(Binding::Variable(index)) => Some(Binding::Variable(self.vars - index - 1)),
            x => x,
        }
    }

    pub fn with_variable<F: FnOnce(&mut Self) -> R, R>(&mut self, name: &Name, f: F) -> R {
        let old = self
            .names
            .insert(name.to_string(), Binding::Variable(self.vars));

        // we make variable binding cheaper by inserting wrong and pulling right.
        // we should increment all values in names instead, but that's slow
        self.vars += 1;
        let res = f(self);
        self.vars -= 1;

        if let Some(val) = old {
            self.names.insert(name.to_string(), val);
        } else {
            self.names.remove(&name.to_string());
        }

        res
    }
}

#[derive(Clone, Debug)]
pub enum ConvertError {
    UndeclaredName(Name),
}

impl From<ConvertError> for syn::Error {
    fn from(e: ConvertError) -> Self {
        let msg = e.to_string();
        let span = match e {
            ConvertError::UndeclaredName(name) => name.span(),
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

        let binding = context
            .lookup(&name)
            .ok_or_else(|| ConvertError::UndeclaredName(name.clone()))?;

        Ok(match binding {
            Binding::Variable(index) => NamedExpression {
                name: Some(name),
                expr: ast::Variable { index }.into(),
                span,
            },
            Binding::Parameter(index) => NamedExpression {
                name: Some(name),
                expr: ast::Parameter { index }.into(),
                span,
            },
            Binding::Global => NamedExpression {
                name: None,
                expr: ast::Call {
                    name,
                    args: Vec::new(),
                }
                .into(),
                span,
            },
        })
    }
}

impl Convert for Call {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let span = self.span();
        let args = self
            .args
            .into_iter()
            .map(|arg| arg.convert(context))
            .collect::<Result<_, _>>()?;
        Ok(NamedExpression {
            name: None,
            expr: ast::Call {
                name: self.name.into(),
                args,
            }
            .into(),
            span,
        })
    }
}

impl Convert for Fix {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let span = self.span();
        let expr = self.expr;
        let arg = self.arg.into();
        let inner = context.with_variable(&arg, |context| expr.convert(context))?;
        Ok(NamedExpression {
            name: None,
            expr: ast::Fix {
                arg: Some(arg),
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
            Self::Call(c) => c.convert(context),
            Self::Fix(f) => f.convert(context),
            Self::Parens(p) => p.convert(context),
        }
    }
}

impl Convert for Cat {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let mut iter = self.0.into_pairs();

        let (first, punct) = match iter.next().unwrap() {
            Pair::Punctuated(t, p) => (t.convert(context)?, Some(p.span)),
            Pair::End(t) => (t.convert(context)?, None),
        };

        let mut rest = Vec::new();
        let (span, _) = iter.try_fold(
            (
                first.span.and_then(|s| punct.and_then(|p| s.join(p))),
                punct,
            ),
            |(span, punct), pair| {
                let (snd, p) = match pair {
                    Pair::Punctuated(t, p) => (t.convert(context)?, Some(p.span)),
                    Pair::End(t) => (t.convert(context)?, None),
                };

                let span = span
                    .and_then(|s| snd.span.and_then(|t| s.join(t)))
                    .and_then(|s| p.and_then(|p| s.join(p)));
                rest.push((punct, snd));
                Ok((span, p))
            },
        )?;

        let mut iter = rest.into_iter();
        if let Some((punct, second)) = iter.next() {
            Ok(NamedExpression {
                name: None,
                expr: ast::Cat {
                    first: Box::new(first),
                    punct,
                    second: Box::new(second),
                    rest: iter.collect(),
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
        let mut iter = self.0.into_pairs();

        let (first, punct) = match iter.next().unwrap() {
            Pair::Punctuated(t, p) => (t.convert(context)?, Some(p.span)),
            Pair::End(t) => (t.convert(context)?, None),
        };

        let mut rest = Vec::new();
        let (span, _) = iter.try_fold(
            (
                first.span.and_then(|s| punct.and_then(|p| s.join(p))),
                punct,
            ),
            |(span, punct), pair| {
                let (snd, p) = match pair {
                    Pair::Punctuated(t, p) => (t.convert(context)?, Some(p.span)),
                    Pair::End(t) => (t.convert(context)?, None),
                };

                let span = span
                    .and_then(|s| snd.span.and_then(|t| s.join(t)))
                    .and_then(|s| p.and_then(|p| s.join(p)));
                rest.push((punct, snd));
                Ok((span, p))
            },
        )?;

        let mut iter = rest.into_iter();
        if let Some((punct, second)) = iter.next() {
            Ok(NamedExpression {
                name: None,
                expr: ast::Alt {
                    first: Box::new(first),
                    punct,
                    second: Box::new(second),
                    rest: iter.collect(),
                }
                .into(),
                span,
            })
        } else {
            Ok(first)
        }
    }
}
