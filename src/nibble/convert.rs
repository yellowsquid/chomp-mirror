use std::{fmt, mem};

use proc_macro2::Span;
use syn::{punctuated::Pair, spanned::Spanned, Token};

use crate::chomp::{ast::{self, NamedExpression}, name::{Content, Name}};

use super::{
    Alt, Call, Cat, Expression, Fix, GoalStatement, Ident, Labelled, Lambda, LetStatement,
    ParenExpression, Statement, Term,
};

#[derive(Debug, Default)]
pub struct Context {
    bindings: Vec<Content>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the De Bruijn index of `name`, if it is defined.
    pub fn lookup(&self, name: &Name) -> Option<usize> {
        self.bindings
            .iter()
            .rev()
            .enumerate()
            .find(|(_, n)| *n == &name.content)
            .map(|(idx, _)| idx)
    }

    /// Permanently add the variable `name` to the top of the stack.
    pub fn push_variable(&mut self, name: Name) {
        self.bindings.push(name.content);
    }

    /// Call `f` with the variable `name` pushed to the top of the stack.
    pub fn with_variable<F: FnOnce(&mut Self) -> R, R>(&mut self, name: Name, f: F) -> R {
        self.bindings.push(name.content);
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
        self.bindings.extend(names.into_iter().map(|n| n.content));
        let res = f(self);
        self.bindings.resize_with(len, || unreachable!());
        res
    }
}

#[derive(Clone, Debug)]
pub enum ConvertError {
    UndeclaredName(Box<Name>),
    EmptyCat(Span),
    EmptyAlt(Span),
    EmptyCall(Span),
}

impl From<ConvertError> for syn::Error {
    fn from(e: ConvertError) -> Self {
        let msg = e.to_string();
        let span = match e {
            ConvertError::UndeclaredName(name) => name.span(),
            ConvertError::EmptyCat(span)
            | ConvertError::EmptyAlt(span)
            | ConvertError::EmptyCall(span) => span,
        };

        Self::new(span, msg)
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
        }
    }
}

impl std::error::Error for ConvertError {}

pub trait Convert {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError>;
}

impl Convert for Ident {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let span = self.span();
        let name = Name::new_variable(self);

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
                span: e.span,
            }),
            Self::Ident(i) => i.convert(context),
            Self::Literal(l) => Ok(NamedExpression {
                name: None,
                expr: l.value().into(),
                span: l.span(),
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
            .ok_or(ConvertError::EmptyCall(span))?
            .convert(context)?;
        let args = iter
            .map(|arg| arg.convert(context))
            .collect::<Result<Vec<_>, _>>()?;
        if args.is_empty() {
            Ok(on)
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
        ) -> Result<(NamedExpression, Span), ConvertError> {
            match pair {
                Pair::Punctuated(t, p) => t.convert(context).map(|expr| (expr, p.span)),
                Pair::End(t) => t.convert(context).map(|expr| (expr, Span::call_site())),
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

        if rest.peek().is_some() {
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
        let label = self.label.map(|l| Name::new_label(l.label));
        let name = Name::merge(label, named.name);

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
        ) -> Result<(NamedExpression, Span), ConvertError> {
            match pair {
                Pair::Punctuated(t, p) => t.convert(context).map(|expr| (expr, p.span)),
                Pair::End(t) => t.convert(context).map(|expr| (expr, Span::call_site())),
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

        if rest.peek().is_some() {
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
        let args: Vec<_> = self.args.into_iter().map(Name::new_variable).collect();
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

impl Convert for GoalStatement {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let span = self.span();
        let inner = self.expr.convert(context)?;
        Ok(NamedExpression {
            name: Name::merge(inner.name, Some(Name::new_variable("Ast".to_owned()))),
            expr: inner.expr,
            span,
        })
    }
}

impl Convert for LetStatement {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let span = self.span();
        let bound = if self.args.is_empty() {
            self.expr.convert(context)?
        } else {
            let args: Vec<_> = self.args.into_iter().map(Name::new_variable).collect();
            let expr = self.expr;
            let inner = context.with_variables(args.clone(), |ctx| expr.convert(ctx))?;
            NamedExpression {
                name: None,
                expr: ast::Lambda {
                    args,
                    inner: Box::new(inner),
                }
                .into(),
                span,
            }
        };
        let name = Name::new_let(self.name);
        context.push_variable(name.clone());
        let body = self.next.convert(context)?;
        Ok(NamedExpression {
            name: None,
            expr: ast::Let {
                name: name.clone(),
                bound: Box::new(NamedExpression {
                    name: Some(name),
                    ..bound
                }),
                body: Box::new(body),
            }
            .into(),
            span,
        })
    }
}

impl Convert for Statement {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        match self {
            Self::Goal(g) => g.convert(context),
            Self::Let(l) => l.convert(context),
        }
    }
}
