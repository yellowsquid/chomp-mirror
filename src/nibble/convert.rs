use std::collections::HashMap;

use syn::punctuated::Pair;

use crate::chomp::ast;

use super::cst::{Alt, Call, Cat, Fix, Ident, ParenExpression, Term};

#[derive(Clone, Copy, Debug)]
pub enum Binding {
    Variable(usize),
    Parameter(usize),
    Global,
}

#[derive(Debug)]
pub struct Context {
    names: HashMap<String, Binding>,
    vars: usize,
}

impl Context {
    pub fn new<I: IntoIterator<Item = Ident>>(globals: &[Ident], params: I) -> Self {
        let mut names = HashMap::new();
        for global in globals {
            names.insert(global.to_string(), Binding::Global);
        }

        for (index, param) in params.into_iter().enumerate() {
            names.insert(param.to_string(), Binding::Parameter(index));
        }

        Self { names, vars: 0 }
    }

    pub fn lookup(&self, name: &Ident) -> Option<Binding> {
        // we make variable binding cheaper by inserting wrong and pulling right.
        match self.names.get(&name.to_string()).copied() {
            Some(Binding::Variable(index)) => Some(Binding::Variable(self.vars - index - 1)),
            x => x,
        }
    }

    pub fn with_variable<F: FnOnce(&mut Self) -> R, R>(&mut self, name: &Ident, f: F) -> R {
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

pub trait Convert {
    fn convert(self, context: &mut Context) -> Option<ast::Expression>;
}

impl Convert for Ident {
    fn convert(self, context: &mut Context) -> Option<ast::Expression> {
        let span = self.span();

        match context.lookup(&self)? {
            Binding::Variable(index) => Some(ast::Variable::new(self.into(), index).into()),
            Binding::Parameter(index) => Some(ast::Parameter::new(self.into(), index).into()),
            Binding::Global => Some(ast::Call::new(self.into(), Vec::new(), Some(span)).into()),
        }
    }
}

impl Convert for Call {
    fn convert(self, context: &mut Context) -> Option<ast::Expression> {
        let span = self.span();
        let args = self
            .args
            .into_iter()
            .map(|arg| arg.convert(context))
            .collect::<Option<_>>()?;
        Some(ast::Call::new(self.name.into(), args, span).into())
    }
}

impl Convert for Fix {
    fn convert(self, context: &mut Context) -> Option<ast::Expression> {
        let span = self.span();
        let expr = self.expr;
        let inner = context.with_variable(&self.arg, |context| expr.convert(context))?;
        Some(ast::Fix::new(self.arg.into(), inner, span).into())
    }
}

impl Convert for ParenExpression {
    fn convert(self, context: &mut Context) -> Option<ast::Expression> {
        self.expr.convert(context)
    }
}

impl Convert for Term {
    fn convert(self, context: &mut Context) -> Option<ast::Expression> {
        match self {
            Self::Epsilon(e) => Some(Some(e).into()),
            Self::Ident(i) => i.convert(context),
            Self::Literal(l) => Some(ast::Literal::from(l).into()),
            Self::Call(c) => c.convert(context),
            Self::Fix(f) => f.convert(context),
            Self::Parens(p) => p.convert(context),
        }
    }
}

impl Convert for Cat {
    fn convert(self, context: &mut Context) -> Option<ast::Expression> {
        let mut iter = self.0.into_pairs();
        let mut out = match iter.next().unwrap() {
            Pair::Punctuated(t, p) => (t.convert(context)?, Some(p)),
            Pair::End(t) => (t.convert(context)?, None),
        };

        for pair in iter {
            let (fst, punct) = out;
            out = match pair {
                Pair::Punctuated(t, p) => (
                    ast::Cat::new(fst, punct, t.convert(context)?).into(),
                    Some(p),
                ),
                Pair::End(t) => (ast::Cat::new(fst, punct, t.convert(context)?).into(), None),
            };
        }

        Some(out.0)
    }
}

impl Convert for Alt {
    fn convert(self, context: &mut Context) -> Option<ast::Expression> {
        let mut iter = self.0.into_pairs();
        let mut out = match iter.next().unwrap() {
            Pair::Punctuated(t, p) => (t.convert(context)?, Some(p)),
            Pair::End(t) => (t.convert(context)?, None),
        };

        for pair in iter {
            let (fst, punct) = out;
            out = match pair {
                Pair::Punctuated(t, p) => (
                    ast::Alt::new(fst, punct, t.convert(context)?).into(),
                    Some(p),
                ),
                Pair::End(t) => (ast::Alt::new(fst, punct, t.convert(context)?).into(), None),
            };
        }

        Some(out.0)
    }
}
