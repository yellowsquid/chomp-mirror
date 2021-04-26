use super::{
    error::TranslateError, Alt, Call, Cat, Epsilon, Fix, Lambda, Literal, NamedExpression, Variable,
};
use crate::chomp::{
    visit::{Folder, Mapper, Visitable},
    Name,
};
use proc_macro2::Span;

struct Deepen {
    pub depth: usize,
    pub by: usize,
}

impl Mapper for Deepen {
    type Out = ();

    fn map_epsilon(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        _eps: &mut Epsilon,
    ) -> Self::Out {
    }

    fn map_literal(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        _lit: &mut Literal,
    ) -> Self::Out {
    }

    fn map_cat(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        cat: &mut Cat,
    ) -> Self::Out {
        cat.first.map(self);
        for (_, other) in &mut cat.rest {
            other.map(self);
        }
    }

    fn map_alt(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        alt: &mut Alt,
    ) -> Self::Out {
        alt.first.map(self);
        for (_, other) in &mut alt.rest {
            other.map(self);
        }
    }

    fn map_fix(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        fix: &mut Fix,
    ) -> Self::Out {
        fix.inner.map(self);
    }

    fn map_variable(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        var: &mut Variable,
    ) -> Self::Out {
        if var.index >= self.depth {
            var.index += self.by;
        }
    }

    fn map_call(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        call: &mut Call,
    ) -> Self::Out {
        call.on.map(self);
        for arg in &mut call.args {
            arg.map(self);
        }
    }

    fn map_lambda(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        lambda: &mut Lambda,
    ) -> Self::Out {
        self.depth += lambda.args.len();
        lambda.inner.map(self);
        self.depth -= lambda.args.len();
    }
}

pub struct Substitute {
    pub index: usize,
    pub expr: NamedExpression,
}

impl Folder for Substitute {
    type Out = NamedExpression;

    fn fold_epsilon(&mut self, name: Option<Name>, span: Option<Span>, eps: Epsilon) -> Self::Out {
        NamedExpression {
            name,
            expr: eps.into(),
            span,
        }
    }

    fn fold_literal(&mut self, name: Option<Name>, span: Option<Span>, lit: Literal) -> Self::Out {
        NamedExpression {
            name,
            expr: lit.into(),
            span,
        }
    }

    fn fold_cat(&mut self, name: Option<Name>, span: Option<Span>, mut cat: Cat) -> Self::Out {
        cat.first = Box::new(cat.first.fold(self));
        cat.rest = cat
            .rest
            .into_iter()
            .map(|(span, e)| (span, e.fold(self)))
            .collect();
        NamedExpression {
            name,
            expr: cat.into(),
            span,
        }
    }

    fn fold_alt(&mut self, name: Option<Name>, span: Option<Span>, mut alt: Alt) -> Self::Out {
        alt.first = Box::new(alt.first.fold(self));
        alt.rest = alt
            .rest
            .into_iter()
            .map(|(span, e)| (span, e.fold(self)))
            .collect();
        NamedExpression {
            name,
            expr: alt.into(),
            span,
        }
    }

    fn fold_fix(&mut self, name: Option<Name>, span: Option<Span>, mut fix: Fix) -> Self::Out {
        fix.inner = Box::new(fix.inner.fold(self));
        NamedExpression {
            name,
            expr: fix.into(),
            span,
        }
    }

    fn fold_variable(
        &mut self,
        name: Option<Name>,
        span: Option<Span>,
        var: Variable,
    ) -> Self::Out {
        if var.index == self.index {
            self.expr.clone()
        } else {
            NamedExpression {
                name,
                expr: Variable {
                    index: if var.index > self.index {
                        var.index - 1
                    } else {
                        var.index
                    },
                }
                .into(),
                span,
            }
        }
    }

    fn fold_call(&mut self, name: Option<Name>, span: Option<Span>, mut call: Call) -> Self::Out {
        call.on = Box::new(call.on.fold(self));
        call.args = call.args.into_iter().map(|e| e.fold(self)).collect();
        NamedExpression {
            name,
            expr: call.into(),
            span,
        }
    }

    fn fold_lambda(
        &mut self,
        name: Option<Name>,
        span: Option<Span>,
        mut lambda: Lambda,
    ) -> Self::Out {
        self.index += lambda.args.len();
        lambda.inner = Box::new(lambda.inner.fold(self));
        self.index -= lambda.args.len();
        NamedExpression {
            name,
            expr: lambda.into(),
            span,
        }
    }
}

pub struct Translate {
    changed: bool,
}

impl Translate {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl Default for Translate {
    fn default() -> Self {
        Self::new()
    }
}

impl Folder for Translate {
    type Out = Result<NamedExpression, TranslateError>;

    fn fold_epsilon(&mut self, name: Option<Name>, span: Option<Span>, eps: Epsilon) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: eps.into(),
            span,
        })
    }

    fn fold_literal(&mut self, name: Option<Name>, span: Option<Span>, lit: Literal) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: lit.into(),
            span,
        })
    }

    fn fold_cat(&mut self, name: Option<Name>, span: Option<Span>, cat: Cat) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: cat.into(),
            span,
        })
    }

    fn fold_alt(&mut self, name: Option<Name>, span: Option<Span>, alt: Alt) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: alt.into(),
            span,
        })
    }

    fn fold_fix(&mut self, name: Option<Name>, span: Option<Span>, fix: Fix) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: fix.into(),
            span,
        })
    }

    fn fold_variable(
        &mut self,
        name: Option<Name>,
        span: Option<Span>,
        var: Variable,
    ) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: var.into(),
            span,
        })
    }

    fn fold_call(&mut self, name: Option<Name>, span: Option<Span>, call: Call) -> Self::Out {
        let mut on = *call.on;
        self.changed = true;
        while self.changed {
            self.changed = false;
            on = on.fold(self)?;
        }

        let lambda = on
            .expr
            .try_into_lambda()
            .map_err(|on| TranslateError::CallNotAFunction { on, span })?;

        if lambda.args.len() != call.args.len() {
            return Err(TranslateError::WrongArgCount {
                lambda,
                args: call.args,
                span,
            });
        }

        let mut out = *lambda.inner;

        for ((i, mut arg), name) in call.args.into_iter().enumerate().zip(lambda.args).rev() {
            arg.name = arg.name.or(Some(name));
            arg.map(&mut Deepen { depth: 0, by: i });
            out = out.fold(&mut Substitute {
                index: 0,
                expr: arg,
            });
        }

        self.changed = true;
        while self.changed {
            self.changed = false;
            out = out.fold(self)?;
        }

        self.changed = true;
        out.name = name.or(out.name);
        out.span = span.or(out.span);
        Ok(out)
    }

    fn fold_lambda(&mut self, name: Option<Name>, span: Option<Span>, lambda: Lambda) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: lambda.into(),
            span,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Returns (/x/ x) 'index
    fn make_expr(index: usize) -> NamedExpression {
        let var = NamedExpression {
            name: None,
            expr: Variable { index: 0 }.into(),
            span: None,
        };

        let lambda = NamedExpression {
            name: None,
            expr: Lambda {
                args: vec!["x".to_owned().into()],
                inner: Box::new(var),
            }
            .into(),
            span: None,
        };

        let var = NamedExpression {
            name: None,
            expr: Variable { index }.into(),
            span: None,
        };

        NamedExpression {
            name: None,
            expr: Call {
                on: Box::new(lambda),
                args: vec![var],
            }
            .into(),
            span: None,
        }
    }

    #[test]
    fn deepen_lambda() {
        let mut expr = make_expr(0);
        expr.map(&mut Deepen { depth: 0, by: 1 });
        assert_eq!(expr, make_expr(1))
    }

    #[test]
    fn substitute_renames_bigger() {
        let expr = make_expr(1);
        let expr = expr.fold(&mut Substitute {
            index: 0,
            expr: NamedExpression {
                name: None,
                expr: Epsilon.into(),
                span: None,
            },
        });
        assert_eq!(expr, make_expr(0))
    }

    #[test]
    fn translate_lambda() {
        let expr = make_expr(1);
        let expr = expr.fold(&mut Translate::new()).unwrap();
        assert_eq!(
            expr,
            NamedExpression {
                name: Some("x".to_owned().into()),
                expr: Variable { index: 1 }.into(),
                span: None
            }
        )
    }
}
