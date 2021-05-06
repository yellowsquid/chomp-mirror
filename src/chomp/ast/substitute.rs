use std::array;

use super::{
    error::ReductionError, Alt, Call, Cat, Epsilon, Expression, Fix, Lambda, Let, Literal,
    NamedExpression, Variable,
};
use crate::chomp::{
    name::Name,
    visit::{Folder, Mapper, Visitable},
};
use proc_macro2::Span;

enum Direction {
    Deepen,
    Shallow,
}
struct RenameVars {
    depth: usize,
    by: usize,
    direction: Direction,
}

impl Mapper for RenameVars {
    type Out = ();

    fn map_epsilon(
        &mut self,
        _name: &mut Option<Name>,
        _span: Span,
        _eps: &mut Epsilon,
    ) -> Self::Out {
    }

    fn map_literal(
        &mut self,
        _name: &mut Option<Name>,
        _span: Span,
        _lit: &mut Literal,
    ) -> Self::Out {
    }

    fn map_cat(
        &mut self,
        _name: &mut Option<Name>,
        _span: Span,
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
        _span: Span,
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
        _span: Span,
        fix: &mut Fix,
    ) -> Self::Out {
        fix.inner.map(self);
    }

    fn map_variable(
        &mut self,
        _name: &mut Option<Name>,
        _span: Span,
        var: &mut Variable,
    ) -> Self::Out {
        if var.index >= self.depth {
            match self.direction {
                Direction::Deepen => var.index += self.by,
                Direction::Shallow => var.index -= self.by,
            }
        }
    }

    fn map_call(
        &mut self,
        _name: &mut Option<Name>,
        _span: Span,
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
        _span: Span,
        lambda: &mut Lambda,
    ) -> Self::Out {
        self.depth += lambda.args.len();
        lambda.inner.map(self);
        self.depth -= lambda.args.len();
    }

    fn map_let(
        &mut self,
        _name: &mut Option<Name>,
        _span: Span,
        stmt: &mut Let,
    ) -> Self::Out {
        stmt.bound.map(self);
        self.depth += 1;
        stmt.body.map(self);
        self.depth -= 1;
    }
}

#[derive(Debug)]
pub struct Substitute {
    pub index: usize,
    pub expr: NamedExpression,
}

impl Substitute {
    fn with_args<V: Visitable>(&mut self, args: usize, visitable: V) -> <Self as Folder>::Out {
        self.index += args;
        self.expr.map(&mut RenameVars {
            depth: 0,
            by: args,
            direction: Direction::Deepen,
        });
        let out = visitable.fold(self);
        self.expr.map(&mut RenameVars {
            depth: 0,
            by: args,
            direction: Direction::Shallow,
        });
        self.index -= args;
        out
    }
}

impl Folder for Substitute {
    type Out = NamedExpression;

    fn fold_epsilon(&mut self, name: Option<Name>, span: Span, eps: Epsilon) -> Self::Out {
        NamedExpression {
            name,
            expr: eps.into(),
            span,
        }
    }

    fn fold_literal(&mut self, name: Option<Name>, span: Span, lit: Literal) -> Self::Out {
        NamedExpression {
            name,
            expr: lit.into(),
            span,
        }
    }

    fn fold_cat(&mut self, name: Option<Name>, span: Span, mut cat: Cat) -> Self::Out {
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

    fn fold_alt(&mut self, name: Option<Name>, span: Span, mut alt: Alt) -> Self::Out {
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

    fn fold_fix(&mut self, name: Option<Name>, span: Span, mut fix: Fix) -> Self::Out {
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
        span: Span,
        var: Variable,
    ) -> Self::Out {
        if var.index == self.index {
            let mut out = self.expr.clone();
            out.name = Name::merge(out.name, name);
            out
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

    fn fold_call(&mut self, name: Option<Name>, span: Span, mut call: Call) -> Self::Out {
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
        span: Span,
        mut lambda: Lambda,
    ) -> Self::Out {
        let depth = lambda.args.len();
        lambda.inner = Box::new(self.with_args(depth, lambda.inner));
        NamedExpression {
            name,
            expr: lambda.into(),
            span,
        }
    }

    fn fold_let(&mut self, name: Option<Name>, span: Span, mut stmt: Let) -> Self::Out {
        stmt.bound = Box::new(stmt.bound.fold(self));
        stmt.body = Box::new(self.with_args(1, stmt.body));
        NamedExpression {
            name,
            expr: stmt.into(),
            span,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Reduce;

impl Folder for Reduce {
    type Out = Result<NamedExpression, ReductionError>;

    fn fold_epsilon(&mut self, name: Option<Name>, span: Span, eps: Epsilon) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: eps.into(),
            span,
        })
    }

    fn fold_literal(&mut self, name: Option<Name>, span: Span, lit: Literal) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: lit.into(),
            span,
        })
    }

    fn fold_cat(&mut self, name: Option<Name>, span: Span, mut cat: Cat) -> Self::Out {
        cat.first = Box::new(cat.first.fold(self)?);
        cat.rest = cat
            .rest
            .into_iter()
            .map(|(p, e)| Ok((p, e.fold(self)?)))
            .collect::<Result<_, _>>()?;
        Ok(NamedExpression {
            name,
            expr: cat.into(),
            span,
        })
    }

    fn fold_alt(&mut self, name: Option<Name>, span: Span, mut alt: Alt) -> Self::Out {
        alt.first = Box::new(alt.first.fold(self)?);
        alt.rest = alt
            .rest
            .into_iter()
            .map(|(p, e)| Ok((p, e.fold(self)?)))
            .collect::<Result<_, _>>()?;
        Ok(NamedExpression {
            name,
            expr: alt.into(),
            span,
        })
    }

    fn fold_fix(&mut self, name: Option<Name>, span: Span, mut fix: Fix) -> Self::Out {
        let mut inner = fix.inner.fold(self)?;
        if let Expression::Lambda(mut l) = inner.expr {
            l.inner = Box::new(l.inner.fold(self)?);
            inner.expr = Expression::Lambda(l);
        }
        fix.inner = Box::new(inner);
        Ok(NamedExpression {
            name,
            expr: fix.into(),
            span,
        })
    }

    fn fold_variable(
        &mut self,
        name: Option<Name>,
        span: Span,
        var: Variable,
    ) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: var.into(),
            span,
        })
    }

    fn fold_call(&mut self, name: Option<Name>, span: Span, call: Call) -> Self::Out {
        let on = call.on.fold(self)?;

        let lambda = on
            .expr
            .try_into_lambda()
            .map_err(|on| ReductionError::CallNotAFunction { on, span })?;

        if lambda.args.len() != call.args.len() {
            return Err(ReductionError::WrongArgCount {
                lambda,
                args: call.args,
                span,
            });
        }

        let mut out = *lambda.inner;

        for ((i, mut arg), name) in call.args.into_iter().enumerate().zip(lambda.args).rev() {
            arg.name = Name::merge(arg.name, Some(name));
            arg.map(&mut RenameVars {
                depth: 0,
                by: i,
                direction: Direction::Deepen,
            });
            out = out.fold(&mut Substitute {
                index: 0,
                expr: arg,
            });
        }

        out = out.fold(self)?;
        out.name = Name::merge_all(array::IntoIter::new([name, on.name, out.name]));
        out.span = span;
        Ok(out)
    }

    fn fold_lambda(&mut self, name: Option<Name>, span: Span, lambda: Lambda) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: lambda.into(),
            span,
        })
    }

    fn fold_let(&mut self, name: Option<Name>, span: Span, stmt: Let) -> Self::Out {
        let mut out = stmt
            .body
            .fold(&mut Substitute {
                index: 0,
                expr: *stmt.bound,
            })
            .fold(self)?;
        out.name = Name::merge(out.name, name);
        out.span = span;
        Ok(out)
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
            span: Span::call_site(),
        };

        let lambda = NamedExpression {
            name: None,
            expr: Lambda {
                args: vec![Name::new_variable("x".to_owned())],
                inner: Box::new(var),
            }
            .into(),
            span: Span::call_site(),
        };

        let var = NamedExpression {
            name: None,
            expr: Variable { index }.into(),
            span: Span::call_site(),
        };

        NamedExpression {
            name: None,
            expr: Call {
                on: Box::new(lambda),
                args: vec![var],
            }
            .into(),
            span: Span::call_site(),
        }
    }

    #[test]
    fn deepen_lambda() {
        let mut expr = make_expr(0);
        expr.map(&mut RenameVars {
            depth: 0,
            by: 1,
            direction: Direction::Deepen,
        });
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
                span: Span::call_site(),
            },
        });
        assert_eq!(expr, make_expr(0))
    }

    #[test]
    fn reduce_lambda() {
        let expr = make_expr(1);
        let expr = expr.fold(&mut Reduce).unwrap();
        assert_eq!(
            expr,
            NamedExpression {
                name: Some(Name::new_variable("x".to_owned())),
                expr: Variable { index: 1 }.into(),
                span: Span::call_site(),
            }
        )
    }
}
