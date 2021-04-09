use proc_macro2::Span;

use crate::chomp::{
    visit::{Folder, Mapper, Visitable},
    Name,
};

use super::{
    error::SubstituteError, Alt, Call, Cat, Epsilon, Expression, Fix, Function, Global, Literal,
    NamedExpression, Parameter,
};

#[derive(Clone, Copy, Debug, Default)]
pub struct DeepenVars {
    pub depth: usize,
}

impl Mapper for DeepenVars {
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
        cat.second.map(self);

        for (_, term) in &mut cat.rest {
            term.map(self);
        }
    }

    fn map_alt(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        alt: &mut Alt,
    ) -> Self::Out {
        alt.first.map(self);
        alt.second.map(self);

        for (_, term) in &mut alt.rest {
            term.map(self);
        }
    }

    fn map_fix(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        fix: &mut Fix,
    ) -> Self::Out {
        self.depth += 1;
        fix.inner.map(self);
        self.depth -= 1;
    }

    fn map_parameter(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        _param: &mut Parameter,
    ) -> Self::Out {
        todo!()
    }

    fn map_global(
        &mut self,
        name: &mut Option<Name>,
        span: Option<Span>,
        global: &mut Global,
    ) -> Self::Out {
    }

    fn map_call(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        call: &mut Call,
    ) -> Self::Out {
        for arg in &mut call.args {
            arg.map(self)
        }
    }

    fn map_function(
        &mut self,
        name: &mut Option<Name>,
        span: Option<Span>,
        fun: &mut Function,
    ) -> Self::Out {
        todo!()
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct ShallowVars {
    pub depth: usize,
}

impl Mapper for ShallowVars {
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
        cat.second.map(self);

        for (_, term) in &mut cat.rest {
            term.map(self);
        }
    }

    fn map_alt(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        alt: &mut Alt,
    ) -> Self::Out {
        alt.first.map(self);
        alt.second.map(self);

        for (_, term) in &mut alt.rest {
            term.map(self);
        }
    }

    fn map_fix(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        fix: &mut Fix,
    ) -> Self::Out {
        self.depth += 1;
        fix.inner.map(self);
        self.depth -= 1;
    }

    fn map_parameter(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        _param: &mut Parameter,
    ) -> Self::Out {
        todo!()
    }

    fn map_global(
        &mut self,
        name: &mut Option<Name>,
        span: Option<Span>,
        global: &mut Global,
    ) -> Self::Out {
    }

    fn map_call(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        call: &mut Call,
    ) -> Self::Out {
        for arg in &mut call.args {
            arg.map(self)
        }
    }

    fn map_function(
        &mut self,
        name: &mut Option<Name>,
        span: Option<Span>,
        fun: &mut Function,
    ) -> Self::Out {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub struct SubstituteParams {
    pub params: Vec<NamedExpression>,
}

impl Folder for SubstituteParams {
    type Out = Result<NamedExpression, SubstituteError>;

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

    fn fold_cat(&mut self, name: Option<Name>, span: Option<Span>, mut cat: Cat) -> Self::Out {
        cat.first = Box::new(cat.first.fold(self)?);
        cat.second = Box::new(cat.second.fold(self)?);
        cat.rest = cat
            .rest
            .into_iter()
            .map(|(p, term)| Ok((p, term.fold(self)?)))
            .collect::<Result<_, _>>()?;
        Ok(NamedExpression {
            name,
            expr: cat.into(),
            span,
        })
    }

    fn fold_alt(&mut self, name: Option<Name>, span: Option<Span>, mut alt: Alt) -> Self::Out {
        alt.first = Box::new(alt.first.fold(self)?);
        alt.second = Box::new(alt.second.fold(self)?);
        alt.rest = alt
            .rest
            .into_iter()
            .map(|(p, term)| Ok((p, term.fold(self)?)))
            .collect::<Result<_, _>>()?;
        Ok(NamedExpression {
            name,
            expr: alt.into(),
            span,
        })
    }

    fn fold_fix(&mut self, name: Option<Name>, span: Option<Span>, mut fix: Fix) -> Self::Out {
        for param in &mut self.params {
            param.map(&mut DeepenVars::default());
        }

        fix.inner = Box::new(fix.inner.fold(self)?);

        for param in &mut self.params {
            param.map(&mut ShallowVars::default());
        }

        Ok(NamedExpression {
            name,
            expr: fix.into(),
            span,
        })
    }

    fn fold_parameter(
        &mut self,
        name: Option<Name>,
        span: Option<Span>,
        param: Parameter,
    ) -> Self::Out {
        let mut expr = self.params.get(param.index).cloned().ok_or_else(|| {
            SubstituteError::FreeParameter {
                param,
                span,
                name: name.clone(),
            }
        })?;
        expr.name = expr.name.or(name);
        expr.span = expr.span.or(span);
        Ok(expr)
    }

    fn fold_global(&mut self, name: Option<Name>, span: Option<Span>, global: Global) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: global.into(),
            span,
        })
    }

    fn fold_call(&mut self, name: Option<Name>, span: Option<Span>, mut call: Call) -> Self::Out {
        call.args = call
            .args
            .into_iter()
            .map(|arg| arg.fold(self))
            .collect::<Result<_, _>>()?;
        Ok(NamedExpression {
            name,
            expr: call.into(),
            span,
        })
    }

    fn fold_function(
        &mut self,
        name: Option<Name>,
        span: Option<Span>,
        fun: Function,
    ) -> Self::Out {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub struct InlineGlobal {
    pub expr: Expression,
    pub name: Name,
    pub span: Option<Span>,
}

impl Folder for InlineGlobal {
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
        cat.second = Box::new(cat.second.fold(self));
        cat.rest = cat
            .rest
            .into_iter()
            .map(|(punct, term)| (punct, term.fold(self)))
            .collect();

        NamedExpression {
            name,
            expr: cat.into(),
            span,
        }
    }

    fn fold_alt(&mut self, name: Option<Name>, span: Option<Span>, mut alt: Alt) -> Self::Out {
        alt.first = Box::new(alt.first.fold(self));
        alt.second = Box::new(alt.second.fold(self));
        alt.rest = alt
            .rest
            .into_iter()
            .map(|(punct, term)| (punct, term.fold(self)))
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

    fn fold_parameter(
        &mut self,
        name: Option<Name>,
        span: Option<Span>,
        param: Parameter,
    ) -> Self::Out {
        NamedExpression {
            name,
            expr: param.into(),
            span,
        }
    }

    fn fold_global(&mut self, name: Option<Name>, span: Option<Span>, global: Global) -> Self::Out {
        if global.name == self.name {
            NamedExpression {
                name: Some(name.unwrap_or(self.name.clone())),
                expr: self.expr.clone(),
                span: span.or(self.span),
            }
        } else {
            NamedExpression {
                name,
                expr: global.into(),
                span,
            }
        }
    }

    fn fold_call(&mut self, name: Option<Name>, span: Option<Span>, mut call: Call) -> Self::Out {
        call.fun = Box::new(call.fun.fold(self));
        call.args = call.args.into_iter().map(|arg| arg.fold(self)).collect();

        NamedExpression {
            name,
            expr: call.into(),
            span,
        }
    }

    fn fold_function(
        &mut self,
        name: Option<Name>,
        span: Option<Span>,
        fun: Function,
    ) -> Self::Out {
        fun.expr = Box::new(fun.expr.fold(self));
        NamedExpression {
            name,
            expr: fun.into(),
            span,
        }
    }
}

pub struct ExpandCalls {
    pub changed: bool,
}

impl ExpandCalls {
    fn fold_until_done(
        &mut self,
        mut term: NamedExpression,
    ) -> Result<NamedExpression, SubstituteError> {
        let last = self.changed;
        self.changed = true;
        while self.changed {
            self.changed = false;
            term = term.fold(self)?
        }
        self.changed = last;
        Ok(term)
    }
}

impl Folder for ExpandCalls {
    type Out = Result<NamedExpression, SubstituteError>;

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
        cat.first = Box::new(self.fold_until_done(*cat.first)?);
        cat.second = Box::new(self.fold_until_done(*cat.second)?);
        cat.rest = cat
            .rest
            .into_iter()
            .map(|(punct, term)| Ok((punct, self.fold_until_done(term)?)))
            .collect::<Result<_, _>>()?;

        Ok(NamedExpression {
            name,
            expr: cat.into(),
            span,
        })
    }

    fn fold_alt(&mut self, name: Option<Name>, span: Option<Span>, alt: Alt) -> Self::Out {
        alt.first = Box::new(self.fold_until_done(*alt.first)?);
        alt.second = Box::new(self.fold_until_done(*alt.second)?);
        alt.rest = alt
            .rest
            .into_iter()
            .map(|(punct, term)| Ok((punct, self.fold_until_done(term)?)))
            .collect::<Result<_, _>>()?;

        Ok(NamedExpression {
            name,
            expr: alt.into(),
            span,
        })
    }

    fn fold_fix(&mut self, name: Option<Name>, span: Option<Span>, fix: Fix) -> Self::Out {
        fix.inner = Box::new(self.fold_until_done(*fix.inner)?);
        Ok(NamedExpression {
            name,
            expr: fix.into(),
            span,
        })
    }

    fn fold_parameter(
        &mut self,
        name: Option<Name>,
        span: Option<Span>,
        param: Parameter,
    ) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: param.into(),
            span,
        })
    }

    fn fold_global(&mut self, name: Option<Name>, span: Option<Span>, global: Global) -> Self::Out {
        Ok(NamedExpression {
            name,
            expr: global.into(),
            span,
        })
    }

    fn fold_call(&mut self, name: Option<Name>, span: Option<Span>, call: Call) -> Self::Out {
        let fun = self.fold_until_done(*call.fun)?;
        let args = call
            .args
            .into_iter()
            .map(|arg| self.fold_until_done(arg))
            .collect::<Result<Vec<_>, _>>()?;

        if let Expression::Function(f) = fun.expr {
            if f.args.len() == args.len() {
                let mut out = f.expr.fold(&mut SubstituteParams { params: args })?;
                self.changed = true;
                out.name = name.or(out.name);
                out.span = span.or(out.span);
                Ok(out)
            } else {
                Err(SubstituteError::WrongArgCount {
                    call: Call {
                        fun: Box::new(fun),
                        args,
                    },
                    expected: f.args.len(),
                    span,
                })
            }
        } else {
            Ok(NamedExpression {
                name,
                expr: Call {
                    fun: Box::new(fun),
                    args,
                }
                .into(),
                span,
            })
        }
    }

    fn fold_function(
        &mut self,
        name: Option<Name>,
        span: Option<Span>,
        fun: Function,
    ) -> Self::Out {
        fun.expr = Box::new(self.fold_until_done(*fun.expr)?);
        Ok(NamedExpression {
            name,
            expr: fun.into(),
            span,
        })
    }
}
