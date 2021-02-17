use proc_macro2::Span;

use crate::chomp::{
    visit::{Folder, Mapper, Visitable},
    Name,
};

use super::{
    error::SubstituteError, Alt, Call, Cat, Epsilon, Fix, Function, Literal, NamedExpression,
    Parameter, Variable,
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

    fn map_variable(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        var: &mut Variable,
    ) -> Self::Out {
        if var.index >= self.depth {
            var.index += 1;
        }
    }

    fn map_parameter(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        _param: &mut Parameter,
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

    fn map_variable(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        var: &mut Variable,
    ) -> Self::Out {
        if var.index >= self.depth {
            var.index -= 1;
        }
    }

    fn map_parameter(
        &mut self,
        _name: &mut Option<Name>,
        _span: Option<Span>,
        _param: &mut Parameter,
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

    fn fold_parameter(
        &mut self,
        name: Option<Name>,
        span: Option<Span>,
        param: Parameter,
    ) -> Self::Out {
        let mut expr = self
            .params
            .get(param.index)
            .cloned()
            .ok_or_else(|| SubstituteError::FreeParameter { param, span, name: name.clone() })?;
        expr.name = expr.name.or(name);
        expr.span = expr.span.or(span);
        Ok(expr)
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
}

#[derive(Clone, Debug)]
pub struct InlineCalls {
    pub function: Function,
}

impl Folder for InlineCalls {
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
            .map(|(punct, term)| Ok((punct, term.fold(self)?)))
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
            .map(|(punct, term)| Ok((punct, term.fold(self)?)))
            .collect::<Result<_, _>>()?;

        Ok(NamedExpression {
            name,
            expr: alt.into(),
            span,
        })
    }

    fn fold_fix(&mut self, name: Option<Name>, span: Option<Span>, mut fix: Fix) -> Self::Out {
        fix.inner = Box::new(fix.inner.fold(self)?);

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

    fn fold_call(&mut self, name: Option<Name>, span: Option<Span>, mut call: Call) -> Self::Out {
        call.args = call
            .args
            .into_iter()
            .map(|arg| arg.fold(self))
            .collect::<Result<_, _>>()?;

        if call.name != self.function.name {
            Ok(NamedExpression {
                name,
                expr: call.into(),
                span,
            })
        } else if call.args.len() == self.function.params.len() {
            let mut expr = self
                .function
                .expr
                .clone()
                .fold(&mut SubstituteParams { params: call.args })?;
            expr.name = Some(name.or(expr.name).unwrap_or(call.name));
            expr.span = span.or(expr.span);

            Ok(expr)
        } else {
            Err(SubstituteError::WrongArgCount {
                call,
                expected: self.function.params.len(),
                span,
            })
        }
    }
}
