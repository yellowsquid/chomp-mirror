use proc_macro2::Span;

use crate::chomp::{
    ast::{Alt, Call, Cat, Epsilon, Fix, Function, Global, Literal, Parameter},
    visit::{Folder, Visitable},
    Name,
};

use super::{
    context::Context,
    error::{TypeError, VariableError},
    GroundType, Typed, TypedExpression,
};

pub struct TypeInfer<'a> {
    pub context: &'a mut Context,
}

impl Folder for TypeInfer<'_> {
    type Out = Result<TypedExpression, TypeError>;

    fn fold_epsilon(&mut self, name: Option<Name>, span: Option<Span>, _eps: Epsilon) -> Self::Out {
        Ok(TypedExpression {
            inner: super::Epsilon.into(),
            name,
            span,
        })
    }

    fn fold_literal(&mut self, name: Option<Name>, span: Option<Span>, lit: Literal) -> Self::Out {
        Ok(TypedExpression {
            inner: super::Literal::from(lit).into(),
            name,
            span,
        })
    }

    fn fold_cat(&mut self, name: Option<Name>, span: Option<Span>, cat: Cat) -> Self::Out {
        let first = cat.first.fold(self)?;
        let second = cat.second;
        let rest = cat.rest;
        let punct = cat.punct;
        self.context
            .with_unguard(|context| -> Result<TypedExpression, TypeError> {
                let mut infer = TypeInfer { context };
                let second = second.fold(&mut infer)?;
                let rest = rest
                    .into_iter()
                    .map(|(punct, term)| -> Result<_, TypeError> {
                        Ok((punct, term.fold(&mut infer)?))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(TypedExpression {
                    inner: super::Cat::new(first, punct, second, rest)?.into(),
                    name,
                    span,
                })
            })
    }

    fn fold_alt(&mut self, name: Option<Name>, span: Option<Span>, alt: Alt) -> Self::Out {
        let first = alt.first.fold(self)?;
        let second = alt.second.fold(self)?;
        let rest = alt
            .rest
            .into_iter()
            .map(|(punct, term)| -> Result<_, TypeError> { Ok((punct, term.fold(self)?)) })
            .collect::<Result<Vec<_>, _>>()?;
        let punct = alt.punct;
        Ok(TypedExpression {
            inner: super::Alt::new(first, punct, second, rest)?.into(),
            name,
            span,
        })
    }

    fn fold_fix(&mut self, name: Option<Name>, span: Option<Span>, fix: Fix) -> Self::Out {
        let mut ty = GroundType::default();

        loop {
            let last = ty;
            let res = self
                .context
                .with_variable_type(last.clone().into(), |context| {
                    fix.inner.clone().fold(&mut TypeInfer { context })
                })?;

            ty = res.get_type().as_ground(span)?.clone();

            if last == ty {
                return Ok(TypedExpression {
                    inner: super::Fix {
                        inner: Box::new(res),
                    }
                    .into(),
                    name,
                    span,
                });
            }
        }
    }

    fn fold_parameter(
        &mut self,
        _name: Option<Name>,
        _span: Option<Span>,
        _param: Parameter,
    ) -> Self::Out {
        unimplemented!()
    }

    fn fold_global(&mut self, name: Option<Name>, span: Option<Span>, global: Global) -> Self::Out {
        todo!()
    }

    fn fold_call(&mut self, _name: Option<Name>, _span: Option<Span>, _call: Call) -> Self::Out {
        unimplemented!()
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
