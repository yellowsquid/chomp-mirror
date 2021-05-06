use std::{array, iter};

use proc_macro2::Span;

use crate::chomp::{
    ast::{Alt, Call, Cat, Epsilon, Expression, Fix, Lambda, Let, Literal, Variable},
    name::Name,
    visit::{Folder, Visitable},
};

use super::{
    context::Context,
    error::{AltError, CatError, TypeError, VariableError},
    Type, Typed, TypedExpression,
};

#[derive(Debug)]
pub struct TypeInfer<'a> {
    pub context: &'a mut Context,
}

impl Folder for TypeInfer<'_> {
    type Out = Result<TypedExpression, TypeError>;

    fn fold_epsilon(&mut self, name: Option<Name>, span: Span, eps: Epsilon) -> Self::Out {
        Ok(TypedExpression {
            inner: super::Epsilon::from(eps).into(),
            name,
            span,
        })
    }

    fn fold_literal(&mut self, name: Option<Name>, span: Span, lit: Literal) -> Self::Out {
        Ok(TypedExpression {
            inner: super::Literal::from(lit).into(),
            name,
            span,
        })
    }

    fn fold_cat(&mut self, name: Option<Name>, span: Span, cat: Cat) -> Self::Out {
        let first = cat.first.fold(self)?;

        if first.get_type().nullable() {
            let punct = cat.rest.into_iter().next().map(|(p, _)| p).unwrap_or_else(Span::call_site);
            return Err(CatError::FirstNullable { expr: first, punct }.into());
        }

        let rest = cat.rest;
        let mut ty = first.get_type().clone();
        let terms = self.context.with_unguard(|context| {
            let mut infer = TypeInfer { context };
            rest.into_iter()
                .map(|(punct, right)| {
                    let right = right.fold(&mut infer)?;
                    if ty.flast_set().disjoint(right.get_type().first_set()) {
                        ty.cat(right.get_type().clone());
                        Ok(right)
                    } else {
                        Err(CatError::FirstFlastOverlap {
                            front_ty: ty.clone(),
                            punct,
                            next: right,
                        }
                        .into())
                    }
                })
                .collect::<Result<Vec<_>, TypeError>>()
        })?;
        Ok(TypedExpression {
            inner: super::Cat {
                terms: iter::once(first).chain(terms).collect(),
                ty,
            }
            .into(),
            name,
            span,
        })
    }

    fn fold_alt(&mut self, name: Option<Name>, span: Span, alt: Alt) -> Self::Out {
        let first = alt.first.fold(self)?;
        let mut ty = first.get_type().clone();
        let terms = alt
            .rest
            .into_iter()
            .map(|(punct, right)| {
                let right = right.fold(self)?;
                if ty.nullable() && right.get_type().nullable() {
                    Err(AltError::BothNullable {
                        left_ty: ty.clone(),
                        punct,
                        right,
                    }
                    .into())
                } else if ty.first_set().disjoint(right.get_type().first_set()) {
                    ty.alt(right.get_type().clone());
                    Ok(right)
                } else {
                    Err(AltError::FirstOverlap {
                        left_ty: ty.clone(),
                        punct,
                        right,
                    }
                    .into())
                }
            })
            .collect::<Result<Vec<_>, TypeError>>()?;
        Ok(TypedExpression {
            inner: super::Alt {
                terms: iter::once(first).chain(terms).collect(),
                ty,
            }
            .into(),
            name,
            span,
        })
    }

    fn fold_fix(&mut self, name: Option<Name>, span: Span, fix: Fix) -> Self::Out {
        let mut ty = Type::default();
        let body = if let Expression::Lambda(l) = fix.inner.expr {
            let mut body = *l.inner;
            body.name = Name::merge_all(array::IntoIter::new([fix.inner.name, body.name, name.clone()]));
            body
        } else {
            return Err(TypeError::ExpectedLambda { span, fix });
        };

        loop {
            let last = ty;
            let res = self.context.with_variable_type(last.clone(), |context| {
                body.clone().fold(&mut TypeInfer { context })
            })?;
            ty = res.get_type().clone();

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

    fn fold_variable(
        &mut self,
        name: Option<Name>,
        span: Span,
        var: Variable,
    ) -> Self::Out {
        let ty = match self.context.get_variable_type(var) {
            Ok(ty) => ty.clone(),
            Err(inner) => {
                return Err(VariableError {
                    inner,
                    var,
                    span,
                    name,
                }
                .into())
            }
        };
        Ok(TypedExpression {
            inner: super::Variable { inner: var, ty }.into(),
            name,
            span,
        })
    }

    fn fold_call(&mut self, _name: Option<Name>, span: Span, call: Call) -> Self::Out {
        Err(TypeError::UnexpectedCall { span, call })
    }

    fn fold_lambda(
        &mut self,
        _name: Option<Name>,
        span: Span,
        lambda: Lambda,
    ) -> Self::Out {
        Err(TypeError::UnexpectedLambda { span, lambda })
    }

    fn fold_let(&mut self, _name: Option<Name>, span: Span, stmt: Let) -> Self::Out {
        Err(TypeError::UnexpectedLet { span, stmt })
    }
}

#[cfg(test)]
mod tests {
    use crate::chomp::{ast::NamedExpression, typed::RawTypedExpression};

    use super::*;

    #[test]
    fn cat_uses_all() {
        let ast = Cat {
            first: Box::new(NamedExpression {
                name: None,
                expr: "a".to_owned().into(),
                span: Span::call_site(),
            }),
            rest: vec![(
                Span::call_site(),
                NamedExpression {
                    name: None,
                    expr: "b".to_owned().into(),
                    span: Span::call_site(),
                },
            )],
        };

        let typed = NamedExpression {
            name: None,
            expr: ast.into(),
            span: Span::call_site(),
        }
        .fold(&mut TypeInfer {
            context: &mut Context::default(),
        })
        .unwrap();
        match typed.inner {
            RawTypedExpression::Cat(super::super::Cat { terms, .. }) => assert_eq!(terms.len(), 2),
            RawTypedExpression::Epsilon(_)
            | RawTypedExpression::Literal(_)
            | RawTypedExpression::Alt(_)
            | RawTypedExpression::Fix(_)
            | RawTypedExpression::Variable(_) => panic!("Cat should type check to Cat"),
        };
    }

    #[test]
    fn alt_uses_all() {
        let ast = Alt {
            first: Box::new(NamedExpression {
                name: None,
                expr: "a".to_owned().into(),
                span: Span::call_site(),
            }),
            rest: vec![(
                Span::call_site(),
                NamedExpression {
                    name: None,
                    expr: "b".to_owned().into(),
                    span: Span::call_site(),
                },
            )],
        };

        let typed = NamedExpression {
            name: None,
            expr: ast.into(),
            span: Span::call_site(),
        }
        .fold(&mut TypeInfer {
            context: &mut Context::default(),
        })
        .unwrap();
        match typed.inner {
            RawTypedExpression::Alt(super::super::Alt { terms, .. }) => assert_eq!(terms.len(), 2),
            RawTypedExpression::Epsilon(_)
            | RawTypedExpression::Literal(_)
            | RawTypedExpression::Cat(_)
            | RawTypedExpression::Fix(_)
            | RawTypedExpression::Variable(_) => panic!("Alt should type check to Alt"),
        };
    }
}
