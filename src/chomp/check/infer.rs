use super::super::{
    ast::{Alt, Call, Cat, Epsilon, Fix, Literal, Parameter, Variable},
    context::Context,
    error::{AltError, CatError, FixError, TypeError},
    set::{FirstSet, FlastSet},
    typed::Type,
    visit::{Visitable, Visitor},
};

#[derive(Debug)]
pub struct TypeInfer<'a> {
    pub context: &'a mut Context,
}

impl Visitor for TypeInfer<'_> {
    type Out = Result<Type, TypeError>;

    fn visit_epsilon(&mut self, _eps: &Epsilon) -> Self::Out {
        Ok(Type::new(true, FirstSet::default(), FlastSet::default()))
    }

    fn visit_literal(&mut self, lit: &Literal) -> Self::Out {
        Ok(Type::of_str(&lit.value()))
    }

    fn visit_cat(&mut self, cat: &Cat) -> Self::Out {
        let first = cat.first().visit(self)?;
        let second = self
            .context
            .with_unguard(|context| cat.second().visit(&mut TypeInfer { context }))?;

        if first.nullable() {
            Err(TypeError::Cat(CatError::FirstNullable(cat.clone())))
        } else if !first
            .flast_set()
            .intersect_first(second.first_set())
            .is_empty()
        {
            Err(TypeError::Cat(CatError::FirstFlastOverlap(cat.clone())))
        } else {
            Ok(first.cat(second))
        }
    }

    fn visit_alt(&mut self, alt: &Alt) -> Self::Out {
        let left = alt.left().visit(self)?;
        let right = alt.right().visit(self)?;

        if left.nullable() && right.nullable() {
            Err(TypeError::Alt(AltError::BothNullable(alt.clone())))
        } else if !left.first_set().intersect(right.first_set()).is_empty() {
            Err(TypeError::Alt(AltError::FirstOverlap(alt.clone())))
        } else {
            Ok(left.alt(right))
        }
    }

    fn visit_fix(&mut self, fix: &Fix) -> Self::Out {
        let mut res = Type::default();
        let mut last = None;

        while last.map(|r| r != res).unwrap_or(true) {
            last = Some(res);
            res = self
                .context
                .with_variable_type(last.as_ref().cloned().unwrap(), |context| {
                    fix.inner().visit(&mut TypeInfer { context })
                })
                .map_err(|e| {
                    TypeError::Fix(FixError(
                        fix.clone(),
                        last.as_ref().cloned().unwrap(),
                        Box::new(e),
                    ))
                })?;
        }

        Ok(res)
    }

    fn visit_variable(&mut self, var: &Variable) -> Self::Out {
        Ok(self.context.get_variable_type(&var)?.clone())
    }

    fn visit_parameter(&mut self, _param: &Parameter) -> Self::Out {
        todo!()
    }

    fn visit_call(&mut self, _call: &Call) -> Self::Out {
        todo!()
    }
}
