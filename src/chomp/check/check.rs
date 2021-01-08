use super::{
    super::{
        ast::{Alt, Call, Cat, Epsilon, Fix, Literal, Parameter, Variable},
        context::Context,
        error::TypeError,
        typed::{self, TypedExpression},
        visit::{Folder, Visitable, Visitor},
    },
    TypeInfer,
};

#[derive(Debug)]
pub struct TypeCheck<'a> {
    pub context: &'a mut Context,
}

impl Folder for TypeCheck<'_> {
    type Out = Result<TypedExpression, TypeError>;

    fn fold_epsilon(&mut self, eps: Epsilon) -> Self::Out {
        Ok(typed::Epsilon::from(eps).into())
    }

    fn fold_literal(&mut self, lit: Literal) -> Self::Out {
        Ok(typed::Literal::from(lit).into())
    }

    fn fold_cat(&mut self, cat: Cat) -> Self::Out {
        let ty = TypeInfer {
            context: self.context,
        }
        .visit_cat(&cat)?;
        let fst = cat.fst.fold(self)?;
        let snd = cat.snd;
        let snd = self
            .context
            .with_unguard(|context| snd.fold(&mut TypeCheck { context }))?;

        Ok(typed::Cat::new(fst, cat.punct, snd, ty).into())
    }

    fn fold_alt(&mut self, alt: Alt) -> Self::Out {
        let ty = TypeInfer {
            context: self.context,
        }
        .visit_alt(&alt)?;
        let left = alt.left.fold(self)?;
        let right = alt.right.fold(self)?;

        Ok(typed::Alt::new(left, alt.punct, right, ty).into())
    }

    fn fold_fix(&mut self, fix: Fix) -> Self::Out {
        let ty = TypeInfer {
            context: self.context,
        }
        .visit_fix(&fix)?;
        let inner = fix.inner;
        let inner = self
            .context
            .with_variable_type(ty.clone(), |context| inner.fold(&mut TypeCheck { context }))?;

        Ok(typed::Fix::new(fix.arg, inner, fix.span, ty).into())
    }

    fn fold_variable(&mut self, var: Variable) -> Self::Out {
        let ty = TypeInfer {
            context: self.context,
        }
        .visit_variable(&var)?;
        Ok(typed::Variable::new(var, ty).into())
    }

    fn fold_parameter(&mut self, _param: Parameter) -> Self::Out {
        todo!()
    }

    fn fold_call(&mut self, _call: Call) -> Self::Out {
        todo!()
    }
}
