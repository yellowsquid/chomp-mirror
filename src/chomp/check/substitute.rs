use super::{
    super::{
        ast::{Alt, Call, Cat, Epsilon, Expression, Fix, Literal, Parameter, Variable},
        error::SubstituteError,
        visit::{Folder, Visitable},
    },
    DeepenVars, ShallowVars,
};

#[derive(Clone, Debug)]
pub struct SubstituteParams {
    params: Vec<Expression>,
}

impl SubstituteParams {
    pub fn new(params: Vec<Expression>) -> Self {
        Self { params }
    }
}

impl Folder for SubstituteParams {
    type Out = Result<Expression, SubstituteError>;

    fn fold_epsilon(&mut self, eps: Epsilon) -> Self::Out {
        Ok(eps.into())
    }

    fn fold_literal(&mut self, lit: Literal) -> Self::Out {
        Ok(lit.into())
    }

    fn fold_cat(&mut self, mut cat: Cat) -> Self::Out {
        cat.fst = Box::new(cat.fst.fold(self)?);
        cat.snd = Box::new(cat.snd.fold(self)?);
        Ok(cat.into())
    }

    fn fold_alt(&mut self, mut alt: Alt) -> Self::Out {
        alt.left = Box::new(alt.left.fold(self)?);
        alt.right = Box::new(alt.right.fold(self)?);
        Ok(alt.into())
    }

    fn fold_fix(&mut self, mut fix: Fix) -> Self::Out {
        for param in &mut self.params {
            param.map(&mut DeepenVars::default());
        }

        fix.inner = Box::new(fix.inner.fold(self)?);

        for param in &mut self.params {
            param.map(&mut ShallowVars::default());
        }

        Ok(fix.into())
    }

    fn fold_variable(&mut self, var: Variable) -> Self::Out {
        Ok(Expression::Variable(var))
    }

    fn fold_call(&mut self, mut call: Call) -> Self::Out {
        call.args = call
            .args
            .into_iter()
            .map(|arg| arg.fold(self))
            .collect::<Result<_, _>>()?;
        Ok(call.into())
    }

    fn fold_parameter(&mut self, param: Parameter) -> Self::Out {
        self.params
            .get(param.index())
            .cloned()
            .ok_or(SubstituteError::FreeParameter(param))
    }
}
