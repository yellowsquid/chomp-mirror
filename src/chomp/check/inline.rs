use super::{
    super::{
        ast::{Alt, Call, Cat, Epsilon, Expression, Fix, Function, Literal, Parameter, Variable},
        error::SubstituteError,
        visit::{Folder, Visitable},
    },
    SubstituteParams,
};

#[derive(Clone, Debug)]
pub struct InlineCalls {
    function: Function,
}

impl InlineCalls {
    pub fn new(function: Function) -> Self {
        Self { function }
    }
}

impl Folder for InlineCalls {
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
        fix.inner = Box::new(fix.inner.fold(self)?);
        Ok(fix.into())
    }

    fn fold_variable(&mut self, var: Variable) -> Self::Out {
        Ok(var.into())
    }

    fn fold_parameter(&mut self, param: Parameter) -> Self::Out {
        Ok(param.into())
    }

    fn fold_call(&mut self, mut call: Call) -> Self::Out {
        call.args = call
            .args
            .into_iter()
            .map(|arg| arg.fold(self))
            .collect::<Result<_, _>>()?;

        if call.name != self.function.name {
            Ok(call.into())
        } else if call.args.len() != self.function.params {
            Err(SubstituteError::WrongArgCount {
                call,
                expected: self.function.params,
            })
        } else {
            self.function
                .expr
                .clone()
                .fold(&mut SubstituteParams::new(call.args))
        }
    }
}

#[cfg(test)]
mod tests {
    use proc_macro2::Span;
    use syn::Ident;

    use super::*;

    const OPT_NAME: &str = "opt";
    const OPT: Function = Function::new(
        Ident::new(OPT_NAME, Span::call_site()),
        1,
        Expression::Alt(Alt::new(
            Epsilon::default().into(),
            None,
            Parameter::new(None, 0).into(),
        )),
        None,
    );

    #[test]
    fn test_inline_absent() {
        let expr = Epsilon::default();
        let inlined = expr.fold(&mut InlineCalls::new(OPT));
        assert_eq!(inlined, Ok(Expression::from(Epsilon::default())))
    }
}
