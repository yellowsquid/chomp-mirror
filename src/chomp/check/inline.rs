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
    use crate::chomp::Name;

    use super::*;

    fn opt() -> Function {
        Function::new(
            Name::Spanless("opt".to_string()),
            1,
            Expression::Alt(Alt::new(
                Expression::Epsilon(None),
                None,
                Parameter::new(Name::Spanless("x".to_string()), 0).into(),
            )),
            None,
        )
    }

    #[test]
    fn test_inline_absent() {
        let expr = Epsilon::default();
        let inlined = expr.fold(&mut InlineCalls::new(opt()));
        assert_eq!(inlined, Ok(Epsilon::default().into()))
    }

    #[test]
    fn test_inline_in_fix() {
        let expr = Fix::new(
            Name::Spanless("rec".to_string()),
            Call::new(
                Name::Spanless("opt".to_string()),
                vec![Variable::new(Name::Spanless("rec".to_string()), 0).into()],
                None,
            )
            .into(),
            None,
        );
        let inlined = expr.fold(&mut InlineCalls::new(opt()));
        assert_eq!(
            inlined,
            Ok(Fix::new(
                Name::Spanless("rec".to_string()),
                Alt::new(
                    Epsilon::default().into(),
                    None,
                    Variable::new(Name::Spanless("rec".to_string()), 0).into()
                )
                .into(),
                None
            )
            .into())
        )
    }

    #[test]
    fn test_inline_deepens_vars() {
        let function = Function::new(
            Name::Spanless("plus".into()),
            1,
            Fix::new(
                Name::Spanless("rec".to_string()),
                Cat::new(
                    Parameter::new(Name::Spanless("x".to_string()), 0).into(),
                    None,
                    Variable::new(Name::Spanless("rec".to_string()), 0).into(),
                )
                .into(),
                None,
            )
            .into(),
            None,
        );
        let expr = Fix::new(
            Name::Spanless("var".to_string()),
            Call::new(
                Name::Spanless("plus".into()),
                vec![Variable::new(Name::Spanless("var".to_string()), 0).into()],
                None,
            )
            .into(),
            None,
        );
        let inlined = expr.fold(&mut InlineCalls::new(function));
        assert_eq!(
            inlined,
            Ok(Fix::new(
                Name::Spanless("var".to_string()),
                Fix::new(
                    Name::Spanless("rec".to_string()),
                    Cat::new(
                        Variable::new(Name::Spanless("var".to_string()), 1).into(),
                        None,
                        Variable::new(Name::Spanless("rec".to_string()), 0).into(),
                    )
                    .into(),
                    None,
                )
                .into(),
                None
            )
            .into())
        )
    }

    #[test]
    fn test_inline_resets_vars() {
        let function = Function::new(
            Name::Spanless("plus".into()),
            1,
            Cat::new(
                Fix::new(
                    Name::Spanless("rec".to_string()),
                    Epsilon::default().into(),
                    None,
                )
                .into(),
                None,
                Parameter::new(Name::Spanless("x".to_string()), 0).into(),
            )
            .into(),
            None,
        );
        let expr = Fix::new(
            Name::Spanless("var".to_string()),
            Call::new(
                Name::Spanless("plus".into()),
                vec![Variable::new(Name::Spanless("var".to_string()), 0).into()],
                None,
            )
            .into(),
            None,
        );
        let inlined = expr.fold(&mut InlineCalls::new(function));
        assert_eq!(
            inlined,
            Ok(Fix::new(
                Name::Spanless("var".to_string()),
                Cat::new(
                    Fix::new(
                        Name::Spanless("rec".to_string()),
                        Epsilon::default().into(),
                        None,
                    )
                    .into(),
                    None,
                    Variable::new(Name::Spanless("var".to_string()), 0).into(),
                )
                .into(),
                None
            )
            .into())
        )
    }

    #[test]
    fn test_inline_double_subst() {
        let expr = Call::new(
            Name::Spanless("opt".to_string()),
            vec![Call::new(
                Name::Spanless("opt".to_string()),
                vec![Literal::Spanless("x".to_string()).into()],
                None,
            )
            .into()],
            None,
        );
        let inlined = expr.fold(&mut InlineCalls::new(opt()));
        assert_eq!(
            inlined,
            Ok(Alt::new(
                Epsilon::default().into(),
                None,
                Alt::new(
                    Epsilon::default().into(),
                    None,
                    Literal::Spanless("x".to_string()).into()
                )
                .into()
            )
            .into())
        )
    }

    #[test]
    fn test_inline_call_args() {
        let expr = Fix::new(
            Name::Spanless("rec".to_string()),
            Cat::new(
                Literal::Spanless("a".to_string()).into(),
                None,
                Call::new(
                    Name::Spanless("opt".to_string()),
                    vec![Cat::new(
                        Cat::new(
                            Literal::Spanless("a".to_string()).into(),
                            None,
                            Fix::new(
                                Name::Spanless("star".to_string()),
                                Call::new(
                                    Name::Spanless("opt".to_string()),
                                    vec![Cat::new(
                                        Literal::Spanless(" ".to_string()).into(),
                                        None,
                                        Variable::new(Name::Spanless("star".to_string()), 0).into(),
                                    )
                                    .into()],
                                    None,
                                )
                                .into(),
                                None,
                            )
                            .into(),
                        )
                        .into(),
                        None,
                        Variable::new(Name::Spanless("rec".to_string()), 0).into(),
                    )
                    .into()],
                    None,
                )
                .into(),
            )
            .into(),
            None,
        );
        let inlined = expr.fold(&mut InlineCalls::new(opt()));
        assert_eq!(inlined,
        Ok(Fix::new(
            Name::Spanless("rec".to_string()),
            Cat::new(
                Literal::Spanless("a".to_string()).into(),
                None,
                Alt::new(
                    Epsilon::default().into(),
                    None,
                    Cat::new(
                        Cat::new(
                            Literal::Spanless("a".to_string()).into(),
                            None,
                            Fix::new(
                                Name::Spanless("star".to_string()),
                                Alt::new(
                                    Epsilon::default().into(),
                                    None,
                                    Cat::new(
                                        Literal::Spanless(" ".to_string()).into(),
                                        None,
                                        Variable::new(Name::Spanless("star".to_string()), 0).into(),
                                    )
                                        .into()
                                )
                                .into(),
                                None,
                            )
                            .into(),
                        )
                        .into(),
                        None,
                        Variable::new(Name::Spanless("rec".to_string()), 0).into(),
                    )
                    .into(),
                )
                .into(),
            )
            .into(),
            None,
        ).into()))
    }
}
