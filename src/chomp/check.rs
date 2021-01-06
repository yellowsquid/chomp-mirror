use proc_macro2::Span;

use super::{
    ast::{Alt, Call, Cat, Epsilon, Expression, Fix, Function, Literal, Parameter, Variable},
    context::Context,
    error::{AltError, CatError, FixError, SubstituteError, TypeError},
    set::{FirstSet, FlastSet},
    typed::{self, Type, TypedExpression},
    visit::{Folder, Mapper, Visitable, Visitor},
};

/// Test if term is closed for a context with `depth` variables.
#[derive(Debug, Default)]
struct Closed {
    depth: usize,
}

impl Visitor for Closed {
    type Out = bool;

    fn visit_epsilon(&mut self, _eps: &Epsilon) -> Self::Out {
        true
    }

    fn visit_literal(&mut self, _lit: &Literal) -> Self::Out {
        true
    }

    fn visit_cat(&mut self, cat: &Cat) -> Self::Out {
        cat.first().visit(self) && cat.second().visit(self)
    }

    fn visit_alt(&mut self, alt: &Alt) -> Self::Out {
        alt.left().visit(self) && alt.right().visit(self)
    }

    fn visit_fix(&mut self, fix: &Fix) -> Self::Out {
        self.depth += 1;
        let res = fix.inner().visit(self);
        self.depth -= 1;
        res
    }

    fn visit_variable(&mut self, var: &Variable) -> Self::Out {
        var.index() < self.depth
    }

    fn visit_parameter(&mut self, _param: &Parameter) -> Self::Out {
        true
    }

    fn visit_call(&mut self, call: &Call) -> Self::Out {
        call.args().iter().all(|arg| arg.visit(self))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Spanning;

impl Visitor for Spanning {
    type Out = Option<Span>;

    fn visit_epsilon(&mut self, eps: &Epsilon) -> Self::Out {
        Some(eps.span)
    }

    fn visit_literal(&mut self, lit: &Literal) -> Self::Out {
        Some(lit.span())
    }

    fn visit_cat(&mut self, cat: &Cat) -> Self::Out {
        let fst = cat.first().visit(self);
        let snd = cat.second().visit(self);

        match (fst, snd) {
            (None, snd) => snd,
            (Some(fst), None) => Some(fst),
            (Some(fst), Some(snd)) => fst.join(snd),
        }
    }

    fn visit_alt(&mut self, alt: &Alt) -> Self::Out {
        let left = alt.left().visit(self);
        let right = alt.right().visit(self);

        match (left, right) {
            (None, right) => right,
            (Some(left), None) => Some(left),
            (Some(left), Some(right)) => left.join(right),
        }
    }

    fn visit_fix(&mut self, fix: &Fix) -> Self::Out {
        fix.span()
    }

    fn visit_variable(&mut self, var: &Variable) -> Self::Out {
        Some(var.name().span())
    }

    fn visit_parameter(&mut self, param: &Parameter) -> Self::Out {
        Some(param.name().span())
    }

    fn visit_call(&mut self, call: &Call) -> Self::Out {
        call.span()
    }
}

#[derive(Debug)]
pub struct TypeInfer<'a> {
    context: &'a mut Context,
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

#[derive(Debug, Default)]
struct DeepenVars {
    depth: usize,
}

impl Mapper for DeepenVars {
    type Out = ();

    fn map_epsilon(&mut self, _: &mut Epsilon) -> Self::Out {}

    fn map_literal(&mut self, _: &mut Literal) -> Self::Out {}

    fn map_cat(&mut self, cat: &mut Cat) -> Self::Out {
        cat.first_mut().map(self);
        cat.second_mut().map(self);
    }

    fn map_alt(&mut self, alt: &mut Alt) -> Self::Out {
        alt.left_mut().map(self);
        alt.right_mut().map(self);
    }

    fn map_fix(&mut self, fix: &mut Fix) -> Self::Out {
        self.depth += 1;
        fix.inner_mut().map(self);
        self.depth -= 1;
    }

    fn map_variable(&mut self, bind: &mut Variable) -> Self::Out {
        if bind.index() >= self.depth {
            *bind.index_mut() += 1;
        }
    }

    fn map_parameter(&mut self, _param: &mut Parameter) -> Self::Out {}

    fn map_call(&mut self, call: &mut Call) -> Self::Out {
        for arg in call.args_mut() {
            arg.map(self);
        }
    }
}

#[derive(Debug, Default)]
struct ShallowVars {
    depth: usize,
}

impl Mapper for ShallowVars {
    type Out = ();

    fn map_epsilon(&mut self, _: &mut Epsilon) -> Self::Out {}

    fn map_literal(&mut self, _: &mut Literal) -> Self::Out {}

    fn map_cat(&mut self, cat: &mut Cat) -> Self::Out {
        cat.first_mut().map(self);
        cat.second_mut().map(self);
    }

    fn map_alt(&mut self, alt: &mut Alt) -> Self::Out {
        alt.left_mut().map(self);
        alt.right_mut().map(self);
    }

    fn map_fix(&mut self, fix: &mut Fix) -> Self::Out {
        self.depth += 1;
        fix.inner_mut().map(self);
        self.depth -= 1;
    }

    fn map_variable(&mut self, bind: &mut Variable) -> Self::Out {
        if bind.index() > self.depth {
            *bind.index_mut() -= 1;
        }
    }

    fn map_parameter(&mut self, _param: &mut Parameter) -> Self::Out {}

    fn map_call(&mut self, call: &mut Call) -> Self::Out {
        for arg in call.args_mut() {
            arg.map(self);
        }
    }
}

struct Substitute {
    params: Vec<Expression>,
}

impl Folder for Substitute {
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

#[derive(Clone, Debug)]
pub struct InlineCall {
    function: Function,
}

impl InlineCall {
    pub fn new(function: Function) -> Self {
        Self { function }
    }
}

impl Folder for InlineCall {
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
                .fold(&mut Substitute { params: call.args })
        }
    }
}
