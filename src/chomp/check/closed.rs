use super::super::{
    ast::{Alt, Call, Cat, Epsilon, Fix, Literal, Parameter, Variable},
    visit::{Visitable, Visitor},
};

/// Test if term is closed for a context with `depth` variables.
#[derive(Copy, Clone, Debug, Default)]
pub struct Closed {
    depth: usize,
    params: usize,
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

    fn visit_parameter(&mut self, param: &Parameter) -> Self::Out {
        param.index() < self.params
    }

    fn visit_call(&mut self, call: &Call) -> Self::Out {
        call.args().iter().all(|arg| arg.visit(self))
    }
}
