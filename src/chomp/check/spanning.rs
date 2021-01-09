use proc_macro2::Span;

use super::super::{
    ast::{Alt, Call, Cat, Epsilon, Fix, Literal, Parameter, Variable},
    visit::{Visitable, Visitor},
};

#[derive(Clone, Copy, Debug)]
pub struct Spanning;

impl Visitor for Spanning {
    type Out = Option<Span>;

    fn visit_epsilon(&mut self, eps: &Epsilon) -> Self::Out {
        eps.map(|e| e.span)
    }

    fn visit_literal(&mut self, lit: &Literal) -> Self::Out {
        lit.span()
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
        var.name().span()
    }

    fn visit_parameter(&mut self, param: &Parameter) -> Self::Out {
        param.name().span()
    }

    fn visit_call(&mut self, call: &Call) -> Self::Out {
        call.span()
    }
}
