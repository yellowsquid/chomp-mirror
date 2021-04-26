use proc_macro2::Span;

use super::{
    ast::{Alt, Call, Cat, Epsilon, Expression, Fix, Lambda, Literal, NamedExpression, Variable},
    Name,
};

pub trait Visitor {
    type Out;
    fn visit_epsilon(
        &mut self,
        name: Option<&Name>,
        span: Option<Span>,
        eps: &Epsilon,
    ) -> Self::Out;

    fn visit_literal(
        &mut self,
        name: Option<&Name>,
        span: Option<Span>,
        lit: &Literal,
    ) -> Self::Out;

    fn visit_cat(&mut self, name: Option<&Name>, span: Option<Span>, cat: &Cat) -> Self::Out;

    fn visit_alt(&mut self, name: Option<&Name>, span: Option<Span>, alt: &Alt) -> Self::Out;

    fn visit_fix(&mut self, name: Option<&Name>, span: Option<Span>, fix: &Fix) -> Self::Out;

    fn visit_variable(
        &mut self,
        name: Option<&Name>,
        span: Option<Span>,
        var: &Variable,
    ) -> Self::Out;

    fn visit_call(&mut self, name: Option<&Name>, span: Option<Span>, call: &Call) -> Self::Out;

    fn visit_lambda(
        &mut self,
        name: Option<&Name>,
        span: Option<Span>,
        lambda: &Lambda,
    ) -> Self::Out;
}

pub trait Mapper {
    type Out;

    fn map_epsilon(
        &mut self,
        name: &mut Option<Name>,
        span: Option<Span>,
        eps: &mut Epsilon,
    ) -> Self::Out;

    fn map_literal(
        &mut self,
        name: &mut Option<Name>,
        span: Option<Span>,
        lit: &mut Literal,
    ) -> Self::Out;

    fn map_cat(&mut self, name: &mut Option<Name>, span: Option<Span>, cat: &mut Cat) -> Self::Out;

    fn map_alt(&mut self, name: &mut Option<Name>, span: Option<Span>, alt: &mut Alt) -> Self::Out;

    fn map_fix(&mut self, name: &mut Option<Name>, span: Option<Span>, fix: &mut Fix) -> Self::Out;

    fn map_variable(
        &mut self,
        name: &mut Option<Name>,
        span: Option<Span>,
        var: &mut Variable,
    ) -> Self::Out;

    fn map_call(
        &mut self,
        name: &mut Option<Name>,
        span: Option<Span>,
        call: &mut Call,
    ) -> Self::Out;

    fn map_lambda(
        &mut self,
        name: &mut Option<Name>,
        span: Option<Span>,
        lambda: &mut Lambda,
    ) -> Self::Out;
}

pub trait Folder {
    type Out;

    fn fold_epsilon(&mut self, name: Option<Name>, span: Option<Span>, eps: Epsilon) -> Self::Out;

    fn fold_literal(&mut self, name: Option<Name>, span: Option<Span>, lit: Literal) -> Self::Out;

    fn fold_cat(&mut self, name: Option<Name>, span: Option<Span>, cat: Cat) -> Self::Out;

    fn fold_alt(&mut self, name: Option<Name>, span: Option<Span>, alt: Alt) -> Self::Out;

    fn fold_fix(&mut self, name: Option<Name>, span: Option<Span>, fix: Fix) -> Self::Out;

    fn fold_variable(&mut self, name: Option<Name>, span: Option<Span>, var: Variable)
        -> Self::Out;

    fn fold_call(&mut self, name: Option<Name>, span: Option<Span>, call: Call) -> Self::Out;

    fn fold_lambda(&mut self, name: Option<Name>, span: Option<Span>, lambda: Lambda) -> Self::Out;
}

pub trait Visitable {
    fn visit<V: Visitor>(&self, visitor: &mut V) -> <V as Visitor>::Out;

    fn map<M: Mapper>(&mut self, mapper: &mut M) -> <M as Mapper>::Out;

    fn fold<F: Folder>(self, folder: &mut F) -> <F as Folder>::Out;
}

impl Visitable for NamedExpression {
    fn visit<V: Visitor>(&self, visitor: &mut V) -> <V as Visitor>::Out {
        match &self.expr {
            Expression::Epsilon(e) => visitor.visit_epsilon(self.name.as_ref(), self.span, e),
            Expression::Literal(l) => visitor.visit_literal(self.name.as_ref(), self.span, l),
            Expression::Cat(c) => visitor.visit_cat(self.name.as_ref(), self.span, c),
            Expression::Alt(a) => visitor.visit_alt(self.name.as_ref(), self.span, a),
            Expression::Fix(f) => visitor.visit_fix(self.name.as_ref(), self.span, f),
            Expression::Variable(v) => visitor.visit_variable(self.name.as_ref(), self.span, v),
            Expression::Call(c) => visitor.visit_call(self.name.as_ref(), self.span, c),
            Expression::Lambda(l) => visitor.visit_lambda(self.name.as_ref(), self.span, l),
        }
    }

    fn map<M: Mapper>(&mut self, mapper: &mut M) -> <M as Mapper>::Out {
        match &mut self.expr {
            Expression::Epsilon(e) => mapper.map_epsilon(&mut self.name, self.span, e),
            Expression::Literal(l) => mapper.map_literal(&mut self.name, self.span, l),
            Expression::Cat(c) => mapper.map_cat(&mut self.name, self.span, c),
            Expression::Alt(a) => mapper.map_alt(&mut self.name, self.span, a),
            Expression::Fix(f) => mapper.map_fix(&mut self.name, self.span, f),
            Expression::Variable(v) => mapper.map_variable(&mut self.name, self.span, v),
            Expression::Call(c) => mapper.map_call(&mut self.name, self.span, c),
            Expression::Lambda(l) => mapper.map_lambda(&mut self.name, self.span, l),
        }
    }

    fn fold<F: Folder>(self, folder: &mut F) -> <F as Folder>::Out {
        match self.expr {
            Expression::Epsilon(e) => folder.fold_epsilon(self.name, self.span, e),
            Expression::Literal(l) => folder.fold_literal(self.name, self.span, l),
            Expression::Cat(c) => folder.fold_cat(self.name, self.span, c),
            Expression::Alt(a) => folder.fold_alt(self.name, self.span, a),
            Expression::Fix(f) => folder.fold_fix(self.name, self.span, f),
            Expression::Variable(v) => folder.fold_variable(self.name, self.span, v),
            Expression::Call(c) => folder.fold_call(self.name, self.span, c),
            Expression::Lambda(l) => folder.fold_lambda(self.name, self.span, l),
        }
    }
}
