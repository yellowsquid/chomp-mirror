use proc_macro2::Span;

use super::{
    ast::{
        Alt, Call, Cat, Epsilon, Expression, Fix, Function, Global, Literal, NamedExpression,
        Parameter,
    },
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

    fn visit_parameter(
        &mut self,
        name: Option<&Name>,
        span: Option<Span>,
        param: &Parameter,
    ) -> Self::Out;

    fn visit_global(
        &mut self,
        name: Option<&Name>,
        span: Option<Span>,
        global: &Global,
    ) -> Self::Out;

    fn visit_call(&mut self, name: Option<&Name>, span: Option<Span>, call: &Call) -> Self::Out;

    fn visit_function(
        &mut self,
        name: Option<&Name>,
        span: Option<Span>,
        fun: &Function,
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

    fn map_parameter(
        &mut self,
        name: &mut Option<Name>,
        span: Option<Span>,
        param: &mut Parameter,
    ) -> Self::Out;

    fn map_global(
        &mut self,
        name: &mut Option<Name>,
        span: Option<Span>,
        global: &mut Global,
    ) -> Self::Out;

    fn map_call(
        &mut self,
        name: &mut Option<Name>,
        span: Option<Span>,
        call: &mut Call,
    ) -> Self::Out;

    fn map_function(
        &mut self,
        name: &mut Option<Name>,
        span: Option<Span>,
        fun: &mut Function,
    ) -> Self::Out;
}

pub trait Folder {
    type Out;

    fn fold_epsilon(&mut self, name: Option<Name>, span: Option<Span>, eps: Epsilon) -> Self::Out;

    fn fold_literal(&mut self, name: Option<Name>, span: Option<Span>, lit: Literal) -> Self::Out;

    fn fold_cat(&mut self, name: Option<Name>, span: Option<Span>, cat: Cat) -> Self::Out;

    fn fold_alt(&mut self, name: Option<Name>, span: Option<Span>, alt: Alt) -> Self::Out;

    fn fold_fix(&mut self, name: Option<Name>, span: Option<Span>, fix: Fix) -> Self::Out;

    fn fold_parameter(
        &mut self,
        name: Option<Name>,
        span: Option<Span>,
        param: Parameter,
    ) -> Self::Out;

    fn fold_global(&mut self, name: Option<Name>, span: Option<Span>, global: Global) -> Self::Out;

    fn fold_call(&mut self, name: Option<Name>, span: Option<Span>, call: Call) -> Self::Out;

    fn fold_function(&mut self, name: Option<Name>, span: Option<Span>, fun: Function)
        -> Self::Out;
}

pub trait Visitable {
    fn visit<V: Visitor>(&self, visitor: &mut V) -> <V as Visitor>::Out;

    fn map<M: Mapper>(&mut self, mapper: &mut M) -> <M as Mapper>::Out;

    fn fold<F: Folder>(self, folder: &mut F) -> <F as Folder>::Out;
}

impl Visitable for NamedExpression {
    fn visit<V: Visitor>(&self, visitor: &mut V) -> <V as Visitor>::Out {
        let name = self.name.as_ref();
        let span = self.span;
        match &self.expr {
            Expression::Epsilon(e) => visitor.visit_epsilon(name, span, e),
            Expression::Literal(l) => visitor.visit_literal(name, span, l),
            Expression::Cat(c) => visitor.visit_cat(name, span, c),
            Expression::Alt(a) => visitor.visit_alt(name, span, a),
            Expression::Fix(f) => visitor.visit_fix(name, span, f),
            Expression::Parameter(p) => visitor.visit_parameter(name, span, p),
            Expression::Global(g) => visitor.visit_global(name, span, g),
            Expression::Call(c) => visitor.visit_call(name, span, c),
            Expression::Function(f) => visitor.visit_function(name, span, f),
        }
    }

    fn map<M: Mapper>(&mut self, mapper: &mut M) -> <M as Mapper>::Out {
        let name = &mut self.name;
        let span = self.span;
        match &mut self.expr {
            Expression::Epsilon(e) => mapper.map_epsilon(name, span, e),
            Expression::Literal(l) => mapper.map_literal(name, span, l),
            Expression::Cat(c) => mapper.map_cat(name, span, c),
            Expression::Alt(a) => mapper.map_alt(name, span, a),
            Expression::Fix(f) => mapper.map_fix(name, span, f),
            Expression::Parameter(p) => mapper.map_parameter(name, span, p),
            Expression::Global(g) => mapper.map_global(name, span, g),
            Expression::Call(c) => mapper.map_call(name, span, c),
            Expression::Function(f) => mapper.map_function(name, span, f),
        }
    }

    fn fold<F: Folder>(self, folder: &mut F) -> <F as Folder>::Out {
        let name = self.name;
        let span = self.span;
        match self.expr {
            Expression::Epsilon(e) => folder.fold_epsilon(name, span, e),
            Expression::Literal(l) => folder.fold_literal(name, span, l),
            Expression::Cat(c) => folder.fold_cat(name, span, c),
            Expression::Alt(a) => folder.fold_alt(name, span, a),
            Expression::Fix(f) => folder.fold_fix(name, span, f),
            Expression::Parameter(p) => folder.fold_parameter(name, span, p),
            Expression::Global(g) => folder.fold_global(name, span, g),
            Expression::Call(c) => folder.fold_call(name, span, c),
            Expression::Function(f) => folder.fold_function(name, span, f),
        }
    }
}
