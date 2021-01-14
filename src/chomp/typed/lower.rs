use proc_macro2::Span;

use crate::chomp::Name;

use super::{Alt, Cat, Epsilon, Fix, Literal, RawTypedExpression, TypedExpression, Variable};

pub trait Backend: Default {
    type Id;
    type Code;

    fn gen_epsilon(&mut self, name: Option<Name>, span: Option<Span>, eps: Epsilon) -> Self::Id;

    fn gen_literal(&mut self, name: Option<Name>, span: Option<Span>, lit: Literal) -> Self::Id;

    fn gen_cat(&mut self, name: Option<Name>, span: Option<Span>, cat: Cat) -> Self::Id;

    fn gen_alt(&mut self, name: Option<Name>, span: Option<Span>, alt: Alt) -> Self::Id;

    fn gen_fix(&mut self, name: Option<Name>, span: Option<Span>, fix: Fix) -> Self::Id;

    fn gen_variable(&mut self, name: Option<Name>, span: Option<Span>, var: Variable) -> Self::Id;

    fn emit_code(self, name: Option<Name>, span: Option<Span>, id: Self::Id) -> Self::Code;
}

pub trait GenerateCode {
    fn gen<B: Backend>(self, backend: &mut B) -> B::Id;
}

impl GenerateCode for TypedExpression {
    fn gen<B: Backend>(self, backend: &mut B) -> B::Id {
        match self.inner {
            RawTypedExpression::Epsilon(e) => backend.gen_epsilon(self.name, self.span, e),
            RawTypedExpression::Literal(l) => backend.gen_literal(self.name, self.span, l),
            RawTypedExpression::Cat(c) => backend.gen_cat(self.name, self.span, c),
            RawTypedExpression::Alt(a) => backend.gen_alt(self.name, self.span, a),
            RawTypedExpression::Fix(f) => backend.gen_fix(self.name, self.span, f),
            RawTypedExpression::Variable(v) => backend.gen_variable(self.name, self.span, v),
        }
    }
}
