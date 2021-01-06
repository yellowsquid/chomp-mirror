use crate::chomp::typed::{
    Alt, Cat, Epsilon, Fix, Literal, RawTypedExpression, TypedExpression, Variable,
};

pub mod rust;

pub trait Backend: Default {
    type Id;
    type Code;

    fn gen_epsilon(&mut self, eps: Epsilon) -> Self::Id;

    fn gen_literal(&mut self, lit: Literal) -> Self::Id;

    fn gen_cat(&mut self, cat: Cat) -> Self::Id;

    fn gen_alt(&mut self, alt: Alt) -> Self::Id;

    fn gen_fix(&mut self, fix: Fix) -> Self::Id;

    fn gen_variable(&mut self, var: Variable) -> Self::Id;

    fn emit_code(self, id: Self::Id) -> Self::Code;
}

pub trait GenerateCode {
    fn gen<B: Backend>(self, backend: &mut B) -> B::Id;
}

macro_rules! generate_leaf {
    ($ty:ty, $gen:ident) => {
        impl GenerateCode for $ty {
            fn gen<B: Backend>(self, backend: &mut B) -> B::Id {
                backend.$gen(self)
            }
        }
    };
}

generate_leaf!(Epsilon, gen_epsilon);
generate_leaf!(Literal, gen_literal);
generate_leaf!(Cat, gen_cat);
generate_leaf!(Alt, gen_alt);
generate_leaf!(Fix, gen_fix);
generate_leaf!(Variable, gen_variable);

impl GenerateCode for TypedExpression {
    fn gen<B: Backend>(self, backend: &mut B) -> B::Id {
        match self.inner {
            RawTypedExpression::Epsilon(e) => e.gen(backend),
            RawTypedExpression::Literal(l) => l.gen(backend),
            RawTypedExpression::Cat(c) => c.gen(backend),
            RawTypedExpression::Alt(a) => a.gen(backend),
            RawTypedExpression::Fix(f) => f.gen(backend),
            RawTypedExpression::Variable(v) => v.gen(backend),
        }
    }
}
