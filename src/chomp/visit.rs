use super::ast::{Alt, Call, Cat, Epsilon, Expression, Fix, Literal, Parameter, Variable};

pub trait Visitor {
    type Out;
    fn visit_epsilon(&mut self, eps: &Epsilon) -> Self::Out;

    fn visit_literal(&mut self, lit: &Literal) -> Self::Out;

    fn visit_cat(&mut self, cat: &Cat) -> Self::Out;

    fn visit_alt(&mut self, alt: &Alt) -> Self::Out;

    fn visit_fix(&mut self, fix: &Fix) -> Self::Out;

    fn visit_variable(&mut self, var: &Variable) -> Self::Out;

    fn visit_parameter(&mut self, param: &Parameter) -> Self::Out;

    fn visit_call(&mut self, call: &Call) -> Self::Out;
}

pub trait Mapper {
    type Out;

    fn map_epsilon(&mut self, eps: &mut Epsilon) -> Self::Out;

    fn map_literal(&mut self, lit: &mut Literal) -> Self::Out;

    fn map_cat(&mut self, cat: &mut Cat) -> Self::Out;

    fn map_alt(&mut self, alt: &mut Alt) -> Self::Out;

    fn map_fix(&mut self, fix: &mut Fix) -> Self::Out;

    fn map_variable(&mut self, var: &mut Variable) -> Self::Out;

    fn map_parameter(&mut self, param: &mut Parameter) -> Self::Out;

    fn map_call(&mut self, call: &mut Call) -> Self::Out;
}

pub trait Folder {
    type Out;

    fn fold_epsilon(&mut self, eps: Epsilon) -> Self::Out;

    fn fold_literal(&mut self, lit: Literal) -> Self::Out;

    fn fold_cat(&mut self, cat: Cat) -> Self::Out;

    fn fold_alt(&mut self, alt: Alt) -> Self::Out;

    fn fold_fix(&mut self, fix: Fix) -> Self::Out;

    fn fold_variable(&mut self, var: Variable) -> Self::Out;

    fn fold_parameter(&mut self, param: Parameter) -> Self::Out;

    fn fold_call(&mut self, call: Call) -> Self::Out;
}

pub trait Visitable {
    fn visit<V: Visitor>(&self, visitor: &mut V) -> <V as Visitor>::Out;

    fn map<M: Mapper>(&mut self, mapper: &mut M) -> <M as Mapper>::Out;

    fn fold<F: Folder>(self, folder: &mut F) -> <F as Folder>::Out;
}

macro_rules! visitable_leaf {
    ($ty:ty, $visit:ident, $map:ident, $fold:ident) => {
        impl Visitable for $ty {
            fn visit<V: Visitor>(&self, visitor: &mut V) -> <V as Visitor>::Out {
                visitor.$visit(self)
            }

            fn map<M: Mapper>(&mut self, mapper: &mut M) -> <M as Mapper>::Out {
                mapper.$map(self)
            }

            fn fold<F: Folder>(self, folder: &mut F) -> <F as Folder>::Out {
                folder.$fold(self)
            }
        }
    };
}

visitable_leaf!(Epsilon, visit_epsilon, map_epsilon, fold_epsilon);
visitable_leaf!(Literal, visit_literal, map_literal, fold_literal);
visitable_leaf!(Cat, visit_cat, map_cat, fold_cat);
visitable_leaf!(Alt, visit_alt, map_alt, fold_alt);
visitable_leaf!(Fix, visit_fix, map_fix, fold_fix);
visitable_leaf!(Variable, visit_variable, map_variable, fold_variable);
visitable_leaf!(Parameter, visit_parameter, map_parameter, fold_parameter);
visitable_leaf!(Call, visit_call, map_call, fold_call);

impl Visitable for Expression {
    fn visit<V: Visitor>(&self, visitor: &mut V) -> <V as Visitor>::Out {
        match self {
            Self::Epsilon(e) => visitor.visit_epsilon(e),
            Self::Literal(l) => visitor.visit_literal(l),
            Self::Cat(c) => visitor.visit_cat(c),
            Self::Alt(a) => visitor.visit_alt(a),
            Self::Fix(f) => visitor.visit_fix(f),
            Self::Variable(v) => visitor.visit_variable(v),
            Self::Parameter(p) => visitor.visit_parameter(p),
            Self::Call(c) => visitor.visit_call(c),
        }
    }

    fn map<M: Mapper>(&mut self, mapper: &mut M) -> <M as Mapper>::Out {
        match self {
            Self::Epsilon(e) => mapper.map_epsilon(e),
            Self::Literal(l) => mapper.map_literal(l),
            Self::Cat(c) => mapper.map_cat(c),
            Self::Alt(a) => mapper.map_alt(a),
            Self::Fix(f) => mapper.map_fix(f),
            Self::Variable(v) => mapper.map_variable(v),
            Self::Parameter(p) => mapper.map_parameter(p),
            Self::Call(c) => mapper.map_call(c),
        }
    }

    fn fold<F: Folder>(self, folder: &mut F) -> <F as Folder>::Out {
        match self {
            Self::Epsilon(e) => folder.fold_epsilon(e),
            Self::Literal(l) => folder.fold_literal(l),
            Self::Cat(c) => folder.fold_cat(c),
            Self::Alt(a) => folder.fold_alt(a),
            Self::Fix(f) => folder.fold_fix(f),
            Self::Variable(v) => folder.fold_variable(v),
            Self::Parameter(p) => folder.fold_parameter(p),
            Self::Call(c) => folder.fold_call(c),
        }
    }
}
