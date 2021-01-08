use super::super::{
    ast::{Alt, Call, Cat, Epsilon, Fix, Literal, Parameter, Variable},
    visit::{Mapper, Visitable},
};

#[derive(Clone, Copy, Debug, Default)]
pub struct ShallowVars {
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
