pub mod convert;

type Ident = String;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Epsilon,
    Bottom,
    Literal(String),
    Cat(Box<Term>, Box<Term>),
    Alt(Box<Term>, Box<Term>),
    Fix(Box<Term>), // Uses de Bruijn indices
    Variable(usize),
    Call(Ident, Vec<Term>),
}
