use std::{hash, iter};

use proc_macro2::Span;

use super::{
    ast,
    set::{FirstSet, FlastSet},
    Name,
};

pub mod context;
pub mod error;
pub mod lower;

mod infer;

pub use self::infer::TypeInfer;
use self::error::{AltError, CatError};

#[derive(Debug, Default, Clone, Eq, Hash, PartialEq)]
pub struct Type {
    nullable: bool,
    first_set: FirstSet,
    flast_set: FlastSet,
}

impl Type {
    pub const fn new(nullable: bool, first_set: FirstSet, flast_set: FlastSet) -> Self {
        Self {
            nullable,
            first_set,
            flast_set,
        }
    }

    pub fn of_str(s: &str) -> Self {
        Self {
            nullable: s.is_empty(),
            first_set: FirstSet::of_str(s),
            flast_set: FlastSet::default(),
        }
    }

    pub fn nullable(&self) -> bool {
        self.nullable
    }

    pub fn first_set(&self) -> &FirstSet {
        &self.first_set
    }

    pub fn flast_set(&self) -> &FlastSet {
        &self.flast_set
    }

    pub fn cat(self, other: Self) -> Self {
        Self {
            nullable: self.nullable && other.nullable,
            first_set: self.first_set.union(if self.nullable {
                other.first_set.clone()
            } else {
                FirstSet::default()
            }),
            flast_set: other.flast_set.union(if other.nullable {
                self.flast_set.union_first(other.first_set)
            } else {
                FlastSet::default()
            }),
        }
    }

    pub fn alt(self, other: Self) -> Self {
        Self {
            nullable: self.nullable || other.nullable,
            first_set: self.first_set.union(other.first_set),
            flast_set: self.flast_set.union(other.flast_set),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Epsilon {
    ty: Type,
}

impl From<ast::Epsilon> for Epsilon {
    fn from(_: ast::Epsilon) -> Self {
        Self {
            ty: Type::new(true, FirstSet::default(), FlastSet::default()),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Literal {
    inner: ast::Literal,
    ty: Type,
}

impl Literal {
    pub fn inner(&self) -> &ast::Literal {
        &self.inner
    }
}

impl From<ast::Literal> for Literal {
    fn from(inner: ast::Literal) -> Self {
        let ty = Type::of_str(&inner);
        Self { inner, ty }
    }
}

impl From<Literal> for ast::Literal {
    fn from(lit: Literal) -> Self {
        lit.inner
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Cat {
    terms: Vec<TypedExpression>,
    ty: Type,
}

impl Cat {
    fn new<I: IntoIterator<Item = (Option<Span>, TypedExpression)>>(
        first: TypedExpression,
        punct: Option<Span>,
        second: TypedExpression,
        rest: I,
    ) -> Result<Self, CatError> {
        if first.get_type().nullable() {
            return Err(CatError::FirstNullable(first, punct));
        }

        iter::once((punct, second))
            .chain(rest)
            .try_fold(
                (first.get_type().clone(), vec![first]),
                |(ty, mut terms), (punct, right)| {
                    if !ty
                        .flast_set()
                        .intersect_first(right.get_type().first_set())
                        .is_empty()
                    {
                        Err(CatError::FirstFlastOverlap(terms, punct, right))
                    } else {
                        let ty = ty.cat(right.get_type().clone());
                        terms.push(right);
                        Ok((ty, terms))
                    }
                },
            )
            .map(|(ty, terms)| Self { ty, terms })
    }
}

impl IntoIterator for Cat {
    type Item = TypedExpression;

    type IntoIter = <Vec<TypedExpression> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.terms.into_iter()
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Alt {
    terms: Vec<TypedExpression>,
    ty: Type,
}

impl Alt {
    pub fn new<I: IntoIterator<Item = (Option<Span>, TypedExpression)>>(
        first: TypedExpression,
        punct: Option<Span>,
        second: TypedExpression,
        rest: I,
    ) -> Result<Self, AltError> {
        iter::once((punct, second))
            .chain(rest)
            .try_fold(
                (first.get_type().clone(), vec![first]),
                |(ty, mut terms), (punct, right)| {
                    if ty.nullable() && right.get_type().nullable() {
                        Err(AltError::BothNullable(terms, punct, right))
                    } else if !ty
                        .first_set()
                        .intersect(right.get_type().first_set())
                        .is_empty()
                    {
                        Err(AltError::FirstOverlap(terms, punct, right))
                    } else {
                        let ty = ty.alt(right.get_type().clone());
                        terms.push(right);
                        Ok((ty, terms))
                    }
                },
            )
            .map(|(ty, terms)| Self { ty, terms })
    }
}

impl IntoIterator for Alt {
    type Item = TypedExpression;

    type IntoIter = <Vec<TypedExpression> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.terms.into_iter()
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Fix {
    inner: Box<TypedExpression>,
}

impl Fix {
    pub fn unwrap(self) -> TypedExpression {
        *self.inner
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Variable {
    inner: ast::Variable,
    ty: Type,
}

impl Variable {
    pub fn index(&self) -> usize {
        self.inner.index
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum RawTypedExpression {
    Epsilon(Epsilon),
    Literal(Literal),
    Cat(Cat),
    Alt(Alt),
    Fix(Fix),
    Variable(Variable),
}

impl From<Epsilon> for RawTypedExpression {
    fn from(eps: Epsilon) -> Self {
        Self::Epsilon(eps)
    }
}

impl From<Literal> for RawTypedExpression {
    fn from(lit: Literal) -> Self {
        Self::Literal(lit)
    }
}

impl From<Cat> for RawTypedExpression {
    fn from(cat: Cat) -> Self {
        Self::Cat(cat)
    }
}

impl From<Alt> for RawTypedExpression {
    fn from(alt: Alt) -> Self {
        Self::Alt(alt)
    }
}

impl From<Fix> for RawTypedExpression {
    fn from(fix: Fix) -> Self {
        Self::Fix(fix)
    }
}

impl From<Variable> for RawTypedExpression {
    fn from(var: Variable) -> Self {
        Self::Variable(var)
    }
}

#[derive(Clone, Debug)]
pub struct TypedExpression {
    inner: RawTypedExpression,
    pub name: Option<Name>,
    pub span: Option<Span>,
}

impl PartialEq for TypedExpression {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner && self.name == other.name
    }
}

impl Eq for TypedExpression {}

impl hash::Hash for TypedExpression {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
        self.name.hash(state);
    }
}

pub trait Typed {
    fn get_type(&self) -> &Type;
}

macro_rules! leaf_typed {
    ($ty:ty, $field:ident) => {
        impl Typed for $ty {
            fn get_type(&self) -> &Type {
                &self.$field
            }
        }
    };
}

leaf_typed!(Epsilon, ty);
leaf_typed!(Literal, ty);
leaf_typed!(Cat, ty);
leaf_typed!(Alt, ty);
leaf_typed!(Variable, ty);

impl Typed for Fix {
    fn get_type(&self) -> &Type {
        self.inner.get_type()
    }
}

impl Typed for TypedExpression {
    fn get_type(&self) -> &Type {
        match &self.inner {
            RawTypedExpression::Epsilon(e) => e.get_type(),
            RawTypedExpression::Literal(l) => l.get_type(),
            RawTypedExpression::Cat(c) => c.get_type(),
            RawTypedExpression::Alt(a) => a.get_type(),
            RawTypedExpression::Fix(f) => f.get_type(),
            RawTypedExpression::Variable(v) => v.get_type(),
        }
    }
}
