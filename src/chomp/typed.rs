use std::hash::{Hash, Hasher};

use proc_macro2::Span;
use syn::{Ident, Token};

use super::{
    ast,
    set::{FirstSet, FlastSet},
    Name,
};

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

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Epsilon {
    inner: Option<Token![_]>,
    ty: Type,
}

impl From<ast::Epsilon> for Epsilon {
    fn from(inner: ast::Epsilon) -> Self {
        Self {
            inner,
            ty: Type::new(true, FirstSet::default(), FlastSet::default()),
        }
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Literal {
    inner: ast::Literal,
    ty: Type,
}

impl Literal {
    pub fn inner(&self) -> &ast::Literal {
        &self.inner
    }

    pub fn span(&self) -> Option<Span> {
        self.inner.span()
    }
}

impl From<ast::Literal> for Literal {
    fn from(inner: ast::Literal) -> Self {
        let ty = Type::of_str(&inner.value());
        Self { inner, ty }
    }
}

impl From<Literal> for ast::Literal {
    fn from(lit: Literal) -> Self {
        lit.inner
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Cat {
    fst: Box<TypedExpression>,
    punct: Option<Token![.]>,
    snd: Box<TypedExpression>,
    ty: Type,
}

impl Cat {
    pub(crate) fn new(
        fst: TypedExpression,
        punct: Option<Token![.]>,
        snd: TypedExpression,
        ty: Type,
    ) -> Self {
        Self {
            fst: Box::new(fst),
            punct,
            snd: Box::new(snd),
            ty,
        }
    }

    pub fn unwrap(self) -> (TypedExpression, Option<Token![.]>, TypedExpression) {
        (*self.fst, self.punct, *self.snd)
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Alt {
    left: Box<TypedExpression>,
    punct: Option<Token![|]>,
    right: Box<TypedExpression>,
    ty: Type,
}

impl Alt {
    pub(crate) fn new(
        left: TypedExpression,
        punct: Option<Token![|]>,
        right: TypedExpression,
        ty: Type,
    ) -> Self {
        Self {
            left: Box::new(left),
            punct,
            right: Box::new(right),
            ty,
        }
    }

    pub fn unwrap(self) -> (TypedExpression, Option<Token![|]>, TypedExpression) {
        (*self.left, self.punct, *self.right)
    }
}

#[derive(Debug)]
pub struct Fix {
    arg: Name,
    inner: Box<TypedExpression>,
    span: Option<Span>,
    ty: Type,
}

impl Fix {
    pub(crate) fn new(arg: Name, inner: TypedExpression, span: Option<Span>, ty: Type) -> Self {
        Self {
            arg,
            inner: Box::new(inner),
            span,
            ty,
        }
    }

    pub fn unwrap(self) -> (Name, TypedExpression, Option<Span>) {
        (self.arg, *self.inner, self.span)
    }
}

impl PartialEq for Fix {
    fn eq(&self, other: &Self) -> bool {
        self.arg == other.arg && self.inner == other.inner && self.ty == other.ty
    }
}

impl Eq for Fix {}

impl Hash for Fix {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.arg.hash(state);
        self.inner.hash(state);
        self.ty.hash(state);
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Variable {
    inner: ast::Variable,
    ty: Type,
}

impl Variable {
    pub(crate) fn new(inner: ast::Variable, ty: Type) -> Self {
        Self { inner, ty }
    }

    pub fn index(&self) -> usize {
        self.inner.index
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub(crate) enum RawTypedExpression {
    Epsilon(Epsilon),
    Literal(Literal),
    Cat(Cat),
    Alt(Alt),
    Fix(Fix),
    Variable(Variable),
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct TypedExpression {
    pub(crate) inner: RawTypedExpression,
}

impl From<Epsilon> for TypedExpression {
    fn from(eps: Epsilon) -> Self {
        Self {
            inner: RawTypedExpression::Epsilon(eps),
        }
    }
}

impl From<Literal> for TypedExpression {
    fn from(lit: Literal) -> Self {
        Self {
            inner: RawTypedExpression::Literal(lit),
        }
    }
}

impl From<Cat> for TypedExpression {
    fn from(cat: Cat) -> Self {
        Self {
            inner: RawTypedExpression::Cat(cat),
        }
    }
}

impl From<Alt> for TypedExpression {
    fn from(alt: Alt) -> Self {
        Self {
            inner: RawTypedExpression::Alt(alt),
        }
    }
}

impl From<Fix> for TypedExpression {
    fn from(fix: Fix) -> Self {
        Self {
            inner: RawTypedExpression::Fix(fix),
        }
    }
}

impl From<Variable> for TypedExpression {
    fn from(var: Variable) -> Self {
        Self {
            inner: RawTypedExpression::Variable(var),
        }
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
leaf_typed!(Fix, ty);
leaf_typed!(Variable, ty);

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
