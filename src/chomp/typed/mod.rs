use std::{fmt, iter};

use once_cell::sync::Lazy;
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

use self::error::{AltError, CatError, NeedGroundError, TypeError};
pub use self::infer::TypeInfer;

#[derive(Clone)]
pub enum Type {
    Ground(GroundType),
    Function(Box<dyn FunctionType + Send + Sync>),
}

impl Type {
    pub fn as_ground(&self, span: Option<Span>) -> Result<&GroundType, NeedGroundError> {
        match self {
            Self::Ground(ty) => Ok(ty),
            Self::Function(_) => Err(NeedGroundError { span }),
        }
    }

    pub fn into_ground(self, span: Option<Span>) -> Result<GroundType, NeedGroundError> {
        match self {
            Self::Ground(ty) => Ok(ty),
            Self::Function(_) => Err(NeedGroundError { span }),
        }
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ground(ty) => ty.fmt(f),
            Self::Function(_) => write!(f, "<fn>"),
        }
    }
}

impl From<GroundType> for Type {
    fn from(ty: GroundType) -> Self {
        Self::Ground(ty)
    }
}

impl From<Box<dyn FunctionType + Send + Sync>> for Type {
    fn from(f: Box<dyn FunctionType + Send + Sync>) -> Self {
        Self::Function(f)
    }
}

pub trait FunctionType : Fn(Type) -> Result<Type, TypeError> + Send + Sync{
    fn clone_box(&self) -> Box<dyn FunctionType + Send + Sync>;
}

impl<F: 'static + Fn(Type) -> Result<Type, TypeError> + Clone + Send + Sync> FunctionType for F {
    fn clone_box(&self) -> Box<dyn FunctionType + Send + Sync> {
        Box::new(self.clone()) as Box<dyn FunctionType + Send + Sync>
    }
}

impl Clone for Box<dyn FunctionType + Send + Sync> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

#[derive(Debug, Default, Clone, Eq, Hash, PartialEq)]
pub struct GroundType {
    nullable: bool,
    first_set: FirstSet,
    flast_set: FlastSet,
}

impl GroundType {
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

static EPSILON_TY: Lazy<Type> = Lazy::new(|| GroundType::new(true, FirstSet::default(), FlastSet::default()).into());

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Epsilon;

#[derive(Clone, Debug)]
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
        let ty = GroundType::of_str(&inner).into();
        Self { inner, ty }
    }
}

impl From<Literal> for ast::Literal {
    fn from(lit: Literal) -> Self {
        lit.inner
    }
}

#[derive(Clone, Debug)]
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
    ) -> Result<Self, TypeError> {
        let first_ty = first.get_type().as_ground(punct)?;
        if first_ty.nullable() {
            return Err(CatError::FirstNullable { expr: first, punct }.into());
        }

        iter::once((punct, second))
            .chain(rest)
            .try_fold(
                (first_ty.clone(), vec![first]),
                |(ty, mut terms), (punct, right)| {
                    let right_ty = right.get_type().as_ground(punct)?;
                    if ty
                        .flast_set()
                        .intersect_first(right_ty.first_set())
                        .is_empty()
                    {
                        let ty = ty.cat(right_ty.clone());
                        terms.push(right);
                        Ok((ty, terms))
                    } else {
                        Err(CatError::FirstFlastOverlap {
                            first: terms,
                            punct,
                            next: right,
                        }.into())
                    }
                },
            )
            .map(|(ty, terms)| Self { ty: ty.into(), terms })
    }
}

impl IntoIterator for Cat {
    type Item = TypedExpression;

    type IntoIter = <Vec<TypedExpression> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.terms.into_iter()
    }
}

#[derive(Clone, Debug)]
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
    ) -> Result<Self, TypeError> {
        iter::once((punct, second))
            .chain(rest)
            .try_fold(
                (first.get_type().as_ground(punct)?.clone(), vec![first]),
                |(ty, mut terms), (punct, right)| {
                    let right_ty = right.get_type().as_ground(punct)?;
                    if ty.nullable() && right_ty.nullable() {
                        Err(AltError::BothNullable {
                            left: terms,
                            punct,
                            right,
                        }.into())
                    } else if ty
                        .first_set()
                        .intersect(right_ty.first_set())
                        .is_empty()
                    {
                        let ty = ty.alt(right_ty.clone());
                        terms.push(right);
                        Ok((ty, terms))
                    } else {
                        Err(AltError::FirstOverlap {
                            left: terms,
                            punct,
                            right,
                        }.into())
                    }
                },
            )
            .map(|(ty, terms)| Self { ty: ty.into(), terms })
    }
}

impl IntoIterator for Alt {
    type Item = TypedExpression;

    type IntoIter = <Vec<TypedExpression> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.terms.into_iter()
    }
}

#[derive(Clone, Debug)]
pub struct Fix {
    inner: Box<TypedExpression>,
}

impl Fix {
    pub fn unwrap(self) -> TypedExpression {
        *self.inner
    }
}

#[derive(Clone, Debug)]
pub struct Variable {
    inner: ast::Variable,
    ty: Type,
}

impl Variable {
    pub fn index(&self) -> usize {
        self.inner.index
    }
}

#[derive(Clone, Debug)]
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

pub trait Typed {
    fn get_type(&self) -> &Type;
}

impl Typed for Epsilon {
    fn get_type(&self) -> &Type {
        &EPSILON_TY
    }
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
