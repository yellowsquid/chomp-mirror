use std::convert::Infallible;
use std::fmt::Display;

use proc_macro2::Span;
use syn::{Ident, LitStr, Token};
use typed::FirstSetContext;
use typed::FlastSetContext;
use typed::NullContext;

use self::typed::FirstSet;
use self::typed::FlastSet;
use self::typed::Type;

pub mod convert;
pub mod typed;

fn fix<R: PartialEq, F: FnMut(&R) -> R>(init: R, mut step: F) -> R {
    let mut res = init;
    let mut last = None;

    while last.map(|v| v != res).unwrap_or(true) {
        last = Some(res);
        res = step(last.as_ref().unwrap());
    }

    res
}

#[derive(Copy, Clone, Debug)]
pub struct Epsilon {
    span: Span,
}

impl Epsilon {
    pub fn new(span: Span) -> Self {
        Self { span }
    }
}

impl Type for Epsilon {
    type Err = Infallible;

    fn closed(&self, _depth: usize) -> Result<(), VariableError> {
        Ok(())
    }

    fn is_nullable<C: NullContext>(&self, _context: &mut C) -> Option<bool> {
        Some(true)
    }

    fn first_set<C: FirstSetContext>(&self, _context: &mut C) -> Option<FirstSet> {
        Some(FirstSet::new())
    }

    fn flast_set<C: FlastSetContext>(&self, _context: &mut C) -> Option<FlastSet> {
        Some(FlastSet::new())
    }

    fn well_typed<C: FlastSetContext>(self, _context: &mut C) -> Result<Typed, Self::Err> {
        Ok(Typed {
            kind: TypeKind::Epsilon,
            nullable: true,
            first_set: FirstSet::new(),
            flast_set: FlastSet::new(),
        })
    }
}

pub type Literal = LitStr;

impl Type for Literal {
    type Err = Infallible;

    fn closed(&self, _depth: usize) -> Result<(), VariableError> {
        Ok(())
    }

    fn is_nullable<C: NullContext>(&self, _context: &mut C) -> Option<bool> {
        Some(self.value().is_empty())
    }

    fn first_set<C: FirstSetContext>(&self, _context: &mut C) -> Option<FirstSet> {
        Some(FirstSet::of_str(&self.value()))
    }

    fn flast_set<C: FlastSetContext>(&self, _context: &mut C) -> Option<FlastSet> {
        Some(FlastSet::new())
    }

    fn well_typed<C: FlastSetContext>(self, _context: &mut C) -> Result<Typed, Self::Err> {
        let value = self.value();
        let nullable = value.is_empty();
        let first_set = FirstSet::of_str(&value);
        Ok(Typed {
            kind: TypeKind::Literal(value),
            nullable,
            first_set,
            flast_set: FlastSet::new(),
        })
    }
}

#[derive(Clone, Debug)]
pub struct Cat {
    fst: Box<Term>,
    punct: Token![.],
    snd: Box<Term>,
}

impl Cat {
    pub fn new(fst: Term, punct: Token![.], snd: Term) -> Self {
        Self {
            fst: Box::new(fst),
            punct,
            snd: Box::new(snd),
        }
    }
}

impl Type for Cat {
    type Err = CatError;

    fn closed(&self, depth: usize) -> Result<(), VariableError> {
        self.fst.closed(depth).and_then(|()| self.snd.closed(depth))
    }

    fn is_nullable<C: NullContext>(&self, context: &mut C) -> Option<bool> {
        Some(self.fst.is_nullable(context)? && self.snd.is_nullable(context)?)
    }

    fn first_set<C: FirstSetContext>(&self, context: &mut C) -> Option<FirstSet> {
        let set = self.fst.first_set(context)?;

        if self.fst.is_nullable(context)? {
            Some(set.union(self.snd.first_set(context)?))
        } else {
            Some(set)
        }
    }

    fn flast_set<C: FlastSetContext>(&self, context: &mut C) -> Option<FlastSet> {
        let set = self.snd.flast_set(context)?;

        if self.snd.is_nullable(context)? {
            Some(
                set.union_first(self.snd.first_set(context)?)
                    .union(self.fst.flast_set(context)?),
            )
        } else {
            Some(set)
        }
    }

    fn well_typed<C: FlastSetContext>(self, context: &mut C) -> Result<Typed, Self::Err> {
        let fst = self
            .fst
            .well_typed(context)
            .map_err(|e| CatError::First(Box::new(e)))?;
        let snd = self
            .snd
            .well_typed(context)
            .map_err(|e| CatError::Second(Box::new(e)))?;

        if fst.is_nullable() {
            Err(CatError::FirstNullable(fst))
        } else if !fst.flast_set().intersect_first(&snd.first_set()).is_empty() {
            Err(CatError::FirstFlastOverlap(fst, snd))
        } else {
            // fst.is_nullable always false
            let nullable = false;
            let first_set = fst.first_set().clone();
            let flast_set = if snd.is_nullable() {
                snd.flast_set()
                    .clone()
                    .union_first(snd.first_set().clone())
                    .union(fst.flast_set().clone())
            } else {
                snd.flast_set().clone()
            };
            Ok(Typed {
                kind: TypeKind::Cat(Box::new(fst), Box::new(snd)),
                nullable,
                first_set,
                flast_set,
            })
        }
    }
}

#[derive(Debug)]
pub enum CatError {
    First(Box<TermError>),
    Second(Box<TermError>),
    FirstNullable(Typed),
    FirstFlastOverlap(Typed, Typed),
}

impl Display for CatError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub struct Alt {
    left: Box<Term>,
    punct: Token![|],
    right: Box<Term>,
}

impl Alt {
    pub fn new(left: Term, punct: Token![|], right: Term) -> Self {
        Self {
            left: Box::new(left),
            punct,
            right: Box::new(right),
        }
    }
}

impl Type for Alt {
    type Err = AltError;

    fn closed(&self, depth: usize) -> Result<(), VariableError> {
        self.left
            .closed(depth)
            .and_then(|()| self.right.closed(depth))
    }

    fn is_nullable<C: NullContext>(&self, context: &mut C) -> Option<bool> {
        Some(self.left.is_nullable(context)? || self.right.is_nullable(context)?)
    }

    fn first_set<C: FirstSetContext>(&self, context: &mut C) -> Option<FirstSet> {
        Some(
            self.left
                .first_set(context)?
                .union(self.right.first_set(context)?),
        )
    }

    fn flast_set<C: FlastSetContext>(&self, context: &mut C) -> Option<FlastSet> {
        Some(
            self.left
                .flast_set(context)?
                .union(self.right.flast_set(context)?),
        )
    }

    fn well_typed<C: FlastSetContext>(self, context: &mut C) -> Result<Typed, Self::Err> {
        let left = self
            .left
            .well_typed(context)
            .map_err(|e| AltError::Left(Box::new(e)))?;
        let right = self
            .right
            .well_typed(context)
            .map_err(|e| AltError::Right(Box::new(e)))?;

        if left.is_nullable() && right.is_nullable() {
            Err(AltError::BothNullable(left, right))
        } else if !left.first_set().intersect(&right.first_set()).is_empty() {
            Err(AltError::FirstOverlap(left, right))
        } else {
            let nullable = false;
            let first_set = left.first_set().clone().union(right.first_set().clone());
            let flast_set = left.flast_set().clone().union(right.flast_set().clone());
            Ok(Typed {
                kind: TypeKind::Alt(Box::new(left), Box::new(right)),
                nullable,
                first_set,
                flast_set,
            })
        }
    }
}

#[derive(Debug)]
pub enum AltError {
    Left(Box<TermError>),
    Right(Box<TermError>),
    BothNullable(Typed, Typed),
    FirstOverlap(Typed, Typed),
}

impl Display for AltError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub struct Fix {
    span: Span,
    arg: Ident,
    inner: Box<Term>,
}

impl Fix {
    pub fn new(arg: Ident, inner: Term, span: Span) -> Self {
        Self {
            arg,
            inner: Box::new(inner),
            span,
        }
    }
}

impl Type for Fix {
    type Err = TermError;

    fn closed(&self, depth: usize) -> Result<(), VariableError> {
        self.inner.closed(depth + 1)
    }

    fn is_nullable<C: NullContext>(&self, context: &mut C) -> Option<bool> {
        fix(Some(false), |last| {
            last.as_ref()
                .copied()
                .and_then(|null| context.push_nullable(null, |ctx| self.inner.is_nullable(ctx)))
        })
    }

    fn first_set<C: FirstSetContext>(&self, context: &mut C) -> Option<FirstSet> {
        let nullable = self.is_nullable(context)?;
        fix(Some(FirstSet::new()), |last| {
            last.as_ref().cloned().and_then(|first_set| {
                context.push_first_set(nullable, first_set, |ctx| self.inner.first_set(ctx))
            })
        })
    }

    fn flast_set<C: FlastSetContext>(&self, context: &mut C) -> Option<FlastSet> {
        let nullable = self.is_nullable(context)?;
        let first_set = self.first_set(context)?;
        fix(Some(FlastSet::new()), |last| {
            last.as_ref().cloned().and_then(|flast_set| {
                context.push_flast_set(nullable, first_set.clone(), flast_set, |ctx| {
                    self.inner.flast_set(ctx)
                })
            })
        })
    }

    fn well_typed<C: FlastSetContext>(self, context: &mut C) -> Result<Typed, Self::Err> {
        self.inner.closed(context.get_depth() + 1)?;

        let nullable = self.is_nullable(context).unwrap();
        let first_set = self.first_set(context).unwrap();
        let flast_set = self.flast_set(context).unwrap();

        context
            .push_flast_set(nullable, first_set.clone(), flast_set.clone(), |ctx| {
                self.inner.well_typed(ctx)
            })
            .map(|inner| Typed {
                kind: TypeKind::Fix(Box::new(inner)),
                nullable,
                first_set,
                flast_set,
            })
    }
}

#[derive(Clone, Debug)]
pub struct Variable {
    name: Ident,
    index: usize,
}

impl Variable {
    pub fn new(name: Ident, index: usize) -> Self {
        Self { name, index }
    }
}

impl Type for Variable {
    type Err = VariableError;

    fn closed(&self, depth: usize) -> Result<(), VariableError> {
        if self.index < depth {
            Ok(())
        } else {
            Err(VariableError::FreeVariable(self.clone()))
        }
    }

    fn is_nullable<C: NullContext>(&self, context: &mut C) -> Option<bool> {
        context.get_nullable(self.index)
    }

    fn first_set<C: FirstSetContext>(&self, context: &mut C) -> Option<FirstSet> {
        context.get_first_set(self.index).cloned()
    }

    fn flast_set<C: FlastSetContext>(&self, context: &mut C) -> Option<FlastSet> {
        context.get_flast_set(self.index).cloned()
    }

    fn well_typed<C: FlastSetContext>(self, context: &mut C) -> Result<Typed, Self::Err> {
        self.closed(context.get_depth()).map(|()| Typed {
            kind: TypeKind::Variable(self.index),
            nullable: self.is_nullable(context).unwrap(),
            first_set: self.first_set(context).unwrap(),
            flast_set: self.flast_set(context).unwrap(),
        })
    }
}

#[derive(Debug)]
pub enum VariableError {
    FreeVariable(Variable),
}

impl Display for VariableError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub struct Call {
    span: Span,
    name: Ident,
    args: Vec<Term>,
}

impl Call {
    pub fn new(name: Ident, args: Vec<Term>, span: Span) -> Self {
        Self { name, args, span }
    }
}

impl Type for Call {
    type Err = Infallible;

    fn closed(&self, _depth: usize) -> Result<(), VariableError> {
        todo!()
    }

    fn is_nullable<C: NullContext>(&self, _context: &mut C) -> Option<bool> {
        todo!()
    }

    fn first_set<C: FirstSetContext>(&self, _context: &mut C) -> Option<FirstSet> {
        todo!()
    }

    fn flast_set<C: FlastSetContext>(&self, _context: &mut C) -> Option<FlastSet> {
        todo!()
    }

    fn well_typed<C: FlastSetContext>(self, _context: &mut C) -> Result<Typed, Self::Err> {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub enum Term {
    Epsilon(Epsilon),
    Literal(Literal),
    Cat(Cat),
    Alt(Alt),
    Fix(Fix),
    Variable(Variable),
    Call(Call),
}

impl Type for Term {
    type Err = TermError;

    fn closed(&self, depth: usize) -> Result<(), VariableError> {
        match self {
            Self::Epsilon(e) => e.closed(depth),
            Self::Literal(e) => e.closed(depth),
            Self::Cat(e) => e.closed(depth),
            Self::Alt(e) => e.closed(depth),
            Self::Fix(e) => e.closed(depth),
            Self::Variable(e) => e.closed(depth),
            Self::Call(e) => e.closed(depth),
        }
    }

    fn is_nullable<C: NullContext>(&self, context: &mut C) -> Option<bool> {
        match self {
            Self::Epsilon(e) => e.is_nullable(context),
            Self::Literal(e) => e.is_nullable(context),
            Self::Cat(e) => e.is_nullable(context),
            Self::Alt(e) => e.is_nullable(context),
            Self::Fix(e) => e.is_nullable(context),
            Self::Variable(e) => e.is_nullable(context),
            Self::Call(e) => e.is_nullable(context),
        }
    }

    fn first_set<C: FirstSetContext>(&self, context: &mut C) -> Option<FirstSet> {
        match self {
            Self::Epsilon(e) => e.first_set(context),
            Self::Literal(e) => e.first_set(context),
            Self::Cat(e) => e.first_set(context),
            Self::Alt(e) => e.first_set(context),
            Self::Fix(e) => e.first_set(context),
            Self::Variable(e) => e.first_set(context),
            Self::Call(e) => e.first_set(context),
        }
    }

    fn flast_set<C: FlastSetContext>(&self, context: &mut C) -> Option<FlastSet> {
        match self {
            Self::Epsilon(e) => e.flast_set(context),
            Self::Literal(e) => e.flast_set(context),
            Self::Cat(e) => e.flast_set(context),
            Self::Alt(e) => e.flast_set(context),
            Self::Fix(e) => e.flast_set(context),
            Self::Variable(e) => e.flast_set(context),
            Self::Call(e) => e.flast_set(context),
        }
    }

    fn well_typed<C: FlastSetContext>(self, context: &mut C) -> Result<Typed, Self::Err> {
        match self {
            Self::Epsilon(e) => e.well_typed(context).map_err(TermError::from),
            Self::Literal(e) => e.well_typed(context).map_err(TermError::from),
            Self::Cat(e) => e.well_typed(context).map_err(TermError::from),
            Self::Alt(e) => e.well_typed(context).map_err(TermError::from),
            Self::Fix(e) => e.well_typed(context).map_err(TermError::from),
            Self::Variable(e) => e.well_typed(context).map_err(TermError::from),
            Self::Call(e) => e.well_typed(context).map_err(TermError::from),
        }
    }
}

#[derive(Debug)]
pub enum TermError {
    Cat(CatError),
    Alt(AltError),
    Variable(VariableError),
}

impl From<Infallible> for TermError {
    fn from(other: Infallible) -> Self {
        match other {}
    }
}

impl From<CatError> for TermError {
    fn from(other: CatError) -> Self {
        Self::Cat(other)
    }
}

impl From<AltError> for TermError {
    fn from(other: AltError) -> Self {
        Self::Alt(other)
    }
}

impl From<VariableError> for TermError {
    fn from(other: VariableError) -> Self {
        Self::Variable(other)
    }
}

impl Display for TermError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum TypeKind {
    Epsilon,
    Literal(String),
    Cat(Box<Typed>, Box<Typed>),
    Alt(Box<Typed>, Box<Typed>),
    Fix(Box<Typed>),
    Variable(usize),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Typed {
    kind: TypeKind,
    nullable: bool,
    first_set: FirstSet,
    flast_set: FlastSet,
}

impl Typed {
    pub fn is_nullable(&self) -> bool {
        self.nullable
    }

    pub fn first_set(&self) -> &FirstSet {
        &self.first_set
    }

    pub fn flast_set(&self) -> &FlastSet {
        &self.flast_set
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn null_epsilon() {
//         assert!(Term::Epsilon.null(&[]));
//     }

//     #[test]
//     fn not_null_bottom() {
//         assert!(!Term::Bottom.null(&[]));
//     }

//     #[test]
//     fn not_null_normal_literal() {
//         assert!(!Term::Literal("x".to_owned()).null(&[]))
//     }

//     #[test]
//     fn null_empty_literal() {
//         assert!(Term::Literal("".to_owned()).null(&[]))
//     }

//     #[test]
//     fn null_cat_both_null() {
//         assert!(Term::Cat(Box::new(Term::Epsilon), Box::new(Term::Epsilon)).null(&[]))
//     }

//     #[test]
//     fn not_null_cat_first_not_null() {
//         assert!(!Term::Cat(Box::new(Term::Bottom), Box::new(Term::Epsilon)).null(&[]))
//     }

//     #[test]
//     fn not_null_cat_second_not_null() {
//         assert!(!Term::Cat(Box::new(Term::Epsilon), Box::new(Term::Bottom)).null(&[]))
//     }

//     #[test]
//     fn not_null_alt_both_not_null() {
//         assert!(!Term::Alt(Box::new(Term::Bottom), Box::new(Term::Bottom)).null(&[]))
//     }

//     #[test]
//     fn null_alt_first_null() {
//         assert!(Term::Alt(Box::new(Term::Epsilon), Box::new(Term::Bottom)).null(&[]))
//     }

//     #[test]
//     fn null_alt_second_null() {
//         assert!(Term::Alt(Box::new(Term::Bottom), Box::new(Term::Epsilon)).null(&[]))
//     }

//     #[test]
//     fn not_null_fix_simple_inner() {
//         assert!(!Term::Fix(Box::new(Term::Bottom)).null(&[]))
//     }

//     #[test]
//     fn null_fix_simple_inner() {
//         assert!(Term::Fix(Box::new(Term::Epsilon)).null(&[]))
//     }

//     #[test]
//     fn not_null_fix_var_inner() {
//         assert!(!Term::Fix(Box::new(Term::Variable(0))).null(&[]))
//     }

//     #[test]
//     fn null_fix_var_inner() {
//         assert!(Term::Fix(Box::new(Term::Alt(
//             Box::new(Term::Epsilon),
//             Box::new(Term::Variable(0))
//         )))
//         .null(&[]))
//     }

//     #[test]
//     fn first_epsilon_empty() {
//         assert_eq!(Term::Epsilon.first(&[]), BTreeSet::new())
//     }

//     #[test]
//     fn first_bottom_empty() {
//         assert_eq!(Term::Bottom.first(&[]), BTreeSet::new())
//     }

//     #[test]
//     fn first_literal_first_char() {
//         let set = vec!['x'].into_iter().collect();
//         assert_eq!(Term::Literal("x".to_owned()).first(&[]), set);
//         let set = vec!['h'].into_iter().collect();
//         assert_eq!(Term::Literal("hello".to_owned()).first(&[]), set);
//         assert_eq!(Term::Literal("".to_owned()).first(&[]), BTreeSet::new());
//     }

//     #[test]
//     fn first_cat_first_not_null() {
//         let set = vec!['x'].into_iter().collect();
//         assert_eq!(
//             Term::Cat(
//                 Box::new(Term::Literal("x".to_owned())),
//                 Box::new(Term::Literal("y".to_owned()))
//             )
//             .first(&[]),
//             set
//         );
//     }

//     #[test]
//     fn first_cat_first_null() {
//         let set = vec!['x', 'y'].into_iter().collect();
//         assert_eq!(
//             Term::Cat(
//                 Box::new(Term::Alt(
//                     Box::new(Term::Epsilon),
//                     Box::new(Term::Literal("x".to_owned()))
//                 )),
//                 Box::new(Term::Literal("y".to_owned()))
//             )
//             .first(&[]),
//             set
//         );
//     }

//     // TODO: test flast

//     #[test]
//     fn types_epsilon() {
//         assert_eq!(Term::Epsilon.type_check(&[]), Ok(Typed::Epsilon))
//     }

//     #[test]
//     fn types_bottom() {
//         assert_eq!(Term::Bottom.type_check(&[]), Ok(Typed::Bottom))
//     }

//     #[test]
//     fn types_literal() {
//         assert_eq!(
//             Term::Literal("x".to_owned()).type_check(&[]),
//             Ok(Typed::Literal("x".to_owned()))
//         );
//         assert_eq!(
//             Term::Literal("".to_owned()).type_check(&[]),
//             Ok(Typed::Literal("".to_owned()))
//         )
//     }
// }
