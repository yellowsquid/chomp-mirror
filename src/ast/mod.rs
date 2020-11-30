use self::typed::{FirstContext, FlastContext, NullContext};
use proc_macro2::{Span, TokenStream, TokenTree};
use quote::{format_ident, quote};
use std::collections::{BTreeSet, HashMap};
use std::error::Error;
use std::fmt::{self, Display};
use syn::{Ident, LitStr, Token};

pub mod convert;
pub mod typed;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Never {}

impl From<Never> for syn::Error {
    fn from(other: Never) -> Self {
        match other {}
    }
}

pub type Epsilon = Token![_];

pub type Literal = LitStr;

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

#[derive(Debug)]
pub enum CatError {
    FirstNullable {
        punct: Span,
        fst_span: Span,
        fst: Typed,
    },
    FirstFlastOverlap {
        punct: Span,
        fst_span: Span,
        fst: Typed,
        snd_span: Span,
        snd: Typed,
    },
}

impl From<CatError> for syn::Error {
    fn from(other: CatError) -> Self {
        match other {
            CatError::FirstNullable {
                punct, fst_span, ..
            } => {
                let mut err = Self::new(
                    punct,
                    "first item in sequence cannot accept the empty string",
                );
                err.combine(Self::new(fst_span, "this can accept empty string"));
                err
            }
            CatError::FirstFlastOverlap {
                punct,
                fst_span,
                snd_span,
                ..
            } => {
                let mut err = Self::new(
                    punct,
                    "cannot decide whether to repeat first or start accepting second",
                );
                err.combine(Self::new(fst_span, "a repetition of this"));
                err.combine(Self::new(snd_span, "collides with the start of this"));
                err
            }
        }
    }
}

impl Display for CatError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FirstNullable { punct, fst_span, fst, .. } => {
                let start = punct.start();
                let fst_start = fst_span.start();
                write!(f, "{}:{}: term '{}' ({}:{}) accepts the empty string", start.line, start.column, fst, fst_start.line, fst_start.column)
            }
            Self::FirstFlastOverlap { punct, fst, fst_span, snd, snd_span, .. } => {
                let start =punct.start();
                let fst_start = fst_span.start();
                let snd_start = snd_span.start();
                write!(
                f,
                "{}:{}: cannot decide whether to repeat '{}' ({}:{}) or start accepting '{}' ({}:{}).",
                start.line, start.column, fst, fst_start.line, fst_start.column, snd, snd_start.line, snd_start.column
            )},
        }
    }
}

impl Error for CatError {}

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

#[derive(Debug)]
pub enum AltError {
    BothNullable {
        punct: Span,
        left_span: Span,
        left: Typed,
        right_span: Span,
        right: Typed,
    },
    FirstOverlap {
        punct: Span,
        left_span: Span,
        left: Typed,
        right_span: Span,
        right: Typed,
    },
}

impl From<AltError> for syn::Error {
    fn from(other: AltError) -> Self {
        match other {
            AltError::BothNullable {
                punct,
                left_span,
                right_span,
                ..
            } => {
                let mut err = Self::new(punct, "both branches accept the empty string");
                err.combine(Self::new(left_span, "left branch accepts the empty string"));
                err.combine(Self::new(
                    right_span,
                    "right branch accepts the empty string",
                ));
                err
            }
            AltError::FirstOverlap {
                punct,
                left_span,
                right_span,
                ..
            } => {
                let mut err = Self::new(
                    punct,
                    "cannot decide whether to accept the left or right branch",
                );
                err.combine(Self::new(left_span, "left branch starts with a character"));
                err.combine(Self::new(
                    right_span,
                    "right branch starts with the same character",
                ));
                err
            }
        }
    }
}

impl Display for AltError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BothNullable { punct, left_span, left, right_span, right, .. } => {
                let start = punct.start();
                let left_start = left_span.start();
                let right_start = right_span.start();
                write!(
                f,
                "{}:{}: both '{}' ({}:{}) and '{}' ({}:{}) accept the empty string.",
                start.line, start.column, left, left_start.line, left_start.column, right, right_start.line, right_start.column,
            )},
            Self::FirstOverlap { punct, left_span,left, right_span, right, .. } => {
                let start = punct.start();
                let left_start = left_span.start();
                let right_start = right_span.start();
                write!(
                f,
                "{}:{}: cannot decide whether to accept '{}' ({}:{}) or '{}' ({}:{}).",
                start.line, start.column, left, left_start.line, left_start.column, right, right_start.line, right_start.column,
            )},
        }
    }
}

impl Error for AltError {}

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

    pub fn fixpoint<F: FnMut(&Self, &T) -> Result<T, E>, T: PartialEq, E>(
        &self,
        init: T,
        mut step: F,
    ) -> Result<T, E> {
        let mut res = init;
        let mut last = None;
        while last.map(|r| r != res).unwrap_or(true) {
            last = Some(res);
            res = step(self, last.as_ref().unwrap())?;
        }

        Ok(res)
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

#[derive(Debug)]
pub enum VariableError {
    FreeVariable(Variable),
    GuardedVariable(Variable),
}

impl From<VariableError> for syn::Error {
    fn from(other: VariableError) -> Self {
        match other {
            VariableError::FreeVariable(_) => todo!(),
            VariableError::GuardedVariable(_) => todo!(),
        }
    }
}

impl Display for VariableError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FreeVariable(var) => {
                let start = var.name.span().start();
                write!(f, "{}:{}: unbound variable '{}'", start.line, start.column, var.name)},
            Self::GuardedVariable(var) => {
                let start = var.name.span().start();
                write!(f, "{}:{}: variable '{}' is guarded", start.line, start.column, var.name)},
        }
    }
}

impl Error for VariableError {}

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

impl Term {
    pub fn visit<V: Visitor + ?Sized>(&self, visitor: &mut V) -> <V as Visitor>::Out {
        match self {
            Self::Epsilon(eps) => visitor.visit_epsilon(eps),
            Self::Literal(lit) => visitor.visit_literal(lit),
            Self::Cat(cat) => visitor.visit_cat(cat),
            Self::Alt(alt) => visitor.visit_alt(alt),
            Self::Fix(fix) => visitor.visit_fix(fix),
            Self::Variable(variable) => visitor.visit_variable(variable),
            Self::Call(call) => visitor.visit_call(call),
        }
    }

    pub fn visit_mut<V: VisitorMut>(&mut self, visitor: &mut V) -> <V as VisitorMut>::Out {
        match self {
            Self::Epsilon(eps) => visitor.visit_mut_epsilon(eps),
            Self::Literal(lit) => visitor.visit_mut_literal(lit),
            Self::Cat(cat) => visitor.visit_mut_cat(cat),
            Self::Alt(alt) => visitor.visit_mut_alt(alt),
            Self::Fix(fix) => visitor.visit_mut_fix(fix),
            Self::Variable(variable) => visitor.visit_mut_variable(variable),
            Self::Call(call) => visitor.visit_mut_call(call),
        }
    }

    pub fn fold<F: Folder>(self, folder: &mut F) -> <F as Folder>::Out {
        match self {
            Self::Epsilon(eps) => folder.fold_epsilon(eps),
            Self::Literal(lit) => folder.fold_literal(lit),
            Self::Cat(cat) => folder.fold_cat(cat),
            Self::Alt(alt) => folder.fold_alt(alt),
            Self::Fix(fix) => folder.fold_fix(fix),
            Self::Variable(variable) => folder.fold_variable(variable),
            Self::Call(call) => folder.fold_call(call),
        }
    }
}

#[derive(Debug)]
pub enum TermError {
    Cat(CatError),
    Alt(AltError),
    Variable(VariableError),
}

impl From<Never> for TermError {
    fn from(other: Never) -> Self {
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

impl From<TermError> for syn::Error {
    fn from(other: TermError) -> Self {
        match other {
            TermError::Cat(e) => e.into(),
            TermError::Alt(e) => e.into(),
            TermError::Variable(e) => e.into(),
        }
    }
}

impl Display for TermError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Cat(e) => e.fmt(f),
            Self::Alt(e) => e.fmt(f),
            Self::Variable(e) => e.fmt(f),
        }
    }
}

impl Error for TermError {}

pub trait Visitor {
    type Out;
    fn visit_epsilon(&mut self, eps: &Epsilon) -> Self::Out;

    fn visit_literal(&mut self, lit: &Literal) -> Self::Out;

    fn visit_cat(&mut self, cat: &Cat) -> Self::Out;

    fn visit_alt(&mut self, alt: &Alt) -> Self::Out;

    fn visit_fix(&mut self, fix: &Fix) -> Self::Out;

    fn visit_variable(&mut self, var: &Variable) -> Self::Out;

    fn visit_call(&mut self, call: &Call) -> Self::Out;
}

pub trait VisitorMut {
    type Out;
    fn visit_mut_epsilon(&mut self, eps: &mut Epsilon) -> Self::Out;
    fn visit_mut_literal(&mut self, lit: &mut Literal) -> Self::Out;
    fn visit_mut_cat(&mut self, cat: &mut Cat) -> Self::Out;
    fn visit_mut_alt(&mut self, alt: &mut Alt) -> Self::Out;
    fn visit_mut_fix(&mut self, fix: &mut Fix) -> Self::Out;
    fn visit_mut_variable(&mut self, var: &mut Variable) -> Self::Out;
    fn visit_mut_call(&mut self, call: &mut Call) -> Self::Out;
}

pub trait Folder {
    type Out;
    fn fold_epsilon(&mut self, eps: Epsilon) -> Self::Out;
    fn fold_literal(&mut self, lit: Literal) -> Self::Out;
    fn fold_cat(&mut self, cat: Cat) -> Self::Out;
    fn fold_alt(&mut self, alt: Alt) -> Self::Out;
    fn fold_fix(&mut self, fix: Fix) -> Self::Out;
    fn fold_variable(&mut self, var: Variable) -> Self::Out;
    fn fold_call(&mut self, call: Call) -> Self::Out;
}

struct Closed(usize);

impl Visitor for Closed {
    type Out = bool;

    fn visit_epsilon(&mut self, _eps: &Epsilon) -> Self::Out {
        true
    }

    fn visit_literal(&mut self, _lit: &Literal) -> Self::Out {
        true
    }

    fn visit_cat(&mut self, cat: &Cat) -> Self::Out {
        cat.fst.visit(self) && cat.snd.visit(self)
    }

    fn visit_alt(&mut self, alt: &Alt) -> Self::Out {
        alt.left.visit(self) && alt.right.visit(self)
    }

    fn visit_fix(&mut self, fix: &Fix) -> Self::Out {
        self.0 += 1;
        let res = fix.inner.visit(self);
        self.0 -= 1;
        res
    }

    fn visit_variable(&mut self, var: &Variable) -> Self::Out {
        self.0 > var.index
    }

    fn visit_call(&mut self, call: &Call) -> Self::Out {
        call.args.iter().all(|arg| arg.visit(self))
    }
}

struct Nullable<'a>(NullContext<'a>);

impl Visitor for Nullable<'_> {
    type Out = Result<bool, VariableError>;

    fn visit_epsilon(&mut self, _: &Epsilon) -> Self::Out {
        Ok(true)
    }

    fn visit_literal(&mut self, lit: &Literal) -> Self::Out {
        Ok(lit.value().is_empty())
    }

    fn visit_cat(&mut self, cat: &Cat) -> Self::Out {
        if !cat.fst.visit(self)? {
            return Ok(false);
        }

        self.0.unguard();
        let res = cat.snd.visit(self)?;
        self.0.guard();
        Ok(res)
    }

    fn visit_alt(&mut self, alt: &Alt) -> Self::Out {
        if alt.left.visit(self)? {
            Ok(true)
        } else {
            alt.right.visit(self)
        }
    }

    fn visit_fix(&mut self, fix: &Fix) -> Self::Out {
        fix.fixpoint(false, |fix, last| {
            self.0.push_nullable(*last);
            let res = fix.inner.visit(self)?;
            self.0.pop_nullable();
            Ok(res)
        })
    }

    fn visit_variable(&mut self, var: &Variable) -> Self::Out {
        match self.0.is_guarded(var.index) {
            None => Err(VariableError::FreeVariable(var.clone())),
            Some(true) => Err(VariableError::GuardedVariable(var.clone())),
            Some(false) => Ok(self.0.is_nullable(var.index).unwrap()),
        }
    }

    fn visit_call(&mut self, _call: &Call) -> Self::Out {
        todo!()
    }
}

struct FirstSet<'a>(FirstContext<'a>);

impl Visitor for FirstSet<'_> {
    type Out = Result<typed::FirstSet, VariableError>;

    fn visit_epsilon(&mut self, _: &Epsilon) -> Self::Out {
        Ok(typed::FirstSet::new())
    }

    fn visit_literal(&mut self, lit: &Literal) -> Self::Out {
        Ok(typed::FirstSet::of_str(&lit.value()))
    }

    fn visit_cat(&mut self, cat: &Cat) -> Self::Out {
        let mut set = cat.fst.visit(self)?;

        if cat.fst.visit(&mut Nullable(self.0.as_null()))? {
            self.0.unguard();
            set.union(cat.snd.visit(self)?);
            self.0.guard();
        }

        Ok(set)
    }

    fn visit_alt(&mut self, alt: &Alt) -> Self::Out {
        let mut set = alt.left.visit(self)?;
        set.union(alt.right.visit(self)?);
        Ok(set)
    }

    fn visit_fix(&mut self, fix: &Fix) -> Self::Out {
        let nullable = Nullable(self.0.as_null()).visit_fix(fix)?;
        fix.fixpoint(typed::FirstSet::new(), |fix, last| {
            self.0.push_first_set(nullable, last.clone());
            let res = fix.inner.visit(self)?;
            self.0.pop_first_set();
            Ok(res)
        })
    }

    fn visit_variable(&mut self, var: &Variable) -> Self::Out {
        match self.0.is_guarded(var.index) {
            None => Err(VariableError::FreeVariable(var.clone())),
            Some(true) => Err(VariableError::GuardedVariable(var.clone())),
            Some(false) => Ok(self.0.first_set(var.index).unwrap().clone()),
        }
    }

    fn visit_call(&mut self, _call: &Call) -> Self::Out {
        todo!()
    }
}

struct FlastSet<'a>(&'a mut FlastContext);

impl Visitor for FlastSet<'_> {
    type Out = Result<typed::FlastSet, VariableError>;

    fn visit_epsilon(&mut self, _: &Epsilon) -> Self::Out {
        Ok(typed::FlastSet::new())
    }

    fn visit_literal(&mut self, _: &Literal) -> Self::Out {
        Ok(typed::FlastSet::new())
    }

    fn visit_cat(&mut self, cat: &Cat) -> Self::Out {
        self.0.unguard();
        let mut set = cat.snd.visit(self)?;
        let nullable = cat.snd.visit(&mut Nullable(self.0.as_null()))?;
        self.0.guard();

        if nullable {
            self.0.unguard();
            set.union_first(cat.snd.visit(&mut FirstSet(self.0.as_first()))?);
            self.0.guard();
            set.union(cat.fst.visit(self)?);
        }

        Ok(set)
    }

    fn visit_alt(&mut self, alt: &Alt) -> Self::Out {
        let mut set = alt.left.visit(self)?;
        set.union(alt.right.visit(self)?);
        Ok(set)
    }

    fn visit_fix(&mut self, fix: &Fix) -> Self::Out {
        let nullable = Nullable(self.0.as_null()).visit_fix(fix)?;
        let first_set = FirstSet(self.0.as_first()).visit_fix(fix)?;
        fix.fixpoint(typed::FlastSet::new(), |fix, last| {
            self.0
                .push_flast_set(nullable, first_set.clone(), last.clone());
            let res = fix.inner.visit(self)?;
            self.0.pop_flast_set();
            Ok(res)
        })
    }

    fn visit_variable(&mut self, var: &Variable) -> Self::Out {
        match self.0.is_guarded(var.index) {
            None => Err(VariableError::FreeVariable(var.clone())),
            Some(true) => Err(VariableError::GuardedVariable(var.clone())),
            Some(false) => Ok(self.0.flast_set(var.index).unwrap().clone()),
        }
    }

    fn visit_call(&mut self, _call: &Call) -> Self::Out {
        todo!()
    }
}

struct Spanning;

impl Visitor for Spanning {
    type Out = Span;

    fn visit_epsilon(&mut self, eps: &Epsilon) -> Self::Out {
        eps.span
    }

    fn visit_literal(&mut self, lit: &Literal) -> Self::Out {
        lit.span()
    }

    fn visit_cat(&mut self, cat: &Cat) -> Self::Out {
        let fst = cat.fst.visit(self);
        let snd = cat.snd.visit(self);
        fst.join(snd).unwrap_or_else(Span::call_site)
    }

    fn visit_alt(&mut self, alt: &Alt) -> Self::Out {
        let left = alt.left.visit(self);
        let right = alt.right.visit(self);
        left.join(right).unwrap_or_else(Span::call_site)
    }

    fn visit_fix(&mut self, fix: &Fix) -> Self::Out {
        fix.span
    }

    fn visit_variable(&mut self, var: &Variable) -> Self::Out {
        var.name.span()
    }

    fn visit_call(&mut self, call: &Call) -> Self::Out {
        call.span
    }
}

#[derive(Debug, Default)]
pub struct TypeCheck(FlastContext);

impl TypeCheck {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Folder for TypeCheck {
    type Out = Result<(Typed, Span), TermError>;

    fn fold_epsilon(&mut self, eps: Epsilon) -> Self::Out {
        let nullable = Nullable(self.0.as_null()).visit_epsilon(&eps)?;
        let first_set = FirstSet(self.0.as_first()).visit_epsilon(&eps)?;
        let flast_set = FlastSet(&mut self.0).visit_epsilon(&eps)?;
        let span = Spanning.visit_epsilon(&eps);
        let kind = TypeKind::Epsilon;
        Ok((
            Typed {
                kind,
                nullable,
                first_set,
                flast_set,
            },
            span,
        ))
    }

    fn fold_literal(&mut self, lit: Literal) -> Self::Out {
        let nullable = Nullable(self.0.as_null()).visit_literal(&lit)?;
        let first_set = FirstSet(self.0.as_first()).visit_literal(&lit)?;
        let flast_set = FlastSet(&mut self.0).visit_literal(&lit)?;
        let span = Spanning.visit_literal(&lit);
        let kind = TypeKind::Literal(lit.value());
        Ok((
            Typed {
                kind,
                nullable,
                first_set,
                flast_set,
            },
            span,
        ))
    }

    fn fold_cat(&mut self, cat: Cat) -> Self::Out {
        let nullable = Nullable(self.0.as_null()).visit_cat(&cat)?;
        let first_set = FirstSet(self.0.as_first()).visit_cat(&cat)?;
        let flast_set = FlastSet(&mut self.0).visit_cat(&cat)?;
        let span = Spanning.visit_cat(&cat);

        let (fst, fst_span) = cat.fst.fold(self)?;

        self.0.unguard();
        let (snd, snd_span) = cat.snd.fold(self)?;
        self.0.guard();

        if fst.is_nullable() {
            Err(TermError::Cat(CatError::FirstNullable {
                punct: cat.punct.span,
                fst_span,
                fst,
            }))
        } else if !fst.flast_set().intersect_first(&snd.first_set()).is_empty() {
            Err(TermError::Cat(CatError::FirstFlastOverlap {
                punct: cat.punct.span,
                fst_span,
                fst,
                snd_span,
                snd,
            }))
        } else {
            let kind = TypeKind::Cat(Box::new(fst), Box::new(snd));
            Ok((
                Typed {
                    kind,
                    nullable,
                    first_set,
                    flast_set,
                },
                span,
            ))
        }
    }

    fn fold_alt(&mut self, alt: Alt) -> Self::Out {
        let nullable = Nullable(self.0.as_null()).visit_alt(&alt)?;
        let first_set = FirstSet(self.0.as_first()).visit_alt(&alt)?;
        let flast_set = FlastSet(&mut self.0).visit_alt(&alt)?;
        let span = Spanning.visit_alt(&alt);

        let (left, left_span) = alt.left.fold(self)?;
        let (right, right_span) = alt.right.fold(self)?;

        if left.is_nullable() && right.is_nullable() {
            Err(TermError::Alt(AltError::BothNullable {
                punct: alt.punct.span,
                left_span,
                left,
                right_span,
                right,
            }))
        } else if !left.first_set().intersect(&right.first_set()).is_empty() {
            Err(TermError::Alt(AltError::FirstOverlap {
                punct: alt.punct.span,
                left_span,
                left,
                right_span,
                right,
            }))
        } else {
            let kind = TypeKind::Alt(Box::new(left), Box::new(right));
            Ok((
                Typed {
                    kind,
                    nullable,
                    first_set,
                    flast_set,
                },
                span,
            ))
        }
    }

    fn fold_fix(&mut self, fix: Fix) -> Self::Out {
        let nullable = Nullable(self.0.as_null()).visit_fix(&fix)?;
        let first_set = FirstSet(self.0.as_first()).visit_fix(&fix)?;
        let flast_set = FlastSet(&mut self.0).visit_fix(&fix)?;
        let span = Spanning.visit_fix(&fix);

        self.0.push_flast_set(nullable, first_set.clone(), flast_set.clone());
        let (inner, _) = fix.inner.fold(self)?;
        self.0.pop_flast_set();

        let kind = TypeKind::Fix(Box::new(inner));

        Ok((
            Typed {
                kind,
                nullable,
                first_set,
                flast_set,
            },
            span,
        ))
    }

    fn fold_variable(&mut self, var: Variable) -> Self::Out {
        let nullable = Nullable(self.0.as_null()).visit_variable(&var)?;
        let first_set = FirstSet(self.0.as_first()).visit_variable(&var)?;
        let flast_set = FlastSet(&mut self.0).visit_variable(&var)?;
        let span = Spanning.visit_variable(&var);
        let kind = TypeKind::Variable(var.index);

        Ok((
            Typed {
                kind,
                nullable,
                first_set,
                flast_set,
            },
            span,
        ))
    }

    fn fold_call(&mut self, call: Call) -> Self::Out {
        todo!()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
enum TypeKind {
    Epsilon,
    Literal(String),
    Cat(Box<Typed>, Box<Typed>),
    Alt(Box<Typed>, Box<Typed>),
    Fix(Box<Typed>),
    Variable(usize),
}

#[derive(Debug)]
pub struct Code {
    id: usize,
}

impl TypeKind {
    /// Produces ident for the type and token stream for implementation
    fn emit_code(self, context: &mut CodeContext) -> Code {
        match self {
            Self::Epsilon => context.epsilon(),
            Self::Literal(s) => context.literal(s),
            Self::Cat(fst, snd) => {
                let Typed { kind: fst_kind, .. } = *fst;
                let Typed { kind: snd_kind, .. } = *snd;
                let fst_code = fst_kind.emit_code(context);
                let snd_code = snd_kind.emit_code(context);
                context.cat(fst_code, snd_code)
            }
            Self::Alt(left, right) => {
                let Typed {
                    kind: left_kind,
                    nullable: left_null,
                    first_set: left_first,
                    ..
                } = *left;
                let Typed {
                    kind: right_kind,
                    nullable: right_null,
                    first_set: right_first,
                    ..
                } = *right;
                let left_code = left_kind.emit_code(context);
                let right_code = right_kind.emit_code(context);
                context.alt(
                    left_code,
                    left_null,
                    left_first,
                    right_code,
                    right_null,
                    right_first,
                )
            }
            Self::Fix(inner) => {
                let Typed {
                    kind: inner_kind, ..
                } = *inner;
                context.fix(inner_kind)
            }
            Self::Variable(index) => context.variable(index),
        }
    }
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Epsilon => write!(f, "_"),
            Self::Literal(s) => write!(f, "{:?}", s),
            Self::Cat(fst, snd) => write!(f, "{}.{}", fst, snd),
            Self::Alt(left, right) => write!(f, "({} | {})", left, right),
            Self::Fix(inner) => write!(f, "[]({})", inner),
            Self::Variable(index) => write!(f, "{}", index),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Typed {
    kind: TypeKind,
    nullable: bool,
    first_set: typed::FirstSet,
    flast_set: typed::FlastSet,
}

impl Typed {
    pub fn is_nullable(&self) -> bool {
        self.nullable
    }

    pub fn first_set(&self) -> &typed::FirstSet {
        &self.first_set
    }

    pub fn flast_set(&self) -> &typed::FlastSet {
        &self.flast_set
    }

    pub fn emit_code(self, name: Ident) -> TokenStream {
        let mut context = CodeContext::new();
        let code = self.kind.emit_code(&mut context);
        context.all_code(name, code)
    }
}

impl Display for Typed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

#[derive(Debug)]
pub struct CodeContext {
    data: Vec<(TokenStream, TokenStream, BTreeSet<usize>)>,
    lit_map: HashMap<String, usize>,
    cat_map: HashMap<(usize, usize), usize>,
    alt_map: HashMap<(usize, usize), usize>,
    fix_map: HashMap<TypeKind, usize>,
    var_map: HashMap<usize, usize>, // Key is fix point ID
    context: Vec<usize>,
}

impl CodeContext {
    fn new() -> Self {
        let eps = (quote! {()}, TokenStream::new(), BTreeSet::new());

        Self {
            data: vec![eps],
            lit_map: HashMap::new(),
            cat_map: HashMap::new(),
            alt_map: HashMap::new(),
            fix_map: HashMap::new(),
            var_map: HashMap::new(),
            context: Vec::new(),
        }
    }

    fn epsilon(&mut self) -> Code {
        Code { id: 0 }
    }

    fn literal(&mut self, lit: String) -> Code {
        if let Some(&id) = self.lit_map.get(&lit) {
            Code { id }
        } else {
            let id = self.data.len();
            let name = format_ident!("Lit{}", id);
            let doc_name = format!(
                r#"The literal string `"{}"`."#,
                lit.escape_debug().collect::<String>()
            );
            let tokens = quote! {
                #[doc=#doc_name]
                pub struct #name;

                impl Parse for #name {
                    fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                        input.take_str(#lit).map(|()| #name)
                    }
                }
            };

            self.data
                .push((TokenTree::from(name).into(), tokens, BTreeSet::new()));
            self.lit_map.insert(lit, id);

            Code { id }
        }
    }

    fn cat(&mut self, fst: Code, snd: Code) -> Code {
        if let Some(&id) = self.cat_map.get(&(fst.id, snd.id)) {
            Code { id }
        } else {
            let id = self.data.len();
            let fst_ty = self.data[fst.id].0.clone();
            let snd_ty = self.data[snd.id].0.clone();
            self.data.push((
                quote! {(#fst_ty, #snd_ty)},
                TokenStream::new(),
                [fst.id, snd.id].iter().cloned().collect(),
            ));
            self.cat_map.insert((fst.id, snd.id), id);
            Code { id }
        }
    }

    fn alt(
        &mut self,
        left_code: Code,
        left_null: bool,
        left_first: typed::FirstSet,
        right_code: Code,
        right_null: bool,
        right_first: typed::FirstSet,
    ) -> Code {
        if let Some(&id) = self.alt_map.get(&(left_code.id, right_code.id)) {
            Code { id }
        } else {
            let id = self.data.len();
            let left_ty = self.data[left_code.id].0.clone();
            let right_ty = self.data[right_code.id].0.clone();
            let name = format_ident!("Alt{}", id);
            let mut tokens = quote! {
                pub enum #name {
                    Left(#left_ty),
                    Right(#right_ty),
                }
            };

            let other = if left_null {
                let iter = right_first.into_iter();

                quote! {
                    impl Parse for #name {
                        fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                            match input.peek() {
                                #(Some(#iter))|* => input.parse().map(Self::Right),
                                _ => input.parse().map(Self::Left),
                            }
                        }
                    }
                }
            } else if right_null {
                let iter = left_first.into_iter();

                quote! {
                    impl Parse for #name {
                        fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                            match input.peek() {
                                #(Some(#iter))|* => input.parse().map(Self::Left),
                                _ => input.parse().map(Self::Right),
                            }
                        }
                    }
                }
            } else {
                let mut first = left_first.clone();
                first.union(right_first.clone());
                let iter_first = first.into_iter();
                let iter_left = left_first.into_iter();
                let iter_right = right_first.into_iter();

                quote! {
                    impl Parse for #name {
                        fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                            match input.peek().ok_or(Error::EndOfStream)? {
                                #(#iter_left)|* => input.parse().map(Self::Left),
                                #(#iter_right)|* => input.parse().map(Self::Right),
                                c => input.error(Error::BadBranch(c, &[#(#iter_first),*]))
                            }
                        }
                    }
                }
            };

            tokens.extend(other);
            self.data.push((
                TokenTree::from(name).into(),
                tokens,
                [left_code.id, right_code.id].iter().cloned().collect(),
            ));
            self.alt_map.insert((left_code.id, right_code.id), id);
            Code { id }
        }
    }

    fn fix(&mut self, inner: TypeKind) -> Code {
        if let Some(&id) = self.fix_map.get(&inner) {
            Code { id }
        } else {
            let id = self.data.len();
            let name = format_ident!("Fix{}", id);
            self.data.push((
                TokenTree::from(name.clone()).into(),
                TokenStream::new(),
                BTreeSet::new(),
            ));
            self.context.push(id);
            let inner = inner.emit_code(self).id;
            let inner_ty = self.data[inner].0.clone();
            let tokens = quote! {
                pub struct #name(#inner_ty);

                impl Parse for #name {
                    fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                        input.parse().map(Self)
                    }
                }
            };
            self.data[id].1 = tokens;
            self.data[id].2 = [inner].iter().cloned().collect();
            Code { id }
        }
    }

    fn variable(&mut self, index: usize) -> Code {
        let fix_id = self.context[self.context.len() - index - 1];
        if let Some(&id) = self.var_map.get(&fix_id) {
            Code { id }
        } else {
            let id = self.data.len();
            let fix_ty = self.data[fix_id].0.clone();
            let name = quote! {Box<#fix_ty>};
            self.data.push((name, TokenStream::new(), BTreeSet::new()));
            self.var_map.insert(fix_id, id);
            Code { id }
        }
    }

    pub fn all_code(&mut self, name: Ident, code: Code) -> TokenStream {
        let root = self.data[code.id].clone();
        let mut tokens = root.1;
        let mut completed = [code.id].iter().cloned().collect::<BTreeSet<_>>();
        let mut todo = root.2;

        while !todo.is_empty() {
            let mut next = BTreeSet::new();
            completed.extend(todo.clone());

            for ty in todo {
                let ty = self.data[ty].clone();
                tokens.extend(ty.1);
                next.extend(ty.2.into_iter().filter(|id| !completed.contains(id)));
            }

            todo = next;
        }

        let root_ty = root.0;
        tokens.extend(quote! {
            pub type #name = #root_ty;

            pub enum Error {
                BadBranch(char, &'static [char]),
                EndOfStream,
            }

            pub type Result<T> = std::result::Result<T, Error>;

            pub trait Parser: Iterator<Item = char> {
                fn peek(&mut self) -> Option<char>;

                fn parse<T: Parse>(&mut self) -> Result<T> where Self: Sized {
                    T::parse(self)
                }

                fn take_str(&mut self, s: &str) -> Result<()>;

                fn error<T>(&mut self, e: Error) -> Result<T>;
            }

            pub trait Parse: Sized {
                fn parse<P: Parser>(input: &mut P) -> Result<Self>;
            }

            impl Parse for () {
                fn parse<P: Parser>(_input: &mut P) -> Result<Self> {
                    Ok(())
                }
            }

            impl<A: Parse, B: Parse> Parse for (A, B) {
                fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                    let a = input.parse()?;
                    let b = input.parse()?;
                    Ok((a, b))
                }
            }

            impl<T: Parse + Sized> Parse for Box<T> {
                fn parse<P: Parser>(input: &mut P) -> Result<Self> {
                    input.parse().map(Box::new)
                }
            }
        });

        tokens
    }
}
