use std::collections::BTreeSet;
use std::collections::HashMap;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::Display;

use proc_macro2::Span;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use quote::format_ident;
use quote::quote;
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

impl Error for TermError {}

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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
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

    pub fn emit_code(self, name: Ident) -> TokenStream {
        let mut context = CodeContext::new();
        let code = self.kind.emit_code(&mut context);
        context.all_code(name, code)
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
        left_first: FirstSet,
        right_code: Code,
        right_null: bool,
        right_first: FirstSet,
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
                let iter_first = left_first.clone().union(right_first.clone()).into_iter();
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
