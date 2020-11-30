use proc_macro2::Span;

use super::Typed;
use super::VariableError;
use std::collections::BTreeSet;

#[derive(Clone, Debug, Default, Eq, PartialEq, Hash)]
pub struct FirstSet {
    inner: BTreeSet<char>,
}

impl FirstSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn of_str(s: &str) -> Self {
        let mut inner = BTreeSet::new();
        s.chars().next().map(|c| inner.insert(c));

        Self { inner }
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn union(&mut self, mut other: Self) {
        self.inner.append(&mut other.inner);
    }

    pub fn intersect(&self, other: &Self) -> Self {
        Self {
            inner: self.inner.intersection(&other.inner).copied().collect(),
        }
    }
}

impl IntoIterator for FirstSet {
    type Item = char;

    type IntoIter = <BTreeSet<char> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Hash)]
pub struct FlastSet {
    inner: BTreeSet<char>,
}

impl FlastSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn union_first(&mut self, mut other: FirstSet) {
        self.inner.append(&mut other.inner);
    }

    pub fn union(&mut self, mut other: Self) {
        self.inner.append(&mut other.inner);
    }

    pub fn intersect_first(&self, other: &FirstSet) -> Self {
        Self {
            inner: self.inner.intersection(&other.inner).copied().collect(),
        }
    }

    pub fn intersect(&self, other: &Self) -> Self {
        Self {
            inner: self.inner.intersection(&other.inner).copied().collect(),
        }
    }
}

#[derive(Debug)]
pub struct NullContext<'a> {
    inner: &'a mut FlastContext,
}

impl NullContext<'_> {
    pub fn depth(&self) -> usize {
        self.inner.depth()
    }

    pub fn is_guarded(&self, index: usize) -> Option<bool> {
        self.inner.is_guarded(index)
    }

    pub fn with_unguard<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.unguard();
        let res = f(self);
        self.guard();
        res
    }

    pub(crate) fn unguard(&mut self) {
        self.inner.unguard()
    }

    pub(crate) fn guard(&mut self) {
        self.inner.guard()
    }

    pub fn is_nullable(&self, index: usize) -> Option<bool> {
        self.inner.is_nullable(index)
    }

    pub fn with_nullable<F: FnOnce(&mut Self) -> R, R>(&mut self, nullable: bool, f: F) -> R {
        self.push_nullable(nullable);
        let res = f(self);
        self.guard();
        res
    }

    pub(crate) fn push_nullable(&mut self, nullable: bool) {
        self.inner.push_nullable(nullable)
    }

    pub(crate) fn pop_nullable(&mut self) {
        self.inner.pop_nullable()
    }
}

#[derive(Debug)]
pub struct FirstContext<'a> {
    inner: &'a mut FlastContext,
}

impl FirstContext<'_> {
    pub fn depth(&self) -> usize {
        self.inner.depth()
    }

    pub fn is_guarded(&self, index: usize) -> Option<bool> {
        self.inner.is_guarded(index)
    }

    pub fn with_unguard<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.unguard();
        let res = f(self);
        self.guard();
        res
    }

    pub(crate) fn unguard(&mut self) {
        self.inner.unguard()
    }

    pub(crate) fn guard(&mut self) {
        self.inner.guard()
    }

    pub fn first_set(&self, index: usize) -> Option<&FirstSet> {
        self.inner.first_set(index)
    }

    pub fn with_first_set<F: FnOnce(&mut Self) -> R, R>(
        &mut self,
        nullable: bool,
        first_set: FirstSet,
        f: F,
    ) -> R {
        self.push_first_set(nullable, first_set);
        let res = f(self);
        self.pop_first_set();
        res
    }

    pub(crate) fn push_first_set(&mut self, nullable: bool, first_set: FirstSet) {
        self.inner.push_first_set(nullable, first_set)
    }

    pub(crate) fn pop_first_set(&mut self) {
        self.inner.pop_first_set()
    }

    pub fn as_null(&mut self) -> NullContext<'_> {
        NullContext { inner: self.inner }
    }
}

#[derive(Debug, Default)]
pub struct FlastContext {
    data: Vec<(bool, FirstSet, FlastSet)>,
    guard: Vec<usize>,
}

impl FlastContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn depth(&self) -> usize {
        self.data.len()
    }

    pub fn is_guarded(&self, index: usize) -> Option<bool> {
        if self.data.len() <= index {
            None
        } else if self.guard.is_empty() {
            Some(true)
        } else {
            Some(self.guard[self.guard.len() - 1] + index < self.data.len())
        }
    }

    pub fn with_unguard<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.unguard();
        let res = f(self);
        self.guard();
        res
    }

    pub(crate) fn unguard(&mut self) {
        self.guard.push(self.data.len());
    }

    pub(crate) fn guard(&mut self) {
        self.guard.pop();
    }

    fn is_nullable(&self, index: usize) -> Option<bool> {
        self.data.get(index).map(|(null, _, _)| *null)
    }

    fn push_nullable(&mut self, nullable: bool) {
        self.data
            .push((nullable, FirstSet::default(), FlastSet::default()))
    }

    fn pop_nullable(&mut self) {
        self.data.pop();
    }

    fn first_set(&self, index: usize) -> Option<&FirstSet> {
        self.data.get(index).map(|(_, first, _)| first)
    }

    fn push_first_set(&mut self, nullable: bool, first_set: FirstSet) {
        self.data.push((nullable, first_set, FlastSet::default()))
    }

    fn pop_first_set(&mut self) {
        self.data.pop();
    }

    pub fn flast_set(&self, index: usize) -> Option<&FlastSet> {
        self.data.get(index).map(|(_, _, flast)| flast)
    }

    pub fn with_flast_set<F: FnOnce(&mut Self) -> R, R>(
        &mut self,
        nullable: bool,
        first_set: FirstSet,
        flast_set: FlastSet,
        f: F,
    ) -> R {
        self.push_flast_set(nullable, first_set, flast_set);
        let res = f(self);
        self.pop_flast_set();
        res
    }

    pub(crate) fn push_flast_set(&mut self, nullable: bool, first_set: FirstSet, flast_set: FlastSet) {
        self.data.push((nullable, first_set, flast_set));
    }

    pub(crate) fn pop_flast_set(&mut self) {
        self.data.pop();
    }

    pub fn as_first(&mut self) -> FirstContext<'_> {
        FirstContext { inner: self }
    }

    pub fn as_null(&mut self) -> NullContext<'_> {
        NullContext { inner: self }
    }
}

pub trait Type {
    type Err: Into<syn::Error>;

    /// # Errors
    /// Returns [`Err`] if there is a variable with an index greater than or equal
    /// to `depth`.
    fn closed(&self, depth: usize) -> Result<(), VariableError>;

    /// # Errors
    /// Returns [`None`] only if `self.closed(context.get_depth())` returns an
    /// [`Err`].
    fn is_nullable(&self, context: &mut NullContext<'_>) -> Option<bool>;

    /// # Errors
    /// Returns [`None`] only if `self.closed(context.get_depth())` returns an
    /// [`Err`].
    fn first_set(&self, context: &mut FirstContext<'_>) -> Option<FirstSet>;

    /// # Errors
    /// Returns [`None`] only if `self.closed(context.get_depth())` returns an
    /// [`Err`].
    fn flast_set(&self, context: &mut FlastContext) -> Option<FlastSet>;

    /// # Errors
    /// Returns an [`Err`] if this term is not well typed.
    fn well_typed(self, context: &mut FlastContext) -> Result<(Typed, Span), Self::Err>;
}
