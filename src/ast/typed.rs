use proc_macro2::Span;

use super::Typed;
use super::VariableError;
use std::collections::BTreeSet;
use std::fmt::Display;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FirstSet {
    inner: BTreeSet<char>,
}

impl FirstSet {
    pub fn new() -> Self {
        Self {
            inner: BTreeSet::new(),
        }
    }

    pub fn of_str(s: &str) -> Self {
        let mut inner = BTreeSet::new();
        s.chars().next().map(|c| inner.insert(c));

        Self { inner }
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn union(mut self, mut other: Self) -> Self {
        self.inner.append(&mut other.inner);
        self
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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FlastSet {
    inner: BTreeSet<char>,
}

impl FlastSet {
    pub fn new() -> Self {
        Self {
            inner: BTreeSet::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn union_first(mut self, mut other: FirstSet) -> Self {
        self.inner.append(&mut other.inner);
        self
    }

    pub fn union(mut self, mut other: Self) -> Self {
        self.inner.append(&mut other.inner);
        self
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
    inner: &'a mut FlastContext
}

impl NullContext<'_> {
    pub fn depth(&self) -> usize {
        self.inner.depth()
    }

    pub fn is_guarded(&self, index: usize) -> Option<bool> {
        self.inner.is_guarded(index)
    }

    pub fn unguard<F: FnOnce(&mut NullContext<'_>) -> R, R>(&mut self, f: F) -> R {
        self.inner.unguard(|ctx| f(&mut NullContext {inner: ctx}))
    }

    pub fn is_nullable(&self, index: usize) -> Option<bool> {
        self.inner.is_nullable(index)
    }

    pub fn push_nullable<F: FnOnce(&mut NullContext<'_>) -> R, R>(
        &mut self,
        nullable: bool,
        f: F,
    ) -> R {
        self.inner.push_nullable(nullable, f)
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

    pub fn unguard<F: FnOnce(&mut FirstContext<'_>) -> R, R>(&mut self, f: F) -> R {
        self.inner.unguard(|ctx| {
            f(&mut FirstContext{inner: ctx})
        })
    }

    pub fn is_nullable(&self, index: usize) -> Option<bool> {
        self.inner.is_nullable(index)
    }

    pub fn push_nullable<F: FnOnce(&mut NullContext<'_>) -> R, R>(
        &mut self,
        nullable: bool,
        f: F,
    ) -> R {
        self.inner.push_nullable(nullable, f)
    }

    pub fn first_set(&self, index: usize) -> Option<&FirstSet> {
        self.inner.first_set(index)
    }

    pub fn push_first_set<F: FnOnce(&mut FirstContext<'_>) -> R, R>(
        &mut self,
        nullable: bool,
        first_set: FirstSet,
        f: F,
    ) -> R {
        self.inner.push_first_set(nullable, first_set, f)
    }

    pub fn as_null(&mut self) -> NullContext<'_> {
        NullContext {
           inner: self.inner
        }
    }
}

#[derive(Debug)]
pub struct FlastContext {
    data: Vec<(bool, FirstSet, FlastSet)>,
    guard: Vec<usize>,
}

impl FlastContext {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            guard: Vec::new(),
        }
    }

    pub fn depth(&self) -> usize {
        self.data.len()
    }

    pub fn is_guarded(&self, index: usize) -> Option<bool> {
        if self.data.len() <= index {
            None
        } else if self.guard.is_empty() {
            Some(false)
        } else {
            Some(self.guard[self.guard.len() - 1] + index >= self.data.len())
        }
    }

    pub fn unguard<F: FnOnce(&mut Self) -> R, R>(&mut self, f: F) -> R {
        self.guard.push(self.data.len());
        let res = f(self);
        self.guard.pop();
        res
    }

    pub fn is_nullable(&self, index: usize) -> Option<bool> {
        self.data.get(index).map(|(null, _, _)| *null)
    }

    pub fn push_nullable<F: FnOnce(&mut NullContext<'_>) -> R, R>(
        &mut self,
        nullable: bool,
        f: F,
    ) -> R {
        self.push_first_set(nullable, FirstSet::new(), |ctx| {
            f(&mut ctx.as_null())
        })
    }

    pub fn first_set(&self, index: usize) -> Option<&FirstSet> {
        self.data.get(index).map(|(_, first, _)| first)
    }

    pub fn push_first_set<F: FnOnce(&mut FirstContext<'_>) -> R, R>(
        &mut self,
        nullable: bool,
        first_set: FirstSet,
        f: F,
    ) -> R {
        self.push_flast_set(nullable, first_set, FlastSet::new(), |ctx| f(&mut ctx.as_first()))
    }

    pub fn flast_set(&self, index: usize) -> Option<&FlastSet> {
        self.data.get(index).map(|(_, _, flast)| flast)
    }

    pub fn push_flast_set<F: FnOnce(&mut Self) -> R, R>(
        &mut self,
        nullable: bool,
        first_set: FirstSet,
        flast_set: FlastSet,
        f: F,
    ) -> R {
        self.data.push((nullable, first_set, flast_set));
        let res = f(self);
        self.data.pop();
        res
    }

    pub fn as_first(&mut self) -> FirstContext<'_> {
        FirstContext {
            inner: self
        }
    }

    pub fn as_null(&mut self) -> NullContext<'_> {
        NullContext{
            inner: self
        }
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
