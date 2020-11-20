use super::Typed;
use super::VariableError;
use std::collections::BTreeSet;
use std::fmt::Display;

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
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

pub trait NullContext {
    type PushNull: NullContext;

    fn get_depth(&self) -> usize;

    fn get_nullable(&self, index: usize) -> Option<bool>;

    fn push_nullable<F: FnOnce(&mut Self::PushNull) -> R, R>(&mut self, nullable: bool, f: F) -> R;
}

pub trait FirstSetContext: NullContext {
    type PushFirstSet: FirstSetContext;

    fn get_first_set(&self, index: usize) -> Option<FirstSet>;

    fn push_first_set<F: FnOnce(&mut Self::PushFirstSet) -> R, R>(
        &mut self,
        nullable: bool,
        first_set: FirstSet,
        f: F,
    ) -> R;
}

pub trait FlastSetContext: FirstSetContext {
    type PushFlastSet: FlastSetContext;

    fn get_flast_set(&self, index: usize) -> Option<FlastSet>;

    fn push_flast_set<F: FnOnce(&mut Self::PushFlastSet) -> R, R>(
        &mut self,
        nullable: bool,
        first_set: FirstSet,
        flast_set: FlastSet,
        f: F,
    ) -> R;
}

pub trait Type {
    type Err: Display;

    /// # Errors
    /// Returns [`Err`] if there is a variable with an index greater than or equal
    /// to `depth`.
    fn closed(&self, depth: usize) -> Result<(), VariableError>;

    /// # Errors
    /// Returns [`None`] only if `self.closed(context.get_depth())` returns an
    /// [`Err`].
    fn is_nullable<C: NullContext>(&self, context: &mut C) -> Option<bool>;

    /// # Errors
    /// Returns [`None`] only if `self.closed(context.get_depth())` returns an
    /// [`Err`].
    fn first_set<C: FirstSetContext>(&self, context: &mut C) -> Option<FirstSet>;

    /// # Errors
    /// Returns [`None`] only if `self.closed(context.get_depth())` returns an
    /// [`Err`].
    fn flast_set<C: FlastSetContext>(&self, context: &mut C) -> Option<FlastSet>;

    /// # Errors
    /// Returns an [`Err`] if this term is not well typed.
    fn well_typed<C: FlastSetContext>(self, context: &mut C) -> Result<Typed, Self::Err>;
}
