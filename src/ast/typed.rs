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

    pub fn into_iter(self) -> impl Iterator<Item = char> {
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

pub trait NullContext {
    type PushNull: NullContext;

    fn get_depth(&self) -> usize;

    fn get_nullable(&self, index: usize) -> Option<bool>;

    fn push_nullable<F: FnOnce(&mut Self::PushNull) -> R, R>(&mut self, nullable: bool, f: F) -> R;
}

impl NullContext for Vec<bool> {
    type PushNull = Self;

    fn get_depth(&self) -> usize {
        self.len()
    }

    fn get_nullable(&self, index: usize) -> Option<bool> {
        self.get(self.len() - index - 1).copied()
    }

    fn push_nullable<F: FnOnce(&mut Self::PushNull) -> R, R>(&mut self, nullable: bool, f: F) -> R {
        self.push(nullable);
        let res = f(self);
        self.pop();
        res
    }
}

impl NullContext for Vec<(bool, FirstSet)> {
    type PushNull = Self;

    fn get_depth(&self) -> usize {
        self.len()
    }

    fn get_nullable(&self, index: usize) -> Option<bool> {
        self.get(self.len() - index - 1).map(|(null, _)| null).copied()
    }

    fn push_nullable<F: FnOnce(&mut Self::PushNull) -> R, R>(&mut self, nullable: bool, f: F) -> R {
        self.push((nullable, FirstSet::new()));
        let res = f(self);
        self.pop();
        res
    }
}

impl NullContext for Vec<(bool, FirstSet, FlastSet)> {
    type PushNull = Self;

    fn get_depth(&self) -> usize {
        self.len()
    }

    fn get_nullable(&self, index: usize) -> Option<bool> {
        self.get(self.len() - index - 1).map(|(null, _, _)| null).copied()
    }

    fn push_nullable<F: FnOnce(&mut Self::PushNull) -> R, R>(&mut self, nullable: bool, f: F) -> R {
        self.push((nullable, FirstSet::new(), FlastSet::new()));
        let res = f(self);
        self.pop();
        res
    }
}

pub trait FirstSetContext: NullContext {
    type PushFirstSet: FirstSetContext;

    fn get_first_set(&self, index: usize) -> Option<&FirstSet>;

    fn push_first_set<F: FnOnce(&mut Self::PushFirstSet) -> R, R>(
        &mut self,
        nullable: bool,
        first_set: FirstSet,
        f: F,
    ) -> R;
}

impl FirstSetContext for Vec<(bool, FirstSet)> {
    type PushFirstSet = Self;

    fn get_first_set(&self, index: usize) -> Option<&FirstSet> {
        self.get(self.len() - index - 1).map(|(_, first_set)| first_set)
    }

    fn push_first_set<F: FnOnce(&mut Self::PushFirstSet) -> R, R>(
        &mut self,
        nullable: bool,
        first_set: FirstSet,
        f: F,
    ) -> R {
        self.push((nullable, first_set));
        let res = f(self);
        self.pop();
        res
    }
}

impl FirstSetContext for Vec<(bool, FirstSet, FlastSet)> {
    type PushFirstSet = Self;

    fn get_first_set(&self, index: usize) -> Option<&FirstSet> {
        self.get(self.len() - index - 1).map(|(_, first_set, _)| first_set)
    }

    fn push_first_set<F: FnOnce(&mut Self::PushFirstSet) -> R, R>(
        &mut self,
        nullable: bool,
        first_set: FirstSet,
        f: F,
    ) -> R {
        self.push((nullable, first_set, FlastSet::new()));
        let res = f(self);
        self.pop();
        res
    }
}

pub trait FlastSetContext: FirstSetContext {
    type PushFlastSet: FlastSetContext;

    fn get_flast_set(&self, index: usize) -> Option<&FlastSet>;

    fn push_flast_set<F: FnOnce(&mut Self::PushFlastSet) -> R, R>(
        &mut self,
        nullable: bool,
        first_set: FirstSet,
        flast_set: FlastSet,
        f: F,
    ) -> R;
}

impl FlastSetContext for Vec<(bool, FirstSet, FlastSet)> {
    type PushFlastSet = Self;

    fn get_flast_set(&self, index: usize) -> Option<&FlastSet> {
        self.get(self.len() - index - 1).map(|(_, _, flast_set)| flast_set)
    }

    fn push_flast_set<F: FnOnce(&mut Self::PushFlastSet) -> R, R>(
        &mut self,
        nullable: bool,
        first_set: FirstSet,
        flast_set: FlastSet,
        f: F,
    ) -> R {
        self.push((nullable, first_set, flast_set));
        let res = f(self);
        self.pop();
        res
    }
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
