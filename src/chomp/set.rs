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

    pub fn disjoint(&self, other: &Self) -> bool {
        self.inner.intersection(&other.inner).next().is_none()
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

    pub fn disjoint(&self, other: &FirstSet) -> bool {
        self.inner.intersection(&other.inner).next().is_none()
    }
}

impl IntoIterator for FlastSet {
    type Item = char;

    type IntoIter = <BTreeSet<char> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}
