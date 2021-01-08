use std::{fmt, hash};

use proc_macro2::{Ident, Span};
use syn::ext::IdentExt;

pub mod ast;
pub mod check;
pub mod context;
pub mod error;
pub mod set;
pub mod typed;
pub mod visit;

#[derive(Clone, Debug)]
pub enum Name {
    Spanned(Ident),
    Spanless(String),
}

impl Name {
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::Spanned(i) => Some(i.span()),
            Self::Spanless(_) => None,
        }
    }

    pub fn as_ident(self, span: Span) -> Ident {
        match self {
            Self::Spanned(i) => i,
            Self::Spanless(s) => Ident::new(&s, span),
        }
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Spanned(me), Self::Spanned(them)) => me == them,
            (Self::Spanned(me), Self::Spanless(them)) => me.unraw() == them,
            (Self::Spanless(me), Self::Spanned(them)) => them.unraw() == me,
            (Self::Spanless(me), Self::Spanless(them)) => me == them,
        }
    }
}

impl<T: AsRef<str>> PartialEq<T> for Name {
    fn eq(&self, other: &T) -> bool {
        match self {
            Name::Spanned(me) => me.unraw() == other,
            Name::Spanless(me) => me == other.as_ref(),
        }
    }
}

impl Eq for Name {}

impl hash::Hash for Name {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Spanned(i) => i.unraw().to_string().hash(state),
            Self::Spanless(s) => s.hash(state),
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Name::Spanned(i) => i.fmt(f),
            Name::Spanless(s) => s.fmt(f),
        }
    }
}

impl From<Ident> for Name {
    fn from(ident: Ident) -> Self {
        Self::Spanned(ident)
    }
}

impl From<String> for Name {
    fn from(string: String) -> Self {
        Self::Spanless(string)
    }
}
