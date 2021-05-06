use std::{
    array::IntoIter,
    cmp::{self, Ordering},
    fmt, hash,
};

use heck::{CamelCase, SnakeCase};
use proc_macro2::{Ident, Span};
use syn::{ext::IdentExt, spanned::Spanned};

#[derive(Clone, Debug)]
pub enum Content {
    Spanned(Ident),
    Spanless(String),
}

impl Spanned for Content {
    fn span(&self) -> Span {
        match self {
            Content::Spanned(i) => i.span(),
            Content::Spanless(_) => Span::call_site(),
        }
    }
}

impl From<Content> for Ident {
    fn from(content: Content) -> Self {
        match content {
            Content::Spanned(i) => i,
            Content::Spanless(s) => Ident::new(&s, Span::call_site()),
        }
    }
}

impl CamelCase for Content {
    fn to_camel_case(&self) -> Self::Owned {
        match self {
            Self::Spanned(ident) => {
                let span = ident.span();
                let name = ident.unraw().to_string();
                Ident::new(&name.to_camel_case(), span).into()
            }
            Self::Spanless(name) => name.to_camel_case().into(),
        }
    }
}

impl SnakeCase for Content {
    fn to_snake_case(&self) -> Self::Owned {
        match self {
            Self::Spanned(ident) => {
                let span = ident.span();
                let name = ident.unraw().to_string();
                Ident::new(&name.to_snake_case(), span).into()
            }
            Self::Spanless(name) => name.to_snake_case().into(),
        }
    }
}

impl PartialEq for Content {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Spanned(me), Self::Spanned(them)) => me == them,
            (Self::Spanned(me), Self::Spanless(them)) => me.unraw() == them,
            (Self::Spanless(me), Self::Spanned(them)) => them.unraw() == me,
            (Self::Spanless(me), Self::Spanless(them)) => me == them,
        }
    }
}

impl<T: AsRef<str>> PartialEq<T> for Content {
    fn eq(&self, other: &T) -> bool {
        match self {
            Self::Spanned(me) => me.unraw() == other,
            Self::Spanless(me) => me == other.as_ref(),
        }
    }
}

impl Eq for Content {}

impl hash::Hash for Content {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Spanned(i) => i.unraw().to_string().hash(state),
            Self::Spanless(s) => s.hash(state),
        }
    }
}

impl fmt::Display for Content {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Spanned(i) => i.fmt(f),
            Self::Spanless(s) => s.fmt(f),
        }
    }
}

impl From<Ident> for Content {
    fn from(ident: Ident) -> Self {
        Self::Spanned(ident)
    }
}

impl From<String> for Content {
    fn from(string: String) -> Self {
        Self::Spanless(string)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Kind {
    Label,
    Let,
    Variable,
}

impl PartialOrd for Kind {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(match (self, other) {
            (Kind::Label, Kind::Label)
            | (Kind::Let, Kind::Let)
            | (Kind::Variable, Kind::Variable) => Ordering::Equal,
            (Kind::Label, Kind::Let)
            | (Kind::Label, Kind::Variable)
            | (Kind::Let, Kind::Variable) => Ordering::Greater,
            (Kind::Let, Kind::Label)
            | (Kind::Variable, Kind::Label)
            | (Kind::Variable, Kind::Let) => Ordering::Less,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Name {
    pub content: Content,
    kind: Kind,
}

impl Name {
    pub fn new_variable<T: Into<Content>>(content: T) -> Self {
        Self {
            content: content.into(),
            kind: Kind::Variable,
        }
    }

    pub fn new_let<T: Into<Content>>(content: T) -> Self {
        Self {
            content: content.into(),
            kind: Kind::Let,
        }
    }

    pub fn new_label<T: Into<Content>>(content: T) -> Self {
        Self {
            content: content.into(),
            kind: Kind::Label,
        }
    }

    pub fn merge(this: Option<Self>, that: Option<Self>) -> Option<Self> {
        match (this, that) {
            (None, that) => that,
            (Some(this), None) => Some(this),
            (Some(this), Some(that)) => {
                if this.kind >= that.kind {
                    Some(this)
                } else {
                    Some(that)
                }
            }
        }
    }

    pub fn merge_all<I: IntoIterator<Item = Option<Self>>>(iter: I) -> Option<Self> {
        iter.into_iter().fold(None, Self::merge)
    }
}

impl Spanned for Name {
    fn span(&self) -> Span {
        self.content.span()
    }
}

impl From<Name> for Ident {
    fn from(name: Name) -> Self {
        name.content.into()
    }
}

impl CamelCase for Name {
    fn to_camel_case(&self) -> Self::Owned {
        Self {
            content: self.content.to_camel_case(),
            kind: self.kind,
        }
    }
}

impl SnakeCase for Name {
    fn to_snake_case(&self) -> Self::Owned {
        Self {
            content: self.content.to_snake_case(),
            kind: self.kind,
        }
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.content.fmt(f)
    }
}
