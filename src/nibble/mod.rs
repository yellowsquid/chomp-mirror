pub mod convert;

use std::fmt;

use proc_macro2::Span;
use syn::{
    bracketed,
    ext::IdentExt,
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::{Pair, Punctuated},
    token::{Bracket, Comma, Let, Match, Paren},
    LitStr, Token,
};

use crate::chomp::{Name, ast::{self, TopLevel}};

use convert::{Context, Convert, ConvertError};

pub type Epsilon = Token![_];

pub type Ident = syn::Ident;

pub type Literal = LitStr;

#[derive(Clone)]
pub struct ArgList<T> {
    paren_token: Paren,
    args: Punctuated<T, Comma>,
}

impl<T> ArgList<T> {
    pub fn span(&self) -> Span {
        self.paren_token.span
    }

    pub fn len(&self) -> usize {
        self.args.len()
    }

    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }
}

impl<T> IntoIterator for ArgList<T> {
    type Item = T;

    type IntoIter = <Punctuated<T, Comma> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.args.into_iter()
    }
}

impl<T: Parse> Parse for ArgList<T> {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let args;
        let paren_token = parenthesized!(args in input);
        let args = args.call(Punctuated::parse_terminated)?;
        Ok(Self { paren_token, args })
    }
}

impl<T: fmt::Debug> fmt::Debug for ArgList<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ArgList")?;
        f.debug_list().entries(self.args.iter()).finish()
    }
}

#[derive(Clone)]
pub struct Fix {
    bang_token: Token![!],
    pub expr: Box<Term>,
}

impl Fix {
    pub fn span(&self) -> Option<Span> {
        self.bang_token.span.join(self.expr.span()?)
    }
}

impl Parse for Fix {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let bang_token = input.parse()?;
        let expr = input.parse()?;
        Ok(Self { bang_token, expr })
    }
}

impl fmt::Debug for Fix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Fix").field("expr", &self.expr).finish()
    }
}

#[derive(Clone)]
pub struct ParenExpression {
    paren_token: Paren,
    pub expr: Expression,
}

impl Parse for ParenExpression {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let expr;
        let paren_token = parenthesized!(expr in input);
        let expr = expr.parse()?;
        Ok(Self { paren_token, expr })
    }
}

impl fmt::Debug for ParenExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ParenExpression")
            .field("expr", &self.expr)
            .finish()
    }
}

#[derive(Clone)]
pub enum Term {
    Epsilon(Epsilon),
    Ident(Ident),
    Literal(Literal),
    Fix(Fix),
    Parens(ParenExpression),
}

impl Term {
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::Epsilon(e) => Some(e.span),
            Self::Ident(i) => Some(i.span()),
            Self::Literal(l) => Some(l.span()),
            Self::Fix(f) => f.span(),
            Self::Parens(p) => Some(p.paren_token.span),
        }
    }
}

impl Parse for Term {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(Token![_]) {
            input.parse().map(Self::Epsilon)
        } else if lookahead.peek(LitStr) {
            input.parse().map(Self::Literal)
        } else if lookahead.peek(Token![!]) {
            input.parse().map(Self::Fix)
        } else if lookahead.peek(Paren) {
            input.parse().map(Self::Parens)
        } else if lookahead.peek(Ident::peek_any) {
            input.call(Ident::parse_any).map(Self::Ident)
        } else {
            Err(lookahead.error())
        }
    }
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Epsilon(_) => write!(f, "Term::Epsilon"),
            Term::Ident(i) => write!(f, "Term::Ident({:?})", i),
            Term::Literal(l) => write!(f, "Term::Literal({:?})", l.value()),
            Term::Fix(x) => write!(f, "Term::Fix({:?})", x),
            Term::Parens(p) => write!(f, "Term::Parens({:?})", p),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Call(pub Vec<Term>);

impl Call {
    pub fn span(&self) -> Option<Span> {
        let mut iter = self.0.iter();
        let first = iter.next()?.span()?;
        iter.try_fold(first, |span, t| t.span().and_then(|s| span.join(s)))
    }
}

impl Parse for Call {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut out = Vec::new();
        out.push(input.parse()?);
        loop {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![_])
                || lookahead.peek(LitStr)
                || lookahead.peek(Token![!])
                || lookahead.peek(Paren)
                || lookahead.peek(Ident::peek_any)
            {
                out.push(input.parse()?);
            } else {
                break;
            }
        }

        Ok(Self(out))
    }
}

#[derive(Clone)]
pub struct Cat(pub Punctuated<Call, Token![.]>);

impl Parse for Cat {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        input.call(Punctuated::parse_separated_nonempty).map(Self)
    }
}

impl Cat {
    pub fn span(&self) -> Option<Span> {
        let mut iter = self.0.pairs();
        let span = match iter.next()? {
            Pair::Punctuated(t, p) => t.span().and_then(|s| s.join(p.span)),
            Pair::End(t) => t.span(),
        }?;

        iter.try_fold(span, |span, pair| match pair {
            Pair::Punctuated(t, p) => t
                .span()
                .and_then(|s| span.join(s))
                .and_then(|s| s.join(p.span)),
            Pair::End(t) => t.span().and_then(|s| span.join(s)),
        })
    }
}

impl fmt::Debug for Cat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Cat")?;
        f.debug_list().entries(self.0.iter()).finish()
    }
}

#[derive(Clone)]
pub struct Label {
    colon_tok: Token![:],
    pub label: Ident,
}

impl Label {
    pub fn span(&self) -> Option<Span> {
        self.colon_tok.span.join(self.label.span())
    }
}

impl Parse for Label {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let colon_tok = input.parse()?;
        let label = input.call(Ident::parse_any)?;
        Ok(Self { colon_tok, label })
    }
}

impl fmt::Debug for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Label").field("label", &self.label).finish()
    }
}

#[derive(Clone, Debug)]
pub struct Labelled {
    pub cat: Cat,
    pub label: Option<Label>,
}

impl Labelled {
    pub fn span(&self) -> Option<Span> {
        self.cat.span().and_then(|s| {
            self.label
                .as_ref()
                .and_then(|l| l.span().and_then(|t| s.join(t)))
        })
    }
}

impl Parse for Labelled {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let cat = input.parse()?;
        let label = if input.peek(Token![:]) {
            Some(input.parse()?)
        } else {
            None
        };
        Ok(Self { cat, label })
    }
}

#[derive(Clone)]
pub struct Alt(pub Punctuated<Labelled, Token![|]>);

impl Alt {
    pub fn span(&self) -> Option<Span> {
        let mut iter = self.0.pairs();
        let span = match iter.next()? {
            Pair::Punctuated(t, p) => t.span().and_then(|s| s.join(p.span)),
            Pair::End(t) => t.span(),
        }?;

        iter.try_fold(span, |span, pair| match pair {
            Pair::Punctuated(t, p) => t
                .span()
                .and_then(|s| span.join(s))
                .and_then(|s| s.join(p.span)),
            Pair::End(t) => t.span().and_then(|s| span.join(s)),
        })
    }
}

impl Parse for Alt {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        input.call(Punctuated::parse_separated_nonempty).map(Self)
    }
}

impl fmt::Debug for Alt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Alt")?;
        f.debug_list().entries(self.0.iter()).finish()
    }
}
#[derive(Clone)]
pub struct Lambda {
    slash_tok_left: Token![/],
    pub args: ArgList<Ident>,
    slash_tok_right: Token![/],
    pub expr: Alt,
}

impl Lambda {
    pub fn span(&self) -> Option<Span> {
        self.slash_tok_left.span.join(self.expr.span()?)
    }
}

impl Parse for Lambda {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let slash_tok_left = input.parse()?;
        let args = input.parse()?;
        let slash_tok_right = input.parse()?;
        let expr = input.parse()?;
        Ok(Self {
            slash_tok_left,
            args,
            slash_tok_right,
            expr,
        })
    }
}

impl fmt::Debug for Lambda {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Lambda")
            .field("args", &self.args)
            .field("expr", &self.expr)
            .finish()
    }
}

#[derive(Clone, Debug)]
pub enum Expression {
    Alt(Alt),
    Lambda(Lambda),
}

impl Parse for Expression {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        if input.peek(Token![/]) {
            input.parse().map(Self::Lambda)
        } else {
            input.parse().map(Self::Alt)
        }
    }
}

#[derive(Clone)]
pub struct GoalStatement {
    match_token: Token![match],
    expr: Expression,
    semi_token: Token![;],
}

impl Parse for GoalStatement {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let match_token = input.parse()?;
        let expr = input.parse()?;
        let semi_token = input.parse()?;

        Ok(Self {
            match_token,
            expr,
            semi_token,
        })
    }
}

impl fmt::Debug for GoalStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GoalStatement")
            .field("expr", &self.expr)
            .finish()
    }
}

#[derive(Clone)]
pub struct LetStatement {
    let_token: Token![let],
    name: Ident,
    args: Option<ArgList<Ident>>,
    eq_token: Token![=],
    expr: Expression,
    semi_token: Token![;],
    next: Box<Statement>,
}

impl Parse for LetStatement {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let let_token = input.parse()?;
        let name = input.call(Ident::parse_any)?;
        let args = if input.peek(Paren) {
            Some(input.parse()?)
        } else {
            None
        };
        let eq_token = input.parse()?;
        let expr = input.parse()?;
        let semi_token = input.parse()?;
        let next = Box::new(input.parse()?);

        Ok(Self {
            let_token,
            name,
            args,
            eq_token,
            expr,
            semi_token,
            next,
        })
    }
}

impl fmt::Debug for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LetStatement")
            .field("name", &self.name)
            .field("args", &self.args)
            .field("expr", &self.expr)
            .field("next", &self.next)
            .finish()
    }
}

#[derive(Clone, Debug)]
pub enum Statement {
    Goal(GoalStatement),
    Let(LetStatement),
}

impl Statement {
    pub fn convert(self) -> Result<TopLevel, ConvertError> {
        let mut stmt = self;
        let mut context = Context::new();
        let mut name_val = Vec::new();
        while let Self::Let(let_stmt) = stmt {
            let mut val = match let_stmt.args {
                Some(args) => {
                    todo!()
                }
                None => let_stmt.expr.convert(&mut context),
            }?;
            let name: Name = let_stmt.name.into();
            val.name = val.name.or_else(|| Some(name.clone()));
            context.push_variable(name.clone());
            name_val.push((name, val));
            stmt = *let_stmt.next;
        }

        let goal = match stmt {
            Statement::Goal(goal) => TopLevel::Goal(goal.expr.convert(&mut context)?),
            Statement::Let(_) => unreachable!(),
        };

        Ok(name_val.into_iter().rfold(goal, |inner, (name, val)| {
            TopLevel::Let(ast::Let {
                name,
                val,
                inner: Box::new(inner),
            })
        }))
    }
}

impl Parse for Statement {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut lookahead = input.lookahead1();

        if lookahead.peek(Let) {
            input.parse().map(Self::Let)
        } else if lookahead.peek(Match) {
            input.parse().map(Self::Goal)
        } else {
            Err(lookahead.error())
        }
    }
}
