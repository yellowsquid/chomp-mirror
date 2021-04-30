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

use crate::chomp::{ast, Name};

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

#[derive(Clone, Debug)]
pub struct Call {
    pub name: Ident,
    pub args: ArgList<Expression>,
}

impl Call {
    pub fn span(&self) -> Option<Span> {
        self.name.span().join(self.args.span())
    }
}

impl Parse for Call {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let name = input.call(Ident::parse_any)?;
        let args = input.parse()?;
        Ok(Self { name, args })
    }
}

#[derive(Clone)]
pub struct Fix {
    bracket_token: Bracket,
    pub arg: Ident,
    paren_token: Paren,
    pub expr: Expression,
}

impl Fix {
    pub fn span(&self) -> Option<Span> {
        self.bracket_token.span.join(self.paren_token.span)
    }
}

impl Parse for Fix {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let arg;
        let bracket_token = bracketed!(arg in input);
        let arg = arg.call(Ident::parse_any)?;
        let expr;
        let paren_token = parenthesized!(expr in input);
        let expr = expr.parse()?;
        Ok(Self {
            bracket_token,
            arg,
            paren_token,
            expr,
        })
    }
}

impl fmt::Debug for Fix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Fix")
            .field("arg", &self.arg)
            .field("expr", &self.expr)
            .finish()
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
    Call(Call),
    Fix(Fix),
    Parens(ParenExpression),
}

impl Term {
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::Epsilon(e) => Some(e.span),
            Self::Ident(i) => Some(i.span()),
            Self::Literal(l) => Some(l.span()),
            Self::Call(c) => c.span(),
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
        } else if lookahead.peek(Bracket) {
            input.parse().map(Self::Fix)
        } else if lookahead.peek(Paren) {
            input.parse().map(Self::Parens)
        } else if lookahead.peek(Ident::peek_any) {
            let name = input.call(Ident::parse_any)?;

            if input.peek(Paren) {
                input.parse().map(|args| Self::Call(Call { name, args }))
            } else {
                Ok(Self::Ident(name))
            }
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
            Term::Call(c) => write!(f, "Term::Call({:?})", c),
            Term::Fix(x) => write!(f, "Term::Fix({:?})", x),
            Term::Parens(p) => write!(f, "Term::Parens({:?})", p),
        }
    }
}

#[derive(Clone)]
pub struct Cat(pub Punctuated<Term, Token![.]>);

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

pub type Expression = Alt;

#[derive(Clone)]
pub struct LetStatement {
    let_token: Token![let],
    name: Ident,
    args: Option<ArgList<Ident>>,
    eq_token: Token![=],
    expr: Expression,
    semi_token: Token![;],
}

impl LetStatement {
    pub fn span(&self) -> Option<Span> {
        self.let_token.span.join(self.semi_token.span)
    }
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

        Ok(Self {
            let_token,
            name,
            args,
            eq_token,
            expr,
            semi_token,
        })
    }
}

impl fmt::Debug for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LetStatement")
            .field("name", &self.name)
            .field("args", &self.args)
            .field("expr", &self.expr)
            .finish()
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

#[derive(Clone, Debug)]
pub struct File {
    lets: Vec<LetStatement>,
    goal: GoalStatement,
}

impl File {
    pub fn convert(self) -> Result<(Vec<ast::Function>, ast::NamedExpression), ConvertError> {
        let mut names = Vec::new();
        let mut map = Vec::new();
        for stmt in self.lets {
            let span = stmt.span();
            let name: Name = stmt.name.into();
            let params = stmt
                .args
                .into_iter()
                .flat_map(ArgList::into_iter)
                .map(Name::from);
            let mut context = Context::new(&names, params.clone());
            let mut expr = stmt.expr.convert(&mut context)?;
            names.push(name.clone());
            expr.name = Some(name.clone());
            map.push(ast::Function {
                name,
                params: params.map(Some).collect(),
                expr,
                span,
            });
        }

        let mut context = Context::new(&names, Vec::new());
        let goal = self.goal.expr.convert(&mut context)?;
        Ok((map, goal))
    }
}

impl Parse for File {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut lets = Vec::new();
        let mut lookahead = input.lookahead1();

        while lookahead.peek(Let) {
            lets.push(input.parse()?);
            lookahead = input.lookahead1();
        }

        let goal = if lookahead.peek(Match) {
            input.parse()?
        } else {
            return Err(lookahead.error());
        };

        Ok(Self { lets, goal })
    }
}
