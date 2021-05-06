pub mod convert;

use std::fmt;

use proc_macro2::TokenStream;
use quote::{ToTokens, TokenStreamExt};
use syn::{
    ext::IdentExt,
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::{Let, Match, Paren},
    LitStr, Token,
};

pub type Epsilon = Token![_];

pub type Ident = syn::Ident;

pub type Literal = LitStr;

#[derive(Clone)]
pub struct Fix {
    bang_token: Token![!],
    pub expr: Box<Term>,
}

impl ToTokens for Fix {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.bang_token.to_tokens(tokens);
        self.expr.to_tokens(tokens);
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

impl ToTokens for ParenExpression {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.paren_token.surround(tokens, |tokens| self.expr.to_tokens(tokens))
    }
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

impl ToTokens for Term {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Epsilon(e) => e.to_tokens(tokens),
            Self::Ident(i) => i.to_tokens(tokens),
            Self::Literal(l) => l.to_tokens(tokens),
            Self::Fix(f) => f.to_tokens(tokens),
            Self::Parens(p) => p.to_tokens(tokens),
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

impl ToTokens for Call {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(&self.0)
    }
}

impl Parse for Call {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut out = vec![input.parse()?];
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

impl ToTokens for Cat {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens)
    }
}

impl Parse for Cat {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        input.call(Punctuated::parse_separated_nonempty).map(Self)
    }
}

impl fmt::Debug for Cat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Cat")?;
        f.debug_list().entries(&self.0).finish()
    }
}

#[derive(Clone)]
pub struct Label {
    colon_tok: Token![:],
    pub label: Ident,
}

impl ToTokens for Label {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.colon_tok.to_tokens(tokens);
        self.label.to_tokens(tokens);
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

impl ToTokens for Labelled {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.cat.to_tokens(tokens);
        if let Some(label) = &self.label {
            label.to_tokens(tokens);
        }
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

impl ToTokens for Alt {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens)
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
        f.debug_list().entries(&self.0).finish()
    }
}
#[derive(Clone)]
pub struct Lambda {
    slash_tok_left: Token![/],
    pub args: Vec<Ident>,
    slash_tok_right: Token![/],
    pub expr: Alt,
}

impl ToTokens for Lambda {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.slash_tok_left.to_tokens(tokens);
        tokens.append_all(&self.args);
        self.slash_tok_right.to_tokens(tokens);
        self.expr.to_tokens(tokens);
    }
}

impl Parse for Lambda {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let slash_tok_left = input.parse()?;
        let mut args = Vec::new();
        loop {
            args.push(input.parse()?);
            let lookahead = input.lookahead1();

            if lookahead.peek(Token![/]) {
                break;
            } else if !lookahead.peek(Ident::peek_any) {
                return Err(lookahead.error());
            }
        }
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

impl ToTokens for Expression {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Alt(a) => a.to_tokens(tokens),
            Self::Lambda(l) => l.to_tokens(tokens),
        }
    }
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

impl ToTokens for GoalStatement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.match_token.to_tokens(tokens);
        self.expr.to_tokens(tokens);
        self.semi_token.to_tokens(tokens);
    }
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
    args: Vec<Ident>,
    eq_token: Token![=],
    expr: Expression,
    semi_token: Token![;],
    next: Box<Statement>,
}

impl ToTokens for LetStatement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.let_token.to_tokens(tokens);
        self.name.to_tokens(tokens);
        tokens.append_all(&self.args);
        self.eq_token.to_tokens(tokens);
        self.expr.to_tokens(tokens);
        self.semi_token.to_tokens(tokens);
        self.next.to_tokens(tokens);
    }
}

impl Parse for LetStatement {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let let_token = input.parse()?;
        let name = input.call(Ident::parse_any)?;
        let mut args = Vec::new();
        loop {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![=]) {
                break;
            } else if lookahead.peek(Ident::peek_any) {
                args.push(input.parse()?);
            } else {
                return Err(lookahead.error());
            }
        }
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

impl ToTokens for Statement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Goal(g) => g.to_tokens(tokens),
            Self::Let(l) => l.to_tokens(tokens),
        }
    }
}

impl Parse for Statement {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(Let) {
            input.parse().map(Self::Let)
        } else if lookahead.peek(Match) {
            input.parse().map(Self::Goal)
        } else {
            Err(lookahead.error())
        }
    }
}
