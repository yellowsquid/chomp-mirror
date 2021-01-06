use proc_macro2::Span;
use syn::{
    bracketed,
    ext::IdentExt,
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::{Bracket, Comma, Let, Match, Paren},
    LitStr, Token,
};

use crate::chomp::ast;

use super::convert::{Context, Convert};

pub type Epsilon = Token![_];

pub type Ident = syn::Ident;

pub type Literal = LitStr;

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Epsilon(Epsilon),
    Ident(Ident),
    Literal(Literal),
    Call(Call),
    Fix(Fix),
    Parens(ParenExpression),
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Cat(pub Punctuated<Term, Token![.]>);

impl Parse for Cat {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        input.call(Punctuated::parse_separated_nonempty).map(Self)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Alt(pub Punctuated<Cat, Token![|]>);

impl Parse for Alt {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        input.call(Punctuated::parse_separated_nonempty).map(Self)
    }
}

pub type Expression = Alt;

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct File {
    lets: Vec<LetStatement>,
    goal: GoalStatement,
}

impl File {
    pub fn convert(self) -> Option<(Vec<ast::Function>, ast::Expression)> {
        let mut names = Vec::new();
        let mut map = Vec::new();
        for stmt in self.lets {
            let count = stmt.args.as_ref().map(ArgList::len).unwrap_or_default();
            let span = stmt.span();
            let mut context = Context::new(
                &names,
                stmt.args.into_iter().flat_map(|args| args.into_iter()),
            );
            names.push(stmt.name.clone());
            map.push(ast::Function::new(
                stmt.name.clone(),
                count,
                stmt.expr.convert(&mut context)?,
                span,
            ));
        }

        let mut context = Context::new(&names, Vec::new());
        let goal = self.goal.expr.convert(&mut context)?;
        Some((map, goal))
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
