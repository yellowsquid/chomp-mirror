use std::rc::Rc;

use crate::ast::convert::ConvertMode;
use crate::ast::{
    self,
    convert::{Context, Convert},
};
use proc_macro2::Span;
use syn::{
    ext::IdentExt,
    parse::{Parse, ParseStream},
    punctuated::{Pair, Punctuated},
    token, Result, Token,
};

pub type Epsilon = Token![_];

impl Convert for Epsilon {
    fn convert(&self, _: &mut Context, _: ConvertMode) -> ast::Term {
        ast::Term::Epsilon(*self)
    }
}

type Ident = syn::Ident;

impl Convert for Ident {
    fn convert(&self, context: &mut Context, mode: ConvertMode) -> ast::Term {
        use ast::Term;
        let name = self.to_string();

        if let Some(variable) = context.get_binding(&name) {
            Term::Variable(ast::Variable::new(self.clone(), variable))
        } else {
            match mode {
                ConvertMode::NoSubstitution => {
                    let span = self.span();
                    Term::Call(ast::Call::new(self.clone(), Vec::new(), span))
                }
                ConvertMode::WithSubstitution => {
                    if let Some(term) = context.get_variable(&name) {
                        term.clone()
                    } else if let Some(term) =
                        context.call_function(&name, std::iter::empty(), mode)
                    {
                        term
                    } else {
                        let span = self.span();
                        Term::Call(ast::Call::new(self.clone(), Vec::new(), span))
                    }
                }
            }
        }
    }
}

type Literal = syn::LitStr;

impl Convert for Literal {
    fn convert(&self, _: &mut Context, _: ConvertMode) -> ast::Term {
        ast::Term::Literal(self.clone())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArgList<T> {
    paren_token: token::Paren,
    args: Punctuated<T, token::Comma>,
}

impl<T> ArgList<T> {
    fn span(&self) -> Span {
        self.paren_token.span
    }
}

impl<T> IntoIterator for ArgList<T> {
    type Item = T;
    type IntoIter = <Punctuated<T, Token![,]> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.args.into_iter()
    }
}

impl<T: Parse> Parse for ArgList<T> {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let args;
        let paren_token = syn::parenthesized!(args in input);
        let args = args.call(Punctuated::parse_terminated)?;
        Ok(Self { paren_token, args })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Call {
    name: Ident,
    args: ArgList<Expression>,
}

impl Call {
    fn span(&self) -> Span {
        self.name
            .span()
            .join(self.args.span())
            .unwrap_or_else(Span::call_site)
    }
}

impl Parse for Call {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let name = input.call(Ident::parse_any)?;
        let args = input.parse()?;
        Ok(Self { name, args })
    }
}

impl Convert for Call {
    fn convert(&self, context: &mut Context, mode: ConvertMode) -> ast::Term {
        use ast::Term;
        let args = self
            .args
            .clone()
            .into_iter()
            .map(|arg| arg.convert(context, mode))
            .collect::<Vec<_>>();
        match mode {
            ConvertMode::NoSubstitution => {
                Term::Call(ast::Call::new(self.name.clone(), args, self.span()))
            }
            ConvertMode::WithSubstitution => {
                if let Some(term) =
                    context.call_function(&self.name.to_string(), args.clone(), mode)
                {
                    term
                } else {
                    Term::Call(ast::Call::new(self.name.clone(), args, self.span()))
                }
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Fix {
    bracket_token: token::Bracket,
    arg: Ident,
    paren_token: token::Paren,
    expr: Expression,
}

impl Fix {
    fn span(&self) -> Span {
        self.bracket_token
            .span
            .join(self.paren_token.span)
            .unwrap_or_else(Span::call_site)
    }
}

impl Parse for Fix {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let arg;
        let bracket_token = syn::bracketed!(arg in input);
        let arg = arg.call(Ident::parse_any)?;
        let expr;
        let paren_token = syn::parenthesized!(expr in input);
        let expr = expr.parse()?;
        Ok(Self {
            bracket_token,
            arg,
            paren_token,
            expr,
        })
    }
}

impl Convert for Fix {
    fn convert(&self, context: &mut Context, mode: ConvertMode) -> ast::Term {
        use ast::Term;
        let span = self.span();
        let expr = &self.expr;
        let arg_name = self.arg.to_string();
        Term::Fix(ast::Fix::new(
            self.arg.clone(),
            context.push_binding(arg_name, |ctx| expr.convert(ctx, mode)),
            span,
        ))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParenExpression {
    paren_tok: token::Paren,
    expr: Expression,
}

impl ParenExpression {
    pub fn span(&self) -> Span {
        self.paren_tok.span
    }
}

impl Parse for ParenExpression {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let expr;
        let paren_tok = syn::parenthesized!(expr in input);
        let expr = expr.parse()?;
        Ok(Self { paren_tok, expr })
    }
}

impl Convert for ParenExpression {
    fn convert(&self, context: &mut Context, mode: ConvertMode) -> ast::Term {
        self.expr.convert(context, mode)
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

impl Term {
    pub fn span(&self) -> Span {
        match self {
            Self::Epsilon(eps) => eps.span,
            Self::Ident(ident) => ident.span(),
            Self::Literal(lit) => lit.span(),
            Self::Call(call) => call.span(),
            Self::Fix(fix) => fix.span(),
            Self::Parens(paren) => paren.span(),
        }
    }
}

impl Parse for Term {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![_]) {
            input.parse().map(Self::Epsilon)
        } else if lookahead.peek(syn::LitStr) {
            input.parse().map(Self::Literal)
        } else if lookahead.peek(token::Bracket) {
            input.parse().map(Self::Fix)
        } else if lookahead.peek(token::Paren) {
            let content;
            let paren_tok = syn::parenthesized!(content in input);
            content
                .parse()
                .map(|expr| Self::Parens(ParenExpression { paren_tok, expr }))
        } else if lookahead.peek(Ident::peek_any) {
            let name = input.call(Ident::parse_any)?;
            if input.peek(token::Paren) {
                input.parse().map(|args| Self::Call(Call { name, args }))
            } else {
                Ok(Self::Ident(name))
            }
        } else {
            Err(lookahead.error())
        }
    }
}

impl Convert for Term {
    fn convert(&self, context: &mut Context, mode: ConvertMode) -> ast::Term {
        match self {
            Self::Epsilon(e) => e.convert(context, mode),
            Self::Ident(i) => i.convert(context, mode),
            Self::Literal(l) => l.convert(context, mode),
            Self::Call(c) => c.convert(context, mode),
            Self::Fix(f) => f.convert(context, mode),
            Self::Parens(e) => e.convert(context, mode),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Cat {
    terms: Punctuated<Term, Token![.]>,
}

impl Cat {
    pub fn span(&self) -> Span {
        let mut pairs = self.terms.pairs();
        let mut span = pairs.next().and_then(|pair| match pair {
            Pair::Punctuated(term, punct) => term.span().join(punct.span),
            Pair::End(term) => Some(term.span()),
        });

        for pair in pairs {
            span = span.and_then(|span| match pair {
                Pair::Punctuated(term, punct) => {
                    span.join(term.span()).and_then(|s| s.join(punct.span))
                }
                Pair::End(term) => span.join(term.span()),
            })
        }

        span.unwrap_or_else(Span::call_site)
    }
}

impl Parse for Cat {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(Self {
            terms: input.call(Punctuated::parse_separated_nonempty)?,
        })
    }
}

impl Convert for Cat {
    fn convert(&self, context: &mut Context, mode: ConvertMode) -> ast::Term {
        use ast::Term;
        let mut iter = self.terms.pairs();
        let init = match iter.next().unwrap() {
            Pair::Punctuated(t, p) => Ok((t.convert(context, mode), p)),
            Pair::End(t) => Err(t.convert(context, mode)),
        };
        iter.fold(init, |term, pair| {
            let (fst, punct) = term.unwrap();
            match pair {
                Pair::Punctuated(t, p) => Ok((
                    Term::Cat(ast::Cat::new(fst, *punct, t.convert(context, mode))),
                    p,
                )),
                Pair::End(t) => Err(Term::Cat(ast::Cat::new(
                    fst,
                    *punct,
                    t.convert(context, mode),
                ))),
            }
        })
        .unwrap_err()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Alt {
    cats: Punctuated<Cat, Token![|]>,
}

impl Alt {
    pub fn span(&self) -> Span {
        let mut pairs = self.cats.pairs();
        let mut span = pairs.next().and_then(|pair| match pair {
            Pair::Punctuated(cat, punct) => cat.span().join(punct.span),
            Pair::End(cat) => Some(cat.span()),
        });

        for pair in pairs {
            span = span.and_then(|span| match pair {
                Pair::Punctuated(cat, punct) => {
                    span.join(cat.span()).and_then(|s| s.join(punct.span))
                }
                Pair::End(cat) => span.join(cat.span()),
            })
        }

        span.unwrap_or_else(Span::call_site)
    }
}

impl Parse for Alt {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(Self {
            cats: input.call(Punctuated::parse_separated_nonempty)?,
        })
    }
}

impl Convert for Alt {
    fn convert(&self, context: &mut Context, mode: ConvertMode) -> ast::Term {
        use ast::Term;
        let mut iter = self.cats.pairs();
        let init = match iter.next().unwrap() {
            Pair::Punctuated(t, p) => Ok((t.convert(context, mode), p)),
            Pair::End(t) => Err(t.convert(context, mode)),
        };
        iter.fold(init, |cat, pair| {
            let (left, punct) = cat.unwrap();
            match pair {
                Pair::Punctuated(t, p) => Ok((
                    Term::Alt(ast::Alt::new(left, *punct, t.convert(context, mode))),
                    p,
                )),
                Pair::End(t) => Err(Term::Alt(ast::Alt::new(
                    left,
                    *punct,
                    t.convert(context, mode),
                ))),
            }
        })
        .unwrap_err()
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

impl Parse for LetStatement {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let let_token = input.parse()?;
        let name = input.call(Ident::parse_any)?;
        let args = if input.peek(token::Paren) {
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
pub struct Goal {
    match_token: Token![match],
    expr: Expression,
}

impl Parse for Goal {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let match_token = input.parse()?;
        let expr = input.parse()?;

        Ok(Self { match_token, expr })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct File {
    lets: Vec<LetStatement>,
    goal: Goal,
}

impl File {
    pub fn convert_with_substitution(self) -> ast::Term {
        let mut context = Context::new();
        for statement in self.lets {
            context.add_function(
                statement.name.to_string(),
                Rc::new(statement.expr),
                statement
                    .args
                    .map(|args| args.into_iter().map(|arg| arg.to_string()).collect())
                    .unwrap_or_default(),
            );
        }

        self.goal.expr.convert(&mut context, ConvertMode::WithSubstitution)
    }
}

impl Parse for File {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let mut lets = Vec::new();
        while input.peek(Token![let]) {
            lets.push(input.parse()?);
        }

        let goal = input.parse()?;

        Ok(Self { lets, goal })
    }
}

#[cfg(test)]
mod tests {
    use super::{Epsilon, Ident, Literal};
    use syn::parse_str;

    #[test]
    fn parse_epsilon() {
        assert!(parse_str::<Epsilon>("_").is_ok());
    }

    #[test]
    fn parse_ident() {
        assert_eq!(parse_str::<Ident>("x").unwrap().to_string(), "x");
        assert_eq!(parse_str::<Ident>("x_yz").unwrap().to_string(), "x_yz");
        assert_eq!(parse_str::<Ident>("a123").unwrap().to_string(), "a123");
        assert_eq!(parse_str::<Ident>("𝒢𝒢").unwrap().to_string(), "𝒢𝒢");
        assert!(parse_str::<Ident>("1").is_err());
        assert!(parse_str::<Ident>("_").is_err());
    }

    #[test]
    fn parse_literal() {
        assert_eq!(parse_str::<Literal>(r#""hello""#).unwrap().value(), "hello")
    }
}
