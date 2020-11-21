use crate::ast::{
    self,
    convert::{Context, Convert},
};
use proc_macro2::Span;
use syn::ext::IdentExt;
use syn::punctuated::Pair;
use syn::punctuated::Punctuated;
use syn::{
    parse::{Parse, ParseStream},
    token, Result,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Epsilon {
    paren_tok: token::Paren,
}

impl Parse for Epsilon {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let content;
        let paren_tok = syn::parenthesized!(content in input);
        if content.is_empty() {
            Ok(Self { paren_tok })
        } else {
            Err(content.error("expected empty parentheses"))
        }
    }
}

impl Convert for Epsilon {
    fn convert(self, _: &Context) -> ast::Term {
        ast::Term::Epsilon(ast::Epsilon::new(self.paren_tok.span))
    }
}

type Ident = syn::Ident;

impl Convert for Ident {
    fn convert(self, context: &Context) -> ast::Term {
        use ast::Term;
        let name = self.to_string();

        if let Some(variable) = context.get(&name) {
            Term::Variable(ast::Variable::new(self, variable))
        } else {
            let span = self.span();
            Term::Call(ast::Call::new(self, Vec::new(), span))
        }
    }
}

type Literal = syn::LitStr;

impl Convert for Literal {
    fn convert(self, _context: &Context) -> ast::Term {
        ast::Term::Literal(self)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Call {
    name: Ident,
    paren_tok: token::Paren,
    args: Punctuated<Expression, syn::Token![,]>,
}

impl Parse for Call {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let args;
        let name = input.call(Ident::parse_any)?;
        let paren_tok = syn::parenthesized!(args in input);
        let args = args.call(Punctuated::parse_terminated)?;
        Ok(Self {
            name,
            paren_tok,
            args,
        })
    }
}

impl Convert for Call {
    fn convert(self, context: &Context) -> ast::Term {
        use ast::Term;
        let span = self.name
            .span()
            .join(self.paren_tok.span)
            .unwrap_or_else(Span::call_site);
        Term::Call(ast::Call::new(
            self.name,
            self.args
                .into_pairs()
                .map(|pair| match pair {
                    Pair::Punctuated(t, _) => t.convert(context),
                    Pair::End(t) => t.convert(context),
                })
                .collect(),
            span,
        ))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Fix {
    bracket_token: token::Bracket,
    arg: Ident,
    paren_token: token::Paren,
    expr: Expression,
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
    fn convert(self, context: &Context) -> ast::Term {
        use ast::Term;
        let expr = self.expr;
        let arg_name = self.arg.to_string();
        Term::Fix(ast::Fix::new(
            self.arg,
            context.push(arg_name, |c| expr.convert(c)),
            self.bracket_token
                .span
                .join(self.paren_token.span)
                .unwrap_or_else(Span::call_site),
        ))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ParenExpression {
    paren_tok: token::Paren,
    expr: Expression,
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
    fn convert(self, context: &Context) -> ast::Term {
        self.expr.convert(context)
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
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(token::Paren) {
            let content;
            let paren_tok = syn::parenthesized!(content in input);
            if content.is_empty() {
                Ok(Self::Epsilon(Epsilon { paren_tok }))
            } else {
                content
                    .parse()
                    .map(|expr| Self::Parens(ParenExpression { paren_tok, expr }))
            }
        } else if lookahead.peek(Ident::peek_any) {
            let name = input.call(Ident::parse_any)?;
            if input.peek(token::Paren) {
                let args;
                let paren_tok = syn::parenthesized!(args in input);
                args.call(Punctuated::parse_terminated).map(|args| {
                    Self::Call(Call {
                        name,
                        paren_tok,
                        args,
                    })
                })
            } else {
                Ok(Self::Ident(name))
            }
        } else if lookahead.peek(token::Bracket) {
            input.parse().map(Self::Fix)
        } else if lookahead.peek(syn::LitStr) {
            input.parse().map(Self::Literal)
        } else {
            Err(lookahead.error())
        }
    }
}

impl Convert for Term {
    fn convert(self, context: &Context) -> ast::Term {
        match self {
            Self::Epsilon(e) => e.convert(context),
            Self::Ident(i) => i.convert(context),
            Self::Literal(l) => l.convert(context),
            Self::Call(c) => c.convert(context),
            Self::Fix(f) => f.convert(context),
            Self::Parens(e) => e.convert(context),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Cat {
    terms: Punctuated<Term, syn::Token![.]>,
}

impl Parse for Cat {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(Self {
            terms: input.call(Punctuated::parse_separated_nonempty)?,
        })
    }
}

impl Convert for Cat {
    fn convert(self, context: &Context) -> ast::Term {
        use ast::Term;
        let mut iter = self.terms.into_pairs();
        let init = match iter.next_back().unwrap() {
            Pair::Punctuated(_, _) => unreachable!(),
            Pair::End(t) => t.convert(context),
        };
        iter.rfold(init, |term, pair| match pair {
            Pair::Punctuated(t, p) => Term::Cat(ast::Cat::new(t.convert(context), p, term)),
            Pair::End(_) => unreachable!(),
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Alt {
    cats: Punctuated<Cat, syn::Token![|]>,
}

impl Parse for Alt {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(Self {
            cats: input.call(Punctuated::parse_separated_nonempty)?,
        })
    }
}

impl Convert for Alt {
    fn convert(self, context: &Context) -> ast::Term {
        use ast::Term;
        let mut iter = self.cats.into_pairs();
        let init = match iter.next_back().unwrap() {
            Pair::Punctuated(_, _) => unreachable!(),
            Pair::End(t) => t.convert(context),
        };
        iter.rfold(init, |term, pair| match pair {
            Pair::Punctuated(t, p) => Term::Alt(ast::Alt::new(t.convert(context), p, term)),
            Pair::End(_) => unreachable!(),
        })
    }
}

pub type Expression = Alt;

#[cfg(test)]
mod tests {
    use super::{Epsilon, Ident, Literal};
    use syn::parse_str;

    #[test]
    fn parse_epsilon() {
        assert!(parse_str::<Epsilon>("()").is_ok());
        assert!(parse_str::<Epsilon>("(  )").is_ok());
        assert!(parse_str::<Epsilon>("(").is_err());
        assert!(parse_str::<Epsilon>(")").is_err());
        assert!(parse_str::<Epsilon>("(x)").is_err());
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
