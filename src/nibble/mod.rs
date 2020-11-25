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

const PREFER_SUBST_OVER_CALL: bool = true;

pub type Epsilon = Token![_];

impl Convert for Epsilon {
    fn convert(&self, _: &mut Context) -> ast::Term {
        ast::Term::Epsilon(*self)
    }
}

type Ident = syn::Ident;

impl Convert for Ident {
    fn convert(&self, context: &mut Context) -> ast::Term {
        use ast::Term;
        let name = self.to_string();

        if let Some(variable) = context.get_binding(&name) {
            Term::Variable(ast::Variable::new(self.clone(), variable))
        } else if PREFER_SUBST_OVER_CALL {
            if let Some(term) = context.get_variable(&name) {
                term.clone()
            } else if let Some(term) = context.call_function(&name, std::iter::empty()) {
                term
            } else {
                let span = self.span();
                Term::Call(ast::Call::new(self.clone(), Vec::new(), span))
            }
        } else {
            let span = self.span();
            Term::Call(ast::Call::new(self.clone(), Vec::new(), span))
        }
    }
}

type Literal = syn::LitStr;

impl Convert for Literal {
    fn convert(&self, _: &mut Context) -> ast::Term {
        ast::Term::Literal(self.clone())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Call {
    name: Ident,
    paren_tok: token::Paren,
    args: Punctuated<Expression, Token![,]>,
}

impl Call {
    fn span(&self) -> Span {
        self.name
            .span()
            .join(self.paren_tok.span)
            .unwrap_or_else(Span::call_site)
    }
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
    fn convert(&self, context: &mut Context) -> ast::Term {
        use ast::Term;
        let args: Vec<_> = self
            .args
            .pairs()
            .map(|pair| match pair {
                Pair::Punctuated(t, _) => t.convert(context),
                Pair::End(t) => t.convert(context),
            })
            .collect();
        if PREFER_SUBST_OVER_CALL {
            if let Some(term) = context.call_function(&self.name.to_string(), args.clone()) {
                term
            } else {
                Term::Call(ast::Call::new(self.name.clone(), args, self.span()))
            }
        } else {
            Term::Call(ast::Call::new(self.name.clone(), args, self.span()))
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
    fn convert(&self, context: &mut Context) -> ast::Term {
        use ast::Term;
        let span = self.span();
        let expr = &self.expr;
        let arg_name = self.arg.to_string();
        Term::Fix(ast::Fix::new(
            self.arg.clone(),
            context.push_binding(arg_name, |c| expr.convert(c)),
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
    fn convert(&self, context: &mut Context) -> ast::Term {
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
        } else {
            Err(lookahead.error())
        }
    }
}

impl Convert for Term {
    fn convert(&self, context: &mut Context) -> ast::Term {
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
    fn convert(&self, context: &mut Context) -> ast::Term {
        use ast::Term;
        let mut iter = self.terms.pairs();
        let init = match iter.next().unwrap() {
            Pair::Punctuated(t, p) => Ok((t.convert(context), p)),
            Pair::End(t) => Err(t.convert(context)),
        };
        iter.fold(init, |term, pair| {
            let (fst, punct) = term.unwrap();
            match pair {
                Pair::Punctuated(t, p) => {
                    Ok((Term::Cat(ast::Cat::new(fst, *punct, t.convert(context))), p))
                }
                Pair::End(t) => Err(Term::Cat(ast::Cat::new(fst, *punct, t.convert(context)))),
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
    fn convert(&self, context: &mut Context) -> ast::Term {
        use ast::Term;
        let mut iter = self.cats.pairs();
        let init = match iter.next().unwrap() {
            Pair::Punctuated(t, p) => Ok((t.convert(context), p)),
            Pair::End(t) => Err(t.convert(context)),
        };
        iter.fold(init, |cat, pair| {
            let (left, punct) = cat.unwrap();
            match pair {
                Pair::Punctuated(t, p) => Ok((
                    Term::Alt(ast::Alt::new(left, *punct, t.convert(context))),
                    p,
                )),
                Pair::End(t) => Err(Term::Alt(ast::Alt::new(left, *punct, t.convert(context)))),
            }
        })
        .unwrap_err()
    }
}

pub type Expression = Alt;

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
