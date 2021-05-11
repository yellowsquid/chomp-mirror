use std::{convert::TryInto, mem};

use chomp::{
    chomp::{
        ast::{self, Alt, Call, Cat, Fix, Lambda, Let, Literal, NamedExpression, Variable},
        name::{Content, Name},
    },
    nibble::convert::{Context, Convert, ConvertError},
};
use chomp_macro::nibble;
use proc_macro2::Span;

nibble! {
    let bot = !(/rec/ "a" . rec);
    let zero = /zero suc/ zero;
    let suc n = /zero suc/ suc (n zero suc);

    let opt  some = _ : None | some;
    let plus iter = !(/plus/ iter . (opt plus));
    let star iter = opt (plus iter);

    let up_to x n = n bot (/rec/ x . opt rec);

    let Pattern_Whitespace = "\t"|"\n"|"\x0B"|"\x0c"|"\r"|" "|"\u{85}"|"\u{200e}"|"\u{200f}"|"\u{2028}"|"\u{2029}";

    let oct_digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" ;
    let digit     = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
    let hex_digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" |
                    "a" | "b" | "c" | "d" | "e" | "f" |
                    "A" | "B" | "C" | "D" | "E" | "F" ;

    let XID_Start =
        "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" |
        "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" |
        "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" |
        "y" | "z" |
        "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" |
        "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" |
        "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" |
        "Y" | "Z" ;
    let XID_Continue =
        XID_Start | "_" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;

    let literal_char =
        (" " | "!"       | "#" | "$" | "%" | "&" | "'" |
         "(" | ")" | "*" | "+" | "," | "-" | "." | "/" |
         "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" |
         "8" | "9" | ":" | ";" | "<" | "=" | ">" | "?" |
         "@" | "A" | "B" | "C" | "D" | "E" | "F" | "G" |
         "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" |
         "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" |
         "X" | "Y" | "Z" | "["       | "]" | "^" | "_" |
         "`" | "a" | "b" | "c" | "d" | "e" | "f" | "g" |
         "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" |
         "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" |
         "x" | "y" | "z" | "{" | "|" | "}" | "~") : Literal |
        "\\" . (
            ("\"" | "'" | "n" | "r" | "t" | "\\" | "0") : Ascii |
            "x"  . oct_digit . hex_digit : Oct |
            "u{" . up_to hex_digit (suc (suc (suc (suc (suc (suc zero)))))) . "}" : Unicode
        ) : Escape ;

    let ws = plus Pattern_Whitespace;
    let ows = opt ws;

    let list inner = !(/list/ inner . opt (ws . opt list));
    let separated inner sep = !(/separated/ inner . opt (sep . ows . separated));

    let epsilon = "_";
    let ident = XID_Start . star XID_Continue;
    let literal = "\"" . (plus literal_char : Contents) . "\"";
    let fix term = "!" . ows . term;
    let parens expr = "(" . ows . expr . ")";

    let names = list ident;

    let term expr = !(/term/
          epsilon : Epsilon
        | literal : Literal
        | parens expr : Parens
        | fix term : Fix
        | ident : Variable
        );

    let label = ":" . ows . ident . ows;

    let call   expr = list (term expr);
    let cat    expr = separated (call expr) ".";
    let alt    expr = separated (cat expr . opt label : Labelled) "|";
    let lambda expr = "/" . ows . names . "/" . ows . alt expr;
    let expr = !(/expr/ alt expr | lambda expr);
    let goal = "match" . ws . expr . ";" . ows;
    let let stmt = "let" . ws . names . "=" . ows . expr . ";" . ows . stmt;
    let stmt = !(/stmt/ let stmt | goal);
    match !(/skip/ Pattern_Whitespace . skip | stmt);
}

impl Convert for Ast {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let mut inner = self;

        while let Ast::Branch1(cat) = inner {
            inner = *cat.skip1;
        }

        match inner {
            Ast::Branch1(_) => unreachable!(),
            Ast::Stmt1(stmt) => stmt.convert(context),
        }
    }
}

impl Convert for Stmt1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        match self {
            Stmt1::Goal1(goal) => goal.expr1.convert(context),
            Stmt1::Let1(stmt) => {
                let mut names = stmt.names1.into_iter().peekable();
                let name = Name::new_let(names.next().unwrap());
                let bound = if names.peek().is_none() {
                    stmt.expr1.convert(context)?
                } else {
                    let args: Vec<Name> = names.map(Name::new_variable).collect();
                    let expr = stmt.expr1;
                    let inner = context.with_variables(args.clone(), |ctx| expr.convert(ctx))?;
                    NamedExpression {
                        name: None,
                        expr: Lambda {
                            args,
                            inner: Box::new(inner),
                        }
                        .into(),
                        span: Span::call_site(),
                    }
                };
                context.push_variable(name.clone());
                let body = stmt.stmt1.convert(context)?;
                Ok(NamedExpression {
                    name: None,
                    expr: Let {
                        name: name.clone(),
                        bound: Box::new(NamedExpression {
                            name: Some(name),
                            ..bound
                        }),
                        body: Box::new(body),
                    }
                    .into(),
                    span: Span::call_site(),
                })
            }
        }
    }
}

impl Convert for Expr1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        match self {
            Expr1::Alt1(alt) => alt.convert(context),
            Expr1::Lambda1(lambda) => lambda.convert(context),
        }
    }
}

impl Convert for Lambda1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let args: Vec<Name> = self.names1.into_iter().map(Name::new_variable).collect();
        let alt = self.alt1;
        let inner = context.with_variables(args.clone(), |ctx| alt.convert(ctx))?;
        Ok(NamedExpression {
            name: None,
            expr: Lambda {
                args,
                inner: Box::new(inner),
            }
            .into(),
            span: Span::call_site(),
        })
    }
}

impl Convert for Alt1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let first = self.labelled1.convert(context)?;
        let mut rest = self
            .opt1
            .into_iter()
            .map(|inner| inner.convert(context).map(|e| (Span::call_site(), e)))
            .peekable();
        if rest.peek().is_some() {
            Ok(NamedExpression {
                name: None,
                expr: Alt {
                    first: Box::new(first),
                    rest: rest.collect::<Result<_, _>>()?,
                }
                .into(),
                span: Span::call_site(),
            })
        } else {
            Ok(first)
        }
    }
}

impl Convert for Labelled1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let named = self.cat1.convert(context)?;
        let label = match self.opt1 {
            Opt15::None1(_) => None,
            Opt15::Label1(l) => Some(Name::new_label(l.ident1)),
        };
        let name = Name::merge(label, named.name);
        Ok(NamedExpression {
            name,
            expr: named.expr,
            span: named.span,
        })
    }
}

impl Convert for Cat1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let first = self.call1.convert(context)?;
        let mut rest = self
            .opt1
            .into_iter()
            .map(|inner| inner.convert(context).map(|e| (Span::call_site(), e)))
            .peekable();
        if rest.peek().is_some() {
            Ok(NamedExpression {
                name: None,
                expr: Cat {
                    first: Box::new(first),
                    rest: rest.collect::<Result<_, _>>()?,
                }
                .into(),
                span: Span::call_site(),
            })
        } else {
            Ok(first)
        }
    }
}

impl Convert for Call1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let first = self.term1.convert(context)?;
        let mut rest = self
            .opt1
            .into_iter()
            .map(|inner| inner.convert(context))
            .peekable();
        if rest.peek().is_some() {
            Ok(NamedExpression {
                name: None,
                expr: Call {
                    on: Box::new(first),
                    args: rest.collect::<Result<_, _>>()?,
                }
                .into(),
                span: Span::call_site(),
            })
        } else {
            Ok(first)
        }
    }
}

impl Convert for Term1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        match self {
            Term1::Epsilon1(_) => Ok(NamedExpression {
                name: None,
                expr: ast::Epsilon.into(),
                span: Span::call_site(),
            }),
            Term1::Literal1(literal) => Ok(NamedExpression {
                name: None,
                expr: literal.contents1.into_iter().collect::<Literal>().into(),
                span: Span::call_site(),
            }),
            Term1::Parens1(parens) => parens.expr1.convert(context),
            Term1::Fix1(fix) => fix.convert(context),
            Term1::Variable1(var) => var.convert(context),
        }
    }
}

impl Convert for Fix1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let inner = self.term1.convert(context)?;
        Ok(NamedExpression {
            name: None,
            expr: Fix {
                inner: Box::new(inner),
            }
            .into(),
            span: Span::call_site(),
        })
    }
}

impl Convert for Variable1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let name = Name::new_variable(self);
        let index = context
            .lookup(&name)
            .ok_or_else(|| ConvertError::UndeclaredName(Box::new(name.clone())))?;

        Ok(NamedExpression {
            name: Some(name),
            expr: Variable { index }.into(),
            span: Span::call_site(),
        })
    }
}

impl IntoIterator for Names1 {
    type Item = Ident1;

    type IntoIter = Opt3;

    fn into_iter(self) -> Self::IntoIter {
        Opt3::List1(Box::new(self))
    }
}

impl Iterator for Opt3 {
    type Item = Ident1;

    fn next(&mut self) -> Option<Self::Item> {
        let orig = mem::replace(self, Opt3::None1(Epsilon));
        match orig {
            Opt3::None1(_) => None,
            Opt3::List1(names) => {
                if let Opt4::Some1(some) = names.opt1 {
                    *self = some.opt1;
                }
                Some(names.ident1)
            }
        }
    }
}

impl Iterator for Opt16 {
    type Item = Labelled1;

    fn next(&mut self) -> Option<Self::Item> {
        let orig = mem::replace(self, Opt16::None1(Epsilon));
        match orig {
            Opt16::None1(_) => None,
            Opt16::Some1(some) => {
                *self = some.separated1.opt1;
                Some(some.separated1.labelled1)
            }
        }
    }
}

impl Iterator for Opt14 {
    type Item = Call1;

    fn next(&mut self) -> Option<Self::Item> {
        let orig = mem::replace(self, Opt14::None1(Epsilon));
        match orig {
            Opt14::None1(_) => None,
            Opt14::Some1(some) => {
                *self = some.separated1.opt1;
                Some(some.separated1.call1)
            }
        }
    }
}

impl Iterator for Opt13 {
    type Item = Term1;

    fn next(&mut self) -> Option<Self::Item> {
        let orig = mem::replace(self, Opt13::None1(Epsilon));
        match orig {
            Opt13::None1(_) => None,
            Opt13::Some1(some) => match some.opt1 {
                Opt12::None1(_) => None,
                Opt12::List1(call) => {
                    *self = call.opt1;
                    Some(call.term1)
                }
            },
        }
    }
}

impl IntoIterator for Contents1 {
    type Item = char;

    type IntoIter = Opt11;

    fn into_iter(self) -> Self::IntoIter {
        Opt11::Plus1(Box::new(self))
    }
}

impl Iterator for Opt11 {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let orig = mem::replace(self, Opt11::None1(Epsilon));
        match orig {
            Opt11::None1(_) => None,
            Opt11::Plus1(contents) => {
                *self = contents.opt1;
                Some(contents.literal_char1.into())
            }
        }
    }
}

impl From<LiteralChar1> for char {
    fn from(c: LiteralChar1) -> Self {
        match c {
            LiteralChar1::Literal1(c) => c.into(),
            LiteralChar1::Escape1(e) => e.into(),
        }
    }
}

impl From<Escape1> for char {
    fn from(e: Escape1) -> Self {
        match e.1 {
            Alt171::Ascii1(a) => a.escape(),
            Alt171::Oct1(o) => o.into(),
            Alt171::Unicode1(u) => u.into(),
        }
    }
}

impl Ascii1 {
    fn escape(self) -> char {
        match self {
            Ascii1::Branch1(_) => '\"',
            Ascii1::Branch2(_) => '\'',
            Ascii1::Branch3(_) => '\n',
            Ascii1::Branch4(_) => '\r',
            Ascii1::Branch5(_) => '\t',
            Ascii1::Branch6(_) => '\\',
            Ascii1::Branch7(_) => '\0',
        }
    }
}

impl From<Oct1> for char {
    fn from(o: Oct1) -> Self {
        let s: String = [char::from(o.oct_digit1), char::from(o.hex_digit1)]
            .iter()
            .collect();
        u32::from_str_radix(&s, 16).unwrap().try_into().unwrap()
    }
}

impl From<Unicode1> for char {
    fn from(u: Unicode1) -> Self {
        let s = u.up_to1.to_string();
        u32::from_str_radix(&s, 16).unwrap().try_into().unwrap()
    }
}

impl From<Variable1> for Content {
    fn from(i: Variable1) -> Self {
        i.to_string().into()
    }
}

impl From<Ident2> for Content {
    fn from(i: Ident2) -> Self {
        i.to_string().into()
    }
}

impl From<Ident1> for Content {
    fn from(i: Ident1) -> Self {
        i.to_string().into()
    }
}
