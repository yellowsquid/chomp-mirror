use std::convert::TryInto;

use chomp::{
    chomp::{
        ast::{self, Alt, Call, Cat, Fix, Function, NamedExpression, Parameter, Variable},
        Name,
    },
    nibble::convert::{Binding, Context, Convert, ConvertError},
};

chomp_macro::nibble! {
    let opt(x) = _ : None | x : Some;
    let plus(x) = [plus]((x : First) . (opt(plus) : Next));
    let star(x) = opt(plus(x));
    let star_(base, step) = [rec](base : Base | step . rec : Step);

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
            "x" . oct_digit . hex_digit : Oct |
            "u{" .hex_digit
                 .opt(hex_digit
                     .opt(hex_digit
                         .opt(hex_digit
                             .opt(hex_digit . opt(hex_digit))))) . "}" : Unicode
        ) : Escape ;

    let ws = plus(Pattern_Whitespace);

    let punctuated(x, p) = [rec]((x : First) . (opt(p . opt(ws) . rec) : Next));
    let list(x) = "(" . opt(ws) . [rec]((x : First) . (opt("," . opt(ws) . opt(rec)) : Next)) . ")";

    let epsilon = "_";
    let ident = XID_Start . star(XID_Continue);
    let literal = "\"" . (plus(literal_char) : Contents) . "\"";
    let parens(expr) = "(" . opt(ws) . (expr : Inner) . ")";
    let fix(expr) = "[" . opt(ws) . (ident : Arg) . opt(ws) . "]" . opt(ws) . (parens(expr) : Inner);

    let term(expr) =
          epsilon . opt(ws) : Epsilon
        | literal . opt(ws) : Literal
        | parens(expr) . opt(ws) : Parens
        | fix(expr) . opt(ws) : Fix
        | ident . opt(ws) . opt(list(expr) . opt(ws)) : CallOrVariable
        ;

    let label = ":" . opt(ws) . (ident : Label) . opt(ws);
    let cat(expr) = punctuated(term(expr), ".");
    let alt(expr) = punctuated((cat(expr) : Cat) . (opt(label) : Name), "|");
    let expr = [expr](alt(expr));
    let let = "let" . ws . (ident : Name) . opt(ws) . (opt(list(ident . opt(ws)) . opt(ws)) : Args) . "=" . opt(ws) . (expr : Expr) . ";" . opt(ws);
    let goal = "match" . ws . (expr : Expr) . ";" . opt(ws);

    match star_(star_(goal : Goal, let : Let), Pattern_Whitespace);
}

impl Ast {
    pub fn convert(self) -> Result<(Vec<Function>, NamedExpression), ConvertError> {
        let content = Star2::from(self.0);
        let mut names = Vec::new();
        let mut map = Vec::new();

        let mut iter = content.into_iter();

        for stmt in &mut iter {
            let name: Name = stmt.name1.into();
            let params = Option::from(stmt.args1)
                .into_iter()
                .flat_map(List2::into_iter)
                .map(Name::from);
            let mut context = Context::new(&names, params.clone());
            let mut expr = stmt.expr1.convert(&mut context)?;
            names.push(name.clone());
            expr.name = Some(name.clone());
            map.push(Function {
                name,
                params: params.map(Some).collect(),
                expr,
                span: None,
            });
        }

        let mut context = Context::new(&names, Vec::new());
        let goal = iter.consume().expr1.convert(&mut context)?;

        Ok((map, goal))
    }
}

impl Convert for Expr1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let mut iter = self.0.into_iter();
        let first = iter.next().unwrap().convert(context)?;
        let rest = iter
            .map(|term| Ok((None, term.convert(context)?)))
            .collect::<Result<Vec<_>, _>>()?;

        let mut iter = rest.into_iter();
        if let Some((punct, second)) = iter.next() {
            Ok(NamedExpression {
                name: None,
                expr: Alt {
                    first: Box::new(first),
                    punct,
                    second: Box::new(second),
                    rest: iter.collect(),
                }
                .into(),
                span: None,
            })
        } else {
            Ok(first)
        }
    }
}

impl Convert for First1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let named = self.cat1.convert(context)?;
        let name = Option::from(self.name1).or(named.name);

        Ok(NamedExpression {
            name,
            expr: named.expr,
            span: named.span,
        })
    }
}

impl Convert for Cat1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let mut iter = self.into_iter();
        let first = iter.next().unwrap().convert(context)?;
        let rest = iter
            .map(|term| Ok((None, term.convert(context)?)))
            .collect::<Result<Vec<_>, _>>()?;

        let mut iter = rest.into_iter();
        if let Some((punct, second)) = iter.next() {
            Ok(NamedExpression {
                name: None,
                expr: Cat {
                    first: Box::new(first),
                    punct,
                    second: Box::new(second),
                    rest: iter.collect(),
                }
                .into(),
                span: None,
            })
        } else {
            Ok(first)
        }
    }
}

impl Convert for Term1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        match self {
            Self::Epsilon1(_) => Ok(NamedExpression {
                name: None,
                expr: ast::Epsilon.into(),
                span: None,
            }),
            Self::Literal1(l) => Ok(NamedExpression {
                name: None,
                expr: l.value().into(),
                span: None,
            }),
            Self::Parens1(p) => p.parens1.expr1.convert(context),
            Self::Fix1(f) => f.fix1.convert(context),
            Self::CallOrVariable1(c) => c.convert(context),
        }
    }
}

impl Convert for Fix1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let arg = self.arg1.into();
        let expr = *self.inner1.expr1;
        let inner = context.with_variable(&arg, |context| expr.convert(context))?;

        Ok(NamedExpression {
            name: None,
            expr: Fix {
                arg: Some(arg),
                inner: Box::new(inner),
            }
            .into(),
            span: None,
        })
    }
}

impl Convert for CallOrVariable1 {
    fn convert(self, context: &mut Context) -> Result<NamedExpression, ConvertError> {
        let name = self.ident1.into();

        match self.opt2 {
            Opt20::None1(_) => {
                let binding = context
                    .lookup(&name)
                    .ok_or_else(|| ConvertError::UndeclaredName(Box::new(name.clone())))?;

                Ok(match binding {
                    Binding::Variable(index) => NamedExpression {
                        name: Some(name),
                        expr: Variable { index }.into(),
                        span: None,
                    },
                    Binding::Parameter(index) => NamedExpression {
                        name: Some(name),
                        expr: Parameter { index }.into(),
                        span: None,
                    },
                    Binding::Global => NamedExpression {
                        name: None,
                        expr: Call {
                            name,
                            args: Vec::new(),
                        }
                        .into(),
                        span: None,
                    },
                })
            }
            Opt20::Some1(s) => {
                let args = s
                    .list1
                    .into_iter()
                    .map(|arg| arg.convert(context))
                    .collect::<Result<_, _>>()?;
                Ok(NamedExpression {
                    name: None,
                    expr: Call { name, args }.into(),
                    span: None,
                })
            }
        }
    }
}

impl Literal3 {
    pub fn value(self) -> String {
        self.literal1
            .contents1
            .into_iter()
            .map(LiteralChar1::value)
            .collect()
    }
}

impl LiteralChar1 {
    pub fn value(self) -> char {
        match self {
            Self::Literal1(c) => c.into(),
            Self::Escape1(e) => e.1.value(),
        }
    }
}

impl Alt143 {
    pub fn value(self) -> char {
        match self {
            Self::Ascii1(a) => a.value(),
            Self::Oct1(o) => o.value(),
            Self::Unicode1(u) => u.value(),
        }
    }
}

impl Ascii1 {
    pub fn value(self) -> char {
        match self {
            Self::Branch1(_) => '\"',
            Self::Branch2(_) => '\'',
            Self::Branch3(_) => '\n',
            Self::Branch4(_) => '\r',
            Self::Branch5(_) => '\t',
            Self::Branch6(_) => '\\',
            Self::Branch7(_) => '\0',
        }
    }
}

impl Oct1 {
    pub fn value(self) -> char {
        let s: String = [char::from(self.oct_digit1), char::from(self.hex_digit1)]
            .iter()
            .collect();
        u32::from_str_radix(&s, 16).unwrap().try_into().unwrap()
    }
}

impl Unicode1 {
    pub fn value(self) -> char {
        let s: String = [self.hex_digit1.to_string(), self.opt1.to_string()]
            .iter()
            .map::<&str, _>(|s| s)
            .collect();
        u32::from_str_radix(&s, 16).unwrap().try_into().unwrap()
    }
}

impl IntoIterator for Cat1 {
    type Item = Term1;

    type IntoIter = Cat1Iter;

    fn into_iter(self) -> Self::IntoIter {
        Cat1Iter(Some(self))
    }
}

pub struct Cat1Iter(Option<Cat1>);

impl Iterator for Cat1Iter {
    type Item = Term1;

    fn next(&mut self) -> Option<Self::Item> {
        let cat = self.0.take()?.0;
        let term = cat.term1;
        self.0 = cat.next1.into();
        Some(term)
    }
}

impl IntoIterator for Contents1 {
    type Item = LiteralChar1;

    type IntoIter = Contents1Iter;

    fn into_iter(self) -> Self::IntoIter {
        Contents1Iter(Some(self))
    }
}

pub struct Contents1Iter(Option<Contents1>);

impl Iterator for Contents1Iter {
    type Item = LiteralChar1;

    fn next(&mut self) -> Option<Self::Item> {
        let cat = self.0.take()?.0;
        let lit = cat.literal_char1;
        self.0 = cat.next1.into();
        Some(lit)
    }
}

impl IntoIterator for List1 {
    type Item = Expr1;

    type IntoIter = Fix192Iter;

    fn into_iter(self) -> Self::IntoIter {
        Fix192Iter(Some(self.part3))
    }
}

pub struct Fix192Iter(Option<Fix192>);

impl Iterator for Fix192Iter {
    type Item = Expr1;

    fn next(&mut self) -> Option<Self::Item> {
        let cat = self.0.take()?.0;
        let expr = *cat.expr1;
        self.0 = cat.next1.into();
        Some(expr)
    }
}

impl IntoIterator for Star2 {
    type Item = Let1;

    type IntoIter = Star2Iter;

    fn into_iter(self) -> Self::IntoIter {
        Star2Iter(Some(self))
    }
}

pub struct Star2Iter(Option<Star2>);

impl Star2Iter {
    pub fn consume(self) -> Goal1 {
        let mut star = self.0.unwrap();

        loop {
            match star.0 {
                Alt274::Step1(step) => star = *step.rec1,
                Alt274::Goal1(goal) => return goal,
            }
        }
    }
}

impl Iterator for Star2Iter {
    type Item = Let1;

    fn next(&mut self) -> Option<Self::Item> {
        let star = self.0.take().unwrap();

        // You can probably be safer about this and use `mem::swap` or similar.
        // I cannot think of a way how, so this will do.
        if let Alt274::Step1(step) = star.0 {
            let stmt = step.let1;
            self.0 = Some(*step.rec1);
            Some(stmt)
        } else {
            self.0 = Some(star);
            None
        }
    }
}

impl IntoIterator for Alt1 {
    type Item = First1;

    type IntoIter = Alt1Iter;

    fn into_iter(self) -> Self::IntoIter {
        Alt1Iter(Some(self))
    }
}

#[derive(Clone)]
pub struct Alt1Iter(Option<Alt1>);

impl Iterator for Alt1Iter {
    type Item = First1;

    fn next(&mut self) -> Option<Self::Item> {
        let cat = self.0.take()?.0;
        let first = cat.first1;
        self.0 = cat.next1.into();
        Some(first)
    }
}

impl IntoIterator for List2 {
    type Item = Ident2;

    type IntoIter = Fix246Iter;

    fn into_iter(self) -> Self::IntoIter {
        Fix246Iter(Some(self.part3))
    }
}

#[derive(Clone)]
pub struct Fix246Iter(Option<Fix246>);

impl Iterator for Fix246Iter {
    type Item = Ident2;

    fn next(&mut self) -> Option<Self::Item> {
        let cat = self.0.take()?.0;
        let expr = cat.first1.ident1;
        self.0 = cat.next1.into();
        Some(expr)
    }
}

impl From<Next6> for Option<Alt1> {
    fn from(o: Next6) -> Self {
        match o {
            Next6::None1(_) => None,
            Next6::Some1(s) => Some(*s.rec1),
        }
    }
}

impl From<Args1> for Option<List2> {
    fn from(o: Args1) -> Self {
        match o {
            Args1::None1(_) => None,
            Args1::Some1(s) => Some(s.list1),
        }
    }
}

impl From<Next2> for Option<Contents1> {
    fn from(o: Next2) -> Self {
        match o {
            Next2::None1(_) => None,
            Next2::Plus1(s) => Some(*s),
        }
    }
}

impl From<Next4> for Option<Fix192> {
    fn from(o: Next4) -> Self {
        match o {
            Next4::None1(_) => None,
            Next4::Some1(s) => match s.opt2 {
                Opt18::None1(_) => None,
                Opt18::Rec1(e) => Some(*e),
            },
        }
    }
}

impl From<Next5> for Option<Cat1> {
    fn from(o: Next5) -> Self {
        match o {
            Next5::None1(_) => None,
            Next5::Some1(s) => Some(*s.rec1)
        }
    }
}

impl From<Next7> for Option<Fix246> {
    fn from(o: Next7) -> Self {
        match o {
            Next7::None1(_) => None,
            Next7::Some1(s) => match s.opt2 {
                Opt30::None1(_) => None,
                Opt30::Rec1(e) => Some(*e),
            },
        }
    }
}

impl From<Name1> for Option<Name> {
    fn from(o: Name1) -> Self {
        match o {
            Name1::None1(_) => None,
            Name1::Label1(l) => Some(l.label1.to_string().into()),
        }
    }
}

impl From<Name2> for Name {
    fn from(i: Name2) -> Self {
        i.to_string().into()
    }
}

impl From<Arg1> for Name {
    fn from(i: Arg1) -> Self {
        i.to_string().into()
    }
}

impl From<Ident1> for Name {
    fn from(i: Ident1) -> Self {
        i.to_string().into()
    }
}

impl From<Ident2> for Name {
    fn from(i: Ident2) -> Self {
        i.to_string().into()
    }
}


impl From<Alt277> for Star2 {
    fn from(mut a: Alt277) -> Self {
        while let Alt277::Step1(step) = a {
            a = (*step.rec1).0;
        }

        if let Alt277::Star1(s) = a {
            s
        } else {
            unreachable!()
        }
    }
}
