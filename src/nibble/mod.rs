pub mod parse;

use self::parse::{
    iter::{Peek, PeekExt, PeekableTakeWhile},
    requires, Parse, ParseError,
};
use std::convert::Infallible;
use std::str::FromStr;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ident {
    name: String,
}

impl Parse for Ident {
    type Err = Infallible;

    fn parse<I: Peek<Item = char>>(mut iter: I) -> Result<Self, ParseError<Self::Err>> {
        // rustc can't predict type, but rust-analyzer can...
        let name: String = PeekableTakeWhile::new(&mut iter, |c| c.is_alphabetic()).collect();
        if name.is_empty() {
            if let Some(c) = iter.peek() {
                Err(ParseError::InvalidCharacter(*c))
            } else {
                Err(ParseError::EndOfFile)
            }
        } else {
            Ok(Self { name })
        }
    }
}

impl FromStr for Ident {
    type Err = ParseError<<Ident as Parse>::Err>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ident::parse(s.chars().peekable())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Literal {
    contents: String,
}

impl Parse for Literal {
    type Err = LiteralError;

    fn parse<I: Peek<Item = char>>(mut iter: I) -> Result<Self, ParseError<Self::Err>> {
        fn parse_digit<I: Peek<Item = char>>(
            mut iter: I,
            radix: u32,
        ) -> Result<u32, ParseError<LiteralError>> {
            Ok(iter
                .next_if(|c| c.is_digit(radix))
                .ok_or(ParseError::EndOfFile)?
                .map_err(|i| ParseError::InvalidCharacter(*i.peek().unwrap()))?
                .to_digit(radix)
                .unwrap())
        }

        /// Parse full character escape.
        fn parse_escape<I: Peek<Item = char>>(
            mut iter: I,
        ) -> Result<char, ParseError<LiteralError>> {
            requires(&mut iter, '\\')?;
            match iter.peek().ok_or(ParseError::EndOfFile)? {
                '\'' | '\"' | '\\' => Ok(iter.next().unwrap()),
                'n' => {
                    requires(iter, 'n')?;
                    Ok('\n')
                }
                'r' => {
                    requires(iter, 'r')?;
                    Ok('\r')
                }
                't' => {
                    requires(iter, 't')?;
                    Ok('\t')
                }
                '0' => {
                    requires(iter, '0')?;
                    Ok('\0')
                }
                'x' => {
                    requires(&mut iter, 'x')?;
                    let upper = parse_digit(&mut iter, 8)?;
                    let ord = 16 * upper + parse_digit(&mut iter, 16)?;
                    std::char::from_u32(ord)
                        .ok_or(ParseError::Other(LiteralError::InvalidCharacterCode(ord)))
                }
                'u' => {
                    requires(&mut iter, 'u')?;
                    requires(&mut iter, '{')?;
                    let mut ord = 0;
                    for _ in 1..=6 {
                        ord *= 16;
                        ord += parse_digit(&mut iter, 16)?;
                        iter.advance_while(|&c| c == '_');

                        if iter.peek() == Some(&'}') {
                            break;
                        }
                    }
                    requires(&mut iter, '}')?;

                    std::char::from_u32(ord)
                        .ok_or(ParseError::Other(LiteralError::InvalidCharacterCode(ord)))
                }
                &c => Err(ParseError::InvalidCharacter(c)),
            }
        }

        requires(&mut iter, '\'')?;

        let mut s = String::new();

        loop {
            match iter.peek().ok_or(ParseError::EndOfFile)? {
                '\'' => {
                    iter.next();
                    return if s.is_empty() {
                        Err(ParseError::Other(LiteralError::EmptyLiteral))
                    } else {
                        Ok(Literal { contents: s })
                    };
                }
                &c @ '\n' | &c @ '\r' | &c @ '\t' => return Err(ParseError::InvalidCharacter(c)),
                '\\' => s.push(parse_escape(&mut iter)?),
                _ => s.push(iter.next().unwrap()),
            }
        }
    }
}

impl FromStr for Literal {
    type Err = ParseError<<Literal as Parse>::Err>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Literal::parse(s.chars().peekable())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LiteralError {
    EmptyLiteral,
    InvalidCharacterCode(u32),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Call {
    name: Ident,
    args: Vec<Expression>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Fix {
    arg: Ident,
    expr: Expression,
}

impl Parse for Fix {
    type Err = FixError;

    fn parse<I: Peek<Item = char>>(mut iter: I) -> Result<Self, ParseError<Self::Err>> {
        requires(&mut iter, '[')?;
        let arg = Ident::parse(&mut iter).map_err(ParseError::map)?;
        requires(&mut iter, ']')?;
        requires(&mut iter, '(')?;
        iter.advance_while(|c| c.is_whitespace());
        let expr = Expression::parse(Box::new(&mut iter as &mut dyn Peek<Item = char>))
            .map_err(ParseError::map)?;
        requires(&mut iter, ')')?;
        Ok(Self { arg, expr })
    }
}

impl FromStr for Fix {
    type Err = ParseError<<Fix as Parse>::Err>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Fix::parse(s.chars().peekable())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FixError(Box<ExpressionError>);

impl From<Infallible> for FixError {
    fn from(other: Infallible) -> Self {
        match other {}
    }
}

impl From<ExpressionError> for FixError {
    fn from(other: ExpressionError) -> Self {
        Self(Box::new(other))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Epsilon,
    Ident(Ident),
    Literal(Literal),
    Call { name: Ident, args: Vec<Expression> },
    Fix(Fix),
    Parens(Expression),
}

impl Parse for Term {
    type Err = TermError;

    fn parse<I: Peek<Item = char>>(mut iter: I) -> Result<Self, ParseError<Self::Err>> {
        match iter.peek().ok_or(ParseError::EndOfFile)? {
            '(' => {
                iter.next();

                if iter.consume_if_next(&')') {
                    Ok(Self::Epsilon)
                } else {
                    let expr = Expression::parse(Box::new(&mut iter as &mut dyn Peek<Item = char>))
                        .map_err(ParseError::map)?;
                    requires(iter, ')').map(|_| Self::Parens(expr))
                }
            }
            '[' => Fix::parse(iter).map(Self::Fix).map_err(ParseError::map),
            '\'' => Literal::parse(iter)
                .map(Self::Literal)
                .map_err(ParseError::map),
            c if c.is_alphabetic() => {
                let ident = Ident::parse(&mut iter).map_err(ParseError::map)?;

                if iter.consume_if_next(&'(') {
                    iter.advance_while(|c| c.is_whitespace());
                    let mut args = Vec::new();
                    loop {
                        args.push(
                            Expression::parse(Box::new(&mut iter as &mut dyn Peek<Item = char>))
                                .map_err(ParseError::<CallError>::map)
                                .map_err(ParseError::map)?,
                        );

                        if iter.consume_if_next(&')') {
                            return Ok(Self::Call { name: ident, args });
                        }

                        requires(&mut iter, ',')?;
                        iter.advance_while(|c| c.is_whitespace());
                    }
                } else {
                    Ok(Self::Ident(ident))
                }
            }
            &c => Err(ParseError::InvalidCharacter(c)),
        }
    }
}

impl FromStr for Term {
    type Err = ParseError<<Term as Parse>::Err>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Term::parse(s.chars().peekable())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CallError(Box<ExpressionError>);

impl From<ExpressionError> for CallError {
    fn from(other: ExpressionError) -> Self {
        Self(Box::new(other))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TermError {
    Literal(LiteralError),
    Call(CallError),
    Fix(FixError),
    Parens(Box<ExpressionError>),
}

impl From<Infallible> for TermError {
    fn from(other: Infallible) -> Self {
        match other {}
    }
}

impl From<ExpressionError> for TermError {
    fn from(other: ExpressionError) -> Self {
        Self::Parens(Box::new(other))
    }
}

impl From<FixError> for TermError {
    fn from(other: FixError) -> Self {
        Self::Fix(other)
    }
}

impl From<CallError> for TermError {
    fn from(other: CallError) -> Self {
        Self::Call(other)
    }
}

impl From<LiteralError> for TermError {
    fn from(other: LiteralError) -> Self {
        Self::Literal(other)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Cat {
    terms: Vec<Term>,
}

impl Parse for Cat {
    type Err = SeqError;

    fn parse<I: Peek<Item = char>>(mut iter: I) -> Result<Self, ParseError<Self::Err>> {
        let mut terms = Vec::new();
        terms.push(Term::parse(&mut iter).map_err(ParseError::map)?);
        iter.advance_while(|c| c.is_whitespace());

        while iter.consume_if_next(&'.') {
            iter.advance_while(|c| c.is_whitespace());
            terms.push(Term::parse(&mut iter).map_err(ParseError::map)?);
            iter.advance_while(|c| c.is_whitespace());
        }

        Ok(Self { terms })
    }
}

impl FromStr for Cat {
    type Err = ParseError<<Cat as Parse>::Err>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Cat::parse(s.chars().peekable())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SeqError(TermError);

impl From<TermError> for SeqError {
    fn from(other: TermError) -> Self {
        Self(other)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Alt {
    seqs: Vec<Cat>,
}

impl Parse for Alt {
    type Err = AltError;

    fn parse<I: Peek<Item = char>>(mut iter: I) -> Result<Self, ParseError<Self::Err>> {
        let mut seqs = Vec::new();
        seqs.push(Cat::parse(&mut iter).map_err(ParseError::map)?);

        while iter.consume_if_next(&'|') {
            iter.advance_while(|c| c.is_whitespace());
            seqs.push(Cat::parse(&mut iter).map_err(ParseError::map)?);
        }

        Ok(Self { seqs })
    }
}

impl FromStr for Alt {
    type Err = ParseError<<Alt as Parse>::Err>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Alt::parse(s.chars().peekable())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AltError(SeqError);

impl From<SeqError> for AltError {
    fn from(other: SeqError) -> Self {
        Self(other)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Expression {
    alt: Alt,
}

impl Parse for Expression {
    type Err = ExpressionError;

    fn parse<I: Peek<Item = char>>(iter: I) -> Result<Self, ParseError<Self::Err>> {
        Ok(Self {
            alt: Alt::parse(iter).map_err(ParseError::map)?,
        })
    }
}

impl FromStr for Expression {
    type Err = ParseError<<Expression as Parse>::Err>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Expression::parse(s.chars().peekable())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExpressionError(AltError);

impl From<AltError> for ExpressionError {
    fn from(other: AltError) -> Self {
        Self(other)
    }
}

#[cfg(test)]
mod tests {
    use super::Alt;
    use super::Expression;
    use super::Fix;
    use super::Cat;
    use super::Term;
    use super::{
        parse::{Parse, ParseError},
        Ident, Literal, LiteralError,
    };
    use std::iter;

    macro_rules! parse_indent {
        ($s:literal) => {
            assert_eq!(
                $s.parse(),
                Ok(Ident {
                    name: $s.to_owned()
                })
            )
        };
    }

    #[test]
    fn parse_ident_simple() {
        parse_indent!("x");
        parse_indent!("variable");
    }

    #[test]
    fn parse_indent_unicode() {
        parse_indent!("ℕꥇℜ");
    }

    #[test]
    fn parse_indent_stops() {
        let s = "variable then more text";
        let mut iter = s.chars().peekable();
        assert_eq!(
            Ident::parse(&mut iter),
            Ok(Ident {
                name: "variable".to_owned()
            })
        );
        assert_eq!(iter.collect::<String>(), " then more text");
    }

    #[test]
    fn parse_indent_not_empty() {
        assert_eq!("".parse::<Ident>(), Err(ParseError::EndOfFile));
    }

    #[test]
    fn parse_indent_alphabetic_only() {
        assert_eq!(
            "123".parse::<Ident>(),
            Err(ParseError::InvalidCharacter('1'))
        );
    }

    macro_rules! parse_lit {
        ($str:literal) => {
            assert_eq!(
                Literal::parse(
                    iter::once('\'')
                        .chain($str.escape_debug())
                        .chain(iter::once('\''),)
                        .peekable()
                ),
                Ok(Literal {
                    contents: $str.to_owned()
                }),
            )
        };
    }

    #[test]
    fn parse_literal_basic() {
        parse_lit!("hello");
        parse_lit!("this has whitespace");
    }

    #[test]
    fn parse_literal_quotes() {
        parse_lit!(r#"quote ' " and backslash \ quoting"#);
    }

    #[test]
    fn parse_literal_ascii_escape() {
        parse_lit!("newline \n tab \t carriage return \r");

        assert_eq!(
            r#"'\0'"#.parse(),
            Ok(Literal {
                contents: "\0".to_owned()
            })
        );
    }

    #[test]
    fn parse_literal_hex_escape() {
        assert_eq!(
            r#"'\x20'"#.parse(),
            Ok(Literal {
                contents: " ".to_owned()
            })
        );
    }

    #[test]
    fn parse_literal_unicode_escape() {
        parse_lit!("emoji ❤ escape \u{200c}");
    }

    #[test]
    fn parse_literal_stops() {
        let mut iter = "'hi there' Not quoted".chars().peekable();
        assert_eq!(
            Literal::parse(&mut iter),
            Ok(Literal {
                contents: "hi there".to_owned()
            })
        );
        assert_eq!(iter.collect::<String>(), " Not quoted");
    }

    #[test]
    fn parse_literal_needs_quotes() {
        assert_eq!(
            "hello".parse::<Literal>(),
            Err(ParseError::InvalidCharacter('h'))
        );
        assert_eq!("'hello".parse::<Literal>(), Err(ParseError::EndOfFile));
    }

    #[test]
    fn parse_literal_not_empty() {
        assert_eq!(
            "''".parse::<Literal>(),
            Err(ParseError::Other(LiteralError::EmptyLiteral))
        );
    }

    #[test]
    fn parse_literal_no_forbidden_whitespace() {
        assert_eq!(
            "'\n".parse::<Literal>(),
            Err(ParseError::InvalidCharacter('\n'))
        );
        assert_eq!(
            "'\r".parse::<Literal>(),
            Err(ParseError::InvalidCharacter('\r'))
        );
        assert_eq!(
            "'\t".parse::<Literal>(),
            Err(ParseError::InvalidCharacter('\t'))
        );
    }

    #[test]
    fn parse_literal_invalid_escape() {
        assert_eq!(
            r#"'\F"#.parse::<Literal>(),
            Err(ParseError::InvalidCharacter('F'))
        );
    }

    #[test]
    fn parse_literal_bad_hex_escape() {
        assert_eq!(
            r#"'\xF"#.parse::<Literal>(),
            Err(ParseError::InvalidCharacter('F'))
        );
        assert_eq!(
            r#"'\x0z"#.parse::<Literal>(),
            Err(ParseError::InvalidCharacter('z'))
        );
    }

    #[test]
    fn parse_literal_bad_unicode_escape() {
        // Negative tests
        assert_eq!(
            r#"'\u0"#.parse::<Literal>(),
            Err(ParseError::InvalidCharacter('0'))
        );
        assert_eq!(
            r#"'\u{}"#.parse::<Literal>(),
            Err(ParseError::InvalidCharacter('}'))
        );
        assert_eq!(
            r#"'\u{z"#.parse::<Literal>(),
            Err(ParseError::InvalidCharacter('z'))
        );
        assert_eq!(
            r#"'\u{_"#.parse::<Literal>(),
            Err(ParseError::InvalidCharacter('_'))
        );
        assert_eq!(
            r#"'\u{0___h"#.parse::<Literal>(),
            Err(ParseError::InvalidCharacter('h'))
        );
        assert_eq!(
            r#"'\u{D800}"#.parse::<Literal>(),
            Err(ParseError::Other(LiteralError::InvalidCharacterCode(
                0xd800
            )))
        );
        assert_eq!(
            r#"'\u{0000000"#.parse::<Literal>(),
            Err(ParseError::InvalidCharacter('0'))
        );
    }

    #[test]
    fn parse_fix_basic() {
        let expr = Expression {
            alt: Alt {
                seqs: vec![Cat {
                    terms: vec![Term::Epsilon],
                }],
            },
        };
        assert_eq!(
            "[x](())".parse(),
            Ok(Fix {
                arg: Ident {
                    name: "x".to_owned()
                },
                expr: expr.clone()
            })
        );
        assert_eq!(
            "[x](()    )".parse(),
            Ok(Fix {
                arg: Ident {
                    name: "x".to_owned()
                },
                expr: expr.clone()
            })
        );
        assert_eq!(
            "[x](    ())".parse(),
            Ok(Fix {
                arg: Ident {
                    name: "x".to_owned()
                },
                expr
            })
        );
    }

    #[test]
    fn parse_fix_needs_arg() {
        assert_eq!(
            "x]()".parse::<Fix>(),
            Err(ParseError::InvalidCharacter('x'))
        );
        assert_eq!(
            "[]()".parse::<Fix>(),
            Err(ParseError::InvalidCharacter(']'))
        );
    }

    #[test]
    fn parse_fix_needs_parens() {
        assert_eq!(
            "[x]x".parse::<Fix>(),
            Err(ParseError::InvalidCharacter('x'))
        );
        assert_eq!("[x](x".parse::<Fix>(), Err(ParseError::EndOfFile))
    }

    #[test]
    fn parse_fix_stops() {
        let mut iter = "[x](()) Not in fix".chars().peekable();
        assert_eq!(
            Fix::parse(&mut iter),
            Ok(Fix {
                arg: Ident {
                    name: "x".to_owned()
                },
                expr: Expression {
                    alt: Alt {
                        seqs: vec![Cat {
                            terms: vec![Term::Epsilon],
                        }],
                    },
                },
            })
        );

        assert_eq!(iter.collect::<String>(), " Not in fix");
    }
}
