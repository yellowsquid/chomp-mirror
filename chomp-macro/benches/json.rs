use chomp_macro::nibble;
use std::{collections::HashMap, convert::TryInto, mem, ops::RangeInclusive};

enum Value {
    True,
    False,
    Null,
    Number(f64),
    String(String),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
}

// Note: this is only an ASCII subset. Need to add character sets.
nibble! {
    let opt(x) = _ : None | x : Some;
    let plus(x) = [rec](x . opt(rec));
    let star(x) = [rec](opt(x . rec));
    let sep(x, p) = [rec](x . opt(p . rec));

    let ws_char = " " | "\t" | "\n" | "\r";
    let ws = star(ws_char);
    let digit_1_9 = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
    let digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";

    let unsigned_number = (("0" | digit_1_9 . star(digit)) : Int)
        . (opt("." . plus(digit)) : Frac)
        . (opt(("e" | "E") . opt("+" | "-") . plus(digit)) : Exp);
    let number = unsigned_number : Positive | "-" . unsigned_number : Negative;

    let hex =
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" |
        "a" | "b" | "c" | "d" | "e" | "f" |
        "A" | "B" | "C" | "D" | "E" | "F" ;
    let char =
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
        ("\"" | "\\" | "/" | "b" | "f" | "n" | "r" | "t") : Ascii |
        "u" . hex . hex . hex . hex : Unicode
    ) : Escape;

    let string = "\"" . star(char) . "\"";

    let member(value) = (string : Key) . ws . ":" . ws . (value : Value) . ws;
    let object(value) = "{" . ws . opt(sep(member(value), "," . ws)) . "}";

    let array(value) = "[" . ws . opt(sep(value . ws, "," . ws)) . "]";

    let value = [value](
        "true" : True |
        "false" : False |
        "null" : Null |
        number : Number |
        string : String |
        object(value) : Object |
        array(value) : Array
    );

    match [rec](ws_char . rec | value . ws);
}

impl From<Ast> for Value {
    fn from(mut ast: Ast) -> Self {
        loop {
            match ast.0 {
                Alt184::Branch1(cat) => ast = *cat.rec1,
                Alt184::Branch2(cat) => return cat.value1.into(),
            }
        }
    }
}

impl From<Value1> for Value {
    fn from(value: Value1) -> Self {
        match value.0 {
            Alt182::True1(_) => Self::True,
            Alt182::False1(_) => Self::False,
            Alt182::Null1(_) => Self::Null,
            Alt182::Number1(n) => Self::Number(n.into()),
            Alt182::String1(s) => Self::String(s.into()),
            Alt182::Object1(o) => Self::Object(o.into()),
            Alt182::Array1(a) => Self::Array(a.into()),
        }
    }
}

impl From<Number1> for f64 {
    fn from(number: Number1) -> Self {
        number.to_string().parse().unwrap()
    }
}

impl From<String1> for String {
    fn from(string: String1) -> Self {
        string.star1.into_iter().collect()
    }
}

impl Iterator for Star2 {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        fn next(star: &mut Star2) -> Option<Char1> {
            match mem::replace(star, Star2(Opt9::None1(Epsilon))).0 {
                Opt9::None1(_) => None,
                Opt9::Some1(s) => {
                    *star = *s.rec1;
                    Some(s.char1)
                }
            }
        }

        fn decode(u: Unicode1) -> u16 {
            let chars: [char; 4] = [u.hex1.into(), u.hex2.into(), u.hex3.into(), u.hex4.into()];
            u16::from_str_radix(&chars.iter().collect::<String>(), 16).unwrap()
        }

        fn decode_pair(one: u16, other: u16) -> u32 {
            // Ranges are confusingly backwards
            const LOW_SURROGATE_RANGE: RangeInclusive<u16> = 0xDC00..=0xDFFF;
            const HIGH_SURROGATE_RANGE: RangeInclusive<u16> = 0xD800..=0xDBFF;

            let (low, high) = if LOW_SURROGATE_RANGE.contains(&one) {
                assert!(HIGH_SURROGATE_RANGE.contains(&other));
                (one, other)
            } else {
                assert!(LOW_SURROGATE_RANGE.contains(&other));
                assert!(HIGH_SURROGATE_RANGE.contains(&one));
                (other, one)
            };

            u32::from(high - 0xD800) * 0x400 + u32::from(low - 0xDC00) + 0x10000
        }

        const SURROGATE_PAIR_RANGE: RangeInclusive<u16> = 0xD800..=0xDFFF;

        match next(self) {
            None => None,
            Some(Char1::Literal1(l)) => Some(l.into()),
            Some(Char1::Escape1(e)) => match e.1 {
                Alt148::Ascii1(Ascii1::Branch1(_)) => Some('\"'),
                Alt148::Ascii1(Ascii1::Branch2(_)) => Some('\\'),
                Alt148::Ascii1(Ascii1::Branch3(_)) => Some('/'),
                Alt148::Ascii1(Ascii1::Branch4(_)) => Some('\x08'),
                Alt148::Ascii1(Ascii1::Branch5(_)) => Some('\x0c'),
                Alt148::Ascii1(Ascii1::Branch6(_)) => Some('\n'),
                Alt148::Ascii1(Ascii1::Branch7(_)) => Some('\r'),
                Alt148::Ascii1(Ascii1::Branch8(_)) => Some('\t'),
                Alt148::Unicode1(u) => {
                    let codepoint = decode(u);

                    if SURROGATE_PAIR_RANGE.contains(&codepoint) {
                        let other = if let Some(Char1::Escape1(e)) = next(self) {
                            if let Alt148::Unicode1(u) = e.1 {
                                decode(u)
                            } else {
                                panic!()
                            }
                        } else {
                            panic!()
                        };

                        assert!(SURROGATE_PAIR_RANGE.contains(&other));

                        Some(decode_pair(codepoint, other).try_into().unwrap())
                    } else {
                        Some(u32::from(codepoint).try_into().unwrap())
                    }
                }
            },
        }
    }
}

impl From<Object1> for HashMap<String, Value> {
    fn from(object: Object1) -> Self {
        match object.opt1 {
            Opt12::None1(_) => HashMap::new(),
            Opt12::Sep1(s) => s
                .into_iter()
                .map(|m| (m.string1.into(), (*m.value1).into()))
                .collect(),
        }
    }
}

impl IntoIterator for Sep1 {
    type Item = Member1;

    type IntoIter = Sep1Iter;

    fn into_iter(self) -> Self::IntoIter {
        Sep1Iter(Some(self))
    }
}

pub struct Sep1Iter(Option<Sep1>);

impl Iterator for Sep1Iter {
    type Item = Member1;

    fn next(&mut self) -> Option<Self::Item> {
        let inner = self.0.take()?.0;
        let res = inner.member1;
        self.0 = match inner.opt1 {
            Opt11::None1(_) => None,
            Opt11::Some1(s) => Some(*s.rec1),
        };
        Some(res)
    }
}

impl From<Array1> for Vec<Value> {
    fn from(array: Array1) -> Self {
        match array.opt1 {
            Opt14::None1(_) => Vec::new(),
            Opt14::Sep1(s) => s.into_iter().map(|x| (*x.value1).into()).collect(),
        }
    }
}

impl IntoIterator for Sep2 {
    type Item = X1;

    type IntoIter = Sep2Iter;

    fn into_iter(self) -> Self::IntoIter {
        Sep2Iter(Some(self))
    }
}

pub struct Sep2Iter(Option<Sep2>);

impl Iterator for Sep2Iter {
    type Item = X1;

    fn next(&mut self) -> Option<Self::Item> {
        let inner = self.0.take()?.0;
        let res = inner.x1;
        self.0 = match inner.opt1 {
            Opt13::None1(_) => None,
            Opt13::Some1(s) => Some(*s.rec1),
        };
        Some(res)
    }
}
