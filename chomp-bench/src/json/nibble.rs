use super::{decode_pair, Value};

use std::{collections::HashMap, convert::TryInto, mem, ops::RangeInclusive};

use chomp_macro::nibble;

// Note: this is only an ASCII subset. Need to add character sets.
nibble! {
    let opt  some = _ : None | some;
    let plus iter = !(/rec/ iter . opt rec);
    let star iter = !(/rec/ opt (iter . rec));

    let ws_char = " " | "\t" | "\n" | "\r";
    let ws      = star ws_char;

    let sep iter = !(/rec/ iter . opt ("," . ws . rec));

    let digit_1_9 = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
    let digit     = "0" | digit_1_9;

    let unsigned_number =
          (("0" | digit_1_9 . star digit) : Int)
        . (opt ("." . plus digit) : Frac)
        . (opt (("e" | "E") . opt ("+" | "-") . plus digit) : Exp);
    let number =
          unsigned_number       : Positive
        | "-" . unsigned_number : Negative;

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
         "x" | "y" | "z" | "{" | "|" | "}" | "~"
        ) : Literal |
        "\\" . (
            ("\"" | "\\" | "/" | "b" | "f" | "n" | "r" | "t") : Ascii |
            "u" . hex . hex . hex . hex : Unicode
        ) : Escape;

    let string = "\"" . star char . "\"";

    let member value = (string : Key) . ws . ":" . ws . value . ws;
    let object value = "{" . ws . opt (sep (member value)) . "}";

    let array  value = "[" . ws . opt (sep (value . ws)) . "]";

    let value = !(/value/
          "true"       : True
        | "false"      : False
        | "null"       : Null
        | number       : Number
        | string       : String
        | object value : Object
        | array value  : Array
    );

    match !(/rec/ ws_char . rec | value . ws);
}

impl From<Ast> for Value {
    fn from(mut ast: Ast) -> Self {
        loop {
            match ast {
                Ast::Branch1(cat) => ast = *cat.rec1,
                Ast::Branch2(cat) => return cat.value1.into(),
            }
        }
    }
}

impl From<Value1> for Value {
    fn from(value: Value1) -> Self {
        match value {
            Value1::Null1(_) => Self::Null,
            Value1::True1(_) => Self::Bool(true),
            Value1::False1(_) => Self::Bool(false),
            Value1::Number1(n) => Self::Number(n.into()),
            Value1::String1(s) => Self::String(s.into()),
            Value1::Object1(o) => Self::Object(o.into()),
            Value1::Array1(a) => Self::Array(a.into()),
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

impl Iterator for Opt5 {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        fn next(star: &mut Opt5) -> Option<Char1> {
            match std::mem::replace(star, Opt5::None1(Epsilon)) {
                Opt5::None1(_) => None,
                Opt5::Some1(some) => {
                    *star = *some.rec1;
                    Some(some.char1)
                }
            }
        }

        fn decode(u: Unicode1) -> u16 {
            let chars: [char; 4] = [u.hex1.into(), u.hex2.into(), u.hex3.into(), u.hex4.into()];
            u16::from_str_radix(&chars.iter().collect::<String>(), 16).unwrap()
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
        object
            .opt1
            .into_iter()
            .map(|m| (m.key1.into(), (*m.value1).into()))
            .collect()
    }
}

impl IntoIterator for Opt8 {
    type Item = Member1;

    type IntoIter = Opt7;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::None1(_) => Opt7::None1(Epsilon),
            Self::Sep1(sep) => Opt7::Some1(Some8 {
                part1: Lit75,
                ws1: Ws1::None1(Epsilon),
                rec1: Box::new(sep),
            }),
        }
    }
}

impl Iterator for Opt7 {
    type Item = Member1;

    fn next(&mut self) -> Option<Self::Item> {
        let orig = mem::replace(self, Self::None1(Epsilon));
        match orig {
            Opt7::None1(_) => None,
            Opt7::Some1(some) => {
                *self = some.rec1.opt1;
                Some(some.rec1.member1)
            }
        }
    }
}

impl From<Array1> for Vec<Value> {
    fn from(array: Array1) -> Self {
        array.opt1.into_iter().map(Value::from).collect()
    }
}

impl IntoIterator for Opt10 {
    type Item = Value1;

    type IntoIter = Opt9;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::None1(_) => Opt9::None1(Epsilon),
            Self::Sep1(sep) => Opt9::Some1(Some9 { part1: Lit75, ws1: Ws1::None1(Epsilon), rec1: Box::new(sep)})
        }
    }
}

impl Iterator for Opt9 {
    type Item = Value1;

    fn next(&mut self) -> Option<Self::Item> {
        let orig = mem::replace(self, Self::None1(Epsilon));
        match orig {
            Opt9::None1(_) => None,
            Opt9::Some1(some) => {
                *self = some.rec1.opt1;
                Some(*some.rec1.iter1.value1)
            }
        }
    }
}

// impl IntoIterator for Sep2 {
//     type Item = X1;

//     type IntoIter = Sep2Iter;

//     fn into_iter(self) -> Self::IntoIter {
//         Sep2Iter(Some(self))
//     }
// }

// pub struct Sep2Iter(Option<Sep2>);

// impl Iterator for Sep2Iter {
//     type Item = X1;

//     fn next(&mut self) -> Option<Self::Item> {
//         let inner = self.0.take()?.0;
//         let res = inner.x1;
//         self.0 = match inner.opt1 {
//             Opt9::None1(_) => None,
//             Opt9::Some1(s) => Some(*s.rec1),
//         };
//         Some(res)
//     }
// }
