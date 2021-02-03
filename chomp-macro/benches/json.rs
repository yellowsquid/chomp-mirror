use std::{collections::HashMap, convert::TryInto, fmt, mem, ops::RangeInclusive};

use chewed::{Parse, TakeError};
use chomp_macro::nibble;
use criterion::{
    criterion_group, criterion_main, AxisScale, BenchmarkId, Criterion, PlotConfiguration,
    Throughput,
};

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

#[derive(Debug)]
enum Value {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn write_str(s: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "\"")?;
            for c in s.chars() {
                match c {
                    '\x20'..='\x21' | '\x23'..='\x5B' | '\x5D'..='\u{10FFFF}' => write!(f, "{}", c),
                    '\x22' => write!(f, r#"\""#),
                    '\x5C' => write!(f, r#"\\"#),
                    // '\x2F' => write!(f, r#"\/"#),
                    '\x08' => write!(f, r#"\b"#),
                    '\x0C' => write!(f, r#"\f"#),
                    '\x0A' => write!(f, r#"\n"#),
                    '\x0D' => write!(f, r#"\r"#),
                    '\x09' => write!(f, r#"\t"#),
                    _ => {
                        let codepoint = u32::from(c) - 0x10000;
                        let high: u16 = (codepoint / 0x400 + 0xD800).try_into().unwrap();
                        let low: u16 = (codepoint % 0x400 + 0xDC00).try_into().unwrap();
                        write!(f, r#"\u{:04X}\u{:04X}"#, high, low)
                    }
                }?;
            }
            write!(f, "\"")
        }

        match self {
            Self::Null => write!(f, "null"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Number(n) => write!(f, "{}", n),
            Self::String(s) => write_str(s, f),
            Self::Array(a) => {
                write!(f, "[")?;
                let mut iter = a.iter();
                if let Some(last) = iter.next_back() {
                    for val in iter {
                        write!(f, "{}, ", val)?;
                    }

                    write!(f, "{}", last)?;
                }
                write!(f, "]")
            }
            Self::Object(o) => {
                '{'.fmt(f)?;
                let mut iter = o.iter();
                if let Some((last_key, last_val)) = iter.next() {
                    for (key, val) in iter {
                        write_str(key, f)?;
                        write!(f, " : {}, ", val)?;
                    }

                    write_str(last_key, f)?;
                    write!(f, " : {}", last_val)?;
                }
                '}'.fmt(f)
            }
        }
    }
}

impl Parse for Value {
    fn take<P: chewed::Parser + ?Sized>(input: &mut P) -> Result<Self, TakeError> {
        const WS: &[char] = &[' ', '\t', '\n', '\r'];
        const FIRST: &[char] = &[
            't', 'f', 'n', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-', '"', '[', '{',
        ];
        const FIRST_WS: &[char] = &[
            't', 'f', 'n', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-', '"', '[', '{',
            ' ', '\t', '\n', '\r',
        ];

        fn skip_ws<P: chewed::Parser + ?Sized>(input: &mut P) {
            input.skip_while(|c| WS.contains(&c))
        }

        fn parse_u16<P: chewed::Parser + ?Sized>(input: &mut P) -> Result<u16, TakeError> {
            const HEX: &[char] = &[
                '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
                'A', 'B', 'C', 'D', 'E', 'F',
            ];
            let mut chars = ['\0'; 4];
            input.take_chars_from(HEX, &mut chars)?;
            let mut out = 0;
            for c in &chars {
                match c {
                    '0' => out *= 16,
                    '1' => out *= 16 + 1,
                    '2' => out *= 16 + 2,
                    '3' => out *= 16 + 3,
                    '4' => out *= 16 + 4,
                    '5' => out *= 16 + 5,
                    '6' => out *= 16 + 6,
                    '7' => out *= 16 + 7,
                    '8' => out *= 16 + 8,
                    '9' => out *= 16 + 9,
                    'a' | 'A' => out *= 16 + 10,
                    'b' | 'B' => out *= 16 + 11,
                    'c' | 'C' => out *= 16 + 12,
                    'd' | 'D' => out *= 16 + 13,
                    'e' | 'E' => out *= 16 + 14,
                    'f' | 'F' => out *= 16 + 15,
                    _ => unreachable!(),
                };
            }
            Ok(out)
        }

        fn parse_str<P: chewed::Parser + ?Sized>(input: &mut P) -> Result<String, TakeError> {
            input.consume_str("\"")?;
            let mut s = String::new();
            loop {
                match input
                    .next()
                    .ok_or_else(|| TakeError::EndOfStream(input.pos()))?
                {
                    '"' => return Ok(s),
                    c @ '\x20'..='\x21' | c @ '\x23'..='\x5B' | c @ '\x5D'..='\u{10FFFF}' => {
                        s.push(c)
                    }
                    '\\' => {
                        match input
                            .next()
                            .ok_or_else(|| TakeError::EndOfStream(input.pos()))?
                        {
                            '\"' => s.push('\x22'),
                            '\\' => s.push('\x5C'),
                            '/' => s.push('\x2F'),
                            'b' => s.push('\x08'),
                            'f' => s.push('\x0C'),
                            'n' => s.push('\x0A'),
                            'r' => s.push('\x0D'),
                            't' => s.push('\x09'),
                            'u' => {
                                let v = parse_u16(input)?;
                                let codepoint = if (0xD800..=0xDFFF).contains(&v) {
                                    input.consume_str(r#"\u"#)?;
                                    let other = parse_u16(input)?;
                                    decode_pair(v, other)
                                } else {
                                    u32::from(v)
                                };
                                s.push(codepoint.try_into().unwrap())
                            }
                            c => return Err(TakeError::BadBranch(input.pos(), c, todo!())),
                        }
                    }
                    c => return Err(TakeError::BadBranch(input.pos(), c, todo!())),
                }
            }
        }

        skip_ws(input);

        let res = match input
            .peek()
            .ok_or_else(|| TakeError::EndOfStream(input.pos()))?
        {
            'n' => input.consume_str("null").map(|_| Value::Null),
            't' => input.consume_str("true").map(|_| Value::Bool(true)),
            'f' => input.consume_str("false").map(|_| Value::Bool(false)),
            '0'..='9' | '-' => {
                let mut s = String::new();
                s.push(input.next().unwrap());
                while input.peek().map_or(false, |c| {
                    matches!(c, '0'..='9' | '+' | '-' | '.' | 'e' | 'E')
                }) {
                    s.push(input.next().unwrap());
                }
                s.parse().map(Value::Number).map_err(|_| todo!())
            }
            '"' => parse_str(input).map(Value::String),
            '[' => {
                const ARRAY_TAIL: &[char] = &[',', ']'];
                input.consume_str("[")?;
                let a = input
                    .iter_strict(Self::take, ',', ']', ARRAY_TAIL, FIRST_WS)
                    .collect::<Result<_, _>>()
                    .map(Value::Array)?;
                input.consume_str("]")?;
                Ok(a)
            }
            '{' => {
                const OBJECT_TAIL: &[char] = &[',', '}'];
                input.consume_str("{")?;
                let o = input
                    .iter_strict(
                        |p| {
                            skip_ws(p);
                            let key = parse_str(p)?;
                            skip_ws(p);
                            p.consume_str(":")?;
                            p.take().map(|val| (key, val))
                        },
                        ',',
                        '}',
                        OBJECT_TAIL,
                        &['"', ' ', '\t', '\n', '\r'],
                    )
                    .collect::<Result<_, _>>()
                    .map(Value::Object)?;
                input.consume_str("}")?;
                Ok(o)
            }
            c => Err(TakeError::BadBranch(input.pos(), c, FIRST)),
        }?;

        skip_ws(input);
        Ok(res)
    }

    // fn from_str(s: &str) -> Result<Self, Self::Err> {
    //     const WS: &[char] = &[' ', '\t', '\n', '\r'];

    //     let mut chars = s.chars();

    //     let val = match chars.find(|c| !WS.contains(c)) {
    //         None => Err(()),
    //         Some('t') => ,
    //     };
    // }
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
            Alt182::Null1(_) => Self::Null,
            Alt182::True1(_) => Self::Bool(true),
            Alt182::False1(_) => Self::Bool(false),
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

const INPUTS: &[&str] = &[
    r#"true"#,
    r#"[true, false]"#,
    r#"{"first" : null, "second" : 123}"#,
    r#"{"first": [ true, "Hello there" ], "second": [123, -12.4e-7]}"#,
    r#"{"first": [ true, "Hello there" ], "second": [123, -12.4e-7], "third": {"left": "Random text", "right": ["\ud83c\udf24\ufe0f"]}}"#,
];

fn parse_chewed(input: &str) -> Value {
    IterWrapper::new(input.chars())
        .parse::<Ast>()
        .unwrap()
        .into()
}

fn parse_handwritten(input: &str) -> Value {
    IterWrapper::new(input.chars()).parse().unwrap()
}

fn bench_parse(c: &mut Criterion) {
    let plot_config = PlotConfiguration::default().summary_scale(AxisScale::Logarithmic);
    let mut group = c.benchmark_group("JSON");
    group.plot_config(plot_config);
    for (i, input) in INPUTS.iter().enumerate() {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::new("Chewed", i), *input, |b, i| {
            b.iter(|| parse_chewed(i))
        });
        group.bench_with_input(BenchmarkId::new("Handwritten", i), *input, |b, i| {
            b.iter(|| parse_handwritten(i))
        });
    }
}

criterion_group!(benches, bench_parse);
criterion_main!(benches);
