use std::{collections::HashMap, convert::TryInto, fmt, ops::RangeInclusive};

use chewed::{IterWrapper, Parse, Parser, TakeError};
use criterion::{
    criterion_group, criterion_main, AxisScale, BenchmarkId, Criterion, PlotConfiguration,
    Throughput,
};

mod nibble;

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
        .parse::<nibble::Ast>()
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
