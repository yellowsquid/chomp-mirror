use std::error::Error;

use chewed::{IterWrapper, Parser};
use chomp::{
    chomp::ast::{Function, NamedExpression},
    nibble,
};
use criterion::{
    criterion_group, criterion_main, AxisScale, BenchmarkId, Criterion, PlotConfiguration,
    Throughput,
};

const INPUTS: &[&str] = &[
    r##"match _ : None | "a" : Some;"##,
    r##"let opt(x) = _ : None | "a" : Some;
    match [plus](("a" : First) . (opt(plus) : Next));"##,
    r##"let opt(x) = _ : None | x : Some;
    let plus(x) = [plus]((x : First) . (opt(plus) : Next));
    let star(x) = opt(plus(x));
    match [rec]("a" : Base | "b" . rec : Step);"##,
    r##"let opt(x) = _ : None | x : Some;
    let plus(x) = [plus]((x : First) . (opt(plus) : Next));
    let star(x) = opt(plus(x));
    let star_(base, step) = [rec](base : Base | step . rec : Step);
    let Pattern_Whitespace = "\t"|"\n"|"\x0B"|"\x0c"|"\r"|" "|"\u{85}"|"\u{200e}"|"\u{200f}"|"\u{2028}"|"\u{2029}";
    let oct_digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" ;
    let digit     = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
    match "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";"##,
    r##"let opt(x) = _ : None | x : Some;
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
        "y" | "z" ;
    match "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" ;"##,
    r##"let opt(x) = _ : None | x : Some;
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
         "`" | "a" | "b" | "c" | "d" | "e" | "f" | "g" );
    match "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" ;"##,
    r##"let opt(x) = _ : None | x : Some;
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
    match star_(star_(goal : Goal, let : Let), Pattern_Whitespace);"##,
];

fn parse_autochomp(input: &str) -> Result<(Vec<Function>, NamedExpression), Box<dyn Error>> {
    IterWrapper::new(input.chars())
        .parse::<autochomp::Ast>()
        .map_err(|e| Box::new(e) as Box<dyn Error>)
        .and_then(|ast| ast.convert().map_err(|e| Box::new(e) as Box<dyn Error>))
}

fn parse_chomp(input: &str) -> Result<(Vec<Function>, NamedExpression), Box<dyn Error>> {
    syn::parse_str::<nibble::cst::File>(input)
        .map_err(|e| Box::new(e) as Box<dyn Error>)
        .and_then(|file| file.convert().map_err(|e| Box::new(e) as Box<dyn Error>))
}

fn bench_parse(c: &mut Criterion) {
    let plot_config = PlotConfiguration::default().summary_scale(AxisScale::Logarithmic);
    let mut group = c.benchmark_group("Parse");
    group.plot_config(plot_config);
    for (i, input) in INPUTS.iter().enumerate() {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::new("Chomp", i), *input, |b, i| {
            b.iter(|| parse_chomp(i))
        });
        group.bench_with_input(BenchmarkId::new("AutoChomp", i), *input, |b, i| {
            b.iter(|| parse_autochomp(i))
        });
    }
}

criterion_group!(benches, bench_parse);
criterion_main!(benches);
