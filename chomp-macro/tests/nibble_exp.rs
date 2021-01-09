chomp_macro::nibble! {
    let opt(x) = _ | x;
    let plus(x) = [plus](x . opt(plus));
    let star(x) = [star](opt(x . star));

    let Pattern_Whitespace = "\n"|" ";

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

    let literal_char = XID_Continue;

    let ws = star(Pattern_Whitespace);
    let must_ws = plus(Pattern_Whitespace);

    let punctuated(x, p) = [rec](x . opt(p . ws . rec));
    let list(x) = "(" . ws . [rec](x . opt("," . ws . opt(rec))) . ")";

    let epsilon = "_";
    let ident = XID_Start . star(XID_Continue);
    let literal = "\"" . plus(literal_char) . "\"";
    let parens(expr) = "(" . ws . expr . ")";
    let fix(expr) = "[" . ws . ident . ws . "]" . ws . parens(expr);

    let term(expr) =
          epsilon . ws
        | literal . ws
        | parens(expr) . ws
        | fix(expr) . ws
        | ident . ws . opt(list(expr) . ws)
        ;

    let cat(expr) = punctuated(term(expr), ".");
    let alt(expr) = punctuated(cat(expr), "|");
    let expr = [expr](alt(expr));
    match expr;
}

#[test]
fn exp_epsilon() {
    Ast::parse_str("_").unwrap();
}

#[test]
fn exp_literal() {
    Ast::parse_str(r#""foo""#).unwrap();
}

#[test]
fn exp_cat() {
    Ast::parse_str(r#""a" . "b""#).unwrap();
}

#[test]
fn exp_alt() {
    Ast::parse_str(r#""a" | "b""#).unwrap();
}

#[test]
fn exp_ident() {
    Ast::parse_str("foo").unwrap();
}

#[test]
fn exp_call() {
    Ast::parse_str(r#"opt("foo")"#).unwrap();
}

#[test]
fn exp_fix() {
    Ast::parse_str(r#"[rec](_ | "a" . rec)"#).unwrap();
}
