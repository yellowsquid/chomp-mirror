chomp_macro::nibble! {
    let opt(x) = _ | x;
    let plus(x) = [plus](x . opt(plus));
    let star(x) = opt(plus(x));
    let star_(base, step) = [rec](base | step . rec);

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
        " " | "!"       | "#" | "$" | "%" | "&" | "'" |
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
        "x" | "y" | "z" | "{" | "|" | "}" | "~" |
        "\\" . ("\"" | "'" | "n" | "r" | "t" | "\\" | "0" |
                "x" . oct_digit . hex_digit |
                "u{" . hex_digit . opt(hex_digit . opt(hex_digit . opt(hex_digit . opt(hex_digit . opt(hex_digit))))) . "}");

    let ws = plus(Pattern_Whitespace);

    let punctuated(x, p) = [rec](x . opt(p . opt(ws) . rec));
    let list(x) = "(" . opt(ws) . [rec](x . opt("," . opt(ws) . opt(rec))) . ")";

    let epsilon = "_";
    let ident = XID_Start . star(XID_Continue);
    let literal = "\"" . plus(literal_char) . "\"";
    let parens(expr) = "(" . opt(ws) . expr . ")";
    let fix(expr) = "[" . opt(ws) . ident . opt(ws) . "]" . opt(ws) . parens(expr);

    let term(expr) =
          epsilon . opt(ws)
        | literal . opt(ws)
        | parens(expr) . opt(ws)
        | fix(expr) . opt(ws)
        | ident . opt(ws) . opt(list(expr) . opt(ws))
        ;

    let cat(expr) = punctuated(term(expr), ".");
    let alt(expr) = punctuated(cat(expr), "|");
    let expr = [expr](alt(expr));
    let let = "let" . ws . ident . opt(ws) . opt(list(ident . opt(ws)) . opt(ws)) . "=" . opt(ws) . expr . ";" . opt(ws);
    let goal = "match" . ws . expr . ";" . opt(ws);

    match star_(star_(goal, let), Pattern_Whitespace);
}
