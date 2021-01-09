chomp_macro::nibble! {
    let opt(x) = _ | x;
    let plus(x) = [plus](x . opt(plus));
    let star(x) = [star](opt(x . star));

    match plus("a") . star("b");
}

#[test]
fn regex_a() {
    Ast::parse_str("a").unwrap();
}

#[test]
fn regex_ab() {
    Ast::parse_str("ab").unwrap();
}

#[test]
fn regex_aa() {
    Ast::parse_str("aa").unwrap();
}

#[test]
fn regex_aaaabbb() {
    Ast::parse_str("aaaabbb").unwrap();
}
