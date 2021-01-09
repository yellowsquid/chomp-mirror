chomp_macro::nibble! {
    let opt(x) = _ | x;
    let plus(x) = [rec](x . opt(rec));
    match "ba" . plus("a");
}

#[test]
fn baa() {
    Ast::parse_str("baa").unwrap();
}

#[test]
fn baaaaaaaaaaaaaaa() {
    Ast::parse_str("baaaaaaaaaaaaaaa").unwrap();
}
