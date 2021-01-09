chomp_macro::nibble! {
    let opt(x) = _ | x;
    let plus(x) = [rec](x . opt(rec));
    match plus(("r" | "t") . "a");
}

#[test]
fn ratata_ratata() {
    Ast::parse_str("ratata").unwrap();
}

#[test]
fn ratata_ratarataratatata() {
    Ast::parse_str("ratarataratatata").unwrap();
}
