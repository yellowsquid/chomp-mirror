chomp_macro::nibble! {
    let opt(x) = _ | x;
    let ws = [star](opt(" " . star));

    match [rec]("a" . opt("." . ws . rec));
}

#[test]
fn regex_fix_cat() {
    Ast::parse_str(r#"a. a"#).unwrap();
}
