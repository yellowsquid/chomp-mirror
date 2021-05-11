use chewed::{IterWrapper, Parser};
use chomp::{
    chomp::ast::NamedExpression,
    nibble::{
        self,
        convert::{Context, Convert},
    },
};

fn chomp(input: &str) -> NamedExpression {
    syn::parse_str::<nibble::Statement>(&input)
        .unwrap()
        .convert(&mut Context::default())
        .unwrap()
}

fn autonibble(input: &str) -> NamedExpression {
    IterWrapper::new(input.chars())
        .parse::<autonibble::Ast>()
        .unwrap()
        .convert(&mut Context::default())
        .unwrap()
}

macro_rules! compare {
    ($name:ident, $file:literal) => {
        #[test]
        fn $name() {
            let input = include_str!($file);
            assert_eq!(chomp(input), autonibble(input))
        }
    };
}

compare!(compare_sheep, "nibble/sheep.nb");
compare!(compare_ratata, "nibble/ratata.nb");
compare!(compare_regex, "nibble/regex.nb");
compare!(compare_regex_fix, "nibble/regex_fix.nb");
compare!(compare_nibble, "nibble/nibble_exp.nb");
