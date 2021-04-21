use chewed::{IterWrapper, Parser};
use chomp::{chomp::ast::{Function, NamedExpression}, nibble};

fn chomp(input: &str) -> (Vec<Function>, NamedExpression) {
    syn::parse_str::<nibble::cst::File>(&input).unwrap().convert().unwrap()
}

fn autonibble(input: &str) -> (Vec<Function>, NamedExpression) {
    IterWrapper::new(input.chars()).parse::<autochomp::Ast>().unwrap().convert().unwrap()
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

compare!(compare_sheep, "sheep.nb");
compare!(compare_ratata, "ratata.nb");
compare!(compare_regex, "regex.nb");
compare!(compare_regex_fix, "regex_fix.nb");
compare!(compare_nibble, "nibble_exp.nb");
