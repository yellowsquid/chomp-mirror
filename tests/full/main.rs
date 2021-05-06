use std::error::Error;

use chomp::{
    chomp::{
        ast::substitute::Reduce,
        typed::{
            context::Context,
            lower::{Backend, GenerateCode},
            TypeInfer,
        },
        visit::Visitable,
    },
    lower::RustBackend,
    nibble::{
        convert::{self, Convert},
        Statement,
    },
};
use proc_macro2::{Span, TokenStream};

fn chomp(input: &str) -> Result<TokenStream, Box<dyn Error>> {
    syn::parse_str::<Statement>(&input)
        .map_err(|e| Box::new(e) as Box<dyn Error>)
        .and_then(|nibble: Statement| {
            nibble
                .convert(&mut convert::Context::default())
                .map_err(|e| Box::new(e) as Box<dyn Error>)
        })
        .and_then(|expr| {
            expr.fold(&mut Reduce)
                .map_err(|e| Box::new(e) as Box<dyn Error>)
        })
        .and_then(|term| {
            let mut context = Context::default();
            term.fold(&mut TypeInfer {
                context: &mut context,
            })
            .map_err(|e| Box::new(e) as Box<dyn Error>)
        })
        .map(|typed| {
            let mut backend = RustBackend::default();
            let id = typed.gen(&mut backend);
            backend.emit_code(None, Span::call_site(), id)
        })
}

macro_rules! compile {
    ($name:ident, $file:literal) => {
        #[test]
        fn $name() {
            let input = include_str!($file);
            chomp(input).unwrap();
        }
    };
}

compile!(compile_sheep, "nibble/sheep.nb");
compile!(compile_ratata, "nibble/ratata.nb");
compile!(compile_regex, "nibble/regex.nb");
compile!(compile_regex_fix, "nibble/regex_fix.nb");
compile!(compile_nibble, "nibble/nibble_exp.nb");
