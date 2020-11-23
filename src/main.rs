use chomp::{
    ast::{
        convert::{Context, Convert},
        typed::{FlastContext, Type},
    },
    nibble::Expression,
};
use proc_macro2::Span;
use std::process::exit;
use std::{
    error::Error,
    io::{self, Read, Write},
};
use syn::Ident;

fn main() {
    let mut input = String::new();
    let res = io::stdin()
        .read_to_string(&mut input)
        .map_err(|e| Box::new(e) as Box<dyn Error>)
        .and_then(|_| syn::parse_str(&input).map_err(|e| Box::new(e) as Box<dyn Error>))
        .and_then(|nibble: Expression| {
            nibble
                .convert(&Context::new())
                .well_typed(&mut FlastContext::new())
                .map_err(|e| Box::new(e) as Box<dyn Error>)
        })
        .map(|(typed, _)| typed.emit_code(Ident::new("Ast", Span::call_site())))
        .and_then(|code| {
            write!(io::stdout(), "{:#}", code).map_err(|e| Box::new(e) as Box<dyn Error>)
        });

    if let Err(e) = res {
        eprintln!("{}", e);
        drop(e);
        exit(1)
    }
}
