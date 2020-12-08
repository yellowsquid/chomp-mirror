use chomp::ast::InlineCall;
use chomp::{ast::TypeCheck, nibble::File};
use proc_macro2::Span;
use std::{
    error::Error,
    io::{self, Read, Write},
    process::exit,
};
use syn::Ident;

fn main() {
    let mut input = String::new();
    let res = io::stdin()
        .read_to_string(&mut input)
        .map_err(|e| Box::new(e) as Box<dyn Error>)
        .and_then(|_| syn::parse_str(&input).map_err(|e| Box::new(e) as Box<dyn Error>))
        .and_then(|nibble: File| {
            let (funs, goal) = nibble.convert();

            funs.into_iter()
                .try_rfold(goal, |goal, (name, term, args)| {
                    goal.fold(&mut InlineCall::new(name, term, args))
                })
                .map_err(|e| Box::new(e) as Box<dyn Error>)
        })
        .and_then(|term| {
            term.fold(&mut TypeCheck::new())
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
