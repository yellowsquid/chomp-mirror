use std::{
    error::Error,
    io::{self, Read, Write},
    process::exit,
};

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
use proc_macro2::Span;

fn main() {
    let mut input = String::new();
    let res = io::stdin()
        .read_to_string(&mut input)
        .map_err(|e| Box::new(e) as Box<dyn Error>)
        .and_then(|_| syn::parse_str(&input).map_err(|e| Box::new(e) as Box<dyn Error>))
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
        .and_then(|code| {
            write!(io::stdout(), "{:#}", code).map_err(|e| Box::new(e) as Box<dyn Error>)
        });

    if let Err(e) = res {
        eprintln!("{}", e);
        exit(1)
    }
}
